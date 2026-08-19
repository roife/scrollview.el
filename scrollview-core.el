;;; scrollview-core.el --- Core rendering and modes for scrollview -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Internal module for scrollview.el.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'scrollview-custom)
(require 'scrollview-faces)

(autoload 'scrollview--initialize-builtins "scrollview-signs")

(defvar-local scrollview-mode nil
  "Non-nil when `scrollview-mode' is enabled.")

(defconst scrollview--scrollbar-priority 0
  "Priority of the scrollbar when it conflicts with signs.")

(defconst scrollview--overlay-priority 1000
  "Overlay priority used for rendered scrollview indicators.")

(defvar-local scrollview-margin--saved-area nil
  "Saved `scrollview-area' state before `scrollview-margin-local-mode'.")

;;; Internal state

(cl-defstruct (scrollview--sign-spec
               (:constructor scrollview--make-sign-spec))
  id group variant priority bitmap face collector)

(defun scrollview--make-window-table ()
  "Return an eq hash table whose window keys are weak."
  (make-hash-table :test #'eq :weakness 'key))

(defvar scrollview--window-overlays (scrollview--make-window-table)
  "Hash table mapping windows to their scrollview overlays.")

(defvar scrollview--window-overlay-pools (scrollview--make-window-table)
  "Hash table mapping windows to detached reusable overlays.")

(defvar scrollview--window-margins (scrollview--make-window-table)
  "Hash table mapping windows to margins saved before scrollview changed them.")

(defvar scrollview--pending-windows (scrollview--make-window-table)
  "Hash table of windows queued for refresh.")

(defvar scrollview--pending-all nil
  "Non-nil means the next scheduled refresh should update all windows.")

(defvar scrollview--refresh-timer nil
  "Idle timer used to debounce scrollview refreshes.")

(defvar scrollview--scroll-refresh-timers (scrollview--make-window-table)
  "Hash table mapping windows to their pending throttled scroll timers.")

(defvar scrollview--last-selected-window nil
  "Selected window observed by `scrollview--post-command'.")

(defvar scrollview--global-hooks-installed nil
  "Non-nil when global refresh hooks have been installed.")

(defvar scrollview--builtins-initialized nil
  "Non-nil after built-in sign groups have been registered.")

(defvar scrollview--refreshing nil
  "Non-nil while scrollview is rebuilding overlays.")

(defvar scrollview--sign-groups (make-hash-table :test #'eq)
  "Hash table mapping sign group symbols to enabled state.")

(defvar scrollview--sign-specs (make-hash-table :test #'eql)
  "Hash table mapping sign specification ids to sign specs.")

(defvar scrollview--window-sign-cache (scrollview--make-window-table)
  "Hash table mapping windows to cached sign items.")

(defvar scrollview--window-sign-row-cache (scrollview--make-window-table)
  "Hash table mapping windows to cached sign-to-row candidates.
The expensive mapping and priority reduction only depends on the identity of
the cached sign item list and on track geometry, so scrolling can reuse it.")

(defvar scrollview--sign-cache-generation 0
  "Generation number for invalidating cached sign items.")

(defvar scrollview--next-sign-id 0
  "Next sign specification id.")

(defvar scrollview--last-search-pattern nil
  "Last isearch pattern used by retained search signs.")

(defvar scrollview--last-search-regexp nil
  "Non-nil when `scrollview--last-search-pattern' is a regexp.")

(defvar-local scrollview--search-cache nil
  "Buffer-local cache for built-in search signs.")

(defvar-local scrollview--collector-cache nil
  "Buffer-local cache used by built-in sign collectors.")

(defvar-local scrollview--spell-state-generation 0
  "Buffer-local generation incremented after known spelling updates.")

(defvar-local scrollview--diagnostic-state-generation 0
  "Buffer-local generation incremented after diagnostics updates.")

(defvar-local scrollview--line-count-cache nil
  "Buffer-local cache of the current line count.")

(defvar-local scrollview--line-change-state nil
  "Line-count state captured by `scrollview--before-change'.")

(defvar-local scrollview--top-line-cache nil
  "Buffer-local cache of (TICK START . LINE) for `scrollview--window-top-line'.
TICK is `buffer-chars-modified-tick', START is a buffer position, and LINE is
the one-based line number at START.  When the buffer is unmodified we can
compute the line number for a different START by scanning only the delta
between START values instead of rescanning from `point-min'.")

(defvar scrollview--window-render-state (scrollview--make-window-table)
  "Hash table mapping windows to their last rendered state.")

(defvar scrollview--display-string-cache (make-hash-table :test #'equal)
  "Cache of immutable overlay display strings.")

(defvar scrollview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [left-fringe mouse-1] #'scrollview-click)
    (define-key map [right-fringe mouse-1] #'scrollview-click)
    (define-key map [left-margin mouse-1] #'scrollview-click)
    (define-key map [right-margin mouse-1] #'scrollview-click)
    map)
  "Keymap for `scrollview-mode'.")


;;; Utilities

(defun scrollview--all-windows ()
  "Return all non-minibuffer live windows on all frames."
  (let (windows)
    (walk-windows (lambda (window)
                    (unless (window-minibuffer-p window)
                      (push window windows)))
                  'no-minibuf t)
    (nreverse windows)))

(defun scrollview--line-count ()
  "Return the current buffer's line count."
  (let ((tick (buffer-chars-modified-tick)))
    (if (and scrollview--line-count-cache
             (= (car scrollview--line-count-cache) tick))
        (cdr scrollview--line-count-cache)
      (let ((count (max 1 (line-number-at-pos (point-max) t))))
        (setq scrollview--line-count-cache (cons tick count))
        count))))

(defun scrollview--count-newlines (start end)
  "Return the number of newline characters between START and END."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char start)
      (let ((count 0))
        (while (search-forward "\n" end t)
          (cl-incf count))
        count))))

(defun scrollview--before-change (start end)
  "Capture cached line-count information before changing START through END."
  (let ((tick (buffer-chars-modified-tick)))
    (setq scrollview--line-change-state
          (when (and scrollview--line-count-cache
                     (= tick (car scrollview--line-count-cache)))
            (vector (cdr scrollview--line-count-cache)
                    (scrollview--count-newlines start end))))))

(defun scrollview--update-line-count-after-change (start end)
  "Update the cached line count after newly inserted text START through END."
  (when scrollview--line-change-state
    (let ((count (+ (aref scrollview--line-change-state 0)
                    (- (scrollview--count-newlines start end)
                       (aref scrollview--line-change-state 1)))))
      (setq scrollview--line-count-cache
            (cons (buffer-chars-modified-tick) (max 1 count)))))
  (setq scrollview--line-change-state nil))

(defun scrollview--collector-cache ()
  "Return the current buffer's collector cache."
  (unless (hash-table-p scrollview--collector-cache)
    (setq scrollview--collector-cache (make-hash-table :test #'equal)))
  scrollview--collector-cache)

(defun scrollview--cached-collector-value (key token collector)
  "Return cached KEY value for TOKEN, or compute it with COLLECTOR."
  (let* ((cache (scrollview--collector-cache))
         (entry (gethash key cache)))
    (if (and entry (equal token (plist-get entry :token)))
        (plist-get entry :value)
      (let ((value (funcall collector)))
        (puthash key (list :token token :value value) cache)
        value))))

(defun scrollview--dedupe-sorted-lines (lines)
  "Return sorted unique integer LINES."
  (seq-uniq (sort (seq-filter #'integerp lines) #'<) #'=))

(defun scrollview--make-line-tracker ()
  "Return mutable state for monotonically increasing position lookups."
  (cons (point-min) (line-number-at-pos (point-min) t)))

(defun scrollview--tracked-line-number (position tracker)
  "Return the absolute line at POSITION and advance mutable TRACKER.
POSITION values must be supplied in nondecreasing order.  Unlike repeated
calls to `line-number-at-pos', this scans each intervening buffer region at
most once."
  (let ((line-start
         (save-excursion
           (goto-char position)
           (line-beginning-position))))
    (unless (= line-start (car tracker))
      (setcdr tracker (+ (cdr tracker)
                         (count-lines (car tracker) line-start)))
      (setcar tracker line-start))
    (cdr tracker)))

(defun scrollview--clamp-lines (lines buffer-lines)
  "Clamp LINES to the one-based range of BUFFER-LINES."
  (scrollview--dedupe-sorted-lines
   (cl-loop for line in lines
            when (integerp line)
            collect (min buffer-lines (max 1 line)))))

(defun scrollview--window-line-height (window)
  "Return WINDOW body height in screen lines."
  (max 1 (truncate (window-body-height window))))

(defun scrollview--window-top-line (window)
  "Return the line number at WINDOW's start.
Uses a buffer-local cache to avoid the O(N) newline scan from `point-min'
that `line-number-at-pos' performs.  When the buffer is unmodified since the
previous call we walk only the delta between the cached and current
positions, which during scrolling is typically a handful of newlines."
  (let ((raw-start (window-start window)))
    (with-current-buffer (window-buffer window)
      (let ((tick (buffer-chars-modified-tick))
            (cache scrollview--top-line-cache)
            ;; Normalize to the beginning of the line containing the
            ;; window-start position so `count-lines' deltas are exact —
            ;; this is the same normalization `line-number-at-pos' does.
            (start (save-excursion
                     (save-restriction
                       (widen)
                       (goto-char raw-start)
                       (forward-line 0)
                       (point)))))
        (cond
         ;; Same tick & same line-start: return cached value verbatim.
         ((and cache
               (= (car cache) tick)
               (= (cadr cache) start))
          (cddr cache))
         ;; Same tick, different start: walk only the delta.
         ((and cache (= (car cache) tick))
          (let* ((cached-start (cadr cache))
                 (cached-line (cddr cache))
                 (line (save-excursion
                         (save-restriction
                           (widen)
                           (if (>= start cached-start)
                               (+ cached-line
                                  (count-lines cached-start start))
                             (max 1 (- cached-line
                                       (count-lines start cached-start))))))))
            (setq scrollview--top-line-cache (cons tick (cons start line)))
            line))
         ;; Buffer modified or no cache: full scan, then memoize.
         (t
          (let ((line (line-number-at-pos start t)))
            (setq scrollview--top-line-cache (cons tick (cons start line)))
            line)))))))

(defun scrollview--window-track-lines (window top-line buffer-lines)
  "Return drawable display rows for WINDOW from TOP-LINE to BUFFER-LINES.
Rows below `point-max' are empty display area and cannot reliably host
overlays."
  (min (scrollview--window-line-height window)
       (max 1 (1+ (- buffer-lines top-line)))))

(defun scrollview--restricted-p (&optional buffer)
  "Return non-nil when BUFFER should use restricted mode."
  (with-current-buffer (or buffer (current-buffer))
    (or (and (>= scrollview-line-limit 0)
             (> (scrollview--line-count) scrollview-line-limit))
        (and (>= scrollview-byte-limit 0)
             (> (buffer-size) scrollview-byte-limit)))))

(defun scrollview--fringe-available-p (window)
  "Return non-nil if WINDOW has a usable fringe on `scrollview-side'."
  (pcase-let ((`(,left-width ,right-width . ,_) (window-fringes window)))
    (pcase scrollview-side
      ('left (> (or left-width 0) 0))
      (_ (> (or right-width 0) 0)))))

(defun scrollview--margin-area-p ()
  "Return non-nil when scrollview renders in the window margin."
  (or (eq scrollview-area 'margin)
      (and scrollview-fallback-to-margin
           (eq scrollview-area 'fringe)
           (not (display-graphic-p)))))

(defun scrollview--display-area-available-p (window)
  "Return non-nil if WINDOW can display the configured scrollview area."
  (if (scrollview--margin-area-p)
      t
    (scrollview--fringe-available-p window)))

(defun scrollview--ensure-window-margins (window)
  "Ensure WINDOW has enough margin space for scrollview."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (when (scrollview--margin-area-p)
        (pcase-let* ((`(,left . ,right) (window-margins window))
                     (target (if (eq scrollview-side 'left) left right)))
          (when (zerop (or target 0))
            (unless (gethash window scrollview--window-margins)
              (puthash window (cons left right) scrollview--window-margins))
            (if (eq scrollview-side 'left)
                (set-window-margins window 1 right)
              (set-window-margins window left 1))))))))

(defun scrollview--restore-window-margins (window)
  "Restore WINDOW margins saved by scrollview."
  (when-let* ((margins (gethash window scrollview--window-margins)))
    (when (window-live-p window)
      (set-window-margins window (car margins) (cdr margins)))
    (remhash window scrollview--window-margins)))

(defun scrollview--prepare-window-display-area (window)
  "Prepare WINDOW to display scrollview in the configured area."
  (if (and (window-live-p window)
           (with-current-buffer (window-buffer window)
             (scrollview--margin-area-p)))
      (scrollview--ensure-window-margins window)
    (scrollview--restore-window-margins window)))

(defun scrollview--excluded-mode-p ()
  "Return non-nil if the current buffer's mode is excluded."
  (and scrollview-excluded-modes
       (apply #'derived-mode-p scrollview-excluded-modes)))

(defun scrollview--multiline-display-replacement-p (window)
  "Return non-nil when WINDOW contains a multiline display replacement.
A string-valued `display' overlay containing newlines decouples visual rows
from buffer lines.  In particular, a whole-buffer replacement can make
redisplay repeatedly adjust `window-start'.  Refreshing scrollview from
`window-scroll-functions' in that state feeds those adjustments back into
redisplay and can keep Emacs in an infinite redisplay loop."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (when (< (point-min) (point-max))
        (let* ((last-position (1- (point-max)))
               (positions
                (delete-dups
                 (list (min last-position
                            (max (point-min) (window-start window)))
                       (min last-position
                            (max (point-min) (window-point window)))))))
          (cl-some
           (lambda (position)
             (cl-some
              (lambda (overlay)
                (when-let* ((display (overlay-get overlay 'display)))
                  (and (stringp display)
                       (string-search "\n" display))))
              (overlays-at position)))
           positions))))))

(defun scrollview--tall-image-display-p (window)
  "Return non-nil when WINDOW's buffer contains an explicitly tall image.
Scrollview maps fringe rows to buffer screen lines.  An image whose declared
height is several text rows still occupies one screen line, so multiple
scrollview overlays can collapse onto the same buffer position and make
redisplay unstable.  Treat an image taller than two default text rows as an
unsupported display layout."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (let ((position (point-min))
            (limit (point-max))
            (max-height (* 2 (window-default-font-height window)))
            found)
        (while (and (< position limit) (not found))
          (let* ((display (get-text-property position 'display))
                 (height (and (eq (car-safe display) 'image)
                              (plist-get (cdr display) :height))))
            (when (and (numberp height) (> height max-height))
              (setq found t)))
          (unless found
            (setq position
                  (next-single-property-change
                   position 'display nil limit))))
        found))))

(defun scrollview--window-eligible-p (window)
  "Return non-nil if WINDOW can display scrollview."
  (and (window-live-p window)
       (not (window-minibuffer-p window))
       (or (not scrollview-current-window-only)
           (eq window (selected-window)))
       (scrollview--display-area-available-p window)
       (not (scrollview--multiline-display-replacement-p window))
       (not (scrollview--tall-image-display-p window))
       (with-current-buffer (window-buffer window)
         (and scrollview-mode
              (not (minibufferp))
              (not (scrollview--excluded-mode-p))))))

(defun scrollview--cleanup-dead-windows ()
  "Delete overlay state for dead windows."
  (let (dead)
    (maphash (lambda (window _overlays)
               (unless (window-live-p window)
                 (cl-pushnew window dead :test #'eq)))
             scrollview--window-overlays)
    (maphash (lambda (window _entry)
               (unless (window-live-p window)
                 (cl-pushnew window dead :test #'eq)))
             scrollview--window-sign-cache)
    (maphash (lambda (window _entry)
               (unless (window-live-p window)
                 (cl-pushnew window dead :test #'eq)))
             scrollview--window-sign-row-cache)
    (maphash (lambda (window _pool)
               (unless (window-live-p window)
                 (cl-pushnew window dead :test #'eq)))
             scrollview--window-overlay-pools)
    (maphash (lambda (window _margins)
               (unless (window-live-p window)
                 (cl-pushnew window dead :test #'eq)))
             scrollview--window-margins)
    (dolist (window dead)
      (scrollview--delete-window-overlays window)
      (remhash window scrollview--window-sign-cache)
      (remhash window scrollview--window-sign-row-cache))))

(defun scrollview--delete-window-overlays (window)
  "Delete scrollview overlays for WINDOW."
  (when-let* ((timer (gethash window scrollview--scroll-refresh-timers)))
    (when (timerp timer)
      (cancel-timer timer))
    (remhash window scrollview--scroll-refresh-timers))
  (mapc #'delete-overlay (gethash window scrollview--window-overlays))
  (mapc #'delete-overlay (gethash window scrollview--window-overlay-pools))
  (remhash window scrollview--window-overlays)
  (remhash window scrollview--window-overlay-pools)
  (remhash window scrollview--window-render-state)
  (remhash window scrollview--window-sign-row-cache)
  (scrollview--restore-window-margins window))

(defun scrollview--delete-buffer-overlays (&optional buffer)
  "Delete scrollview overlays for windows showing BUFFER."
  (let ((buffer (or buffer (current-buffer)))
        windows)
    (maphash (lambda (window _overlays)
               (when (or (not (window-live-p window))
                         (eq (window-buffer window) buffer))
                 (push window windows)))
             scrollview--window-overlays)
    (dolist (window windows)
      (scrollview--delete-window-overlays window))
    (scrollview--invalidate-buffer-sign-cache buffer)))

(defun scrollview--invalidate-sign-cache ()
  "Invalidate cached sign items for all windows."
  (cl-incf scrollview--sign-cache-generation)
  (clrhash scrollview--window-sign-cache)
  (clrhash scrollview--window-sign-row-cache)
  (clrhash scrollview--window-render-state))

(defun scrollview--invalidate-buffer-sign-cache (&optional buffer)
  "Invalidate cached sign items for windows showing BUFFER."
  (let ((buffer (or buffer (current-buffer)))
        windows)
    (maphash (lambda (window _entry)
               (when (or (not (window-live-p window))
                         (eq (window-buffer window) buffer))
                 (push window windows)))
             scrollview--window-sign-cache)
    (dolist (window windows)
      (remhash window scrollview--window-sign-cache)
      (remhash window scrollview--window-sign-row-cache))))


;;; Position calculations

(defun scrollview--compute-thumb-size (window-lines buffer-lines)
  "Return scrollbar thumb size for WINDOW-LINES and BUFFER-LINES."
  (let ((window-lines (max 1 window-lines))
        (buffer-lines (max 1 buffer-lines)))
    (min window-lines
         (max 1 (ceiling (* window-lines
                            (/ (float window-lines) buffer-lines)))))))

(defun scrollview--compute-thumb-top
    (window-lines buffer-lines top-line thumb-size bottom-visible)
  "Return zero-based thumb top row.
WINDOW-LINES and BUFFER-LINES describe the track and document size.
TOP-LINE is one-based.  THUMB-SIZE is the thumb height.
BOTTOM-VISIBLE should be non-nil when point-max is visible."
  (let* ((window-lines (max 1 window-lines))
         (buffer-lines (max 1 buffer-lines))
         (thumb-size (max 1 thumb-size))
         (max-top (max 0 (- window-lines thumb-size))))
    (cond
     ((<= buffer-lines window-lines) 0)
     (bottom-visible max-top)
     (t (min max-top
             (max 0
                  (floor (* max-top
                            (/ (float (max 0 (1- top-line)))
                               (max 1 (1- buffer-lines)))))))))))

(defun scrollview--line-to-row (line window-lines buffer-lines)
  "Map one-based document LINE to a zero-based display row."
  (let* ((window-lines (max 1 window-lines))
         (buffer-lines (max 1 buffer-lines))
         (line (min buffer-lines (max 1 line))))
    (if (<= window-lines 1)
        0
      (min (1- window-lines)
           (max 0 (round (* (1- window-lines)
                            (/ (float (1- line))
                               (max 1 (1- buffer-lines))))))))))

(defun scrollview--row-to-line (row window-lines buffer-lines)
  "Map zero-based display ROW to a one-based document line."
  (let* ((window-lines (max 1 window-lines))
         (buffer-lines (max 1 buffer-lines))
         (row (min (1- window-lines) (max 0 row))))
    (if (<= window-lines 1)
        1
      (min buffer-lines
           (max 1 (1+ (round (* (1- buffer-lines)
                                (/ (float row)
                                   (max 1 (1- window-lines)))))))))))

(defun scrollview--position-info (window)
  "Return scrollbar position data for WINDOW."
  (with-current-buffer (window-buffer window)
    (let* ((window-lines (scrollview--window-line-height window))
           (buffer-lines (scrollview--line-count))
           (top-line (scrollview--window-top-line window))
           (track-lines (scrollview--window-track-lines
                         window top-line buffer-lines))
           (bottom-line (+ top-line window-lines -1))
           (bottom-visible (>= bottom-line buffer-lines))
           (overflow (or (> top-line 1)
                         (> buffer-lines window-lines)))
           (thumb-size (scrollview--compute-thumb-size track-lines buffer-lines))
           (thumb-top (scrollview--compute-thumb-top
                       track-lines buffer-lines top-line thumb-size
                       bottom-visible)))
      (list :window-lines window-lines
            :track-lines track-lines
            :buffer-lines buffer-lines
            :top-line top-line
            :bottom-visible bottom-visible
            :thumb-size thumb-size
            :thumb-top thumb-top
            :overflow overflow))))


;;; Sign registration API

;;;###autoload
(defun scrollview-register-sign-group (group &optional enabled)
  "Register sign GROUP.
When ENABLED is non-nil, enable the group immediately."
  (puthash group (and enabled t) scrollview--sign-groups)
  (scrollview--invalidate-sign-cache)
  group)

;;;###autoload
(defun scrollview-register-sign-spec (&rest args)
  "Register a sign specification and return its id.
ARGS is a plist accepting:

  :group GROUP          Required sign group.
  :variant VARIANT      Optional variant name.
  :priority N           Larger values win row conflicts; default 50.
  :bitmap BITMAP        Fringe bitmap symbol; default
                        `scrollview-sign-dot-bitmap'.  Margin rendering maps
                        the bitmap and variant to a text indicator.
  :face FACE            Face used for the indicator.
  :collector FUNCTION   Called with a window and returns line numbers."
  (let* ((group (plist-get args :group))
         (collector (plist-get args :collector)))
    (unless (gethash group scrollview--sign-groups)
      (unless (memq group (scrollview--sign-group-list))
        (user-error "Sign group is not registered: %s" group)))
    (unless (functionp collector)
      (user-error "A sign collector function is required"))
    (cl-incf scrollview--next-sign-id)
    (let* ((id scrollview--next-sign-id)
           (spec (scrollview--make-sign-spec
                  :id id
                  :group group
                  :variant (plist-get args :variant)
                  :priority (or (plist-get args :priority) 50)
                  :bitmap (or (plist-get args :bitmap)
                              'scrollview-sign-dot-bitmap)
                  :face (or (plist-get args :face)
                            'scrollview-search-face)
                  :collector collector)))
      (puthash id spec scrollview--sign-specs)
      (scrollview--invalidate-sign-cache)
      id)))

;;;###autoload
(defun scrollview-deregister-sign-spec (id)
  "Deregister sign specification ID."
  (remhash id scrollview--sign-specs)
  (scrollview--invalidate-sign-cache)
  (scrollview--schedule-refresh))

(defun scrollview--sign-group-list ()
  "Return registered sign groups."
  (let (groups)
    (maphash (lambda (group _enabled) (push group groups))
             scrollview--sign-groups)
    (sort groups (lambda (a b)
                   (string< (symbol-name a) (symbol-name b))))))

(defun scrollview--startup-sign-enabled-p (group)
  "Return non-nil if GROUP should be enabled on startup."
  (or (eq scrollview-signs-on-startup 'all)
      (and (listp scrollview-signs-on-startup)
           (or (memq 'all scrollview-signs-on-startup)
               (memq group scrollview-signs-on-startup)))))

;;;###autoload
(defun scrollview-set-sign-group-state (group state)
  "Set sign GROUP state to STATE.
STATE should be non-nil to enable, nil to disable, or `:toggle' to toggle."
  (let* ((old (gethash group scrollview--sign-groups :missing))
         (new (and (if (eq state :toggle) (not old) state) t)))
    (when (eq old :missing)
      (user-error "Unknown scrollview sign group: %s" group))
    (unless (eq old new)
      (puthash group new scrollview--sign-groups)
      (scrollview--invalidate-sign-cache)
      (scrollview--schedule-refresh))))

;;;###autoload
(defun scrollview-sign-group-active-p (group)
  "Return non-nil if sign GROUP is enabled."
  (and (gethash group scrollview--sign-groups) t))

(defun scrollview--read-sign-group (&optional include-all)
  "Read a sign group, optionally allowing `all'."
  (let* ((groups (mapcar #'symbol-name (scrollview--sign-group-list)))
         (choices (if include-all (cons "all" groups) groups)))
    (intern (completing-read "Scrollview sign group: " choices nil t))))

;;;###autoload
(defun scrollview-enable-sign-group (group)
  "Enable scrollview sign GROUP."
  (interactive (list (scrollview--read-sign-group t)))
  (if (eq group 'all)
      (dolist (g (scrollview--sign-group-list))
        (scrollview-set-sign-group-state g t))
    (scrollview-set-sign-group-state group t)))

;;;###autoload
(defun scrollview-disable-sign-group (group)
  "Disable scrollview sign GROUP."
  (interactive (list (scrollview--read-sign-group t)))
  (if (eq group 'all)
      (dolist (g (scrollview--sign-group-list))
        (scrollview-set-sign-group-state g nil))
    (scrollview-set-sign-group-state group nil)))

;;;###autoload
(defun scrollview-toggle-sign-group (group)
  "Toggle scrollview sign GROUP."
  (interactive (list (scrollview--read-sign-group t)))
  (if (eq group 'all)
      (dolist (g (scrollview--sign-group-list))
        (scrollview-set-sign-group-state g :toggle))
    (scrollview-set-sign-group-state group :toggle)))


;;; Sign collection and rendering

(defun scrollview--collect-sign-items (window &optional groups)
  "Collect visible sign items for WINDOW.
GROUPS may be nil, a symbol, or a list of symbols."
  (let* ((groups (ensure-list groups))
         (buffer (window-buffer window))
         buffer-lines
         items)
    (with-current-buffer buffer
      (setq buffer-lines (scrollview--line-count))
      (unless (scrollview--restricted-p)
        (maphash
         (lambda (_id spec)
           (let ((group (scrollview--sign-spec-group spec)))
             (when (and (scrollview-sign-group-active-p group)
                        (or (null groups)
                            (memq 'all groups)
                            (memq group groups)))
               (dolist (line (funcall
                              (scrollview--sign-spec-collector spec)
                              window))
                 (when (markerp line)
                   (setq line (and (eq (marker-buffer line) (current-buffer))
                                   (marker-position line)
                                   (line-number-at-pos line t))))
                 (when (and (integerp line)
                            (<= 1 line buffer-lines))
                   (push (cons line spec) items))))))
         scrollview--sign-specs)))
    (nreverse items)))

(defun scrollview--collect-sign-items-cached (window)
  "Collect sign items for WINDOW, comparing cached fields without allocation."
  (let ((buffer (window-buffer window))
        (entry (gethash window scrollview--window-sign-cache)))
    (with-current-buffer buffer
      (if (and entry
               (= scrollview--sign-cache-generation
                  (plist-get entry :generation))
               (eq buffer (plist-get entry :buffer))
               (= (buffer-chars-modified-tick) (plist-get entry :tick))
               (eq (scrollview--restricted-p) (plist-get entry :restricted))
               (eql (and (scrollview-sign-group-active-p 'spell)
                         scrollview--spell-state-generation)
                    (plist-get entry :spell)))
          (plist-get entry :items)
        (let ((items (scrollview--collect-sign-items window)))
          (puthash window
                   (list :generation scrollview--sign-cache-generation
                         :buffer buffer
                         :tick (buffer-chars-modified-tick)
                         :restricted (scrollview--restricted-p)
                         :spell (and (scrollview-sign-group-active-p 'spell)
                                     scrollview--spell-state-generation)
                         :items items)
                   scrollview--window-sign-cache)
          items)))))

(defun scrollview--sign-row-candidates (window info sign-items)
  "Return the winning sign item for every row described by INFO.
Cache the reduction for WINDOW while SIGN-ITEMS and track geometry remain
unchanged.  A cached sign list keeps its identity across scroll refreshes."
  (let* ((window-lines (plist-get info :window-lines))
         (track-lines (or (plist-get info :track-lines) window-lines))
         (buffer-lines (plist-get info :buffer-lines))
         (entry (and window (gethash window scrollview--window-sign-row-cache))))
    (if (and entry
             (eq sign-items (plist-get entry :sign-items))
             (= window-lines (plist-get entry :window-lines))
             (= track-lines (plist-get entry :track-lines))
             (= buffer-lines (plist-get entry :buffer-lines)))
        (plist-get entry :candidates)
      (let ((candidates (make-vector window-lines nil))
            active-rows)
        (dolist (item sign-items)
          (let* ((row (scrollview--line-to-row
                       (car item)
                       track-lines buffer-lines))
                 (old (aref candidates row))
                 (spec (cdr item))
                 (old-spec (cdr-safe old)))
            (when (or (null old)
                      (> (scrollview--sign-spec-priority spec)
                         (scrollview--sign-spec-priority old-spec))
                      (and (= (scrollview--sign-spec-priority spec)
                              (scrollview--sign-spec-priority old-spec))
                           (< (scrollview--sign-spec-id spec)
                              (scrollview--sign-spec-id old-spec))))
              (unless old
                (push row active-rows))
              (aset candidates row item))))
        (when window
          (puthash window
                   (list :sign-items sign-items
                         :window-lines window-lines
                         :track-lines track-lines
                         :buffer-lines buffer-lines
                         :candidates candidates
                         :active-rows (sort active-rows #'<))
                   scrollview--window-sign-row-cache))
        candidates))))

(defun scrollview--build-slots (window info sign-items)
  "Return display slots using INFO and SIGN-ITEMS."
  (let* ((window-lines (plist-get info :window-lines))
         (thumb-top (plist-get info :thumb-top))
         (thumb-size (plist-get info :thumb-size))
         (candidates (and sign-items
                          (scrollview--sign-row-candidates
                           window info sign-items)))
         (active-rows
          (when candidates
            (let ((entry (and window
                              (gethash window
                                       scrollview--window-sign-row-cache))))
              (if (and entry (eq candidates (plist-get entry :candidates)))
                  (plist-get entry :active-rows)
                (cl-loop for row from 0 below (length candidates)
                         when (aref candidates row)
                         collect row)))))
         (slots (make-vector window-lines nil)))
    (dotimes (offset thumb-size)
      (let ((row (+ thumb-top offset)))
        (when (< row window-lines)
          (aset slots row
                (list :type 'scrollbar
                      :priority scrollview--scrollbar-priority
                      :order most-positive-fixnum
                      :bitmap 'filled-rectangle
                      :face 'scrollview-thumb-face)))))
    (when candidates
      (dolist (row active-rows)
        (when-let* ((item (aref candidates row))
                    (line (car item)))
          (let* ((spec (cdr item))
                 (priority (scrollview--sign-spec-priority spec))
                 (order (scrollview--sign-spec-id spec))
                 (old (aref slots row)))
            (when (or (null old)
                      (> priority (plist-get old :priority))
                      (and (= priority (plist-get old :priority))
                           (< order (plist-get old :order))))
              (let ((highlighted (and (<= thumb-top row)
                                      (< row (+ thumb-top thumb-size)))))
                (aset slots row
                      (list :type 'sign
                            :priority priority
                            :order order
                            :bitmap (scrollview--sign-spec-bitmap spec)
                            :face (scrollview--sign-render-face
                                   (scrollview--sign-spec-face spec)
                                   highlighted)
                            :line line
                            :group (scrollview--sign-spec-group spec)
                            :variant (scrollview--sign-spec-variant spec)
                            :highlighted highlighted))))))))
    slots))

(defun scrollview--overlay-display-side ()
  "Return the display side for `scrollview-area' and `scrollview-side'."
  (if (scrollview--margin-area-p)
      (if (eq scrollview-side 'left) 'left-margin 'right-margin)
    (if (eq scrollview-side 'left) 'left-fringe 'right-fringe)))

(defun scrollview--margin-glyph (slot)
  "Return a one-column margin glyph for SLOT."
  (pcase (list (plist-get slot :type)
               (plist-get slot :group)
               (plist-get slot :variant)
               (plist-get slot :bitmap))
    (`(scrollbar . ,_) "|")
    (`(sign conflicts top . ,_) "<")
    (`(sign conflicts middle . ,_) "=")
    (`(sign conflicts bottom . ,_) ">")
    (`(sign bookmarks . ,_) "%")
    (`(sign spell . ,_) "~")
    (`(sign vc add . ,_) "+")
    (`(sign vc change . ,_) "|")
    (`(sign vc delete . ,_) "-")
    (`(sign highlight-changes change . ,_) "C")
    (`(sign highlight-changes delete . ,_) "X")
    (`(sign _ _ scrollview-symbol-bitmap) "+")
    (`(sign _ _ scrollview-search-bitmap) "=")
    (`(sign _ _ scrollview-diagnostic-bitmap) "!")
    (`(sign _ _ scrollview-sign-bar-bitmap) "|")
    (`(sign _ _ scrollview-sign-delete-bitmap) "-")
    (_ "*")))

(defun scrollview--overlay-after-string (slot &optional _target-line)
  "Return the cached, row-independent after-string for SLOT."
  (let* ((face (plist-get slot :face))
         (side (scrollview--overlay-display-side))
         (visual (if (scrollview--margin-area-p)
                     (scrollview--margin-glyph slot)
                   (plist-get slot :bitmap)))
         (key (list side visual face)))
    (or (gethash key scrollview--display-string-cache)
        (let* ((display
                (if (scrollview--margin-area-p)
                    `((margin ,side)
                      ,(propertize visual
                                   'face face
                                   'mouse-face 'highlight))
                  `(,side ,visual ,face)))
               (string (propertize "."
                                   'face face
                                   'mouse-face 'highlight
                                   'display display)))
          (puthash key string scrollview--display-string-cache)
          string))))

(defun scrollview--overlay-help-echo (_window object _position)
  "Return help text for scrollview display OBJECT."
  (if (and (overlayp object)
           (eq (overlay-get object 'scrollview-target-type) 'sign))
      (format "scrollview %s sign at line %d"
              (overlay-get object 'scrollview-group)
              (overlay-get object 'scrollview-target-line))
    "scrollview scrollbar"))

(defun scrollview--update-overlay-at-point (overlay window row slot target-line)
  "Move and update OVERLAY for WINDOW, ROW, SLOT, and TARGET-LINE."
  (let* ((point (point))
         (pos (if (= point (line-end-position))
                  point
                (min (point-max) (1+ point))))
         (after-string (scrollview--overlay-after-string slot))
         (type (plist-get slot :type))
         (group (plist-get slot :group)))
    (unless (and (eq (overlay-buffer overlay) (current-buffer))
                 (= (overlay-start overlay) pos)
                 (= (overlay-end overlay) pos))
      (move-overlay overlay pos pos (current-buffer)))
    (unless (eq after-string (overlay-get overlay 'after-string))
      (overlay-put overlay 'after-string after-string))
    (unless (eq window (overlay-get overlay 'window))
      (overlay-put overlay 'window window))
    (unless (eql scrollview--overlay-priority
                 (overlay-get overlay 'priority))
      (overlay-put overlay 'priority scrollview--overlay-priority))
    (unless (overlay-get overlay 'scrollview)
      (overlay-put overlay 'scrollview t))
    (unless (eq type (overlay-get overlay 'scrollview-target-type))
      (overlay-put overlay 'scrollview-target-type type))
    (unless (eq group (overlay-get overlay 'scrollview-group))
      (overlay-put overlay 'scrollview-group group))
    (unless (eq #'scrollview--overlay-help-echo
                (overlay-get overlay 'help-echo))
      (overlay-put overlay 'help-echo #'scrollview--overlay-help-echo))
    (let ((line (and (eq type 'sign) target-line)))
      (unless (eql line (overlay-get overlay 'scrollview-target-line))
        (overlay-put overlay 'scrollview-target-line line)))
    (unless (eql row (overlay-get overlay 'scrollview-row))
      (overlay-put overlay 'scrollview-row row))
    overlay))

(defun scrollview--plan-overlay-targets (window slots info)
  "Return desired overlay targets for WINDOW, SLOTS, and INFO.
All display-position reads finish before any overlay is moved or modified."
  (let (targets)
    (with-current-buffer (window-buffer window)
      (save-restriction
        (save-excursion
          (goto-char (window-start window))
          (cl-loop with current-row = 0
                   for row from 0 below (length slots)
                   for slot = (aref slots row)
                   when slot
                   do (let ((distance (- row current-row)))
                        (when (or (zerop distance)
                                  (= (vertical-motion distance window) distance))
                          (push
                           (vector
                            row (point) slot
                            (or (plist-get slot :line)
                                (scrollview--row-to-line
                                 row
                                 (or (plist-get info :track-lines)
                                     (plist-get info :window-lines))
                                 (plist-get info :buffer-lines))))
                           targets))
                        (setq current-row row))))))
    (nreverse targets)))

(defun scrollview--apply-overlay-targets (window targets)
  "Apply precomputed TARGETS to WINDOW and return its active overlays."
  (let ((buffer (window-buffer window))
        (by-row (make-hash-table :test #'eql))
        (spare (gethash window scrollview--window-overlay-pools))
        overlays)
    (remhash window scrollview--window-overlay-pools)
    (dolist (overlay (gethash window scrollview--window-overlays))
      (if (eq (overlay-buffer overlay) buffer)
          (puthash (overlay-get overlay 'scrollview-row) overlay by-row)
        (delete-overlay overlay)
        (push overlay spare)))
      (dolist (target targets)
        (let* ((row (aref target 0))
               (position (aref target 1))
               (slot (aref target 2))
               (target-line (aref target 3))
               (same-row-overlay (gethash row by-row))
               (overlay (or same-row-overlay
                            (pop spare)
                            (make-overlay position position buffer))))
          (when same-row-overlay
            (remhash row by-row))
          (with-current-buffer buffer
            (save-excursion
              (goto-char position)
              (push (scrollview--update-overlay-at-point
                     overlay window row slot target-line)
                    overlays)))))
    (let (pool)
      (maphash (lambda (_row overlay)
                 (delete-overlay overlay)
                 (push overlay pool))
               by-row)
      (dolist (overlay spare)
        (delete-overlay overlay)
        (push overlay pool))
      (if pool
          (puthash window pool scrollview--window-overlay-pools)
        (remhash window scrollview--window-overlay-pools)))
    overlays))

(defun scrollview--should-render-p (info sign-items)
  "Return non-nil if INFO and SIGN-ITEMS should be rendered."
  (pcase scrollview-visibility
    ('always t)
    ('info (or (plist-get info :overflow) sign-items))
    (_ (plist-get info :overflow))))

(defun scrollview--render-state (window)
  "Return WINDOW's current render inputs without computing `window-end'."
  (let ((buffer (window-buffer window)))
    (with-current-buffer buffer
      (vector buffer
              (buffer-chars-modified-tick)
              (window-start window)
              (window-vscroll window t)
              (window-hscroll window)
              (window-body-width window t)
              (window-body-height window t)
              (scrollview--window-line-height window)
              scrollview--sign-cache-generation
              scrollview--spell-state-generation
              scrollview--diagnostic-state-generation))))

(defun scrollview--refresh-window (window)
  "Refresh scrollview overlays for WINDOW.
Sign items come from the token-keyed cache, which self-invalidates when
the buffer changes."
  (if (scrollview--window-eligible-p window)
      (let* ((info (scrollview--position-info window))
             (sign-items (scrollview--collect-sign-items-cached window)))
        (if (scrollview--should-render-p info sign-items)
            (let ((slots (scrollview--build-slots window info sign-items))
                  overlays)
              (scrollview--prepare-window-display-area window)
              (setq overlays
                    (scrollview--apply-overlay-targets
                     window
                     (scrollview--plan-overlay-targets window slots info)))
              (puthash window overlays scrollview--window-overlays))
          (scrollview--delete-window-overlays window))
        (puthash window (scrollview--render-state window)
                 scrollview--window-render-state))
    (scrollview--delete-window-overlays window)))

(defun scrollview--refresh-now (&optional window scroll)
  "Refresh scrollview overlays now.
When WINDOW is non-nil, refresh only that window.  When SCROLL is non-nil
this is a scroll-driven refresh, which skips the global setup work
\(face sync, builtin registration, dead-window cleanup) that does not
depend on scroll position and short-circuits when WINDOW's render
signature is unchanged from the previous refresh."
  (unless scrollview--refreshing
    (let ((scrollview--refreshing t)
          (inhibit-redisplay t))
      (cond
       ((and scroll window)
        (unless (equal (scrollview--render-state window)
                       (gethash window scrollview--window-render-state))
          (scrollview--refresh-window window)))
       (t
        (scrollview--sync-faces)
        (scrollview--initialize-builtins)
        (scrollview--cleanup-dead-windows)
        (if window
            (scrollview--refresh-window window)
          (dolist (window (scrollview--all-windows))
            (if (scrollview--window-eligible-p window)
                (scrollview--refresh-window window)
              (scrollview--delete-window-overlays window)))))))))

;;;###autoload
(defun scrollview-refresh (&optional window)
  "Refresh scrollview overlays.
When WINDOW is non-nil, refresh only that window.  Interactively, refresh all
eligible windows."
  (interactive)
  (scrollview--refresh-now window))


;;; Scheduling and hooks

(defun scrollview--flush-refresh ()
  "Run a pending debounced refresh."
  (setq scrollview--refresh-timer nil)
  (if scrollview--pending-all
      (scrollview-refresh)
    (maphash (lambda (window _)
               (when (window-live-p window)
                 (scrollview-refresh window)))
             scrollview--pending-windows))
  (setq scrollview--pending-all nil)
  (clrhash scrollview--pending-windows))

(defun scrollview--schedule-refresh (&optional window)
  "Schedule a refresh for WINDOW, or all windows when WINDOW is nil."
  (if window
      (puthash window t scrollview--pending-windows)
    (setq scrollview--pending-all t))
  (unless (timerp scrollview--refresh-timer)
    (setq scrollview--refresh-timer
          (run-with-idle-timer scrollview-refresh-delay nil
                               #'scrollview--flush-refresh))))

(defun scrollview--schedule-buffer-refresh (&optional buffer)
  "Schedule a refresh for windows showing BUFFER."
  (dolist (window (get-buffer-window-list (or buffer (current-buffer)) nil t))
    (scrollview--schedule-refresh window)))

(defun scrollview--flush-scroll-refresh (window)
  "Run WINDOW's throttled scroll refresh using its latest state."
  (remhash window scrollview--scroll-refresh-timers)
  (when (window-live-p window)
    (scrollview--refresh-now window 'scroll)))

(defun scrollview--schedule-scroll-refresh (window)
  "Ensure WINDOW has at most one pending throttled scroll refresh."
  (unless (timerp (gethash window scrollview--scroll-refresh-timers))
    (puthash window
             (run-with-timer scrollview-update-interval nil
                             #'scrollview--flush-scroll-refresh window)
             scrollview--scroll-refresh-timers)))

(defun scrollview--after-window-scroll (window _start)
  "Refresh WINDOW immediately after it scrolls.
Keeping this synchronous prevents stale scrollview overlays from riding along
with the text for one redisplay frame before the debounced refresh corrects
them.

Performance: `window-scroll-functions' fires on every redisplay step during
scrolling, so this delegates to `scrollview--refresh-now' with SCROLL
non-nil, which skips the global setup work and short-circuits when nothing
relevant has changed since the last refresh."
  (unless (or scrollview--refreshing
              (scrollview--multiline-display-replacement-p window)
              (scrollview--tall-image-display-p window))
    (if (> scrollview-update-interval 0)
        (scrollview--schedule-scroll-refresh window)
      (scrollview--refresh-now window 'scroll))))

(defun scrollview--after-change (start end _old-length)
  "Refresh windows showing the current buffer after buffer changes."
  (scrollview--update-line-count-after-change start end)
  (scrollview--invalidate-buffer-sign-cache)
  (scrollview--schedule-buffer-refresh))

(defun scrollview--window-configuration-change ()
  "Refresh after window configuration changes."
  (scrollview--schedule-refresh))

(defun scrollview--window-size-change (_frame)
  "Refresh after window size changes."
  (scrollview--schedule-refresh))

(defun scrollview--post-command ()
  "Refresh when the selected window changes."
  (let ((window (selected-window)))
    (unless (eq window scrollview--last-selected-window)
      (let ((old scrollview--last-selected-window))
        (setq scrollview--last-selected-window window)
        (scrollview--invalidate-sign-cache)
        (when (window-live-p old)
          (scrollview--schedule-refresh old))
        (scrollview--schedule-refresh window)))))

(defun scrollview--install-global-hooks ()
  "Install global hooks used by scrollview."
  (unless scrollview--global-hooks-installed
    (setq scrollview--global-hooks-installed t)
    (setq scrollview--last-selected-window (selected-window))
    (add-hook 'window-configuration-change-hook
              #'scrollview--window-configuration-change)
    (add-hook 'window-size-change-functions
              #'scrollview--window-size-change)
    (add-hook 'post-command-hook #'scrollview--post-command)))


;;; Mouse navigation

(defun scrollview--click-area ()
  "Return the mouse area symbol for the configured display area."
  (scrollview--overlay-display-side))

(defun scrollview--event-row (position)
  "Return zero-based window row for mouse POSITION."
  (when-let* ((row (cdr-safe (posn-col-row position))))
    (and (numberp row) (max 0 (truncate row)))))

(defun scrollview--clickable-info (window)
  "Return position info when WINDOW currently displays scrollview."
  (when (scrollview--window-eligible-p window)
    (let* ((info (scrollview--position-info window))
           (sign-items (scrollview--collect-sign-items-cached window)))
      (when (scrollview--should-render-p info sign-items)
        info))))

(defun scrollview--event-target-line (window position)
  "Return document line corresponding to mouse POSITION in WINDOW."
  (when-let* ((info (scrollview--clickable-info window))
              (row (scrollview--event-row position))
              (track-lines (or (plist-get info :track-lines)
                               (plist-get info :window-lines)))
              (buffer-lines (plist-get info :buffer-lines)))
    (scrollview--row-to-line row track-lines buffer-lines)))

(defun scrollview--event-overlay (window position)
  "Return WINDOW's active scrollview overlay at mouse POSITION."
  (when-let* ((row (scrollview--event-row position)))
    (cl-find-if (lambda (overlay)
                  (eql row (overlay-get overlay 'scrollview-row)))
                (gethash window scrollview--window-overlays))))

(defun scrollview--goto-line (window line &optional set-start)
  "Select WINDOW and move point to one-based LINE.
When SET-START is non-nil, also make LINE the window start."
  (select-window window)
  (with-current-buffer (window-buffer window)
    (let ((line (min (scrollview--line-count) (max 1 line))))
      (goto-char (point-min))
      (forward-line (1- line))
      (when set-start
        (set-window-start window (point) t)))))

;;;###autoload
(defun scrollview-click (event)
  "Jump to the scrollview position clicked by mouse EVENT."
  (interactive "e")
  (let* ((position (event-start event))
         (window (posn-window position))
         (area (posn-area position)))
    (if (and (window-live-p window)
             (eq area (with-current-buffer (window-buffer window)
                        (scrollview--click-area))))
        (with-current-buffer (window-buffer window)
          (let* ((overlay (scrollview--event-overlay window position))
                 (type (and overlay
                            (overlay-get overlay 'scrollview-target-type)))
                 (line (or (and (eq type 'sign)
                                (overlay-get overlay
                                             'scrollview-target-line))
                           (scrollview--event-target-line window position))))
            (if line
                (scrollview--goto-line window line (not (eq type 'sign)))
              (mouse-set-point event))))
      (mouse-set-point event))))

;;; Navigation and legend

(defun scrollview--visible-sign-lines (&optional groups window)
  "Return sorted visible sign lines for GROUPS in WINDOW."
  (let ((window (or window (selected-window)))
        lines)
    (when (scrollview--window-eligible-p window)
      (dolist (item (scrollview--collect-sign-items window groups))
        (push (car item) lines)))
    (scrollview--dedupe-sorted-lines lines)))

(defun scrollview--goto-sign-line (location &optional count groups)
  "Move point to a sign line.
LOCATION is one of `next', `prev', `first', or `last'."
  (let* ((count (or count 1))
         (lines (scrollview--visible-sign-lines groups))
         (current (line-number-at-pos nil t)))
    (unless lines
      (user-error "No scrollview signs are visible"))
    (let ((target
           (pcase location
             ('first (car lines))
             ('last (car (last lines)))
             ('next (or (nth (1- count)
                             (seq-filter
                              (lambda (line) (> line current)) lines))
                        (user-error "No next scrollview sign")))
             ('prev (or (nth (1- count)
                             (nreverse
                              (seq-filter
                               (lambda (line) (< line current)) lines)))
                        (user-error "No previous scrollview sign"))))))
      (goto-char (point-min))
      (forward-line (1- target)))))

;;;###autoload
(defun scrollview-next (&optional count groups)
  "Move to the COUNT-th next visible sign line.
When GROUPS is non-nil, only those sign groups are considered."
  (interactive "p")
  (scrollview--goto-sign-line 'next count groups))

;;;###autoload
(defun scrollview-prev (&optional count groups)
  "Move to the COUNT-th previous visible sign line.
When GROUPS is non-nil, only those sign groups are considered."
  (interactive "p")
  (scrollview--goto-sign-line 'prev count groups))

;;;###autoload
(defun scrollview-first (&optional groups)
  "Move to the first visible sign line.
When GROUPS is non-nil, only those sign groups are considered."
  (interactive)
  (scrollview--goto-sign-line 'first 1 groups))

;;;###autoload
(defun scrollview-last (&optional groups)
  "Move to the last visible sign line.
When GROUPS is non-nil, only those sign groups are considered."
  (interactive)
  (scrollview--goto-sign-line 'last 1 groups))

;;;###autoload
(defun scrollview-legend ()
  "Show a legend for registered scrollview signs."
  (interactive)
  (scrollview--initialize-builtins)
  (with-help-window "*scrollview legend*"
    (princ "scrollview\n\n")
    (princ (format "%-14s %-12s %-8s %-24s %s\n"
                   "group" "variant" "priority" "face" "state"))
    (princ (make-string 74 ?-))
    (princ "\n")
    (maphash
     (lambda (_id spec)
       (princ
        (format "%-14s %-12s %-8d %-24s %s\n"
                (scrollview--sign-spec-group spec)
                (or (scrollview--sign-spec-variant spec) "")
                (scrollview--sign-spec-priority spec)
                (scrollview--sign-spec-face spec)
                (if (scrollview-sign-group-active-p
                     (scrollview--sign-spec-group spec))
                    "enabled"
                  "disabled"))))
     scrollview--sign-specs)))


;;; Modes

(defun scrollview--turn-on ()
  "Enable `scrollview-mode' in eligible buffers."
  (unless (or (minibufferp) (scrollview--excluded-mode-p))
    (scrollview-mode 1)))

(defun scrollview--refresh-current-buffer-windows ()
  "Refresh scrollview windows showing the current buffer."
  (when (bound-and-true-p scrollview-mode)
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (scrollview-refresh window))))

;;;###autoload
(define-minor-mode scrollview-margin-local-mode
  "Display `scrollview-mode' indicators on the margin in the current buffer."
  :lighter ""
  :group 'scrollview
  (if scrollview-margin-local-mode
      (progn
        (unless scrollview-margin--saved-area
          (setq-local scrollview-margin--saved-area
                      (list :local-p (local-variable-p 'scrollview-area)
                            :value scrollview-area)))
        (setq-local scrollview-area 'margin))
    (when scrollview-margin--saved-area
      (let ((local-p (plist-get scrollview-margin--saved-area :local-p))
            (value (plist-get scrollview-margin--saved-area :value)))
        (if local-p
            (setq-local scrollview-area value)
          (kill-local-variable 'scrollview-area)))
      (kill-local-variable 'scrollview-margin--saved-area)))
  (scrollview--refresh-current-buffer-windows))

(defun scrollview--turn-on-margin ()
  "Enable `scrollview-margin-local-mode' in suitable buffers."
  (unless (minibufferp)
    (scrollview-margin-local-mode 1)))

;;;###autoload
(define-globalized-minor-mode scrollview-margin-mode
  scrollview-margin-local-mode scrollview--turn-on-margin
  :group 'scrollview
  :lighter "")

;;;###autoload
(define-minor-mode scrollview-mode
  "Display a scrollbar and document signs in the current buffer."
  :lighter " sv"
  :group 'scrollview
  :keymap scrollview-mode-map
  (if scrollview-mode
      (progn
        (scrollview--initialize-builtins)
        (scrollview--install-global-hooks)
        (add-hook 'window-scroll-functions
                  #'scrollview--after-window-scroll nil t)
        (add-hook 'before-change-functions
                  #'scrollview--before-change nil t)
        (add-hook 'after-change-functions
                  #'scrollview--after-change nil t)
        (add-hook 'post-command-hook
                  #'scrollview--after-eglot-post-command nil t)
        (add-hook 'kill-buffer-hook
                  #'scrollview--delete-buffer-overlays nil t)
        (dolist (window (get-buffer-window-list (current-buffer) nil t))
          (scrollview--schedule-refresh window)))
    (remove-hook 'window-scroll-functions
                 #'scrollview--after-window-scroll t)
    (remove-hook 'before-change-functions
                 #'scrollview--before-change t)
    (remove-hook 'after-change-functions
                 #'scrollview--after-change t)
    (remove-hook 'post-command-hook
                 #'scrollview--after-eglot-post-command t)
    (remove-hook 'kill-buffer-hook
                 #'scrollview--delete-buffer-overlays t)
    (scrollview--delete-buffer-overlays (current-buffer))))

;;;###autoload
(define-globalized-minor-mode global-scrollview-mode
  scrollview-mode scrollview--turn-on
  :group 'scrollview)


(provide 'scrollview-core)

;;; scrollview-core.el ends here
