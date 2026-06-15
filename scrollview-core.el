;;; scrollview-core.el --- Core rendering and modes for scrollview -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Internal module for scrollview.el.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'scrollview-custom)
(require 'scrollview-faces)

(autoload 'scrollview--initialize-builtins "scrollview-signs")

(defvar-local scrollview-mode nil
  "Non-nil when `scrollview-mode' is enabled.")

(defvar-local scrollview-margin--saved-area nil
  "Saved `scrollview-area' state before `scrollview-margin-local-mode'.")

(defvar-local scrollview--margin-width-state nil
  "Saved margin width state before scrollview changed it.")

;;; Internal state

(cl-defstruct (scrollview--sign-spec
               (:constructor scrollview--make-sign-spec))
  id group variant priority bitmap face collector current-only)

(defvar scrollview--window-overlays (make-hash-table :test #'eq)
  "Hash table mapping windows to their scrollview overlays.")

(defvar scrollview--window-margins (make-hash-table :test #'eq)
  "Hash table mapping windows to margins saved before scrollview changed them.")

(defvar scrollview--pending-windows (make-hash-table :test #'eq)
  "Hash table of windows queued for refresh.")

(defvar scrollview--pending-all nil
  "Non-nil means the next scheduled refresh should update all windows.")

(defvar scrollview--refresh-timer nil
  "Idle timer used to debounce scrollview refreshes.")

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

(defvar scrollview--window-sign-cache (make-hash-table :test #'eq)
  "Hash table mapping windows to cached sign items.")

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

(defvar scrollview--mouse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'scrollview-click)
    (define-key map [left-fringe mouse-1] #'scrollview-click)
    (define-key map [right-fringe mouse-1] #'scrollview-click)
    (define-key map [left-margin mouse-1] #'scrollview-click)
    (define-key map [right-margin mouse-1] #'scrollview-click)
    map)
  "Keymap used on scrollview display strings.")

(defvar scrollview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [left-fringe mouse-1] #'scrollview-click)
    (define-key map [right-fringe mouse-1] #'scrollview-click)
    (define-key map [left-margin mouse-1] #'scrollview-click)
    (define-key map [right-margin mouse-1] #'scrollview-click)
    map)
  "Keymap for `scrollview-mode'.")


;;; Utilities

(defun scrollview--normalize-group (group)
  "Return GROUP as a symbol."
  (cond
   ((symbolp group) group)
   ((stringp group) (intern group))
   (t (user-error "Invalid scrollview sign group: %S" group))))

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
  "Return the line number at WINDOW's start."
  (with-current-buffer (window-buffer window)
    (line-number-at-pos (window-start window) t)))

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

(defun scrollview--margin-width-variable ()
  "Return the margin width variable for `scrollview-side'."
  (if (eq scrollview-side 'left) 'left-margin-width 'right-margin-width))

(defun scrollview--ensure-window-margins (window)
  "Ensure WINDOW has enough margin space for scrollview."
  (when (and (window-live-p window)
             (scrollview--margin-area-p))
    (with-current-buffer (window-buffer window)
      (let ((width-var (scrollview--margin-width-variable)))
        (when (zerop (or (symbol-value width-var) 0))
          (unless (assq width-var scrollview--margin-width-state)
            (setq-local
             scrollview--margin-width-state
             (cons (list width-var
                         (local-variable-p width-var)
                         (symbol-value width-var))
                   scrollview--margin-width-state)))
          (set (make-local-variable width-var) 1))
        (dolist (buffer-window (get-buffer-window-list (current-buffer) nil t))
          (set-window-buffer buffer-window (current-buffer)))))))

(defun scrollview--buffer-has-window-overlays-p (buffer)
  "Return non-nil if BUFFER still has rendered scrollview overlays."
  (let (found)
    (maphash (lambda (window overlays)
               (when (and overlays
                          (window-live-p window)
                          (eq (window-buffer window) buffer))
                 (setq found t)))
             scrollview--window-overlays)
    found))

(defun scrollview--restore-window-margins (window)
  "Restore WINDOW margins saved by scrollview."
  (when-let ((margins (gethash window scrollview--window-margins)))
    (when (window-live-p window)
      (set-window-margins window (car margins) (cdr margins)))
    (remhash window scrollview--window-margins))
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (when (and scrollview--margin-width-state
                 (or (not (scrollview--margin-area-p))
                     (not (scrollview--buffer-has-window-overlays-p
                           (current-buffer)))))
        (dolist (entry scrollview--margin-width-state)
          (pcase-let ((`(,width-var ,local-p ,value) entry))
            (if local-p
                (set (make-local-variable width-var) value)
              (kill-local-variable width-var))))
        (setq scrollview--margin-width-state nil)
        (dolist (buffer-window (get-buffer-window-list (current-buffer) nil t))
          (set-window-buffer buffer-window (current-buffer)))))))

(defun scrollview--prepare-window-display-area (window)
  "Prepare WINDOW to display scrollview in the configured area."
  (if (scrollview--margin-area-p)
      (scrollview--ensure-window-margins window)
    (scrollview--restore-window-margins window)))

(defun scrollview--excluded-mode-p ()
  "Return non-nil if the current buffer's mode is excluded."
  (and scrollview-excluded-modes
       (apply #'derived-mode-p scrollview-excluded-modes)))

(defun scrollview--window-eligible-p (window)
  "Return non-nil if WINDOW can display scrollview."
  (and (window-live-p window)
       (not (window-minibuffer-p window))
       (or (not scrollview-current-window-only)
           (eq window (selected-window)))
       (scrollview--display-area-available-p window)
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
    (maphash (lambda (window _margins)
               (unless (window-live-p window)
                 (cl-pushnew window dead :test #'eq)))
             scrollview--window-margins)
    (dolist (window dead)
      (scrollview--delete-window-overlays window)
      (remhash window scrollview--window-sign-cache))))

(defun scrollview--delete-window-overlays (window)
  "Delete scrollview overlays for WINDOW."
  (when-let ((overlays (gethash window scrollview--window-overlays)))
    (mapc #'delete-overlay overlays)
    (remhash window scrollview--window-overlays))
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

(defun scrollview--invalidate-sign-cache (&optional window)
  "Invalidate cached sign items.
When WINDOW is non-nil, invalidate only that window.  Otherwise invalidate all
cached sign items."
  (if window
      (remhash window scrollview--window-sign-cache)
    (cl-incf scrollview--sign-cache-generation)
    (clrhash scrollview--window-sign-cache)))

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
      (remhash window scrollview--window-sign-cache))))


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
            :overflow overflow
            :restricted (scrollview--restricted-p)))))


;;; Sign registration API

;;;###autoload
(defun scrollview-register-sign-group (group &optional enabled)
  "Register sign GROUP.
When ENABLED is non-nil, enable the group immediately."
  (let ((group (scrollview--normalize-group group)))
    (puthash group (and enabled t) scrollview--sign-groups)
    (scrollview--invalidate-sign-cache)
    group))

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
  :collector FUNCTION   Called with a window and returns line numbers.
  :current-only BOOL    When non-nil, show only in the selected window."
  (let* ((group (scrollview--normalize-group (plist-get args :group)))
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
                            'scrollview-keyword-face)
                  :collector collector
                  :current-only (and (plist-get args :current-only) t))))
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
  (let* ((group (scrollview--normalize-group group))
         (old (gethash group scrollview--sign-groups :missing))
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
  (and (gethash (scrollview--normalize-group group) scrollview--sign-groups)
       t))

(defun scrollview--read-sign-group (&optional include-all)
  "Read a sign group, optionally allowing `all'."
  (let* ((groups (mapcar #'symbol-name (scrollview--sign-group-list)))
         (choices (if include-all (cons "all" groups) groups)))
    (intern (completing-read "Scrollview sign group: " choices nil t))))

(defun scrollview--map-sign-groups (group function)
  "Apply FUNCTION to GROUP, expanding `all'."
  (if (eq group 'all)
      (dolist (group (scrollview--sign-group-list))
        (funcall function group))
    (funcall function group)))

;;;###autoload
(defun scrollview-enable-sign-group (group)
  "Enable scrollview sign GROUP."
  (interactive (list (scrollview--read-sign-group t)))
  (scrollview--map-sign-groups
   group (lambda (group) (scrollview-set-sign-group-state group t))))

;;;###autoload
(defun scrollview-disable-sign-group (group)
  "Disable scrollview sign GROUP."
  (interactive (list (scrollview--read-sign-group t)))
  (scrollview--map-sign-groups
   group (lambda (group) (scrollview-set-sign-group-state group nil))))

;;;###autoload
(defun scrollview-toggle-sign-group (group)
  "Toggle scrollview sign GROUP."
  (interactive (list (scrollview--read-sign-group t)))
  (scrollview--map-sign-groups
   group (lambda (group) (scrollview-set-sign-group-state group :toggle))))


;;; Sign collection and rendering

(defun scrollview--normalize-line (line buffer-lines)
  "Normalize LINE to a one-based line number within BUFFER-LINES."
  (let ((line (cond
               ((markerp line)
                (when (and (eq (marker-buffer line) (current-buffer))
                           (marker-position line))
                  (line-number-at-pos line t)))
               ((integerp line) line))))
    (when (and line (<= 1 line) (<= line buffer-lines))
      line)))

(defun scrollview--collect-sign-items (window &optional groups)
  "Collect visible sign items for WINDOW.
GROUPS may be nil, a symbol, or a list of symbols."
  (let* ((groups (cond
                  ((null groups) nil)
                  ((listp groups) (mapcar #'scrollview--normalize-group groups))
                  (t (list (scrollview--normalize-group groups)))))
         (buffer (window-buffer window))
         (selected (selected-window))
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
                            (memq group groups))
                        (or (not (scrollview--sign-spec-current-only spec))
                            (eq window selected)))
               (dolist (line (ignore-errors
                                (funcall (scrollview--sign-spec-collector spec)
                                         window)))
                 (when-let ((line (scrollview--normalize-line
                                   line buffer-lines)))
                   (push (list :line line :spec spec) items))))))
         scrollview--sign-specs)))
    (nreverse items)))

(defun scrollview--sign-cache-token (window)
  "Return a cache token for sign items in WINDOW."
  (let ((buffer (window-buffer window)))
    (with-current-buffer buffer
      (list :generation scrollview--sign-cache-generation
            :buffer buffer
            :tick (buffer-chars-modified-tick)
            :restricted (scrollview--restricted-p)
            :spell (when (scrollview-sign-group-active-p 'spell)
                     scrollview--spell-state-generation)))))

(defun scrollview--collect-sign-items-cached (window reuse-cache)
  "Collect sign items for WINDOW.
When REUSE-CACHE is non-nil, return cached items if the cache token still
matches.  Fresh collections always update the cache."
  (let* ((token (scrollview--sign-cache-token window))
         (entry (and reuse-cache
                     (gethash window scrollview--window-sign-cache))))
    (if (and entry (equal token (plist-get entry :token)))
        (plist-get entry :items)
      (let ((items (scrollview--collect-sign-items window)))
        (puthash window (list :token token :items items)
                 scrollview--window-sign-cache)
        items))))

(defun scrollview--slot-better-p (new old)
  "Return non-nil if NEW slot should replace OLD slot."
  (or (null old)
      (> (plist-get new :priority) (plist-get old :priority))
      (and (= (plist-get new :priority) (plist-get old :priority))
           (< (plist-get new :order) (plist-get old :order)))))

(defun scrollview--slot-params-better-p (priority order old)
  "Return non-nil if PRIORITY and ORDER should replace OLD."
  (or (null old)
      (> priority (plist-get old :priority))
      (and (= priority (plist-get old :priority))
           (< order (plist-get old :order)))))

(defun scrollview--put-slot (slots row slot)
  "Put SLOT into SLOTS at ROW if it has higher priority."
  (let ((old (aref slots row)))
    (when (scrollview--slot-better-p slot old)
      (aset slots row slot))))

(defun scrollview--build-slots (_window info sign-items)
  "Return display slots using INFO and SIGN-ITEMS."
  (let* ((window-lines (plist-get info :window-lines))
         (track-lines (or (plist-get info :track-lines) window-lines))
         (buffer-lines (plist-get info :buffer-lines))
         (thumb-top (plist-get info :thumb-top))
         (thumb-size (plist-get info :thumb-size))
         (restricted (plist-get info :restricted))
         (slots (make-vector window-lines nil)))
    (dotimes (offset thumb-size)
      (let ((row (+ thumb-top offset)))
        (when (< row window-lines)
          (scrollview--put-slot
           slots row
           (list :type 'scrollbar
                 :priority scrollview-scrollbar-priority
                 :order most-positive-fixnum
                 :bitmap 'filled-rectangle
                 :face (if restricted
                           'scrollview-restricted-face
                         'scrollview-thumb-face)
                 :help-echo "scrollview scrollbar")))))
    (dolist (item sign-items)
      (let* ((line (plist-get item :line))
             (spec (plist-get item :spec))
             (row (scrollview--line-to-row line track-lines buffer-lines))
             (priority (scrollview--sign-spec-priority spec))
             (order (scrollview--sign-spec-id spec))
             (old (aref slots row)))
        (when (scrollview--slot-params-better-p priority order old)
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
                        :highlighted highlighted
                        :help-echo (format "scrollview %s sign at line %d"
                                           (scrollview--sign-spec-group spec)
                                           line)))))))
    slots))

(defun scrollview--overlay-position-at-point ()
  "Return the buffer position for a scrollview overlay at point."
  (let ((pos (point)))
    (if (= pos (line-end-position))
        pos
      (min (point-max) (1+ pos)))))

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
    (`(sign keywords todo . ,_) "T")
    (`(sign keywords fixme . ,_) "F")
    (`(sign keywords hack . ,_) "H")
    (`(sign keywords note . ,_) "N")
    (`(sign keywords workaround . ,_) "W")
    (`(sign keywords trick-r . ,_) "R")
    (`(sign keywords defect . ,_) "D")
    (`(sign keywords issue . ,_) "I")
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

(defun scrollview--propertized-display-string
    (string face target-line target-type &rest properties)
  "Return STRING carrying scrollview display properties."
  (apply #'propertize string
         'face face
         'keymap scrollview--mouse-map
         'mouse-face 'highlight
         'scrollview-target-line target-line
         'scrollview-target-type target-type
         properties))

(defun scrollview--overlay-render-state (slot target-line)
  "Return the rendered overlay state for SLOT and TARGET-LINE."
  (list :side (scrollview--overlay-display-side)
        :bitmap (plist-get slot :bitmap)
        :margin-glyph (and (scrollview--margin-area-p)
                           (scrollview--margin-glyph slot))
        :face (plist-get slot :face)
        :target-line target-line
        :target-type (plist-get slot :type)
        :help-echo (plist-get slot :help-echo)
        :priority scrollview-overlay-priority))

(defun scrollview--overlay-after-string (slot target-line)
  "Return the after-string for SLOT and TARGET-LINE."
  (let* ((face (plist-get slot :face))
         (target-type (plist-get slot :type))
         (display
          (if (scrollview--margin-area-p)
              `((margin ,(scrollview--overlay-display-side))
                ,(scrollview--propertized-display-string
                  (scrollview--margin-glyph slot) face target-line target-type))
            `(,(scrollview--overlay-display-side)
              ,(plist-get slot :bitmap)
              ,face))))
    (scrollview--propertized-display-string
     "." face target-line target-type
     'display display)))

(defun scrollview--update-overlay-at-point (overlay window row slot target-line)
  "Move and update OVERLAY for WINDOW, ROW, SLOT, and TARGET-LINE."
  (let ((pos (scrollview--overlay-position-at-point))
        (state (scrollview--overlay-render-state slot target-line)))
    (unless (and (eq (overlay-buffer overlay) (current-buffer))
                 (= (overlay-start overlay) pos)
                 (= (overlay-end overlay) pos))
      (move-overlay overlay pos pos (current-buffer)))
    (unless (equal state (overlay-get overlay 'scrollview-render-state))
      (overlay-put overlay 'after-string
                   (scrollview--overlay-after-string slot target-line))
      (overlay-put overlay 'window window)
      (overlay-put overlay 'priority scrollview-overlay-priority)
      (overlay-put overlay 'scrollview t)
      (overlay-put overlay 'keymap scrollview--mouse-map)
      (overlay-put overlay 'scrollview-target-line target-line)
      (overlay-put overlay 'scrollview-target-type (plist-get slot :type))
      (overlay-put overlay 'help-echo (plist-get slot :help-echo))
      (overlay-put overlay 'scrollview-render-state state))
    (overlay-put overlay 'scrollview-row row)
    overlay))

(defun scrollview--target-line-for-row (slot row info)
  "Return the click target line for SLOT at ROW using INFO."
  (or (plist-get slot :line)
      (scrollview--row-to-line
       row
       (or (plist-get info :track-lines)
           (plist-get info :window-lines))
       (plist-get info :buffer-lines))))

(defun scrollview--current-window-overlays (window)
  "Return reusable overlays for WINDOW as (BY-ROW SPARE).
Stale overlays are deleted while building the return value."
  (let ((buffer (window-buffer window))
        (by-row (make-hash-table :test #'eql))
        spare)
    (dolist (overlay (gethash window scrollview--window-overlays))
      (if (and (overlayp overlay)
               (eq (overlay-buffer overlay) buffer))
          (let ((row (overlay-get overlay 'scrollview-row)))
            (if (and (integerp row)
                     (not (gethash row by-row)))
                (puthash row overlay by-row)
              (push overlay spare)))
        (delete-overlay overlay)))
    (list by-row spare)))

(defun scrollview--delete-unused-overlays (by-row spare)
  "Delete overlays left unused in BY-ROW and SPARE."
  (maphash (lambda (_row overlay)
             (delete-overlay overlay))
           by-row)
  (mapc #'delete-overlay spare))

(defun scrollview--should-render-p (info sign-items)
  "Return non-nil if INFO and SIGN-ITEMS should be rendered."
  (pcase scrollview-visibility
    ('always t)
    ('info (or (plist-get info :overflow) sign-items))
    (_ (plist-get info :overflow))))

(defun scrollview--refresh-window (window &optional reuse-signs)
  "Refresh scrollview overlays for WINDOW.
When REUSE-SIGNS is non-nil, reuse cached sign items when they are still
valid."
  (if (scrollview--window-eligible-p window)
      (let* ((info (scrollview--position-info window))
             (sign-items (scrollview--collect-sign-items-cached
                          window reuse-signs)))
        (if (scrollview--should-render-p info sign-items)
            (let ((slots (scrollview--build-slots window info sign-items))
                  overlays)
              (scrollview--prepare-window-display-area window)
              (pcase-let ((`(,by-row ,spare)
                           (scrollview--current-window-overlays window)))
                (with-selected-window window
                  (save-excursion
                    (goto-char (window-start window))
                    (cl-loop with current-row = 0
                             for row from 0 below (length slots)
                             for slot = (aref slots row)
                             do (when (< current-row row)
                                  (vertical-motion (- row current-row))
                                  (setq current-row row))
                             when slot
                             do (let* ((target-line
                                        (scrollview--target-line-for-row
                                         slot row info))
                                       (same-row-overlay (gethash row by-row))
                                       (overlay (or same-row-overlay
                                                    (pop spare)
                                                    (make-overlay
                                                     (point) (point)))))
                                  (when same-row-overlay
                                    (remhash row by-row))
                                  (push (scrollview--update-overlay-at-point
                                         overlay window row slot target-line)
                                        overlays)))))
                (scrollview--delete-unused-overlays by-row spare)
                (puthash window overlays scrollview--window-overlays)))
          (scrollview--delete-window-overlays window)))
    (scrollview--delete-window-overlays window)))

(defun scrollview--refresh-now (&optional window reuse-signs)
  "Refresh scrollview overlays now.
When WINDOW is non-nil, refresh only that window.  REUSE-SIGNS has the same
meaning as in `scrollview--refresh-window'."
  (unless scrollview--refreshing
    (let ((scrollview--refreshing t)
          (inhibit-redisplay t))
      (unless reuse-signs
        (scrollview--sync-faces))
      (scrollview--initialize-builtins)
      (unless reuse-signs
        (scrollview--cleanup-dead-windows))
      (if window
          (scrollview--refresh-window window reuse-signs)
        (dolist (window (scrollview--all-windows))
          (if (scrollview--window-eligible-p window)
              (scrollview--refresh-window window reuse-signs)
            (scrollview--delete-window-overlays window)))))))

;;;###autoload
(defun scrollview-refresh (&optional window)
  "Refresh scrollview overlays.
When WINDOW is non-nil, refresh only that window.  Interactively, refresh all
eligible windows."
  (interactive)
  (scrollview--refresh-now window nil))


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

(defun scrollview--after-window-scroll (window _start)
  "Refresh WINDOW immediately after it scrolls.
Keeping this synchronous prevents stale scrollview overlays from riding along
with the text for one redisplay frame before the debounced refresh corrects
them."
  (scrollview--refresh-now window t))

(defun scrollview--after-change (&rest _)
  "Refresh windows showing the current buffer after buffer changes."
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
  (when-let ((row (cdr-safe (posn-col-row position))))
    (and (numberp row) (max 0 (truncate row)))))

(defun scrollview--clickable-info (window)
  "Return position info when WINDOW currently displays scrollview."
  (when (scrollview--window-eligible-p window)
    (let* ((info (scrollview--position-info window))
           (sign-items (scrollview--collect-sign-items-cached window t)))
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
          (let* ((line (or (mouse-posn-property
                            position 'scrollview-target-line)
                           (scrollview--event-target-line window position)))
                 (type (mouse-posn-property
                        position 'scrollview-target-type)))
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
        (push (plist-get item :line) lines)))
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
                        (and scrollview-wrap-navigation
                             (nth (mod (1- count) (length lines)) lines))
                        (car (last lines))))
             ('prev (let ((previous
                           (nreverse
                            (seq-filter
                             (lambda (line) (< line current)) lines))))
                      (or (nth (1- count) previous)
                          (and scrollview-wrap-navigation
                               (nth (mod (1- count) (length lines))
                                    (reverse lines)))
                          (car lines)))))))
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
        (add-hook 'after-change-functions
                  #'scrollview--after-change nil t)
        (add-hook 'kill-buffer-hook
                  #'scrollview--delete-buffer-overlays nil t)
        (dolist (window (get-buffer-window-list (current-buffer) nil t))
          (scrollview--schedule-refresh window)))
    (remove-hook 'window-scroll-functions
                 #'scrollview--after-window-scroll t)
    (remove-hook 'after-change-functions
                 #'scrollview--after-change t)
    (remove-hook 'kill-buffer-hook
                 #'scrollview--delete-buffer-overlays t)
    (scrollview--delete-buffer-overlays (current-buffer))))

;;;###autoload
(define-globalized-minor-mode global-scrollview-mode
  scrollview-mode scrollview--turn-on
  :group 'scrollview)


(provide 'scrollview-core)

;;; scrollview-core.el ends here
