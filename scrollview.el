;;; scrollview.el --- Fringe scrollbars and document signs -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: scrollview.el contributors
;; Keywords: convenience
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; scrollview.el displays a vertical scrollbar and document signs in the
;; selected fringe.  It is intentionally implemented with ordinary overlays and
;; fringe display specs, not child frames.

;;; Code:

(require 'cl-lib)
(require 'fringe)
(require 'isearch)
(require 'subr-x)

(declare-function flymake-diagnostics "flymake" (&optional beg end))
(declare-function flymake-diagnostic-beg "flymake" (diag))
(declare-function flymake-diagnostic-type "flymake" (diag))

(defvar-local scrollview-mode nil
  "Non-nil when `scrollview-mode' is enabled.")


;;; Customization

(defgroup scrollview nil
  "Fringe scrollbars and document signs."
  :group 'convenience
  :prefix "scrollview-")

(defcustom scrollview-side 'right
  "Fringe side used by scrollview.
The value must be either `right' or `left'."
  :type '(choice (const :tag "Right fringe" right)
                 (const :tag "Left fringe" left))
  :group 'scrollview)

(defcustom scrollview-visibility 'overflow
  "When scrollview overlays should be shown.
`overflow' shows scrollview only when the buffer is not fully visible.
`always' shows it whenever the window is eligible.
`info' shows it when the buffer overflows, or when signs are present."
  :type '(choice (const :tag "Overflow" overflow)
                 (const :tag "Always" always)
                 (const :tag "Info" info))
  :group 'scrollview)

(defcustom scrollview-current-window-only nil
  "When non-nil, show scrollview only in the selected window."
  :type 'boolean
  :group 'scrollview)

(defcustom scrollview-excluded-modes '(image-mode doc-view-mode pdf-view-mode)
  "Major modes where scrollview should not be displayed.
Derived modes are excluded as well."
  :type '(repeat symbol)
  :group 'scrollview)

(defcustom scrollview-line-limit 20000
  "Maximum buffer line count before restricted mode is used.
Set to -1 to disable this limit.  Restricted mode disables signs."
  :type 'integer
  :group 'scrollview)

(defcustom scrollview-byte-limit 1000000
  "Maximum buffer size before restricted mode is used.
Set to -1 to disable this limit.  Restricted mode disables signs."
  :type 'integer
  :group 'scrollview)

(defcustom scrollview-signs-on-startup '(search diagnostics)
  "Built-in sign groups enabled when scrollview is first used.
Use the symbol `all' to enable all built-in groups."
  :type '(repeat symbol)
  :group 'scrollview)

(defcustom scrollview-refresh-delay 0.03
  "Idle delay, in seconds, before a scheduled refresh runs."
  :type 'number
  :group 'scrollview)

(defcustom scrollview-scrollbar-priority 0
  "Priority of the scrollbar when it conflicts with signs.
Higher priority signs replace the scrollbar for that fringe row."
  :type 'integer
  :group 'scrollview)

(defcustom scrollview-overlay-priority 1000
  "Overlay priority used for scrollview fringe indicators."
  :type 'integer
  :group 'scrollview)

(defcustom scrollview-wrap-navigation t
  "When non-nil, sign navigation wraps around buffer ends."
  :type 'boolean
  :group 'scrollview)

(defcustom scrollview-signs-no-background nil
  "When non-nil, render signs without painting a background.
If a sign face has a background, that color is used as the sign foreground so
highlight-style faces remain visible without filling the fringe cell."
  :type 'boolean
  :group 'scrollview)

(defcustom scrollview-keyword-patterns
  '((todo . ("\\<TODO\\>"))
    (fixme . ("\\<FIXME\\>" "\\<BUG\\>"))
    (hack . ("\\<HACK\\>" "\\<XXX\\>"))
    (note . ("\\<NOTE\\>")))
  "Keyword sign patterns.
Each entry is (VARIANT . REGEXPS).  Patterns are Emacs regular expressions
searched in the current buffer when the `keywords' sign group is enabled."
  :type '(alist :key-type symbol :value-type (repeat regexp))
  :group 'scrollview)

(defcustom scrollview-keywords-comments-only nil
  "When non-nil, show keyword signs only for matches inside comments."
  :type 'boolean
  :group 'scrollview)


;;; Faces and bitmaps

(defface scrollview-thumb-face
  '((t (:inherit region)))
  "Face for the scrollbar thumb.
The face is synchronized with the current `region' background color when
scrollview is loaded and after themes are enabled."
  :group 'scrollview)

(defface scrollview-restricted-face
  '((t (:inherit scrollview-thumb-face)))
  "Face for the scrollbar when signs are disabled by restricted mode."
  :group 'scrollview)

(defface scrollview-search-face
  '((t (:inherit isearch)))
  "Face for search signs."
  :group 'scrollview)

(defface scrollview-diagnostic-error-face
  '((t (:inherit flymake-error)))
  "Face for diagnostic error signs."
  :group 'scrollview)

(defface scrollview-diagnostic-warning-face
  '((t (:inherit flymake-warning)))
  "Face for diagnostic warning signs."
  :group 'scrollview)

(defface scrollview-diagnostic-info-face
  '((t (:inherit flymake-note)))
  "Face for diagnostic info/note signs."
  :group 'scrollview)

(defface scrollview-conflict-top-face
  '((t (:inherit diff-removed)))
  "Face for conflict start signs."
  :group 'scrollview)

(defface scrollview-conflict-middle-face
  '((t (:inherit diff-changed)))
  "Face for conflict separator signs."
  :group 'scrollview)

(defface scrollview-conflict-bottom-face
  '((t (:inherit diff-added)))
  "Face for conflict end signs."
  :group 'scrollview)

(defface scrollview-keyword-todo-face
  '((t (:inherit font-lock-warning-face)))
  "Face for TODO keyword signs."
  :group 'scrollview)

(defface scrollview-keyword-fixme-face
  '((t (:inherit error)))
  "Face for FIXME keyword signs."
  :group 'scrollview)

(defface scrollview-keyword-hack-face
  '((t (:inherit warning)))
  "Face for HACK keyword signs."
  :group 'scrollview)

(defface scrollview-keyword-note-face
  '((t (:inherit font-lock-doc-face)))
  "Face for NOTE keyword signs."
  :group 'scrollview)

(defface scrollview-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Fallback face for keyword signs."
  :group 'scrollview)

(defface scrollview-spell-face
  '((t (:inherit flyspell-incorrect)))
  "Face for spelling signs."
  :group 'scrollview)

(defface scrollview-vc-add-face
  '((t (:inherit diff-added)))
  "Face for added-line VC signs."
  :group 'scrollview)

(defface scrollview-vc-change-face
  '((t (:inherit diff-changed)))
  "Face for changed-line VC signs."
  :group 'scrollview)

(defface scrollview-vc-delete-face
  '((t (:inherit diff-removed)))
  "Face for deleted-line VC signs."
  :group 'scrollview)

(define-fringe-bitmap 'scrollview-search-bitmap
  [0 0 126 126 126 126 0 0] nil nil 'center)

(define-fringe-bitmap 'scrollview-diagnostic-bitmap
  [0 60 126 126 126 126 60 0] nil nil 'center)

(define-fringe-bitmap 'scrollview-sign-dot-bitmap
  [0 24 60 126 126 60 24 0] nil nil 'center)

(defconst scrollview--vc-bar-bitmap-vector
  [24 24 24 24 24 24 24 24
   24 24 24 24 24 24 24 24
   24 24 24 24 24 24 24 24
   24 24 24 24 24 24 24 24]
  "Bitmap vector for VC add/change vertical bar signs.")

(define-fringe-bitmap 'scrollview-sign-bar-bitmap
  scrollview--vc-bar-bitmap-vector nil nil 'center)

(define-fringe-bitmap 'scrollview-sign-delete-bitmap
  [0 0 0 0 0 126 126 126] nil nil 'center)

(define-fringe-bitmap 'scrollview-spell-bitmap
  [0 0 0 108 54 0 0 0] nil nil 'center)

(defconst scrollview--letter-bitmaps
  '((?A . [0 24 60 102 126 102 102 0])
    (?B . [0 124 102 124 102 102 124 0])
    (?C . [0 60 102 96 96 102 60 0])
    (?D . [0 120 108 102 102 108 120 0])
    (?E . [0 126 96 124 96 96 126 0])
    (?F . [0 126 96 124 96 96 96 0])
    (?G . [0 60 102 96 110 102 60 0])
    (?H . [0 102 102 126 102 102 102 0])
    (?I . [0 60 24 24 24 24 60 0])
    (?J . [0 30 12 12 12 108 56 0])
    (?K . [0 102 108 120 120 108 102 0])
    (?L . [0 96 96 96 96 96 126 0])
    (?M . [0 99 119 127 107 99 99 0])
    (?N . [0 102 118 126 126 110 102 0])
    (?O . [0 60 102 102 102 102 60 0])
    (?P . [0 124 102 102 124 96 96 0])
    (?Q . [0 60 102 102 102 108 54 0])
    (?R . [0 124 102 102 124 108 102 0])
    (?S . [0 60 96 60 6 6 124 0])
    (?T . [0 126 24 24 24 24 24 0])
    (?U . [0 102 102 102 102 102 60 0])
    (?V . [0 102 102 102 102 60 24 0])
    (?W . [0 99 99 107 127 119 99 0])
    (?X . [0 102 102 60 24 60 102 0])
    (?Y . [0 102 102 60 24 24 24 0])
    (?Z . [0 126 6 12 24 48 126 0]))
  "Alist mapping uppercase ASCII letters to 8x8 fringe bitmaps.")

(defun scrollview--keyword-bitmap (variant)
  "Return a fringe bitmap symbol for keyword VARIANT's first letter."
  (let* ((name (upcase (symbol-name variant)))
         (letter (and (> (length name) 0) (aref name 0)))
         (bitmap (and letter (alist-get letter scrollview--letter-bitmaps))))
    (if bitmap
        (let ((symbol (intern (format "scrollview-keyword-%c-bitmap" letter))))
          (define-fringe-bitmap symbol bitmap nil nil 'center)
          symbol)
      'scrollview-sign-dot-bitmap)))

(defvar scrollview--sign-render-face-cache (make-hash-table :test #'eq)
  "Hash table mapping source sign faces to cached render face data.")

(defvar scrollview--thumb-face-state nil
  "Cached state for `scrollview-thumb-face' synchronization.")

(defvar scrollview--diagnostic-face-state nil
  "Cached state for diagnostic face synchronization.")

(defun scrollview--selection-color ()
  "Return the best available color for the current selection."
  (cl-labels ((color (face attribute)
                (let ((value (face-attribute face attribute nil t)))
                  (unless (memq value '(nil unspecified))
                    value))))
    (or (color 'region :background)
        (color 'highlight :background)
        (color 'region :foreground)
        (color 'highlight :foreground))))

(defun scrollview--sync-thumb-face (&rest _)
  "Synchronize the scrollbar thumb with the current selection color."
  (let* ((color (scrollview--selection-color))
         (state (if (and color (not (eq color 'unspecified)))
                    (list :color color)
                  (list :fallback t))))
    (unless (equal state scrollview--thumb-face-state)
      (setq scrollview--thumb-face-state state)
      (if (plist-get state :color)
          (set-face-attribute 'scrollview-thumb-face nil
                              :inherit nil
                              :foreground (plist-get state :color)
                              :background (plist-get state :color)
                              :inverse-video nil)
        (set-face-attribute 'scrollview-thumb-face nil
                            :inherit 'region
                            :foreground 'unspecified
                            :background 'unspecified
                            :inverse-video t)))))

(defun scrollview--face-color (face attributes)
  "Return FACE color from ATTRIBUTES, honoring inheritance."
  (when (facep face)
    (cl-loop for attribute in attributes
             for value = (face-attribute face attribute nil t)
             when (and value (not (eq value 'unspecified)))
             return value)))

(defun scrollview--face-underline-color (face)
  "Return FACE underline color, if any."
  (when (facep face)
    (let ((underline (face-attribute face :underline nil t)))
      (cond
       ((and (consp underline)
             (plist-get underline :color))
        (plist-get underline :color))
       ((and (stringp underline)
             (not (string-empty-p underline)))
        underline)))))

(defun scrollview--diagnostic-source-color (faces)
  "Return the best fringe color from diagnostic source FACES."
  (cl-loop for face in faces
           thereis (or (scrollview--face-color face '(:foreground))
                       (scrollview--face-underline-color face)
                       (scrollview--face-color face '(:background)))))

(defun scrollview--sync-diagnostic-face (target inherit source-faces)
  "Synchronize TARGET with INHERIT and SOURCE-FACES."
  (let ((color (scrollview--diagnostic-source-color source-faces)))
    (set-face-attribute target nil
                        :inherit inherit
                        :foreground (or color 'unspecified)
                        :background 'unspecified
                        :inverse-video nil)
    color))

(defun scrollview--sync-diagnostic-faces (&rest _)
  "Synchronize diagnostic sign faces with diagnostic source faces."
  (let ((state
         (list
          :error (scrollview--diagnostic-source-color
                  '(flymake-error flycheck-error error))
          :warning (scrollview--diagnostic-source-color
                    '(flymake-warning flycheck-warning warning))
          :info (scrollview--diagnostic-source-color
                 '(flymake-note flycheck-info success warning)))))
    (unless (equal state scrollview--diagnostic-face-state)
      (setq scrollview--diagnostic-face-state state)
      (scrollview--sync-diagnostic-face
       'scrollview-diagnostic-error-face
       'flymake-error
       '(flymake-error flycheck-error error))
      (scrollview--sync-diagnostic-face
       'scrollview-diagnostic-warning-face
       'flymake-warning
       '(flymake-warning flycheck-warning warning))
      (scrollview--sync-diagnostic-face
       'scrollview-diagnostic-info-face
       'flymake-note
       '(flymake-note flycheck-info success warning))
      (clrhash scrollview--sign-render-face-cache))))

(defun scrollview--sync-faces (&rest _)
  "Synchronize scrollview faces with theme and diagnostic faces."
  (scrollview--sync-thumb-face)
  (scrollview--sync-diagnostic-faces))

(defun scrollview--sign-render-face (face)
  "Return a background-free render face for sign FACE."
  (if (or (not scrollview-signs-no-background)
          (not (symbolp face)))
      face
    (let* ((color (or (scrollview--face-color face '(:background :foreground))
                      (face-foreground 'default nil t)
                      "black"))
           (entry (gethash face scrollview--sign-render-face-cache))
           (render-face (car-safe entry))
           (cached-color (cdr-safe entry)))
      (unless render-face
        (setq render-face
              (intern (format "scrollview--render-sign-%s" face)))
        (puthash face (cons render-face nil)
                 scrollview--sign-render-face-cache))
      (unless (facep render-face)
        (make-face render-face))
      (unless (equal color cached-color)
        (set-face-attribute render-face nil
                            :inherit nil
                            :foreground color
                            :background 'unspecified
                            :inverse-video nil)
        (puthash face (cons render-face color)
                 scrollview--sign-render-face-cache))
      render-face)))

(scrollview--sync-faces)
(advice-add 'enable-theme :after #'scrollview--sync-faces)

(with-eval-after-load 'flymake
  (scrollview--sync-diagnostic-faces))

(with-eval-after-load 'flycheck
  (scrollview--sync-diagnostic-faces))


;;; Internal state

(cl-defstruct (scrollview--sign-spec
               (:constructor scrollview--make-sign-spec))
  id group variant priority bitmap face collector current-only)

(defvar scrollview--window-overlays (make-hash-table :test #'eq)
  "Hash table mapping windows to their scrollview overlays.")

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

(defvar-local scrollview--line-count-cache nil
  "Buffer-local cache of the current line count.")


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
  (sort (delete-dups (cl-remove-if-not #'integerp (copy-sequence lines))) #'<))

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

(defun scrollview--window-bottom-visible-p (window)
  "Return non-nil if WINDOW shows the end of its buffer."
  (with-current-buffer (window-buffer window)
    (>= (+ (scrollview--window-top-line window)
           (scrollview--window-line-height window)
           -1)
        (scrollview--line-count))))

(defun scrollview--window-overflow-p (window)
  "Return non-nil if WINDOW does not show the whole buffer."
  (with-current-buffer (window-buffer window)
    (or (> (scrollview--window-top-line window) 1)
        (> (scrollview--line-count)
           (scrollview--window-line-height window)))))

(defun scrollview--restricted-p (&optional buffer)
  "Return non-nil when BUFFER should use restricted mode."
  (with-current-buffer (or buffer (current-buffer))
    (or (and (>= scrollview-line-limit 0)
             (> (scrollview--line-count) scrollview-line-limit))
        (and (>= scrollview-byte-limit 0)
             (> (buffer-size) scrollview-byte-limit)))))

(defun scrollview--fringe-side ()
  "Return the display side symbol for `scrollview-side'."
  (pcase scrollview-side
    ('left 'left-fringe)
    (_ 'right-fringe)))

(defun scrollview--fringe-available-p (window)
  "Return non-nil if WINDOW has a usable fringe on `scrollview-side'."
  (pcase-let ((`(,left-width ,right-width . ,_) (window-fringes window)))
    (pcase scrollview-side
      ('left (> (or left-width 0) 0))
      (_ (> (or right-width 0) 0)))))

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
       (scrollview--fringe-available-p window)
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
    (dolist (window dead)
      (scrollview--delete-window-overlays window)
      (remhash window scrollview--window-sign-cache))))

(defun scrollview--delete-window-overlays (window)
  "Delete scrollview overlays for WINDOW."
  (when-let ((overlays (gethash window scrollview--window-overlays)))
    (mapc #'delete-overlay overlays)
    (remhash window scrollview--window-overlays)))

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
  "Map one-based document LINE to a zero-based fringe row."
  (let* ((window-lines (max 1 window-lines))
         (buffer-lines (max 1 buffer-lines))
         (line (min buffer-lines (max 1 line))))
    (if (<= window-lines 1)
        0
      (min (1- window-lines)
           (max 0 (round (* (1- window-lines)
                            (/ (float (1- line))
                               (max 1 (1- buffer-lines))))))))))

(defun scrollview--position-info (window)
  "Return scrollbar position data for WINDOW."
  (with-current-buffer (window-buffer window)
    (let* ((window-lines (scrollview--window-line-height window))
           (buffer-lines (scrollview--line-count))
           (top-line (scrollview--window-top-line window))
           (bottom-visible (scrollview--window-bottom-visible-p window))
           (thumb-size (scrollview--compute-thumb-size window-lines buffer-lines))
           (thumb-top (scrollview--compute-thumb-top
                       window-lines buffer-lines top-line thumb-size
                       bottom-visible)))
      (list :window-lines window-lines
            :buffer-lines buffer-lines
            :top-line top-line
            :bottom-visible bottom-visible
            :thumb-size thumb-size
            :thumb-top thumb-top
            :overflow (scrollview--window-overflow-p window)
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
                        `scrollview-sign-dot-bitmap'.
  :face FACE            Face used for the bitmap.
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
  (scrollview--schedule-refresh-all))

(defun scrollview--sign-group-list ()
  "Return registered sign groups."
  (let (groups)
    (maphash (lambda (group _enabled) (push group groups))
             scrollview--sign-groups)
    (sort groups (lambda (a b)
                   (string< (symbol-name a) (symbol-name b))))))

(defun scrollview--builtin-sign-groups ()
  "Return built-in sign group names."
  '(search diagnostics conflicts keywords spell vc))

(defun scrollview--startup-sign-enabled-p (group)
  "Return non-nil if GROUP should be enabled on startup."
  (or (memq 'all scrollview-signs-on-startup)
      (memq group scrollview-signs-on-startup)))

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
      (scrollview--schedule-refresh-all))))

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


;;; Built-in sign collectors

(defun scrollview--active-isearch-source ()
  "Return active isearch source as (PATTERN REGEXP)."
  (when (and (bound-and-true-p isearch-mode)
             (bound-and-true-p isearch-success)
             (boundp 'isearch-string)
             (stringp isearch-string)
             (> (length isearch-string) 0))
    (list isearch-string
          (and (boundp 'isearch-regexp) isearch-regexp))))

(defun scrollview--lazy-highlight-active-p (&optional buffer)
  "Return non-nil if BUFFER still has live isearch lazy highlight overlays."
  (let ((buffer (or buffer (current-buffer))))
    (and (boundp 'isearch-lazy-highlight-overlays)
         (cl-some (lambda (overlay)
                    (and (overlayp overlay)
                         (eq (overlay-buffer overlay) buffer)))
                  isearch-lazy-highlight-overlays))))

(defun scrollview--retained-isearch-source ()
  "Return retained isearch source as (PATTERN REGEXP)."
  (when (and (not (bound-and-true-p isearch-mode))
             (stringp scrollview--last-search-pattern)
             (> (length scrollview--last-search-pattern) 0)
             (scrollview--lazy-highlight-active-p))
    (list scrollview--last-search-pattern scrollview--last-search-regexp)))

(defun scrollview--search-source ()
  "Return search source when isearch highlights are present."
  (if (bound-and-true-p isearch-mode)
      (scrollview--active-isearch-source)
    (scrollview--retained-isearch-source)))

(defun scrollview--set-isearch-source (pattern regexp)
  "Store active isearch PATTERN and REGEXP, then refresh search signs."
  (unless (and (equal scrollview--last-search-pattern pattern)
               (eq scrollview--last-search-regexp regexp))
    (setq scrollview--last-search-pattern pattern)
    (setq scrollview--last-search-regexp regexp)
    (scrollview--invalidate-buffer-sign-cache)
    (scrollview--schedule-buffer-refresh)))

(defun scrollview--clear-isearch-source ()
  "Clear stored isearch state and refresh search signs."
  (when scrollview--last-search-pattern
    (setq scrollview--last-search-pattern nil)
    (setq scrollview--last-search-regexp nil)
    (scrollview--invalidate-buffer-sign-cache)
    (scrollview--schedule-buffer-refresh)))

(defun scrollview--after-isearch-update ()
  "Refresh search signs after the active isearch changes."
  (if-let ((source (scrollview--active-isearch-source)))
      (pcase-let ((`(,pattern ,regexp) source))
        (scrollview--set-isearch-source pattern regexp))
    (scrollview--clear-isearch-source)))

(defun scrollview--after-isearch-end ()
  "Refresh search signs after isearch exits.
The stored pattern is intentionally kept; whether signs remain visible is
decided by the presence of live isearch lazy highlight overlays."
  (scrollview--invalidate-buffer-sign-cache)
  (scrollview--schedule-buffer-refresh))

(defun scrollview--after-lazy-highlight-cleanup (&rest _)
  "Refresh search signs after isearch lazy highlight overlays change."
  (when scrollview--last-search-pattern
    (scrollview--invalidate-buffer-sign-cache)
    (scrollview--schedule-buffer-refresh)))

(defun scrollview--scan-search-lines (pattern regexp)
  "Return buffer lines matching PATTERN.
When REGEXP is non-nil, search with `re-search-forward'; otherwise search
literally with `search-forward'."
  (let (lines)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (condition-case nil
            (catch 'done
              (while (if regexp
                         (re-search-forward pattern nil t)
                       (search-forward pattern nil t))
                (let ((line (line-number-at-pos (match-beginning 0) t)))
                  (unless (eq line (car lines))
                    (push line lines)))
                (when (= (match-beginning 0) (match-end 0))
                  (if (eobp)
                      (throw 'done nil)
                    (forward-char 1)))))
          (error nil))))
    (nreverse lines)))

(defun scrollview--collect-search-lines (_window)
  "Collect lines matching the current isearch highlight source."
  (when-let ((source (scrollview--search-source)))
    (pcase-let ((`(,pattern ,regexp) source))
      (let ((tick (buffer-chars-modified-tick)))
        (if (and scrollview--search-cache
                 (equal (plist-get scrollview--search-cache :pattern) pattern)
                 (eq (plist-get scrollview--search-cache :regexp) regexp)
                 (= (plist-get scrollview--search-cache :tick) tick))
            (plist-get scrollview--search-cache :lines)
          (let ((lines (scrollview--scan-search-lines pattern regexp)))
            (setq scrollview--search-cache
                  (list :pattern pattern :regexp regexp
                        :tick tick :lines lines))
            lines))))))

(defun scrollview--diagnostic-level (level)
  "Normalize diagnostic LEVEL."
  (let ((category (and (symbolp level)
                       (get level 'flymake-category))))
    (cond
     ((or (memq level '(:error error))
          (eq category 'flymake-error))
      'error)
     ((or (memq level '(:warning warning))
          (eq category 'flymake-warning))
      'warning)
     ((or (memq level '(:note note :info info))
          (eq category 'flymake-note))
      'info)
     ((numberp level)
      (cond
       ((<= level 1) 'error)
       ((= level 2) 'warning)
       (t 'info)))
     (t 'info))))

(defun scrollview--flymake-diagnostic-lines (level)
  "Collect Flymake diagnostic lines for LEVEL."
  (when (fboundp 'flymake-diagnostics)
    (let (lines)
      (ignore-errors
        (dolist (diag (flymake-diagnostics (point-min) (point-max)))
          (when (eq (scrollview--diagnostic-level
                     (flymake-diagnostic-type diag))
                    level)
            (push (line-number-at-pos (flymake-diagnostic-beg diag) t)
                  lines))))
      lines)))

(defun scrollview--flycheck-diagnostic-lines (level)
  "Collect Flycheck diagnostic lines for LEVEL."
  (when (and (boundp 'flycheck-current-errors)
             (fboundp 'flycheck-error-line)
             (fboundp 'flycheck-error-level))
    (let (lines)
      (ignore-errors
        (dolist (err (symbol-value 'flycheck-current-errors))
          (when (eq (scrollview--diagnostic-level
                     (flycheck-error-level err))
                    level)
            (when-let ((line (flycheck-error-line err)))
              (push line lines)))))
      lines)))

(defun scrollview--collect-diagnostic-lines (level)
  "Collect diagnostic lines for LEVEL from Flymake and loaded Flycheck."
  (scrollview--clamp-lines
   (append (scrollview--flymake-diagnostic-lines level)
           (scrollview--flycheck-diagnostic-lines level))
   (scrollview--line-count)))

(defun scrollview--conflict-lines ()
  "Return merge conflict marker lines as a plist."
  (scrollview--cached-collector-value
   'conflicts
   (list :tick (buffer-chars-modified-tick))
   (lambda ()
     (let ((line 1)
           top middle bottom)
       (save-excursion
         (goto-char (point-min))
         (while (not (eobp))
           (cond
            ((looking-at-p "^<<<<<<< ")
             (push line top))
            ((looking-at-p "^=======$")
             (push line middle))
            ((looking-at-p "^>>>>>>> ")
             (push line bottom)))
           (setq line (1+ line))
           (forward-line 1)))
       (list :top (nreverse top)
             :middle (nreverse middle)
             :bottom (nreverse bottom))))))

(defun scrollview--collect-conflict-lines (variant)
  "Collect conflict marker lines for VARIANT."
  (plist-get (scrollview--conflict-lines)
             (scrollview--variant-key variant)))

(defun scrollview--keyword-face (variant)
  "Return the face for keyword VARIANT."
  (pcase variant
    ('todo 'scrollview-keyword-todo-face)
    ('fixme 'scrollview-keyword-fixme-face)
    ('hack 'scrollview-keyword-hack-face)
    ('note 'scrollview-keyword-note-face)
    (_ 'scrollview-keyword-face)))

(defun scrollview--keyword-priority (variant)
  "Return the sign priority for keyword VARIANT."
  (pcase variant
    ('fixme 65)
    ('hack 55)
    ('todo 55)
    ('note 35)
    (_ 45)))

(defun scrollview--match-in-comment-p (position)
  "Return non-nil if POSITION is inside a comment."
  (save-excursion
    (goto-char position)
    (nth 4 (syntax-ppss))))

(defun scrollview--keyword-lines (variant patterns)
  "Return lines matching keyword VARIANT PATTERNS."
  (scrollview--cached-collector-value
   (list 'keywords variant)
   (list :tick (buffer-chars-modified-tick)
         :patterns patterns
         :comments-only scrollview-keywords-comments-only)
   (lambda ()
     (let (lines)
       (save-excursion
         (save-match-data
           (dolist (pattern patterns)
             (goto-char (point-min))
             (condition-case nil
                 (catch 'done
                   (while (re-search-forward pattern nil t)
                     (let ((start (match-beginning 0))
                           (end (match-end 0)))
                       (when (or (not scrollview-keywords-comments-only)
                                 (scrollview--match-in-comment-p start))
                         (push (line-number-at-pos start t) lines))
                       (when (= start end)
                         (if (eobp)
                             (throw 'done nil)
                           (forward-char 1))))))
               (invalid-regexp nil)))))
       (scrollview--dedupe-sorted-lines lines)))))

(defun scrollview--collect-keyword-lines (variant)
  "Collect keyword lines for VARIANT."
  (when-let ((patterns (alist-get variant scrollview-keyword-patterns)))
    (scrollview--keyword-lines variant patterns)))

(defun scrollview--face-symbols (face)
  "Return symbol faces contained in FACE."
  (cond
   ((symbolp face) (list face))
   ((and (consp face) (eq (car face) :inherit))
    (scrollview--face-symbols (cadr face)))
   ((consp face)
    (cl-loop for item in face append (scrollview--face-symbols item)))))

(defun scrollview--flyspell-overlay-p (overlay)
  "Return non-nil if OVERLAY looks like a flyspell overlay."
  (or (overlay-get overlay 'flyspell-overlay)
      (cl-intersection
       (scrollview--face-symbols (overlay-get overlay 'face))
       '(flyspell-incorrect flyspell-duplicate)
       :test #'eq)))

(defun scrollview--spell-lines ()
  "Return lines that currently contain flyspell overlays."
  (scrollview--cached-collector-value
   'spell
   (list :tick (buffer-chars-modified-tick)
         :generation scrollview--spell-state-generation)
   (lambda ()
     (let (lines)
       (dolist (overlay (overlays-in (point-min) (point-max)))
         (when (scrollview--flyspell-overlay-p overlay)
           (push (line-number-at-pos (overlay-start overlay) t) lines)))
       (scrollview--dedupe-sorted-lines lines)))))

(defun scrollview--collect-spell-lines (_window)
  "Collect spelling error lines from flyspell overlays."
  (scrollview--spell-lines))

(defun scrollview--vc-parse-hunk-header (line)
  "Parse unified diff hunk header LINE.
Return (OLD-START OLD-COUNT NEW-START NEW-COUNT), or nil."
  (when (string-match
         "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? +\\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
         line)
    (list (string-to-number (match-string 1 line))
          (if (match-string 2 line)
              (string-to-number (match-string 2 line))
            1)
          (string-to-number (match-string 3 line))
          (if (match-string 4 line)
              (string-to-number (match-string 4 line))
            1))))

(defun scrollview--vc-flush-change-group (start added deleted result)
  "Add one diff change group to RESULT.
START is the current-file line where the group began.  ADDED is a list of
current-file added line numbers.  DELETED is the number of removed base lines."
  (when start
    (setq added (nreverse added))
    (let ((added-count (length added)))
      (cond
       ((and (> added-count 0) (= deleted 0))
        (plist-put result :add (append (plist-get result :add) added)))
       ((and (= added-count 0) (> deleted 0))
        (plist-put result :delete
                   (append (plist-get result :delete) (list start))))
       ((and (> added-count 0) (> deleted 0))
        (let ((change-count (min added-count deleted)))
          (plist-put result :change
                     (append (plist-get result :change)
                             (cl-subseq added 0 change-count)))
          (when (> added-count deleted)
            (plist-put result :add
                       (append (plist-get result :add)
                               (nthcdr deleted added)))))))))
  result)

(defun scrollview--parse-unified-diff-lines (diff)
  "Parse unified DIFF text into a plist of VC sign lines."
  (let ((result (list :add nil :change nil :delete nil))
        (new-line nil)
        (group-start nil)
        (group-added nil)
        (group-deleted 0))
    (cl-labels
        ((flush-group
          ()
          (setq result
                (scrollview--vc-flush-change-group
                 group-start group-added group-deleted result))
          (setq group-start nil
                group-added nil
                group-deleted 0)))
      (dolist (line (split-string diff "\n"))
        (if-let ((header (scrollview--vc-parse-hunk-header line)))
            (progn
              (flush-group)
              (setq new-line (nth 2 header)))
          (when new-line
            (cond
             ((string-prefix-p "+" line)
              (unless (string-prefix-p "+++" line)
                (unless group-start
                  (setq group-start new-line))
                (push new-line group-added)
                (setq new-line (1+ new-line))))
             ((string-prefix-p "-" line)
              (unless (string-prefix-p "---" line)
                (unless group-start
                  (setq group-start new-line))
                (setq group-deleted (1+ group-deleted))))
             ((string-prefix-p " " line)
              (flush-group)
              (setq new-line (1+ new-line)))
             ((string-prefix-p "\\" line)
              nil)
             (t
              (flush-group)
              (setq new-line nil))))))
      (flush-group))
    (list :add (scrollview--dedupe-sorted-lines (plist-get result :add))
          :change (scrollview--dedupe-sorted-lines (plist-get result :change))
          :delete (scrollview--dedupe-sorted-lines (plist-get result :delete)))))

(defun scrollview--vc-git-root (file)
  "Return Git root for FILE, or nil."
  (when (and file (not (file-remote-p file)))
    (locate-dominating-file file ".git")))

(defun scrollview--vc-git-tracked-p (root relative-file)
  "Return non-nil if RELATIVE-FILE is tracked by Git under ROOT."
  (let ((default-directory root))
    (eq 0 (process-file "git" nil nil nil
                        "ls-files" "--error-unmatch" "--" relative-file))))

(defun scrollview--vc-git-write-base (root relative-file destination)
  "Write Git HEAD content for RELATIVE-FILE under ROOT to DESTINATION.
Return non-nil when HEAD content was found."
  (let ((default-directory root))
    (with-temp-buffer
      (let ((status (process-file "git" nil t nil
                                  "--no-pager" "show"
                                  (concat "HEAD:" relative-file))))
        (when (eq status 0)
          (write-region (point-min) (point-max) destination nil 'silent)
          t)))))

(defun scrollview--vc-git-diff-current-buffer (root relative-file)
  "Return unified diff between Git HEAD RELATIVE-FILE and current buffer."
  (let ((base-file (make-temp-file "scrollview-vc-base-"))
        (current-file (make-temp-file "scrollview-vc-current-")))
    (unwind-protect
        (progn
          (unless (scrollview--vc-git-write-base root relative-file base-file)
            (write-region "" nil base-file nil 'silent))
          (write-region (point-min) (point-max) current-file nil 'silent)
          (let ((default-directory root))
            (with-temp-buffer
              (let ((status (process-file "git" nil t nil
                                          "--no-pager" "diff"
                                          "--no-index"
                                          "--unified=0"
                                          "--" base-file current-file)))
                (when (memq status '(0 1))
                  (buffer-string))))))
      (ignore-errors (delete-file base-file))
      (ignore-errors (delete-file current-file)))))

(defun scrollview--vc-lines ()
  "Return VC sign lines for the current buffer."
  (scrollview--cached-collector-value
   'vc
   (list :tick (buffer-chars-modified-tick)
         :file (buffer-file-name)
         :size (buffer-size))
   (lambda ()
     (let* ((file (buffer-file-name))
            (root (scrollview--vc-git-root file))
            (relative-file (and root
                                (file-relative-name
                                 (expand-file-name file)
                                 (expand-file-name root))))
            (buffer-lines (scrollview--line-count)))
       (if (and root
                relative-file
                (executable-find "git")
                (scrollview--vc-git-tracked-p root relative-file))
           (let* ((diff (scrollview--vc-git-diff-current-buffer
                         root relative-file))
                  (lines (scrollview--parse-unified-diff-lines (or diff ""))))
             (list :add (scrollview--clamp-lines
                         (plist-get lines :add) buffer-lines)
                   :change (scrollview--clamp-lines
                            (plist-get lines :change) buffer-lines)
                   :delete (scrollview--clamp-lines
                            (plist-get lines :delete) buffer-lines)))
         (list :add nil :change nil :delete nil))))))

(defun scrollview--collect-vc-lines (variant)
  "Collect VC sign lines for VARIANT."
  (plist-get (scrollview--vc-lines)
             (scrollview--variant-key variant)))

(defun scrollview--variant-key (variant)
  "Return plist keyword for VARIANT."
  (intern (format ":%s" variant)))

(defun scrollview--variant-collector (collector variant)
  "Return a window collector that calls COLLECTOR with VARIANT."
  (lambda (_window)
    (funcall collector variant)))

(defun scrollview--register-builtin-sign-group (group specs)
  "Register built-in sign GROUP and its SPECS.
Each element of SPECS is a plist passed to `scrollview-register-sign-spec'."
  (scrollview-register-sign-group
   group (scrollview--startup-sign-enabled-p group))
  (dolist (spec specs)
    (apply #'scrollview-register-sign-spec :group group spec)))

(defun scrollview--diagnostic-sign-specs ()
  "Return built-in diagnostic sign specs."
  (list
   (list :variant 'error
         :priority 60
         :bitmap 'scrollview-diagnostic-bitmap
         :face 'scrollview-diagnostic-error-face
         :collector (scrollview--variant-collector
                     #'scrollview--collect-diagnostic-lines 'error))
   (list :variant 'warning
         :priority 50
         :bitmap 'scrollview-diagnostic-bitmap
         :face 'scrollview-diagnostic-warning-face
         :collector (scrollview--variant-collector
                     #'scrollview--collect-diagnostic-lines 'warning))
   (list :variant 'info
         :priority 40
         :bitmap 'scrollview-diagnostic-bitmap
         :face 'scrollview-diagnostic-info-face
         :collector (scrollview--variant-collector
                     #'scrollview--collect-diagnostic-lines 'info))))

(defun scrollview--conflict-sign-specs ()
  "Return built-in conflict sign specs."
  (list
   (list :variant 'top
         :priority 80
         :bitmap 'scrollview-sign-dot-bitmap
         :face 'scrollview-conflict-top-face
         :collector (scrollview--variant-collector
                     #'scrollview--collect-conflict-lines 'top))
   (list :variant 'middle
         :priority 80
         :bitmap 'scrollview-sign-dot-bitmap
         :face 'scrollview-conflict-middle-face
         :collector (scrollview--variant-collector
                     #'scrollview--collect-conflict-lines 'middle))
   (list :variant 'bottom
         :priority 80
         :bitmap 'scrollview-sign-dot-bitmap
         :face 'scrollview-conflict-bottom-face
         :collector (scrollview--variant-collector
                     #'scrollview--collect-conflict-lines 'bottom))))

(defun scrollview--keyword-sign-specs ()
  "Return built-in keyword sign specs."
  (cl-loop for (variant . patterns) in scrollview-keyword-patterns
           when (and (symbolp variant) patterns)
           collect
           (list :variant variant
                 :priority (scrollview--keyword-priority variant)
                 :bitmap (scrollview--keyword-bitmap variant)
                 :face (scrollview--keyword-face variant)
                 :collector (scrollview--variant-collector
                             #'scrollview--collect-keyword-lines variant))))

(defun scrollview--vc-sign-specs ()
  "Return built-in VC sign specs."
  (list
   (list :variant 'add
         :priority 30
         :bitmap 'scrollview-sign-bar-bitmap
         :face 'scrollview-vc-add-face
         :collector (scrollview--variant-collector
                     #'scrollview--collect-vc-lines 'add))
   (list :variant 'change
         :priority 30
         :bitmap 'scrollview-sign-bar-bitmap
         :face 'scrollview-vc-change-face
         :collector (scrollview--variant-collector
                     #'scrollview--collect-vc-lines 'change))
   (list :variant 'delete
         :priority 30
         :bitmap 'scrollview-sign-delete-bitmap
         :face 'scrollview-vc-delete-face
         :collector (scrollview--variant-collector
                     #'scrollview--collect-vc-lines 'delete))))

(defun scrollview--initialize-builtins ()
  "Register built-in sign groups once."
  (unless scrollview--builtins-initialized
    (setq scrollview--builtins-initialized t)
    (scrollview--register-builtin-sign-group
     'search
     (list (list :variant 'match
                 :priority 70
                 :bitmap 'scrollview-search-bitmap
                 :face 'scrollview-search-face
                 :collector #'scrollview--collect-search-lines)))
    (scrollview--register-builtin-sign-group
     'diagnostics (scrollview--diagnostic-sign-specs))
    (scrollview--register-builtin-sign-group
     'conflicts (scrollview--conflict-sign-specs))
    (scrollview--register-builtin-sign-group
     'keywords (scrollview--keyword-sign-specs))
    (scrollview--register-builtin-sign-group
     'spell
     (list (list :variant 'misspelled
                 :priority 35
                 :bitmap 'scrollview-spell-bitmap
                 :face 'scrollview-spell-face
                 :collector #'scrollview--collect-spell-lines)))
    (scrollview--register-builtin-sign-group
     'vc (scrollview--vc-sign-specs))

    (add-hook 'isearch-update-post-hook #'scrollview--after-isearch-update)
    (add-hook 'isearch-mode-end-hook #'scrollview--after-isearch-end)
    (unless (advice-member-p #'scrollview--after-lazy-highlight-cleanup
                             'lazy-highlight-cleanup)
      (advice-add 'lazy-highlight-cleanup
                  :after #'scrollview--after-lazy-highlight-cleanup))))


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

(defun scrollview--group-matches-p (group groups)
  "Return non-nil if GROUP is selected by GROUPS."
  (or (null groups)
      (memq 'all groups)
      (memq group groups)))

(defun scrollview--collect-sign-items (window &optional groups)
  "Collect visible sign items for WINDOW.
GROUPS may be nil, a symbol, or a list of symbols."
  (let ((groups (cond
                 ((null groups) nil)
                 ((listp groups) (mapcar #'scrollview--normalize-group groups))
                 (t (list (scrollview--normalize-group groups)))))
        (buffer-lines (with-current-buffer (window-buffer window)
                        (scrollview--line-count)))
        items)
    (unless (with-current-buffer (window-buffer window)
              (scrollview--restricted-p))
      (maphash
       (lambda (_id spec)
         (let ((group (scrollview--sign-spec-group spec)))
           (when (and (scrollview-sign-group-active-p group)
                      (scrollview--group-matches-p group groups)
                      (or (not (scrollview--sign-spec-current-only spec))
                          (eq window (selected-window))))
             (with-current-buffer (window-buffer window)
               (let (lines)
                 (setq lines
                       (ignore-errors
                         (funcall (scrollview--sign-spec-collector spec)
                                  window)))
                 (dolist (line lines)
                   (when-let ((line (scrollview--normalize-line
                                     line buffer-lines)))
                     (push (list :line line :spec spec) items))))))))
       scrollview--sign-specs))
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

(defun scrollview--put-slot (slots row slot)
  "Put SLOT into SLOTS at ROW if it has higher priority."
  (let ((old (aref slots row)))
    (when (scrollview--slot-better-p slot old)
      (aset slots row slot))))

(defun scrollview--build-slots (_window info sign-items)
  "Return fringe slots using INFO and SIGN-ITEMS."
  (let* ((window-lines (plist-get info :window-lines))
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
             (row (scrollview--line-to-row line window-lines buffer-lines)))
        (scrollview--put-slot
         slots row
         (list :type 'sign
               :priority (scrollview--sign-spec-priority spec)
               :order (scrollview--sign-spec-id spec)
               :bitmap (scrollview--sign-spec-bitmap spec)
               :face (scrollview--sign-render-face
                      (scrollview--sign-spec-face spec))
               :line line
               :group (scrollview--sign-spec-group spec)
               :variant (scrollview--sign-spec-variant spec)
               :help-echo (format "scrollview %s sign at line %d"
                                  (scrollview--sign-spec-group spec)
                                  line)))))
    slots))

(defun scrollview--make-overlay (window row slot)
  "Make a fringe overlay for SLOT at zero-based ROW in WINDOW."
  (with-selected-window window
    (save-excursion
      (goto-char (window-start window))
      (vertical-motion row)
      (let* ((pos (point))
             (pos (if (= pos (line-end-position))
                      pos
                    (min (point-max) (1+ pos))))
             (display `(,(scrollview--fringe-side)
                        ,(plist-get slot :bitmap)
                        ,(plist-get slot :face)))
             (string (propertize "." 'display display))
             (overlay (make-overlay pos pos)))
        (overlay-put overlay 'after-string string)
        (overlay-put overlay 'window window)
        (overlay-put overlay 'priority scrollview-overlay-priority)
        (overlay-put overlay 'scrollview t)
        (overlay-put overlay 'help-echo (plist-get slot :help-echo))
        overlay))))

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
  (scrollview--delete-window-overlays window)
  (when (scrollview--window-eligible-p window)
    (let* ((info (scrollview--position-info window))
           (sign-items (scrollview--collect-sign-items-cached
                        window reuse-signs)))
      (when (scrollview--should-render-p info sign-items)
        (let ((slots (scrollview--build-slots window info sign-items))
              overlays)
          (cl-loop for row from 0 below (length slots)
                   for slot = (aref slots row)
                   when slot
                   do (push (scrollview--make-overlay window row slot)
                            overlays))
          (puthash window overlays scrollview--window-overlays))))))

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

(defun scrollview--schedule-refresh-all ()
  "Schedule a refresh for all windows."
  (scrollview--schedule-refresh nil))

(defun scrollview--schedule-buffer-refresh (&optional buffer)
  "Schedule a refresh for windows showing BUFFER."
  (dolist (window (get-buffer-window-list (or buffer (current-buffer)) nil t))
    (scrollview--schedule-refresh window)))

(defun scrollview--after-window-scroll (window _start)
  "Refresh WINDOW immediately after it scrolls.
Keeping this synchronous prevents stale fringe overlays from riding along with
the text for one redisplay frame before the debounced refresh corrects them."
  (scrollview--refresh-now window t))

(defun scrollview--after-change (&rest _)
  "Refresh windows showing the current buffer after buffer changes."
  (scrollview--invalidate-buffer-sign-cache)
  (scrollview--schedule-buffer-refresh))

(defun scrollview--window-configuration-change ()
  "Refresh after window configuration changes."
  (scrollview--schedule-refresh-all))

(defun scrollview--window-size-change (_frame)
  "Refresh after window size changes."
  (scrollview--schedule-refresh-all))

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

(defun scrollview--after-diagnostics-update (&rest _)
  "Refresh scrollview signs after diagnostics are updated."
  (when (bound-and-true-p scrollview-mode)
    (scrollview--sync-diagnostic-faces)
    (scrollview--invalidate-buffer-sign-cache)
    (scrollview--schedule-buffer-refresh)))

(with-eval-after-load 'flymake
  (when (fboundp 'flymake--publish-diagnostics)
    (advice-add 'flymake--publish-diagnostics
                :after #'scrollview--after-diagnostics-update)))

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-after-syntax-check-hook
            #'scrollview--after-diagnostics-update))

(defun scrollview--after-spell-update (&rest _)
  "Refresh scrollview signs after flyspell overlays may have changed."
  (when (bound-and-true-p scrollview-mode)
    (cl-incf scrollview--spell-state-generation)
    (when (scrollview-sign-group-active-p 'spell)
      (scrollview--invalidate-buffer-sign-cache)
      (scrollview--schedule-buffer-refresh))))

(with-eval-after-load 'flyspell
  (add-hook 'flyspell-mode-hook #'scrollview--after-spell-update)
  (dolist (function '(flyspell-word flyspell-region flyspell-buffer))
    (when (and (fboundp function)
               (not (advice-member-p #'scrollview--after-spell-update
                                      function)))
      (advice-add function :after #'scrollview--after-spell-update))))


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
         (current (line-number-at-pos nil t))
         target)
    (unless lines
      (user-error "No scrollview signs are visible"))
    (setq target
          (pcase location
            ('first (car lines))
            ('last (car (last lines)))
            ('next (or (nth (1- count)
                            (cl-remove-if-not
                             (lambda (line) (> line current)) lines))
                       (and scrollview-wrap-navigation
                            (nth (mod (1- count) (length lines)) lines))
                       (car (last lines))))
            ('prev (let ((previous
                          (nreverse
                           (cl-remove-if-not
                            (lambda (line) (< line current)) lines))))
                     (or (nth (1- count) previous)
                         (and scrollview-wrap-navigation
                              (nth (mod (1- count) (length lines))
                                   (reverse lines)))
                         (car lines))))))
    (goto-char (point-min))
    (forward-line (1- target))))

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

;;;###autoload
(define-minor-mode scrollview-mode
  "Display a fringe scrollbar and document signs in the current buffer."
  :lighter " sv"
  :group 'scrollview
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

(provide 'scrollview)

;;; scrollview.el ends here
