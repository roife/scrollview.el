;;; scrollview-signs.el --- Built-in signs for scrollview -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Internal module for scrollview.el.

;;; Code:

(require 'cl-lib)
(require 'isearch)
(require 'seq)
(require 'subr-x)
(require 'scrollview-core)

(declare-function flymake-diagnostics "flymake" (&optional beg end))
(declare-function flymake-diagnostic-beg "flymake" (diag))
(declare-function flymake-diagnostic-type "flymake" (diag))
(declare-function smerge-match-conflict "smerge-mode" ())
(declare-function diff-hl-changes "diff-hl" ())
(declare-function diff-hl-changes-from-buffer "diff-hl" (buf))
(declare-function flyspell-overlay-p "flyspell" (overlay))
(declare-function symbol-overlay-get-list "symbol-overlay" (&optional index symbol))

(defvar bookmark-alist)
(defvar eglot--highlights)
(defvar highlight-changes-mode)
(defvar highlight-changes-visible-mode)
(defvar diff-hl-reference-revision)
(defvar diff-hl-show-staged-changes)
(defvar diff-hl-update-async)

(defvar scrollview--bookmark-state-generation 0
  "Generation incremented after bookmark updates.")

(defvar-local scrollview--eglot-highlight-state-generation 0
  "Buffer-local generation incremented after Eglot highlight updates.")

(defvar-local scrollview--eglot-highlight-token nil
  "Buffer-local token for the last observed Eglot highlight overlays.")

(defvar-local scrollview--highlight-changes-state-generation 0
  "Buffer-local generation incremented after Highlight Changes updates.")

(defvar-local scrollview--symbol-overlay-state-generation 0
  "Buffer-local generation incremented after symbol-overlay updates.")

(defvar-local scrollview--vc-state-generation 0
  "Buffer-local generation incremented after diff-hl updates.")

;;; Built-in sign collectors

(defun scrollview--active-isearch-source ()
  "Return active isearch source as (PATTERN REGEXP)."
  (when (and isearch-mode
             isearch-success
             (stringp isearch-string)
             (not (string-empty-p isearch-string)))
    (list isearch-string
          isearch-regexp)))

(defun scrollview--retained-isearch-source ()
  "Return retained isearch source as (PATTERN REGEXP)."
  (when (and (not (bound-and-true-p isearch-mode))
             (stringp scrollview--last-search-pattern)
             (not (string-empty-p scrollview--last-search-pattern))
             (cl-some (lambda (overlay)
                        (and (overlayp overlay)
                             (eq (overlay-buffer overlay) (current-buffer))))
                      isearch-lazy-highlight-overlays))
    (list scrollview--last-search-pattern scrollview--last-search-regexp)))

(defun scrollview--search-source ()
  "Return search source when isearch highlights are present."
  (if (bound-and-true-p isearch-mode)
      (scrollview--active-isearch-source)
    (scrollview--retained-isearch-source)))

(defun scrollview--after-isearch-update ()
  "Refresh search signs after the active isearch changes."
  (if-let* ((source (scrollview--active-isearch-source)))
      (pcase-let ((`(,pattern ,regexp) source))
        (unless (and (equal scrollview--last-search-pattern pattern)
                     (eq scrollview--last-search-regexp regexp))
          (setq scrollview--last-search-pattern pattern)
          (setq scrollview--last-search-regexp regexp)
          (scrollview--invalidate-buffer-sign-cache)
          (scrollview--schedule-buffer-refresh)))
    (when scrollview--last-search-pattern
      (setq scrollview--last-search-pattern nil)
      (setq scrollview--last-search-regexp nil)
      (scrollview--invalidate-buffer-sign-cache)
      (scrollview--schedule-buffer-refresh))))

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
  (let ((tracker (scrollview--make-line-tracker))
        lines)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (condition-case nil
            (catch 'done
              (while (if regexp
                         (re-search-forward pattern nil t)
                       (search-forward pattern nil t))
                (let ((line (scrollview--tracked-line-number
                             (match-beginning 0) tracker)))
                  (unless (and lines (= line (car lines)))
                    (push line lines)))
                (when (= (match-beginning 0) (match-end 0))
                  (if (eobp)
                      (throw 'done nil)
                    (forward-char 1)))))
          (error nil))))
    (nreverse lines)))

(defun scrollview--collect-search-lines (_window)
  "Collect lines matching the current isearch highlight source."
  (when-let* ((source (scrollview--search-source)))
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
     (t 'info))))

(defun scrollview--flymake-diagnostic-line (diag)
  "Return the live current-buffer line for Flymake DIAG."
  (let ((beg (flymake-diagnostic-beg diag)))
    (when (and (integerp beg)
               (<= (point-min) beg)
               (<= beg (point-max)))
      (line-number-at-pos beg t))))

(defun scrollview--diagnostic-lines ()
  "Collect Flymake diagnostic lines grouped by severity."
  (scrollview--cached-collector-value
   'diagnostics
   (list :tick (buffer-chars-modified-tick)
         :generation scrollview--diagnostic-state-generation)
   (lambda ()
     (let ((buffer-lines (scrollview--line-count))
           (result (list (cons 'error nil)
                         (cons 'warning nil)
                         (cons 'info nil))))
       (when (fboundp 'flymake-diagnostics)
         (dolist (diag (flymake-diagnostics (point-min) (point-max)))
           (let* ((level (scrollview--diagnostic-level
                          (flymake-diagnostic-type diag)))
                  (cell (assq level result)))
             (when-let* ((line (scrollview--flymake-diagnostic-line diag)))
               (setcdr cell (cons line (cdr cell)))))))
       (mapcar (lambda (cell)
                 (cons (car cell)
                       (scrollview--clamp-lines (cdr cell) buffer-lines)))
               result)))))

(defun scrollview--collect-diagnostic-lines (level &rest _)
  "Collect Flymake diagnostic lines for LEVEL."
  (cdr (assq level (scrollview--diagnostic-lines))))

(defun scrollview--highlight-changes-active-p ()
  "Return non-nil when Highlight Changes signs should be visible."
  (and (bound-and-true-p highlight-changes-mode)
       (bound-and-true-p highlight-changes-visible-mode)))

(defun scrollview--property-range-lines (start end)
  "Return one-based lines touched by a text property from START to END."
  (when (< start end)
    (number-sequence (line-number-at-pos start t)
                     (line-number-at-pos (1- end) t))))

(defun scrollview--highlight-changes-property-lines (property)
  "Return lines carrying Highlight Changes text PROPERTY."
  (let ((pos (point-min))
        (limit (point-max))
        lines)
    (while (< pos limit)
      (let* ((value (get-text-property pos property))
             (next (or (next-single-property-change
                        pos property nil limit)
                       limit)))
        (when value
          (setq lines
                (nconc (scrollview--property-range-lines pos next)
                       lines)))
        (setq pos next)))
    (scrollview--dedupe-sorted-lines lines)))

(defun scrollview--highlight-changes-lines ()
  "Return Highlight Changes lines grouped by variant."
  (scrollview--cached-collector-value
   'highlight-changes
   (list :tick (buffer-chars-modified-tick)
         :mode (bound-and-true-p highlight-changes-mode)
         :visible (bound-and-true-p highlight-changes-visible-mode)
         :generation scrollview--highlight-changes-state-generation)
   (lambda ()
     (when (scrollview--highlight-changes-active-p)
       (list (cons 'change (scrollview--highlight-changes-property-lines
                            'hilit-chg))
             (cons 'delete (scrollview--highlight-changes-property-lines
                            'hilit-chg-delete)))))))

(defun scrollview--collect-highlight-changes-lines (variant &rest _)
  "Collect Highlight Changes sign lines for VARIANT."
  (cdr (assq variant (scrollview--highlight-changes-lines))))

(defun scrollview--overlay-line (overlay)
  "Return the one-based current-buffer line for OVERLAY."
  (when (and (overlayp overlay)
             (eq (overlay-buffer overlay) (current-buffer))
             (overlay-start overlay))
    (line-number-at-pos (overlay-start overlay) t)))

(defun scrollview--overlay-lines (overlays)
  "Return sorted unique current-buffer lines for OVERLAYS."
  (scrollview--dedupe-sorted-lines
   (cl-loop for overlay in overlays
            for line = (scrollview--overlay-line overlay)
            when line
            collect line)))

(defun scrollview--symbol-overlay-overlays ()
  "Return active symbol-overlay overlays for the current buffer."
  (when (fboundp 'symbol-overlay-get-list)
    (symbol-overlay-get-list 0)))

(defun scrollview--symbol-overlay-token (overlays)
  "Return a cache token for symbol-overlay OVERLAYS."
  (mapcar (lambda (overlay)
            (list (overlay-start overlay)
                  (overlay-end overlay)
                  (overlay-get overlay 'symbol)))
          overlays))

(defun scrollview--symbol-overlay-lines ()
  "Return lines highlighted by symbol-overlay."
  (let ((overlays (scrollview--symbol-overlay-overlays)))
    (scrollview--cached-collector-value
     'symbol-overlay
     (list :tick (buffer-chars-modified-tick)
           :generation scrollview--symbol-overlay-state-generation
           :overlays (scrollview--symbol-overlay-token overlays))
     (lambda ()
       (scrollview--overlay-lines overlays)))))

(defun scrollview--collect-symbol-overlay-lines (_window)
  "Collect lines highlighted by symbol-overlay."
  (scrollview--symbol-overlay-lines))

(defun scrollview--same-file-p (left right)
  "Return non-nil when LEFT and RIGHT name the same file."
  (when (and (stringp left)
             (stringp right))
    (let ((left (expand-file-name left))
          (right (expand-file-name right)))
      (or (equal left right)
          (ignore-errors (file-equal-p left right))))))

(defun scrollview--bookmark-position-line (position)
  "Return the line for bookmark integer POSITION in the current buffer."
  (when (integerp position)
    (save-excursion
      (goto-char (min (point-max) (max (point-min) position)))
      (line-number-at-pos nil t))))

(defun scrollview--bookmark-record-filename (bookmark)
  "Return the filename stored in BOOKMARK record, if any."
  (cdr (assq 'filename (cdr bookmark))))

(defun scrollview--bookmark-record-position (bookmark)
  "Return the position stored in BOOKMARK record, if any."
  (cdr (assq 'position (cdr bookmark))))

(defun scrollview--bookmark-lines ()
  "Return bookmark lines for the current file buffer."
  (let ((file (buffer-file-name)))
    (scrollview--cached-collector-value
     'bookmarks
     (list :tick (buffer-chars-modified-tick)
           :generation scrollview--bookmark-state-generation
           :file file)
     (lambda ()
       (when (and file
                  (require 'bookmark nil t)
                  (boundp 'bookmark-alist))
         (scrollview--dedupe-sorted-lines
          (cl-loop for bookmark in bookmark-alist
                   for bookmark-file = (scrollview--bookmark-record-filename
                                        bookmark)
                   for position = (scrollview--bookmark-record-position
                                   bookmark)
                   for line = (and (scrollview--same-file-p
                                    file bookmark-file)
                                   (scrollview--bookmark-position-line
                                    position))
                   when line
                   collect line)))))))

(defun scrollview--collect-bookmark-lines (_window)
  "Collect bookmark lines for the current buffer."
  (scrollview--bookmark-lines))

(defun scrollview--eglot-available-p ()
  "Return non-nil when Eglot highlight state may be present."
  (boundp 'eglot--highlights))

(defun scrollview--eglot-highlight-overlays ()
  "Return active Eglot document-highlight overlays for the current buffer."
  (when (and (boundp 'eglot--highlights)
             (listp eglot--highlights))
    (seq-filter #'overlayp eglot--highlights)))

(defun scrollview--eglot-highlight-token-value (overlays)
  "Return a cache token for Eglot highlight OVERLAYS."
  (cl-loop for overlay in overlays
           when (overlayp overlay)
           collect (list (overlay-start overlay)
                         (overlay-end overlay)
                         (overlay-get overlay 'face))))

(defun scrollview--eglot-highlight-token-matches-p (overlays token)
  "Return non-nil when OVERLAYS exactly match saved TOKEN.
The comparison walks both inputs without constructing a current-state
snapshot, so an unchanged command allocates no per-overlay cons cells."
  (catch 'different
    (let ((current overlays)
          (saved token))
      (while current
        (let ((overlay (pop current)))
          (when (overlayp overlay)
            (unless saved
              (throw 'different nil))
            (let ((entry (pop saved)))
              (unless (and (eql (overlay-start overlay) (nth 0 entry))
                           (eql (overlay-end overlay) (nth 1 entry))
                           (equal (overlay-get overlay 'face) (nth 2 entry)))
                (throw 'different nil))))))
      (null saved))))

(defun scrollview--eglot-highlight-lines ()
  "Return lines highlighted by Eglot documentHighlight overlays."
  (let ((overlays (scrollview--eglot-highlight-overlays)))
    (scrollview--cached-collector-value
     'eglot
     (list :tick (buffer-chars-modified-tick)
           :generation scrollview--eglot-highlight-state-generation
           :overlays (scrollview--eglot-highlight-token-value overlays))
     (lambda ()
       (scrollview--overlay-lines overlays)))))

(defun scrollview--collect-eglot-highlight-lines (_window)
  "Collect Eglot document-highlight lines."
  (scrollview--eglot-highlight-lines))

(defun scrollview--smerge-conflict-overlays ()
  "Return existing smerge conflict overlays in the current buffer."
  (sort
   (seq-filter
    (lambda (overlay)
      (and (overlayp overlay)
           (eq (overlay-buffer overlay) (current-buffer))
           (eq (overlay-get overlay 'smerge) 'conflict)
           (overlay-start overlay)
           (overlay-end overlay)
           (<= (point-min) (overlay-start overlay))
           (<= (overlay-end overlay) (point-max))))
    (overlays-in (point-min) (point-max)))
   (lambda (left right)
     (< (overlay-start left) (overlay-start right)))))

(defun scrollview--smerge-conflict-overlay-token (overlays)
  "Return a cache token describing smerge conflict OVERLAYS."
  (mapcar (lambda (overlay)
            (list (overlay-start overlay) (overlay-end overlay)))
          overlays))

(defun scrollview--conflict-overlay-lines (overlays)
  "Return conflict marker lines represented by smerge OVERLAYS."
  (let (top middle bottom)
    (dolist (overlay overlays)
      (let ((start (overlay-start overlay))
            (end (overlay-end overlay)))
        (when (and start end (< start end))
          (save-excursion
            (save-restriction
              (save-match-data
                (narrow-to-region start end)
                (goto-char (point-min))
                (condition-case nil
                    (when (and (fboundp 'smerge-match-conflict)
                               (smerge-match-conflict))
                      (push (line-number-at-pos (match-beginning 0) t) top)
                      (when (match-beginning 5)
                        (push (line-number-at-pos (match-beginning 5) t)
                              middle))
                      (push (line-number-at-pos (match-end 3) t) bottom))
                  (error nil))))))))
    (list (cons 'top (scrollview--dedupe-sorted-lines top))
          (cons 'middle (scrollview--dedupe-sorted-lines middle))
          (cons 'bottom (scrollview--dedupe-sorted-lines bottom)))))

(defun scrollview--conflict-lines ()
  "Return marker lines from existing smerge conflict overlays."
  (let ((overlays (scrollview--smerge-conflict-overlays)))
    (scrollview--cached-collector-value
     'conflicts
     (list :tick (buffer-chars-modified-tick)
           :overlays (scrollview--smerge-conflict-overlay-token overlays))
     (lambda ()
       (scrollview--conflict-overlay-lines overlays)))))

(defun scrollview--collect-conflict-lines (variant &rest _)
  "Collect conflict marker lines for VARIANT."
  (cdr (assq variant (scrollview--conflict-lines))))

(defun scrollview--spell-note-update (&rest _)
  "Invalidate spell signs after spell checker overlays change."
  (cl-incf scrollview--spell-state-generation)
  (when (bound-and-true-p scrollview-mode)
    (scrollview--invalidate-buffer-sign-cache)
    (scrollview--schedule-buffer-refresh)))

(defun scrollview--flyspell-overlay-p (overlay)
  "Return non-nil when OVERLAY is owned by Flyspell."
  (or (overlay-get overlay 'flyspell-overlay)
      (and (fboundp 'flyspell-overlay-p)
           (flyspell-overlay-p overlay))))

(defun scrollview--jinx-overlay-p (overlay)
  "Return non-nil when OVERLAY is owned by Jinx."
  (eq (overlay-get overlay 'category) 'jinx-overlay))

(defun scrollview--spell-overlay-p (overlay)
  "Return non-nil when OVERLAY belongs to the selected spell checker."
  (pcase scrollview-spell-checker
    ('flyspell (scrollview--flyspell-overlay-p overlay))
    ('jinx (scrollview--jinx-overlay-p overlay))))

(defun scrollview--spell-lines ()
  "Return lines containing misspellings from the selected spell checker."
  (scrollview--cached-collector-value
   'spell
   (list :tick (buffer-chars-modified-tick)
         :checker scrollview-spell-checker
         :generation scrollview--spell-state-generation)
   (lambda ()
     (scrollview--dedupe-sorted-lines
      (cl-loop for overlay in (overlays-in (point-min) (point-max))
               when (scrollview--spell-overlay-p overlay)
               collect (line-number-at-pos (overlay-start overlay) t))))))

(defun scrollview--collect-spell-lines (_window)
  "Collect spelling error lines from the selected spell checker."
  (scrollview--spell-lines))

(defun scrollview--diff-hl-available-p ()
  "Return non-nil when diff-hl can provide VC changes."
  (require 'diff-hl nil t))

(defun scrollview--diff-hl-hunks ()
  "Return diff-hl hunk tuples for the current buffer."
  (when (scrollview--diff-hl-available-p)
    (let ((diff-hl-update-async nil))
      (cl-loop for (_ . value) in (diff-hl-changes)
               ;; Recent diff-hl versions return a diff buffer name for
               ;; ordinary files, while added files may still return hunks
               ;; inline.
               if (stringp value)
               append (when-let* ((buffer (get-buffer value)))
                        (diff-hl-changes-from-buffer buffer))
               else if (listp value)
               append value))))

(defun scrollview--vc-lines ()
  "Return VC sign lines reported by diff-hl."
  (scrollview--cached-collector-value
   'vc
   (list :tick (buffer-chars-modified-tick)
         :file (buffer-file-name)
         :reference (and (boundp 'diff-hl-reference-revision)
                         diff-hl-reference-revision)
         :show-staged (and (boundp 'diff-hl-show-staged-changes)
                           diff-hl-show-staged-changes)
         :generation scrollview--vc-state-generation)
   (lambda ()
     (let ((buffer-lines (scrollview--line-count))
           (result (list (cons 'add nil)
                         (cons 'change nil)
                         (cons 'delete nil))))
       (dolist (hunk (scrollview--diff-hl-hunks))
         (pcase-let ((`(,line ,inserts ,_deletes ,type) hunk))
           (when-let* ((variant (pcase type
                                  ('insert 'add)
                                  ('change 'change)
                                  ('delete 'delete)))
                       (cell (assq variant result)))
             (setcdr cell (nconc (number-sequence
                                  line (+ line (max 1
                                                    (if (eq type 'delete)
                                                        1
                                                      inserts))
                                          -1))
                                 (cdr cell))))))
       (mapcar (lambda (cell)
                 (cons (car cell)
                       (scrollview--clamp-lines (cdr cell) buffer-lines)))
               result)))))

(defun scrollview--collect-vc-lines (variant &rest _)
  "Collect VC sign lines for VARIANT."
  (cdr (assq variant (scrollview--vc-lines))))

(defun scrollview--initialize-builtins ()
  "Register built-in sign groups once."
  (unless scrollview--builtins-initialized
    (setq scrollview--builtins-initialized t)
    (scrollview-register-sign-group
     'search (scrollview--startup-sign-enabled-p 'search))
    (scrollview-register-sign-spec
     :group 'search
     :variant 'match
     :priority 100
     :bitmap 'scrollview-search-bitmap
     :face 'scrollview-search-face
     :collector #'scrollview--collect-search-lines)

    (scrollview-register-sign-group
     'highlight-changes (scrollview--startup-sign-enabled-p
                         'highlight-changes))
    (scrollview-register-sign-spec
     :group 'highlight-changes
     :variant 'change
     :priority 80
     :bitmap 'scrollview-highlight-changes-bitmap
     :face 'scrollview-highlight-changes-face
     :collector (apply-partially #'scrollview--collect-highlight-changes-lines
                                 'change))
    (scrollview-register-sign-spec
     :group 'highlight-changes
     :variant 'delete
     :priority 80
     :bitmap 'scrollview-highlight-changes-delete-bitmap
     :face 'scrollview-highlight-changes-delete-face
     :collector (apply-partially #'scrollview--collect-highlight-changes-lines
                                 'delete))

    (scrollview-register-sign-group
     'symbol-overlay (scrollview--startup-sign-enabled-p 'symbol-overlay))
    (scrollview-register-sign-spec
     :group 'symbol-overlay
     :variant 'match
     :priority 90
     :bitmap 'scrollview-search-bitmap
     :face 'scrollview-symbol-overlay-face
     :collector #'scrollview--collect-symbol-overlay-lines)

    (scrollview-register-sign-group
     'bookmarks (scrollview--startup-sign-enabled-p 'bookmarks))
    (scrollview-register-sign-spec
     :group 'bookmarks
     :variant 'bookmark
     :priority 30
     :bitmap 'scrollview-bookmark-bitmap
     :face 'scrollview-bookmark-face
     :collector #'scrollview--collect-bookmark-lines)

    (scrollview-register-sign-group
     'eglot (scrollview--startup-sign-enabled-p 'eglot))
    (scrollview-register-sign-spec
     :group 'eglot
     :variant 'highlight
     :priority 90
     :bitmap 'scrollview-search-bitmap
     :face 'scrollview-eglot-face
     :collector #'scrollview--collect-eglot-highlight-lines)

    (scrollview-register-sign-group
     'diagnostics (scrollview--startup-sign-enabled-p 'diagnostics))
    (scrollview-register-sign-spec
     :group 'diagnostics
     :variant 'error
     :priority 60
     :bitmap 'scrollview-diagnostic-bitmap
     :face 'scrollview-diagnostic-error-face
     :collector (apply-partially #'scrollview--collect-diagnostic-lines
                                 'error))
    (scrollview-register-sign-spec
     :group 'diagnostics
     :variant 'warning
     :priority 58
     :bitmap 'scrollview-diagnostic-bitmap
     :face 'scrollview-diagnostic-warning-face
     :collector (apply-partially #'scrollview--collect-diagnostic-lines
                                 'warning))
    (scrollview-register-sign-spec
     :group 'diagnostics
     :variant 'info
     :priority 35
     :bitmap 'scrollview-diagnostic-bitmap
     :face 'scrollview-diagnostic-info-face
     :collector (apply-partially #'scrollview--collect-diagnostic-lines
                                 'info))

    (scrollview-register-sign-group
     'conflicts (scrollview--startup-sign-enabled-p 'conflicts))
    (scrollview-register-sign-spec
     :group 'conflicts
     :variant 'top
     :priority 70
     :bitmap 'scrollview-sign-dot-bitmap
     :face 'scrollview-conflict-top-face
     :collector (apply-partially #'scrollview--collect-conflict-lines
                                 'top))
    (scrollview-register-sign-spec
     :group 'conflicts
     :variant 'middle
     :priority 70
     :bitmap 'scrollview-sign-dot-bitmap
     :face 'scrollview-conflict-middle-face
     :collector (apply-partially #'scrollview--collect-conflict-lines
                                 'middle))
    (scrollview-register-sign-spec
     :group 'conflicts
     :variant 'bottom
     :priority 70
     :bitmap 'scrollview-sign-dot-bitmap
     :face 'scrollview-conflict-bottom-face
     :collector (apply-partially #'scrollview--collect-conflict-lines
                                 'bottom))

    (scrollview-register-sign-group
     'spell (scrollview--startup-sign-enabled-p 'spell))
    (scrollview-register-sign-spec
     :group 'spell
     :variant 'misspelled
     :priority 50
     :bitmap 'scrollview-spell-bitmap
     :face 'scrollview-spell-face
     :collector #'scrollview--collect-spell-lines)

    (scrollview-register-sign-group
     'vc (scrollview--startup-sign-enabled-p 'vc))
    (scrollview-register-sign-spec
     :group 'vc
     :variant 'add
     :priority 40
     :bitmap 'scrollview-sign-bar-bitmap
     :face 'scrollview-vc-add-face
     :collector (apply-partially #'scrollview--collect-vc-lines 'add))
    (scrollview-register-sign-spec
     :group 'vc
     :variant 'change
     :priority 40
     :bitmap 'scrollview-sign-bar-bitmap
     :face 'scrollview-vc-change-face
     :collector (apply-partially #'scrollview--collect-vc-lines 'change))
    (scrollview-register-sign-spec
     :group 'vc
     :variant 'delete
     :priority 40
     :bitmap 'scrollview-sign-delete-bitmap
     :face 'scrollview-vc-delete-face
     :collector (apply-partially #'scrollview--collect-vc-lines 'delete))

    (add-hook 'isearch-update-post-hook #'scrollview--after-isearch-update)
    (add-hook 'isearch-mode-end-hook #'scrollview--after-isearch-end)
    (unless (advice-member-p #'scrollview--after-lazy-highlight-cleanup
                             'lazy-highlight-cleanup)
      (advice-add 'lazy-highlight-cleanup
                  :after #'scrollview--after-lazy-highlight-cleanup))))


(defun scrollview--after-eglot-post-command ()
  "Refresh Eglot signs when document-highlight overlays change."
  (when (and (bound-and-true-p scrollview-mode)
             (scrollview-sign-group-active-p 'eglot)
             (scrollview--eglot-available-p))
    (let ((overlays (and (boundp 'eglot--highlights)
                         (listp eglot--highlights)
                         eglot--highlights)))
      (unless (scrollview--eglot-highlight-token-matches-p
               overlays scrollview--eglot-highlight-token)
        (setq scrollview--eglot-highlight-token
              (scrollview--eglot-highlight-token-value overlays))
        (cl-incf scrollview--eglot-highlight-state-generation)
        (scrollview--invalidate-buffer-sign-cache)
        (scrollview--schedule-buffer-refresh)))))


(defun scrollview--after-highlight-changes-update (&rest _)
  "Refresh scrollview signs after Highlight Changes updates."
  (cl-incf scrollview--highlight-changes-state-generation)
  (when (and (bound-and-true-p scrollview-mode)
             (scrollview-sign-group-active-p 'highlight-changes))
    (scrollview--invalidate-buffer-sign-cache)
    (scrollview--schedule-buffer-refresh)))

(with-eval-after-load 'hilit-chg
  (add-hook 'highlight-changes-mode-hook
            #'scrollview--after-highlight-changes-update)
  (add-hook 'highlight-changes-visible-mode-hook
            #'scrollview--after-highlight-changes-update)
  (dolist (function '(highlight-changes-remove-highlight
                      highlight-changes-rotate-faces
                      highlight-compare-with-file
                      highlight-compare-buffers))
    (when (and (fboundp function)
               (not (advice-member-p
                     #'scrollview--after-highlight-changes-update function)))
      (advice-add function :after
                  #'scrollview--after-highlight-changes-update))))


(defun scrollview--after-symbol-overlay-update (&rest _)
  "Refresh scrollview signs after symbol-overlay updates."
  (when (bound-and-true-p scrollview-mode)
    (cl-incf scrollview--symbol-overlay-state-generation)
    (scrollview--invalidate-buffer-sign-cache)
    (scrollview--schedule-buffer-refresh)))

(with-eval-after-load 'symbol-overlay
  (dolist (function '(symbol-overlay-put
                      symbol-overlay-put-all
                      symbol-overlay-put-one
                      symbol-overlay-remove
                      symbol-overlay-remove-all
                      symbol-overlay-remove-temp
                      symbol-overlay-maybe-remove
                      symbol-overlay-maybe-put-temp))
    (when (and (fboundp function)
               (not (advice-member-p
                     #'scrollview--after-symbol-overlay-update function)))
      (advice-add function :after
                  #'scrollview--after-symbol-overlay-update))))


(defun scrollview--after-bookmark-update (&rest _)
  "Refresh scrollview signs after bookmark updates."
  (cl-incf scrollview--bookmark-state-generation)
  (when (scrollview-sign-group-active-p 'bookmarks)
    (scrollview--invalidate-sign-cache)
    (scrollview--schedule-refresh)))

(with-eval-after-load 'bookmark
  (dolist (function '(bookmark-set
                      bookmark-set-no-overwrite
                      bookmark-delete
                      bookmark-delete-all
                      bookmark-rename
                      bookmark-relocate
                      bookmark-load
                      bookmark-bmenu-execute-deletions))
    (when (and (fboundp function)
               (not (advice-member-p
                     #'scrollview--after-bookmark-update function)))
      (advice-add function :after
                  #'scrollview--after-bookmark-update))))


(defun scrollview--after-diagnostics-update (&rest _)
  "Refresh scrollview signs after diagnostics are updated."
  (when (bound-and-true-p scrollview-mode)
    (cl-incf scrollview--diagnostic-state-generation)
    (scrollview--sync-diagnostic-faces)
    (scrollview--invalidate-buffer-sign-cache)
    (scrollview--schedule-buffer-refresh)))

(with-eval-after-load 'flymake
  (when (fboundp 'flymake--publish-diagnostics)
    (advice-add 'flymake--publish-diagnostics
                :after #'scrollview--after-diagnostics-update)))

(with-eval-after-load 'flyspell
  (dolist (function '(flyspell-highlight-incorrect-region
                      flyspell-unhighlight-at
                      flyspell-delete-all-overlays
                      flyspell-delete-region-overlays))
    (when (and (fboundp function)
               (not (advice-member-p #'scrollview--spell-note-update
                                     function)))
      (advice-add function :after #'scrollview--spell-note-update))))

(with-eval-after-load 'jinx
  (dolist (function '(jinx--check-region
                      jinx--cleanup
                      jinx--recheck-overlays))
    (when (and (fboundp function)
               (not (advice-member-p #'scrollview--spell-note-update
                                     function)))
      (advice-add function :after #'scrollview--spell-note-update))))

(defun scrollview--after-diff-hl-update (&rest _)
  "Refresh scrollview signs after diff-hl updates."
  (when (bound-and-true-p scrollview-mode)
    (cl-incf scrollview--vc-state-generation)
    (when (scrollview-sign-group-active-p 'vc)
      (scrollview--invalidate-buffer-sign-cache)
      (scrollview--schedule-buffer-refresh))))

(with-eval-after-load 'diff-hl
  (unless (advice-member-p #'scrollview--after-diff-hl-update
                           'diff-hl-update)
    (advice-add 'diff-hl-update :after
                #'scrollview--after-diff-hl-update)))



(provide 'scrollview-signs)

;;; scrollview-signs.el ends here
