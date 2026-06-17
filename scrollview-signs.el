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
(declare-function smerge-find-conflict "smerge-mode" (&optional limit))
(declare-function hl-todo--search "hl-todo" (&optional regexp bound backward))
(declare-function diff-hl-changes "diff-hl" ())
(declare-function flyspell-overlay-p "flyspell" (overlay))
(declare-function compilation--ensure-parse "compile" (limit))
(declare-function compilation--message->loc "compile" (message))
(declare-function compilation--message->type "compile" (message))
(declare-function symbol-overlay-get-list "symbol-overlay" (&optional index symbol))

(defvar bookmark-alist)
(defvar eglot--highlights)
(defvar highlight-changes-mode)
(defvar highlight-changes-visible-mode)
(defvar highlight-symbol-keyword-alist)
(defvar hl-todo-keyword-faces)
(defvar diff-hl-reference-revision)
(defvar diff-hl-show-staged-changes)
(defvar diff-hl-update-async)

(defvar scrollview--bookmark-state-generation 0
  "Generation incremented after bookmark updates.")

(defvar scrollview--compilation-state-generation 0
  "Generation incremented after compilation output updates.")

(defvar-local scrollview--eglot-highlight-state-generation 0
  "Buffer-local generation incremented after Eglot highlight updates.")

(defvar-local scrollview--eglot-highlight-token nil
  "Buffer-local token for the last observed Eglot highlight overlays.")

(defvar-local scrollview--highlight-symbol-state-generation 0
  "Buffer-local generation incremented after highlight-symbol updates.")

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
  (if-let ((source (scrollview--active-isearch-source)))
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
     (t 'info))))

(defun scrollview--flymake-diagnostic-line (diag)
  "Return the live current-buffer line for Flymake DIAG."
  (let ((beg (flymake-diagnostic-beg diag)))
    (when (and (integerp beg)
               (<= (point-min) beg)
               (<= beg (point-max)))
      (line-number-at-pos beg t))))

(defun scrollview--diagnostic-lines ()
  "Collect diagnostic lines from Flymake and Flycheck in one pass."
  (scrollview--cached-collector-value
   'diagnostics
   (list :tick (buffer-chars-modified-tick)
         :generation scrollview--diagnostic-state-generation
         :flycheck (and (boundp 'flycheck-current-errors)
                        (symbol-value 'flycheck-current-errors)))
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
             (when-let ((line (scrollview--flymake-diagnostic-line diag)))
               (setcdr cell (cons line (cdr cell)))))))
       (when (and (boundp 'flycheck-current-errors)
                  (fboundp 'flycheck-error-line)
                  (fboundp 'flycheck-error-level))
         (dolist (err (symbol-value 'flycheck-current-errors))
           (let ((level (scrollview--diagnostic-level
                         (flycheck-error-level err))))
             (when-let ((line (flycheck-error-line err)))
               (let ((cell (assq level result)))
                 (setcdr cell (cons line (cdr cell))))))))
       (mapcar (lambda (cell)
                 (cons (car cell)
                       (scrollview--clamp-lines (cdr cell) buffer-lines)))
               result)))))

(defun scrollview--collect-diagnostic-lines (level &rest _)
  "Collect diagnostic lines for LEVEL from Flymake and loaded Flycheck."
  (cdr (assq level (scrollview--diagnostic-lines))))

(defun scrollview--compilation-buffers ()
  "Return live compilation buffers, excluding grep buffers."
  (when (require 'compile nil t)
    (seq-filter
     (lambda (buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'compilation-mode)
              (not (derived-mode-p 'grep-mode)))))
     (buffer-list))))

(defun scrollview--compilation-buffer-token ()
  "Return a token describing current compilation buffers."
  (mapcar (lambda (buffer)
            (with-current-buffer buffer
              (list buffer (buffer-chars-modified-tick))))
          (scrollview--compilation-buffers)))

(defun scrollview--compilation-type-level (type)
  "Return a sign level for compilation message TYPE."
  (pcase type (2 'error) (1 'warning) (_ 'info)))

(defun scrollview--compilation-message-list ()
  "Return parsed compilation messages in the current compilation buffer."
  (when (fboundp 'compilation--ensure-parse)
    (compilation--ensure-parse (point-max)))
  (let ((pos (point-min))
        messages)
    (while (< pos (point-max))
      (let ((message (get-text-property pos 'compilation-message))
            (next (next-single-property-change
                   pos 'compilation-message nil (point-max))))
        (when message
          (cl-pushnew message messages :test #'eq))
        (setq pos (or next (point-max)))))
    (nreverse messages)))

(defun scrollview--compilation-file-spec-name (file-spec)
  "Return absolute file name described by compilation FILE-SPEC."
  (when-let ((file (car-safe file-spec)))
    (when (stringp file)
      (let ((directory (cond
                        ((stringp (cdr-safe file-spec))
                         (cdr-safe file-spec))
                        ((consp (cdr-safe file-spec))
                         (cadr file-spec)))))
        (expand-file-name file directory)))))

(defun scrollview--compilation-file-struct-matches-p
    (file-struct source-buffer source-file)
  "Return non-nil when FILE-STRUCT points at SOURCE-BUFFER or SOURCE-FILE."
  (let* ((file-spec (car-safe file-struct))
         (target (car-safe file-spec)))
    (cond
     ((bufferp target)
      (eq target source-buffer))
     ((and source-file (stringp target))
      (scrollview--same-file-p
       source-file
       (scrollview--compilation-file-spec-name file-spec))))))

(defun scrollview--compilation-loc-line (loc source-buffer source-file)
  "Return source line for compilation LOC in SOURCE-BUFFER or SOURCE-FILE."
  (let ((marker (nth 3 loc))
        (line (cadr loc))
        (file-struct (nth 2 loc)))
    (cond
     ((and (markerp marker)
           (eq (marker-buffer marker) source-buffer)
           (marker-position marker))
      (with-current-buffer source-buffer
        (line-number-at-pos marker t)))
     ((and (integerp line)
           (scrollview--compilation-file-struct-matches-p
            file-struct source-buffer source-file))
      line))))

(defun scrollview--compilation-message-line
    (message source-buffer source-file)
  "Return source line for compilation MESSAGE in SOURCE-BUFFER or SOURCE-FILE."
  (when-let ((loc (compilation--message->loc message)))
    (scrollview--compilation-loc-line loc source-buffer source-file)))

(defun scrollview--compilation-lines ()
  "Collect compilation result lines grouped by severity."
  (let ((source-buffer (current-buffer))
        (source-file (buffer-file-name))
        (compilation-token (scrollview--compilation-buffer-token)))
    (scrollview--cached-collector-value
     'compilation
     (list :tick (buffer-chars-modified-tick)
           :generation scrollview--compilation-state-generation
           :source-file source-file
           :compilation compilation-token)
     (lambda ()
       (let ((result (list (cons 'error nil)
                           (cons 'warning nil)
                           (cons 'info nil))))
         (dolist (buffer (scrollview--compilation-buffers))
           (with-current-buffer buffer
             (dolist (message (scrollview--compilation-message-list))
               (when-let ((line (scrollview--compilation-message-line
                                  message source-buffer source-file)))
                 (let* ((level (scrollview--compilation-type-level
                                (compilation--message->type message)))
                        (cell (assq level result)))
                   (setcdr cell (cons line (cdr cell))))))))
         (with-current-buffer source-buffer
           (let ((buffer-lines (scrollview--line-count)))
             (mapcar (lambda (cell)
                       (cons (car cell)
                             (scrollview--clamp-lines (cdr cell)
                                                      buffer-lines)))
                     result))))))))

(defun scrollview--collect-compilation-lines (level &rest _)
  "Collect compilation result lines for LEVEL."
  (cdr (assq level (scrollview--compilation-lines))))

(defun scrollview--regexp-lines (pattern)
  "Return buffer lines matching regexp PATTERN."
  (when (and (stringp pattern)
             (not (string-empty-p pattern)))
    (scrollview--scan-search-lines pattern t)))

(defun scrollview--highlight-symbol-patterns ()
  "Return active highlight-symbol regexps for the current buffer."
  (let (patterns)
    (when (boundp 'highlight-symbol-keyword-alist)
      (dolist (entry highlight-symbol-keyword-alist)
        (when-let ((pattern (car-safe entry)))
          (when (and (stringp pattern)
                     (not (string-empty-p pattern)))
            (cl-pushnew pattern patterns :test #'equal)))))
    (nreverse patterns)))

(defun scrollview--highlight-symbol-lines ()
  "Return lines highlighted by highlight-symbol."
  (let ((patterns (scrollview--highlight-symbol-patterns)))
    (scrollview--cached-collector-value
     'highlight-symbol
     (list :tick (buffer-chars-modified-tick)
           :generation scrollview--highlight-symbol-state-generation
           :patterns patterns)
     (lambda ()
       (scrollview--dedupe-sorted-lines
        (cl-loop for pattern in patterns
                 append (scrollview--regexp-lines pattern)))))))

(defun scrollview--collect-highlight-symbol-lines (_window)
  "Collect lines highlighted by highlight-symbol."
  (scrollview--highlight-symbol-lines))

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
  (mapcar (lambda (overlay)
            (list (overlay-start overlay)
                  (overlay-end overlay)
                  (overlay-get overlay 'face)))
          overlays))

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

(defun scrollview--conflict-lines ()
  "Return smerge conflict marker lines as an alist by variant."
  (scrollview--cached-collector-value
   'conflicts
   (list :tick (buffer-chars-modified-tick))
   (lambda ()
     (let (top middle bottom)
       (when (require 'smerge-mode nil t)
         (save-excursion
           (save-match-data
             (goto-char (point-min))
             (while (smerge-find-conflict nil)
               (push (line-number-at-pos (match-beginning 0) t) top)
               (when (match-beginning 5)
                 (push (line-number-at-pos (match-beginning 5) t) middle))
               (push (line-number-at-pos (match-end 3) t) bottom)))))
       (list (cons 'top (scrollview--dedupe-sorted-lines top))
             (cons 'middle (scrollview--dedupe-sorted-lines middle))
             (cons 'bottom (scrollview--dedupe-sorted-lines bottom)))))))

(defun scrollview--collect-conflict-lines (variant &rest _)
  "Collect conflict marker lines for VARIANT."
  (cdr (assq variant (scrollview--conflict-lines))))

(defun scrollview--hl-todo-available-p ()
  "Return non-nil when hl-todo can provide keyword matching."
  (require 'hl-todo nil t))

(defun scrollview--hl-todo-keyword-variant (keyword)
  "Return a sign variant symbol for hl-todo KEYWORD."
  (let ((name (downcase
               (string-trim
                (replace-regexp-in-string "[^[:alnum:]]+" "-" keyword)
                "-" "-"))))
    (if (string-empty-p name)
        'keyword
      (intern name))))

(defun scrollview--hl-todo-match-variant (match)
  "Return the hl-todo variant whose configured keyword matches MATCH."
  (cl-loop for (keyword . _) in (and (boundp 'hl-todo-keyword-faces)
                                     hl-todo-keyword-faces)
           when (string-match-p (concat "\\`\\(?:" keyword "\\)\\'") match)
           return (scrollview--hl-todo-keyword-variant keyword)))

(defun scrollview--hl-todo-lines ()
  "Return hl-todo keyword lines grouped by variant."
  (scrollview--cached-collector-value
   'keywords
   (list :tick (buffer-chars-modified-tick)
         :keyword-faces (and (boundp 'hl-todo-keyword-faces)
                             hl-todo-keyword-faces))
   (lambda ()
     (let (lines)
       (when (scrollview--hl-todo-available-p)
         (syntax-propertize (point-max))
         (save-excursion
           (save-match-data
             (goto-char (point-min))
             (while (hl-todo--search)
               (when-let* ((keyword (match-string-no-properties 2))
                           (variant (scrollview--hl-todo-match-variant
                                     keyword)))
                 (let ((line (line-number-at-pos (match-beginning 1) t))
                       (cell (assq variant lines)))
                   (if cell
                       (setcdr cell (cons line (cdr cell)))
                     (push (cons variant (list line)) lines))))))))
       (mapcar (lambda (entry)
                 (cons (car entry)
                       (scrollview--dedupe-sorted-lines (cdr entry))))
               lines)))))

(defun scrollview--collect-keyword-lines (variant &rest _)
  "Collect keyword lines for VARIANT."
  (cdr (assq variant (scrollview--hl-todo-lines))))

(defun scrollview--flyspell-note-update (&rest _)
  "Invalidate spell signs after Flyspell overlays change."
  (cl-incf scrollview--spell-state-generation)
  (when (bound-and-true-p scrollview-mode)
    (scrollview--invalidate-buffer-sign-cache)
    (scrollview--schedule-buffer-refresh)))

(defun scrollview--flyspell-overlay-p (overlay)
  "Return non-nil when OVERLAY is owned by Flyspell."
  (or (overlay-get overlay 'flyspell-overlay)
      (and (fboundp 'flyspell-overlay-p)
           (flyspell-overlay-p overlay))))

(defun scrollview--spell-lines ()
  "Return lines containing Flyspell misspelling overlays."
  (scrollview--cached-collector-value
   'spell
   (list :tick (buffer-chars-modified-tick)
         :generation scrollview--spell-state-generation)
   (lambda ()
     (scrollview--dedupe-sorted-lines
      (cl-loop for overlay in (overlays-in (point-min) (point-max))
               when (scrollview--flyspell-overlay-p overlay)
               collect (line-number-at-pos (overlay-start overlay) t))))))

(defun scrollview--collect-spell-lines (_window)
  "Collect spelling error lines from Flyspell overlays."
  (scrollview--spell-lines))

(defun scrollview--diff-hl-available-p ()
  "Return non-nil when diff-hl can provide VC changes."
  (require 'diff-hl nil t))

(defun scrollview--diff-hl-hunks ()
  "Return diff-hl hunk tuples for the current buffer."
  (when (scrollview--diff-hl-available-p)
    (let ((diff-hl-update-async nil))
      (cl-loop for (_ . value) in (diff-hl-changes)
               when (listp value)
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

(defun scrollview--keyword-sign-specs ()
  "Return built-in keyword sign specs from hl-todo."
  (when (scrollview--hl-todo-available-p)
    (cl-loop with seen
             for (keyword . _) in (and (boundp 'hl-todo-keyword-faces)
                                       hl-todo-keyword-faces)
             for variant = (scrollview--hl-todo-keyword-variant keyword)
             unless (memq variant seen)
             collect (progn
                       (push variant seen)
                       (list :variant variant
                             :priority (scrollview--keyword-attr variant
                                                                   :priority)
                             :bitmap (scrollview--keyword-attr variant
                                                               :bitmap)
                             :face (scrollview--keyword-attr variant
                                                             :face)
                             :collector (apply-partially
                                         #'scrollview--collect-keyword-lines
                                         variant))))))

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
     'highlight-symbol (scrollview--startup-sign-enabled-p
                        'highlight-symbol))
    (scrollview-register-sign-spec
     :group 'highlight-symbol
     :variant 'match
     :priority 70
     :bitmap 'scrollview-search-bitmap
     :face 'scrollview-highlight-symbol-face
     :collector #'scrollview--collect-highlight-symbol-lines)

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
     'compilation (scrollview--startup-sign-enabled-p 'compilation))
    (scrollview-register-sign-spec
     :group 'compilation
     :variant 'error
     :priority 60
     :bitmap 'scrollview-diagnostic-bitmap
     :face 'scrollview-compilation-error-face
     :collector (apply-partially #'scrollview--collect-compilation-lines
                                 'error))
    (scrollview-register-sign-spec
     :group 'compilation
     :variant 'warning
     :priority 58
     :bitmap 'scrollview-diagnostic-bitmap
     :face 'scrollview-compilation-warning-face
     :collector (apply-partially #'scrollview--collect-compilation-lines
                                 'warning))
    (scrollview-register-sign-spec
     :group 'compilation
     :variant 'info
     :priority 35
     :bitmap 'scrollview-diagnostic-bitmap
     :face 'scrollview-compilation-info-face
     :collector (apply-partially #'scrollview--collect-compilation-lines
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
     'keywords (scrollview--startup-sign-enabled-p 'keywords))
    (dolist (spec (scrollview--keyword-sign-specs))
      (apply #'scrollview-register-sign-spec :group 'keywords spec))

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
    (add-hook 'post-command-hook #'scrollview--after-eglot-post-command)
    (unless (advice-member-p #'scrollview--after-lazy-highlight-cleanup
                             'lazy-highlight-cleanup)
      (advice-add 'lazy-highlight-cleanup
                  :after #'scrollview--after-lazy-highlight-cleanup))))


(defun scrollview--after-eglot-post-command ()
  "Refresh Eglot signs when document-highlight overlays change."
  (when (and (bound-and-true-p scrollview-mode)
             (scrollview-sign-group-active-p 'eglot)
             (scrollview--eglot-available-p))
    (let* ((overlays (scrollview--eglot-highlight-overlays))
           (token (scrollview--eglot-highlight-token-value overlays)))
      (unless (equal token scrollview--eglot-highlight-token)
        (setq scrollview--eglot-highlight-token token)
        (cl-incf scrollview--eglot-highlight-state-generation)
        (scrollview--invalidate-buffer-sign-cache)
        (scrollview--schedule-buffer-refresh)))))


(defun scrollview--after-highlight-symbol-update (&rest _)
  "Refresh scrollview signs after highlight-symbol updates."
  (when (bound-and-true-p scrollview-mode)
    (cl-incf scrollview--highlight-symbol-state-generation)
    (scrollview--invalidate-buffer-sign-cache)
    (scrollview--schedule-buffer-refresh)))

(with-eval-after-load 'highlight-symbol
  (dolist (function '(highlight-symbol
                      highlight-symbol-add-symbol
                      highlight-symbol-remove-symbol
                      highlight-symbol-remove-all
                      highlight-symbol-temp-highlight
                      highlight-symbol-mode-remove-temp))
    (when (and (fboundp function)
               (not (advice-member-p
                     #'scrollview--after-highlight-symbol-update function)))
      (advice-add function :after
                  #'scrollview--after-highlight-symbol-update))))


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


(defun scrollview--after-compilation-update (&rest _)
  "Refresh scrollview signs after compilation output updates."
  (cl-incf scrollview--compilation-state-generation)
  (when (scrollview-sign-group-active-p 'compilation)
    (scrollview--invalidate-sign-cache)
    (scrollview--schedule-refresh)))

(with-eval-after-load 'compile
  (add-hook 'compilation-filter-hook #'scrollview--after-compilation-update)
  (add-hook 'compilation-finish-functions
            #'scrollview--after-compilation-update))


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

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-after-syntax-check-hook
            #'scrollview--after-diagnostics-update))

(with-eval-after-load 'flyspell
  (dolist (function '(flyspell-highlight-incorrect-region
                      flyspell-unhighlight-at
                      flyspell-delete-all-overlays
                      flyspell-delete-region-overlays))
    (when (and (fboundp function)
               (not (advice-member-p #'scrollview--flyspell-note-update
                                     function)))
      (advice-add function :after #'scrollview--flyspell-note-update))))

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
