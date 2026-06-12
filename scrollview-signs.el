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
(declare-function diff-hl-changes-from-buffer "diff-hl" (buf))
(declare-function flyspell-overlay-p "flyspell" (overlay))

(defvar hl-todo-keyword-faces)
(defvar diff-hl-reference-revision)
(defvar diff-hl-show-staged-changes)
(defvar diff-hl-update-async)

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
     ((numberp level)
      (cond
       ((<= level 1) 'error)
       ((= level 2) 'warning)
       (t 'info)))
     (t 'info))))

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
           (result (list :error nil :warning nil :info nil)))
       (when (fboundp 'flymake-diagnostics)
         (ignore-errors
           (dolist (diag (flymake-diagnostics (point-min) (point-max)))
             (let ((level (scrollview--diagnostic-level
                           (flymake-diagnostic-type diag))))
               (let ((key (scrollview--variant-key level)))
                 (plist-put result key
                            (cons (line-number-at-pos
                                   (flymake-diagnostic-beg diag) t)
                                  (plist-get result key))))))))
       (when (and (boundp 'flycheck-current-errors)
                  (fboundp 'flycheck-error-line)
                  (fboundp 'flycheck-error-level))
         (ignore-errors
           (dolist (err (symbol-value 'flycheck-current-errors))
             (let ((level (scrollview--diagnostic-level
                           (flycheck-error-level err))))
               (when-let ((line (flycheck-error-line err)))
                 (let ((key (scrollview--variant-key level)))
                   (plist-put result key
                              (cons line (plist-get result key)))))))))
       (list :error (scrollview--clamp-lines
                     (plist-get result :error) buffer-lines)
             :warning (scrollview--clamp-lines
                       (plist-get result :warning) buffer-lines)
             :info (scrollview--clamp-lines
                    (plist-get result :info) buffer-lines))))))

(defun scrollview--collect-diagnostic-lines (level &rest _)
  "Collect diagnostic lines for LEVEL from Flymake and loaded Flycheck."
  (plist-get (scrollview--diagnostic-lines)
             (scrollview--variant-key level)))

(defun scrollview--conflict-lines ()
  "Return smerge conflict marker lines as a plist."
  (scrollview--cached-collector-value
   'conflicts
   (list :tick (buffer-chars-modified-tick))
   (lambda ()
     (let (top middle bottom)
       (when (require 'smerge-mode nil t)
         (save-excursion
           (save-match-data
             (goto-char (point-min))
             (while (ignore-errors (smerge-find-conflict nil))
               (push (line-number-at-pos (match-beginning 0) t) top)
               (when (match-beginning 5)
                 (push (line-number-at-pos (match-beginning 5) t) middle))
               (push (line-number-at-pos (match-end 3) t) bottom)))))
       (list :top (scrollview--dedupe-sorted-lines top)
             :middle (scrollview--dedupe-sorted-lines middle)
             :bottom (scrollview--dedupe-sorted-lines bottom))))))

(defun scrollview--collect-conflict-lines (variant &rest _)
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
           when (ignore-errors
                  (string-match-p (concat "\\`\\(?:" keyword "\\)\\'")
                                  match))
           return (scrollview--hl-todo-keyword-variant keyword)))

(defun scrollview--ensure-syntax-propertized ()
  "Ensure syntax properties are ready for full-buffer sign scans."
  (ignore-errors
    (syntax-propertize (point-max))))

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
         (scrollview--ensure-syntax-propertized)
         (save-excursion
           (save-match-data
             (goto-char (point-min))
             (while (ignore-errors (hl-todo--search))
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
      (delq nil
            (mapcar (lambda (overlay)
                      (when (scrollview--flyspell-overlay-p overlay)
                        (line-number-at-pos (overlay-start overlay) t)))
                    (overlays-in (point-min) (point-max))))))))

(defun scrollview--collect-spell-lines (_window)
  "Collect spelling error lines from Flyspell overlays."
  (scrollview--spell-lines))

(defun scrollview--diff-hl-available-p ()
  "Return non-nil when diff-hl can provide VC changes."
  (require 'diff-hl nil t))

(defun scrollview--diff-hl-change-value (value)
  "Return diff-hl hunk tuples from VALUE."
  (cond
   ((null value) nil)
   ((listp value) value)
   ((bufferp value)
    (diff-hl-changes-from-buffer value))
   ((stringp value)
    (when-let ((buffer (get-buffer value)))
      (diff-hl-changes-from-buffer buffer)))))

(defun scrollview--diff-hl-hunks ()
  "Return diff-hl hunk tuples for the current buffer."
  (when (scrollview--diff-hl-available-p)
    (let ((diff-hl-update-async nil))
      (cl-loop for (_ . value) in (ignore-errors (diff-hl-changes))
               append (scrollview--diff-hl-change-value value)))))

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
           (result (list :add nil :change nil :delete nil)))
       (dolist (hunk (scrollview--diff-hl-hunks))
         (pcase-let ((`(,line ,inserts ,_deletes ,type) hunk))
           (when-let ((key (pcase type
                              ('insert :add)
                              ('change :change)
                              ('delete :delete))))
             (plist-put result key
                        (nconc (number-sequence
                                line (+ line (max 1
                                                  (if (eq type 'delete)
                                                      1
                                                    inserts))
                                        -1))
                               (plist-get result key))))))
       (list :add (scrollview--clamp-lines
                   (plist-get result :add) buffer-lines)
             :change (scrollview--clamp-lines
                      (plist-get result :change) buffer-lines)
             :delete (scrollview--clamp-lines
                      (plist-get result :delete) buffer-lines))))))

(defun scrollview--collect-vc-lines (variant &rest _)
  "Collect VC sign lines for VARIANT."
  (plist-get (scrollview--vc-lines)
             (scrollview--variant-key variant)))

(defun scrollview--variant-key (variant)
  "Return plist keyword for VARIANT."
  (intern (format ":%s" variant)))

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
                             :priority (scrollview--keyword-priority variant)
                             :bitmap (scrollview--keyword-bitmap variant)
                             :face (scrollview--keyword-face variant)
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
     :priority 70
     :bitmap 'scrollview-search-bitmap
     :face 'scrollview-search-face
     :collector #'scrollview--collect-search-lines)

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
     :priority 50
     :bitmap 'scrollview-diagnostic-bitmap
     :face 'scrollview-diagnostic-warning-face
     :collector (apply-partially #'scrollview--collect-diagnostic-lines
                                 'warning))
    (scrollview-register-sign-spec
     :group 'diagnostics
     :variant 'info
     :priority 40
     :bitmap 'scrollview-diagnostic-bitmap
     :face 'scrollview-diagnostic-info-face
     :collector (apply-partially #'scrollview--collect-diagnostic-lines
                                 'info))

    (scrollview-register-sign-group
     'conflicts (scrollview--startup-sign-enabled-p 'conflicts))
    (scrollview-register-sign-spec
     :group 'conflicts
     :variant 'top
     :priority 80
     :bitmap 'scrollview-sign-dot-bitmap
     :face 'scrollview-conflict-top-face
     :collector (apply-partially #'scrollview--collect-conflict-lines
                                 'top))
    (scrollview-register-sign-spec
     :group 'conflicts
     :variant 'middle
     :priority 80
     :bitmap 'scrollview-sign-dot-bitmap
     :face 'scrollview-conflict-middle-face
     :collector (apply-partially #'scrollview--collect-conflict-lines
                                 'middle))
    (scrollview-register-sign-spec
     :group 'conflicts
     :variant 'bottom
     :priority 80
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
     :priority 35
     :bitmap 'scrollview-spell-bitmap
     :face 'scrollview-spell-face
     :collector #'scrollview--collect-spell-lines)

    (scrollview-register-sign-group
     'vc (scrollview--startup-sign-enabled-p 'vc))
    (scrollview-register-sign-spec
     :group 'vc
     :variant 'add
     :priority 30
     :bitmap 'scrollview-sign-bar-bitmap
     :face 'scrollview-vc-add-face
     :collector (apply-partially #'scrollview--collect-vc-lines 'add))
    (scrollview-register-sign-spec
     :group 'vc
     :variant 'change
     :priority 30
     :bitmap 'scrollview-sign-bar-bitmap
     :face 'scrollview-vc-change-face
     :collector (apply-partially #'scrollview--collect-vc-lines 'change))
    (scrollview-register-sign-spec
     :group 'vc
     :variant 'delete
     :priority 30
     :bitmap 'scrollview-sign-delete-bitmap
     :face 'scrollview-vc-delete-face
     :collector (apply-partially #'scrollview--collect-vc-lines 'delete))

    (add-hook 'isearch-update-post-hook #'scrollview--after-isearch-update)
    (add-hook 'isearch-mode-end-hook #'scrollview--after-isearch-end)
    (unless (advice-member-p #'scrollview--after-lazy-highlight-cleanup
                             'lazy-highlight-cleanup)
      (advice-add 'lazy-highlight-cleanup
                  :after #'scrollview--after-lazy-highlight-cleanup))))


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
