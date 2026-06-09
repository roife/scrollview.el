;;; scrollview-signs.el --- Built-in signs for scrollview -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Internal module for scrollview.el.

;;; Code:

(require 'cl-lib)
(require 'isearch)
(require 'subr-x)
(require 'scrollview-core)

(declare-function flymake-diagnostics "flymake" (&optional beg end))
(declare-function flymake-diagnostic-beg "flymake" (diag))
(declare-function flymake-diagnostic-type "flymake" (diag))

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



(provide 'scrollview-signs)

;;; scrollview-signs.el ends here
