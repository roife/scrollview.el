;;; scrollview-test.el --- Tests for scrollview.el -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'scrollview)

(defvar eglot--highlights nil)
(defvar flycheck-current-errors nil)
(defvar highlight-symbol nil)
(defvar highlight-symbol-keyword-alist nil)
(defvar hl-todo-keyword-faces nil)

(defun scrollview-test--reset-state ()
  "Reset scrollview global state used by tests."
  (maphash (lambda (_window overlays)
             (mapc #'delete-overlay overlays))
           scrollview--window-overlays)
  (setq scrollview--window-overlays (make-hash-table :test #'eq))
  (setq scrollview--pending-windows (make-hash-table :test #'eq))
  (setq scrollview--pending-all nil)
  (when (timerp scrollview--refresh-timer)
    (cancel-timer scrollview--refresh-timer))
  (setq scrollview--refresh-timer nil)
  (remove-hook 'window-configuration-change-hook
               #'scrollview--window-configuration-change)
  (remove-hook 'window-size-change-functions
               #'scrollview--window-size-change)
  (remove-hook 'post-command-hook #'scrollview--post-command)
  (remove-hook 'post-command-hook #'scrollview--after-eglot-post-command)
  (remove-hook 'compilation-filter-hook
               #'scrollview--after-compilation-update)
  (remove-hook 'compilation-finish-functions
               #'scrollview--after-compilation-update)
  (advice-remove 'lazy-highlight-cleanup
                 #'scrollview--after-lazy-highlight-cleanup)
  (setq scrollview--global-hooks-installed nil)
  (setq scrollview--last-selected-window nil)
  (setq scrollview--sign-groups (make-hash-table :test #'eq))
  (setq scrollview--sign-specs (make-hash-table :test #'eql))
  (setq scrollview--window-sign-cache (make-hash-table :test #'eq))
  (setq scrollview--sign-cache-generation 0)
  (setq scrollview--sign-render-face-cache (make-hash-table :test #'equal))
  (setq scrollview--thumb-face-state nil)
  (setq scrollview--diagnostic-face-state nil)
  (setq scrollview--vc-face-state nil)
  (setq scrollview--bookmark-state-generation 0)
  (setq scrollview--compilation-state-generation 0)
  (setq scrollview--next-sign-id 0)
  (setq scrollview--builtins-initialized nil)
  (setq scrollview--refreshing nil)
  (setq scrollview--last-search-pattern nil)
  (setq scrollview--last-search-regexp nil)
  (setq scrollview--eglot-highlight-state-generation 0)
  (setq scrollview--eglot-highlight-token nil)
  (setq scrollview--highlight-symbol-state-generation 0)
  (setq scrollview--symbol-overlay-state-generation 0)
  (setq scrollview--diagnostic-state-generation 0)
  (setq scrollview--spell-state-generation 0)
  (setq scrollview--vc-state-generation 0))

(defun scrollview-test--insert-lines (count &optional prefix)
  "Insert COUNT lines using PREFIX."
  (dotimes (i count)
    (insert (format "%s%d" (or prefix "line ") (1+ i)))
    (when (< i (1- count))
      (insert "\n"))))

(defmacro scrollview-test--with-displayed-buffer (&rest body)
  "Run BODY in a temporary buffer displayed in the selected window."
  (declare (indent 0) (debug t))
  `(let ((buffer (generate-new-buffer " *scrollview-test*"))
         (original-buffer (current-buffer)))
     (unwind-protect
         (progn
           (switch-to-buffer buffer)
           ,@body)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (bound-and-true-p scrollview-mode)
             (scrollview-mode -1)))
         (when (eq (current-buffer) buffer)
           (switch-to-buffer original-buffer))
         (kill-buffer buffer))
       (scrollview-test--reset-state))))

(defun scrollview-test--overlay-displays (window)
  "Return display specs for scrollview overlays in WINDOW."
  (mapcar (lambda (overlay)
            (get-text-property
             0 'display (overlay-get overlay 'after-string)))
          (gethash window scrollview--window-overlays)))

(defun scrollview-test--mouse-event (window area row &optional string)
  "Return a mouse-1 event for WINDOW AREA at zero-based ROW.
When STRING is non-nil, include it as the clicked string object."
  (let ((y (* row (frame-char-height (window-frame window)))))
    (list 'mouse-1
          (list window area (cons 0 y) 0
                (and string (cons string 0))
                nil))))

(defun scrollview-test--face-state (face)
  "Return restorable FACE state."
  (list :inherit (face-attribute face :inherit nil 'default)
        :foreground (face-attribute face :foreground nil 'default)
        :background (face-attribute face :background nil 'default)
        :underline (face-attribute face :underline nil 'default)
        :inverse-video (face-attribute face :inverse-video nil 'default)))

(defun scrollview-test--restore-face-state (face state)
  "Restore FACE from STATE."
  (set-face-attribute face nil
                      :inherit (plist-get state :inherit)
                      :foreground (plist-get state :foreground)
                      :background (plist-get state :background)
                      :underline (plist-get state :underline)
                      :inverse-video (plist-get state :inverse-video)))

(ert-deftest scrollview-entry-loads-internal-modules ()
  (dolist (feature '(scrollview scrollview-custom scrollview-faces
                                scrollview-core scrollview-signs))
    (should (featurep feature))))

(ert-deftest scrollview-usable-color-rejects-unspecified-placeholders ()
  (should-not (scrollview--usable-color-p nil))
  (should-not (scrollview--usable-color-p 'unspecified))
  (should-not (scrollview--usable-color-p "unspecified-fg"))
  (should-not (scrollview--usable-color-p "unspecified-bg"))
  (should (scrollview--usable-color-p "DeepSkyBlue3")))

(ert-deftest scrollview-sign-foreground-ignores-unspecified-default ()
  (let ((face 'scrollview-test-empty-face))
    (unwind-protect
        (progn
          (unless (facep face)
            (make-face face))
          (set-face-attribute face nil
                              :inherit nil
                              :foreground 'unspecified
                              :background 'unspecified)
          (cl-letf (((symbol-function 'face-foreground)
                     (lambda (&rest _)
                       "unspecified-fg")))
            (should (equal (scrollview--sign-foreground face) "black"))))
      (when (facep face)
        (set-face-attribute face nil
                            :inherit nil
                            :foreground 'unspecified
                            :background 'unspecified)))))

(ert-deftest scrollview-thumb-size ()
  (should (= (scrollview--compute-thumb-size 10 100) 1))
  (should (= (scrollview--compute-thumb-size 10 20) 5))
  (should (= (scrollview--compute-thumb-size 10 5) 10)))

(ert-deftest scrollview-thumb-top-clamps-at-bottom ()
  (should (= (scrollview--compute-thumb-top 10 100 1 1 nil) 0))
  (should (= (scrollview--compute-thumb-top 10 100 50 1 nil) 4))
  (should (= (scrollview--compute-thumb-top 10 100 50 2 t) 8)))

(ert-deftest scrollview-line-to-row ()
  (should (= (scrollview--line-to-row 1 10 100) 0))
  (should (= (scrollview--line-to-row 50 10 100) 4))
  (should (= (scrollview--line-to-row 100 10 100) 9))
  (should (= (scrollview--line-to-row 200 10 100) 9)))

(ert-deftest scrollview-row-to-line ()
  (should (= (scrollview--row-to-line 0 10 100) 1))
  (should (= (scrollview--row-to-line 9 10 100) 100))
  (should (= (scrollview--row-to-line 20 10 100) 100)))

(ert-deftest scrollview-position-info-clamps-track-at-eob ()
  (scrollview-test--with-displayed-buffer
    (scrollview-test--insert-lines 200)
    (let ((window (selected-window)))
      (cl-letf (((symbol-function 'scrollview--window-line-height)
                 (lambda (_window) 20))
                ((symbol-function 'scrollview--window-top-line)
                 (lambda (_window) 197)))
        (let ((info (scrollview--position-info window)))
          (should (= (plist-get info :window-lines) 20))
          (should (= (plist-get info :track-lines) 4))
          (should (= (plist-get info :thumb-top) 3)))))))

(ert-deftest scrollview-line-count-cache-invalidates-on-edit ()
  (with-temp-buffer
    (insert "one\ntwo")
    (should (= (scrollview--line-count) 2))
    (should (= (scrollview--line-count) 2))
    (goto-char (point-max))
    (insert "\nthree")
    (should (= (scrollview--line-count) 3))))

(ert-deftest scrollview-diagnostics-collector-reuses-one-scan-for-all-levels ()
  (with-temp-buffer
    (insert "alpha\nbeta\ngamma\n")
    (let ((flymake-calls 0))
      (cl-letf (((symbol-function 'flymake-diagnostics)
                 (lambda (&optional _beg _end)
                   (setq flymake-calls (1+ flymake-calls))
                   '((:line 1 :level :error)
                     (:line 3 :level :note))))
                ((symbol-function 'flymake-diagnostic-beg)
                 (lambda (diag)
                   (save-excursion
                     (goto-char (point-min))
                     (forward-line (1- (plist-get diag :line)))
                     (point))))
                ((symbol-function 'flymake-diagnostic-type)
                 (lambda (diag)
                   (plist-get diag :level)))
                ((symbol-function 'flycheck-error-line)
                 (lambda (err)
                   (plist-get err :line)))
                ((symbol-function 'flycheck-error-level)
                 (lambda (err)
                   (plist-get err :level))))
        (let ((flycheck-current-errors '((:line 2 :level :warning))))
          (let ((error-lines (scrollview--collect-diagnostic-lines 'error))
                (warning-lines (scrollview--collect-diagnostic-lines 'warning))
                (info-lines (scrollview--collect-diagnostic-lines 'info)))
            (should (equal error-lines '(1)))
            (should (equal warning-lines '(2)))
            (should (equal info-lines '(3)))
            (should (= flymake-calls 1))))))))

(ert-deftest scrollview-compilation-collector-uses-parsed-messages ()
  (scrollview-test--reset-state)
  (require 'compile)
  (let ((file (make-temp-file "scrollview-compilation"))
        (source (generate-new-buffer " *scrollview-source*"))
        (compilation (generate-new-buffer " *scrollview-compilation*")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq buffer-file-name file)
            (insert "one\ntwo\nthree\n"))
          (with-current-buffer compilation
            (compilation-mode)
            (let ((inhibit-read-only t))
              (insert (format "%s:2:1: error: bad\n" file))
              (insert (format "%s:3:1: warning: warn\n" file))
              (insert (format "%s:1:1: note: info\n" file))))
          (with-current-buffer source
            (should (equal (scrollview--collect-compilation-lines 'error)
                           '(2)))
            (should (equal (scrollview--collect-compilation-lines 'warning)
                           '(3)))
            (should (equal (scrollview--collect-compilation-lines 'info)
                           '(1)))))
      (when (buffer-live-p source)
        (kill-buffer source))
      (when (buffer-live-p compilation)
        (kill-buffer compilation))
      (delete-file file))))

(ert-deftest scrollview-thumb-face-follows-region-background ()
  (let ((old-region-bg (face-attribute 'region :background nil 'default))
        (old-thumb-fg (face-attribute 'scrollview-thumb-face
                                      :foreground nil 'default))
        (old-thumb-bg (face-attribute 'scrollview-thumb-face
                                      :background nil 'default))
        (old-thumb-inherit (face-attribute 'scrollview-thumb-face
                                           :inherit nil 'default))
        (old-thumb-inverse (face-attribute 'scrollview-thumb-face
                                           :inverse-video nil 'default)))
    (unwind-protect
        (progn
          (set-face-attribute 'region nil :background "red")
          (scrollview--sync-thumb-face)
          (should (equal (face-attribute 'scrollview-thumb-face
                                         :foreground nil t)
                         "red"))
          (should (equal (face-attribute 'scrollview-thumb-face
                                         :background nil t)
                         "red"))
          (should-not (face-attribute 'scrollview-thumb-face
                                      :inherit nil t)))
      (set-face-attribute 'region nil :background old-region-bg)
      (set-face-attribute 'scrollview-thumb-face nil
                          :inherit old-thumb-inherit
                          :foreground old-thumb-fg
                          :background old-thumb-bg
                          :inverse-video old-thumb-inverse))))

(ert-deftest scrollview-sign-render-face-uses-search-foreground-and-backgrounds ()
  (let ((old-function-fg (face-attribute 'font-lock-function-name-face
                                         :foreground nil 'default))
        (old-function-bg (face-attribute 'font-lock-function-name-face
                                         :background nil 'default))
        (old-thumb (scrollview-test--face-state 'scrollview-thumb-face)))
    (unwind-protect
        (progn
          (set-face-attribute 'font-lock-function-name-face nil
                              :foreground "DeepSkyBlue3"
                              :background 'unspecified)
          (set-face-attribute 'scrollview-thumb-face nil
                              :inherit nil
                              :foreground "gray60"
                              :background "gray60"
                              :inverse-video nil)
          (clrhash scrollview--sign-render-face-cache)
          (let ((plain (scrollview--sign-render-face
                        'scrollview-search-face nil))
                (highlighted (scrollview--sign-render-face
                              'scrollview-search-face t)))
            (should (not (eq plain 'scrollview-search-face)))
            (should (equal (face-attribute plain :foreground nil t)
                           "DeepSkyBlue3"))
            (should (eq (face-attribute plain :background nil t)
                        'unspecified))
            (should (equal (face-attribute highlighted :foreground nil t)
                           "DeepSkyBlue3"))
            (should (equal (face-attribute highlighted :background nil t)
                           "gray60"))))
      (set-face-attribute 'font-lock-function-name-face nil
                          :foreground old-function-fg
                          :background old-function-bg)
      (scrollview-test--restore-face-state 'scrollview-thumb-face old-thumb))))

(ert-deftest scrollview-default-startup-enables-only-search-and-diagnostics ()
  (scrollview-test--reset-state)
  (let ((scrollview-signs-on-startup '(search diagnostics)))
    (scrollview--initialize-builtins)
    (should (scrollview-sign-group-active-p 'diagnostics))
    (dolist (group '(highlight-symbol symbol-overlay bookmarks eglot compilation
                                      conflicts keywords spell vc))
      (should-not (scrollview-sign-group-active-p group)))))

(ert-deftest scrollview-startup-all-symbol-enables-all-groups ()
  (scrollview-test--reset-state)
  (let ((scrollview-signs-on-startup 'all))
    (scrollview--initialize-builtins)
    (dolist (group '(search highlight-symbol symbol-overlay bookmarks eglot
                            diagnostics compilation conflicts keywords spell vc))
      (should (scrollview-sign-group-active-p group)))))

(ert-deftest scrollview-builtins-register-new-sign-groups ()
  (scrollview-test--reset-state)
  (let ((scrollview-signs-on-startup nil))
    (scrollview--initialize-builtins)
    (dolist (group '(highlight-symbol symbol-overlay bookmarks eglot compilation
                                      conflicts keywords spell vc))
      (should (memq group (scrollview--sign-group-list))))
    (should-not (memq 'marks (scrollview--sign-group-list)))))

(ert-deftest scrollview-symbol-and-eglot-bitmaps-are-distinct ()
  (scrollview-test--reset-state)
  (let ((scrollview-signs-on-startup 'all)
        bitmaps)
    (scrollview--initialize-builtins)
    (maphash
     (lambda (_id spec)
       (let ((group (scrollview--sign-spec-group spec)))
         (when (memq group '(highlight-symbol symbol-overlay eglot))
           (push (cons group (scrollview--sign-spec-bitmap spec))
                 bitmaps))))
     scrollview--sign-specs)
    (should (eq (alist-get 'highlight-symbol bitmaps)
                'scrollview-symbol-bitmap))
    (should (eq (alist-get 'symbol-overlay bitmaps)
                'scrollview-symbol-bitmap))
    (should (eq (alist-get 'eglot bitmaps)
                'scrollview-search-bitmap))
    (should-not (eq (alist-get 'highlight-symbol bitmaps)
                    (alist-get 'eglot bitmaps)))))

(ert-deftest scrollview-diagnostic-bitmap-is-dot ()
  (scrollview-test--reset-state)
  (let ((scrollview-signs-on-startup '(diagnostics))
        specs)
    (scrollview--initialize-builtins)
    (maphash (lambda (_id spec)
               (when (eq (scrollview--sign-spec-group spec) 'diagnostics)
                 (push spec specs)))
             scrollview--sign-specs)
    (should specs)
    (dolist (spec specs)
      (should (eq (scrollview--sign-spec-bitmap spec)
                  'scrollview-diagnostic-bitmap)))))

(ert-deftest scrollview-keyword-bitmap-uses-semantic-shapes ()
  (scrollview-test--reset-state)
  (let ((scrollview-signs-on-startup '(keywords))
        (hl-todo-keyword-faces '(("HACK" . "goldenrod")
                                 ("TODO" . "orange")
                                 ("CUSTOM" . "cyan")))
        variants)
    (cl-letf (((symbol-function 'scrollview--hl-todo-available-p)
               (lambda () t)))
      (scrollview--initialize-builtins)
      (maphash (lambda (_id candidate)
                 (when (and (eq (scrollview--sign-spec-group candidate)
                                'keywords))
                   (push (cons (scrollview--sign-spec-variant candidate)
                               (scrollview--sign-spec-bitmap candidate))
                         variants)))
               scrollview--sign-specs)
      (should (eq (alist-get 'hack variants)
                  'scrollview-keyword-hack-bitmap))
      (should (eq (alist-get 'todo variants)
                  'scrollview-keyword-todo-bitmap))
      (should (eq (alist-get 'custom variants)
                  'scrollview-keyword-bitmap)))))

(ert-deftest scrollview-keyword-scan-propertizes-before-hl-todo-search ()
  (scrollview-test--reset-state)
  (with-temp-buffer
    (insert "// TODO:\n")
    (let ((hl-todo-keyword-faces '(("TODO" . "red")))
          (prepared nil))
      (cl-letf (((symbol-function 'scrollview--hl-todo-available-p)
                 (lambda () t))
                ((symbol-function 'syntax-propertize)
                 (lambda (end) (setq prepared end)))
                ((symbol-function 'hl-todo--search)
                 (lambda (&optional _regexp bound _backward)
                   (and prepared
                        (re-search-forward "\\(\\(TODO\\):\\)" bound t)))))
        (should (equal (scrollview--hl-todo-lines) '((todo 1))))
        (should (= prepared (point-max)))))))

(ert-deftest scrollview-spell-bitmap-is-tilde ()
  (scrollview-test--reset-state)
  (let ((scrollview-signs-on-startup '(spell))
        spec)
    (scrollview--initialize-builtins)
    (maphash (lambda (_id candidate)
               (when (eq (scrollview--sign-spec-group candidate) 'spell)
                 (setq spec candidate)))
             scrollview--sign-specs)
    (should spec)
    (should (eq (scrollview--sign-spec-bitmap spec)
                'scrollview-spell-bitmap))))

(ert-deftest scrollview-vc-bar-bitmap-fills-tall-line-cells ()
  (scrollview-test--reset-state)
  (let ((scrollview-signs-on-startup '(vc))
        variants)
    (scrollview--initialize-builtins)
    (maphash (lambda (_id spec)
               (when (eq (scrollview--sign-spec-group spec) 'vc)
                 (push (cons (scrollview--sign-spec-variant spec)
                             (scrollview--sign-spec-bitmap spec))
                       variants)))
             scrollview--sign-specs)
    (should (>= (length scrollview--vc-bar-bitmap-vector) 32))
    (should (eq (alist-get 'add variants) 'scrollview-sign-bar-bitmap))
    (should (eq (alist-get 'change variants) 'scrollview-sign-bar-bitmap))
    (should (eq (alist-get 'delete variants) 'scrollview-sign-delete-bitmap))))

(ert-deftest scrollview-vc-add-face-has-green-fallback ()
  (scrollview-test--reset-state)
  (let* ((faces '(diff-added diff-refine-added success scrollview-vc-add-face))
         (states (mapcar (lambda (face)
                           (cons face (scrollview-test--face-state face)))
                         faces)))
    (unwind-protect
        (progn
          (dolist (face '(diff-added diff-refine-added success))
            (set-face-attribute face nil
                                :foreground 'unspecified
                                :underline 'unspecified
                                :background 'unspecified))
          (setq scrollview--vc-face-state nil)
          (scrollview--sync-vc-faces)
          (should (equal (face-attribute 'scrollview-vc-add-face
                                         :foreground nil t)
                         "green3"))
          (should (equal (scrollview--sign-foreground
                          'scrollview-vc-add-face)
                         "green3")))
      (dolist (state states)
        (scrollview-test--restore-face-state (car state) (cdr state)))
      (setq scrollview--vc-face-state nil)
      (scrollview--sync-vc-faces))))

(ert-deftest scrollview-vc-add-face-prefers-diff-added-color ()
  (scrollview-test--reset-state)
  (let* ((faces '(diff-added scrollview-vc-add-face))
         (states (mapcar (lambda (face)
                           (cons face (scrollview-test--face-state face)))
                         faces)))
    (unwind-protect
        (progn
          (set-face-attribute 'diff-added nil
                              :foreground "SeaGreen3"
                              :underline 'unspecified
                              :background "PaleGreen")
          (setq scrollview--vc-face-state nil)
          (scrollview--sync-vc-faces)
          (should (equal (face-attribute 'scrollview-vc-add-face
                                         :foreground nil t)
                         "SeaGreen3"))
          (should (equal (scrollview--sign-foreground
                          'scrollview-vc-add-face)
                         "SeaGreen3")))
      (dolist (state states)
        (scrollview-test--restore-face-state (car state) (cdr state)))
      (setq scrollview--vc-face-state nil)
      (scrollview--sync-vc-faces))))

(ert-deftest scrollview-vc-add-face-ignores-diff-added-background ()
  (scrollview-test--reset-state)
  (let* ((faces '(diff-added diff-refine-added success scrollview-vc-add-face))
         (states (mapcar (lambda (face)
                           (cons face (scrollview-test--face-state face)))
                         faces)))
    (unwind-protect
        (progn
          (dolist (face '(diff-added diff-refine-added))
            (set-face-attribute face nil
                                :foreground 'unspecified
                                :underline 'unspecified
                                :background "PaleGreen"))
          (set-face-attribute 'success nil
                              :foreground "green4"
                              :underline 'unspecified
                              :background 'unspecified)
          (setq scrollview--vc-face-state nil)
          (scrollview--sync-vc-faces)
          (should (equal (face-attribute 'scrollview-vc-add-face
                                         :foreground nil t)
                         "green4"))
          (should (equal (scrollview--sign-foreground
                          'scrollview-vc-add-face)
                         "green4")))
      (dolist (state states)
        (scrollview-test--restore-face-state (car state) (cdr state)))
      (setq scrollview--vc-face-state nil)
      (scrollview--sync-vc-faces))))

(ert-deftest scrollview-diagnostic-level-honors-flymake-category ()
  (let ((symbols '(scrollview-test-eglot-error
                   scrollview-test-eglot-warning
                   scrollview-test-eglot-note)))
    (unwind-protect
        (progn
          (put 'scrollview-test-eglot-error
               'flymake-category 'flymake-error)
          (put 'scrollview-test-eglot-warning
               'flymake-category 'flymake-warning)
          (put 'scrollview-test-eglot-note
               'flymake-category 'flymake-note)
          (should (eq (scrollview--diagnostic-level
                       'scrollview-test-eglot-error)
                      'error))
          (should (eq (scrollview--diagnostic-level
                       'scrollview-test-eglot-warning)
                      'warning))
          (should (eq (scrollview--diagnostic-level
                       'scrollview-test-eglot-note)
                      'info))
          (should (eq (scrollview--diagnostic-level 1) 'error))
          (should (eq (scrollview--diagnostic-level 2) 'warning))
          (should (eq (scrollview--diagnostic-level 3) 'info)))
      (dolist (symbol symbols)
        (cl-remf (symbol-plist symbol) 'flymake-category)))))

(ert-deftest scrollview-diagnostic-faces-inherit-diagnostic-faces ()
  (scrollview-test--reset-state)
  (let ((scrollview-signs-on-startup '(diagnostics))
        specs)
    (scrollview--initialize-builtins)
    (maphash (lambda (_id spec)
               (when (eq (scrollview--sign-spec-group spec) 'diagnostics)
                 (push spec specs)))
             scrollview--sign-specs)
    (dolist (spec specs)
      (pcase (scrollview--sign-spec-variant spec)
        ('error
         (should (eq (scrollview--sign-spec-face spec)
                     'scrollview-diagnostic-error-face))
         (should (eq (face-attribute (scrollview--sign-spec-face spec)
                                     :inherit nil t)
                     'flymake-error)))
        ('warning
         (should (eq (scrollview--sign-spec-face spec)
                     'scrollview-diagnostic-warning-face))
         (should (eq (face-attribute (scrollview--sign-spec-face spec)
                                     :inherit nil t)
                     'flymake-warning)))
        ('info
         (should (eq (scrollview--sign-spec-face spec)
                     'scrollview-diagnostic-info-face))
         (should (eq (face-attribute (scrollview--sign-spec-face spec)
                                     :inherit nil t)
                     'flymake-note)))))))

(ert-deftest scrollview-diagnostic-faces-copy-source-colors ()
  (scrollview-test--reset-state)
  (require 'flymake)
  (let* ((faces '(flymake-error flymake-warning flymake-note
                  scrollview-diagnostic-error-face
                  scrollview-diagnostic-warning-face
                  scrollview-diagnostic-info-face))
         (states (mapcar (lambda (face)
                           (cons face (scrollview-test--face-state face)))
                         faces)))
    (unwind-protect
        (progn
          (set-face-attribute 'flymake-error nil
                              :foreground 'unspecified
                              :underline '(:style wave :color "firebrick")
                              :background 'unspecified)
          (set-face-attribute 'flymake-warning nil
                              :foreground "goldenrod"
                              :underline 'unspecified
                              :background 'unspecified)
          (set-face-attribute 'flymake-note nil
                              :foreground 'unspecified
                              :underline 'unspecified
                              :background "seagreen")
          (setq scrollview--diagnostic-face-state nil)
          (scrollview--sync-diagnostic-faces)
          (should (equal (face-attribute
                          'scrollview-diagnostic-error-face
                          :foreground nil t)
                         "firebrick"))
          (should (equal (face-attribute
                          'scrollview-diagnostic-warning-face
                          :foreground nil t)
                         "goldenrod"))
          (should (equal (face-attribute
                          'scrollview-diagnostic-info-face
                          :foreground nil t)
                         "seagreen")))
      (dolist (state states)
        (scrollview-test--restore-face-state (car state) (cdr state)))
      (setq scrollview--diagnostic-face-state nil)
      (scrollview--sync-diagnostic-faces))))

(ert-deftest scrollview-refresh-resyncs-diagnostic-face-colors ()
  (scrollview-test--reset-state)
  (require 'flymake)
  (let* ((faces '(flymake-error scrollview-diagnostic-error-face))
         (states (mapcar (lambda (face)
                           (cons face (scrollview-test--face-state face)))
                         faces)))
    (unwind-protect
        (scrollview-test--with-displayed-buffer
          (scrollview-test--insert-lines 50)
          (let ((scrollview-signs-on-startup nil)
                (scrollview-line-limit -1)
                (scrollview-byte-limit -1))
            (set-face-attribute 'flymake-error nil
                                :foreground 'unspecified
                                :underline 'unspecified
                                :background 'unspecified)
            (setq scrollview--diagnostic-face-state nil)
            (scrollview--sync-diagnostic-faces)
            (should (eq (face-attribute
                         'scrollview-diagnostic-error-face
                         :foreground nil t)
                        'unspecified))
            (set-face-attribute 'flymake-error nil
                                :foreground 'unspecified
                                :underline '(:style wave :color "firebrick")
                                :background 'unspecified)
            (cl-letf (((symbol-function 'scrollview--fringe-available-p)
                       (lambda (_window) t)))
              (scrollview-mode 1)
              (scrollview-refresh (selected-window)))
            (should (equal (face-attribute
                            'scrollview-diagnostic-error-face
                            :foreground nil t)
                           "firebrick"))))
      (dolist (state states)
        (scrollview-test--restore-face-state (car state) (cdr state)))
      (setq scrollview--diagnostic-face-state nil)
      (scrollview--sync-diagnostic-faces))))

(ert-deftest scrollview-priority-conflict-resolution ()
  (let ((old-thumb (scrollview-test--face-state 'scrollview-thumb-face)))
    (unwind-protect
        (let* ((high (scrollview--make-sign-spec
                      :id 1 :group 'test :variant nil :priority 10
                      :bitmap 'scrollview-search-bitmap
                      :face 'scrollview-search-face
                      :collector #'ignore :current-only nil))
               (low (scrollview--make-sign-spec
                     :id 2 :group 'test :variant nil :priority -1
                     :bitmap 'scrollview-sign-dot-bitmap
                     :face 'scrollview-keyword-face
                     :collector #'ignore :current-only nil))
               (info '(:window-lines 5 :buffer-lines 5
                       :thumb-top 0 :thumb-size 5 :restricted nil))
               slots)
          (set-face-attribute 'scrollview-thumb-face nil
                              :inherit nil
                              :foreground "gray60"
                              :background "gray60"
                              :inverse-video nil)
          (clrhash scrollview--sign-render-face-cache)
          (setq slots
                (scrollview--build-slots
                 nil info
                 (list (list :line 3 :spec high)
                       (list :line 4 :spec low))))
          (should (eq (plist-get (aref slots 0) :type) 'scrollbar))
          (should (eq (plist-get (aref slots 2) :type) 'sign))
          (should (plist-get (aref slots 2) :highlighted))
          (should (eq (plist-get (aref slots 2) :bitmap)
                      'scrollview-search-bitmap))
          (should (equal (face-attribute (plist-get (aref slots 2) :face)
                                         :background nil t)
                         "gray60"))
          (should (eq (plist-get (aref slots 3) :type) 'scrollbar)))
      (scrollview-test--restore-face-state 'scrollview-thumb-face old-thumb))))

(ert-deftest scrollview-sign-outside-scrollbar-has-no-background ()
  (let* ((spec (scrollview--make-sign-spec
                :id 1 :group 'test :variant nil :priority 10
                :bitmap 'scrollview-search-bitmap
                :face 'scrollview-search-face
                :collector #'ignore :current-only nil))
         (info '(:window-lines 5 :buffer-lines 5
                 :thumb-top 0 :thumb-size 1 :restricted nil))
         (slots (scrollview--build-slots
                 nil info
                 (list (list :line 5 :spec spec)))))
    (should (eq (plist-get (aref slots 4) :type) 'sign))
    (should-not (plist-get (aref slots 4) :highlighted))
    (should (eq (face-attribute (plist-get (aref slots 4) :face)
                                :background nil t)
                'unspecified))))

(ert-deftest scrollview-slots-avoid-eob-empty-display-rows ()
  (let* ((info '(:window-lines 10 :track-lines 4 :buffer-lines 100
                 :thumb-top 3 :thumb-size 1 :restricted nil))
         (slots (scrollview--build-slots nil info nil)))
    (should (eq (plist-get (aref slots 3) :type) 'scrollbar))
    (cl-loop for row from 4 below (length slots)
             do (should-not (aref slots row)))))

(ert-deftest scrollview-refresh-renders-fringe-overlays ()
  (scrollview-test--reset-state)
  (scrollview-test--with-displayed-buffer
    (scrollview-test--insert-lines 200)
    (goto-char (point-min))
    (let ((scrollview-visibility 'overflow)
          (scrollview-signs-on-startup nil)
          (scrollview-line-limit -1)
          (scrollview-byte-limit -1))
      (cl-letf (((symbol-function 'scrollview--fringe-available-p)
                 (lambda (_window) t)))
        (scrollview-mode 1)
        (scrollview-refresh (selected-window))
        (let ((displays (scrollview-test--overlay-displays
                         (selected-window))))
          (should displays)
          (should (member '(right-fringe filled-rectangle
                                         scrollview-thumb-face)
                          displays)))
        (scrollview-mode -1)
        (should-not (gethash (selected-window)
                             scrollview--window-overlays))))))

(ert-deftest scrollview-click-jumps-from-fringe-row ()
  (scrollview-test--reset-state)
  (scrollview-test--with-displayed-buffer
    (scrollview-test--insert-lines 100)
    (goto-char (point-min))
    (let ((scrollview-side 'right)
          (scrollview-visibility 'overflow)
          (scrollview-signs-on-startup nil)
          (scrollview-line-limit -1)
          (scrollview-byte-limit -1))
      (cl-letf (((symbol-function 'scrollview--fringe-available-p)
                 (lambda (_window) t))
                ((symbol-function 'scrollview--window-line-height)
                 (lambda (_window) 10)))
        (scrollview-mode 1)
        (scrollview-click
         (scrollview-test--mouse-event
          (selected-window) 'right-fringe 9))
        (should (= (line-number-at-pos nil t) 100))))))

(ert-deftest scrollview-click-on-sign-jumps-to-sign-line ()
  (scrollview-test--reset-state)
  (scrollview-test--with-displayed-buffer
    (scrollview-test--insert-lines 100)
    (goto-char (point-min))
    (let ((scrollview-side 'right)
          (scrollview-visibility 'info)
          (scrollview-signs-on-startup nil)
          (scrollview-line-limit -1)
          (scrollview-byte-limit -1))
      (cl-letf (((symbol-function 'scrollview--fringe-available-p)
                 (lambda (_window) t))
                ((symbol-function 'scrollview--window-line-height)
                 (lambda (_window) 10)))
        (scrollview-register-sign-group 'scrollview-test-click t)
        (scrollview-register-sign-spec
         :group 'scrollview-test-click
         :variant 'mock
         :priority 80
         :bitmap 'scrollview-search-bitmap
         :face 'scrollview-search-face
         :collector (lambda (_window) '(75)))
        (scrollview-mode 1)
        (scrollview-refresh (selected-window))
        (let ((string
               (cl-loop for overlay in (gethash (selected-window)
                                                scrollview--window-overlays)
                        for string = (overlay-get overlay 'after-string)
                        when (eq (get-text-property
                                  0 'scrollview-target-type string)
                                 'sign)
                        return string)))
          (should string)
          (scrollview-click
           (scrollview-test--mouse-event
            (selected-window) 'right-fringe 0 string))
          (should (= (line-number-at-pos nil t) 75)))))))

(ert-deftest scrollview-custom-sign-navigation ()
  (scrollview-test--reset-state)
  (scrollview-test--with-displayed-buffer
    (scrollview-test--insert-lines 20)
    (goto-char (point-min))
    (let ((scrollview-visibility 'info)
          (scrollview-signs-on-startup nil)
          (scrollview-line-limit -1)
          (scrollview-byte-limit -1))
      (cl-letf (((symbol-function 'scrollview--fringe-available-p)
                 (lambda (_window) t)))
        (scrollview-register-sign-group 'scrollview-test t)
        (scrollview-register-sign-spec
         :group 'scrollview-test
         :variant 'mock
         :priority 80
         :bitmap 'scrollview-search-bitmap
         :face 'scrollview-search-face
         :collector (lambda (_window) '(2 5 2)))
        (scrollview-mode 1)
        (should (equal (scrollview--visible-sign-lines
                        '(scrollview-test))
                       '(2 5)))
        (scrollview-next 1 '(scrollview-test))
        (should (= (line-number-at-pos nil t) 2))
        (scrollview-next 1 '(scrollview-test))
        (should (= (line-number-at-pos nil t) 5))
        (scrollview-prev 1 '(scrollview-test))
        (should (= (line-number-at-pos nil t) 2))))))

(ert-deftest scrollview-custom-sign-renders-over-scrollbar ()
  (scrollview-test--reset-state)
  (scrollview-test--with-displayed-buffer
    (scrollview-test--insert-lines 200)
    (goto-char (point-min))
    (let ((old-region-bg (face-attribute 'region :background nil 'default))
          (old-thumb (scrollview-test--face-state 'scrollview-thumb-face)))
      (unwind-protect
          (let ((scrollview-visibility 'overflow)
                (scrollview-signs-on-startup nil)
                (scrollview-line-limit -1)
                (scrollview-byte-limit -1))
            (set-face-attribute 'region nil :background "gray60")
            (setq scrollview--thumb-face-state nil)
            (clrhash scrollview--sign-render-face-cache)
            (cl-letf (((symbol-function 'scrollview--fringe-available-p)
                       (lambda (_window) t)))
              (scrollview-register-sign-group 'scrollview-test-render t)
              (scrollview-register-sign-spec
               :group 'scrollview-test-render
               :variant 'mock
               :priority 80
               :bitmap 'scrollview-search-bitmap
               :face 'scrollview-search-face
               :collector (lambda (_window) '(1)))
              (scrollview-mode 1)
              (scrollview-refresh (selected-window))
              (let ((displays (scrollview-test--overlay-displays
                               (selected-window))))
                (should (cl-find-if
                         (lambda (display)
                           (and (eq (car display) 'right-fringe)
                                (eq (cadr display) 'scrollview-search-bitmap)
                                (equal (face-attribute (caddr display)
                                                       :background nil t)
                                       "gray60")))
                         displays)))))
        (set-face-attribute 'region nil :background old-region-bg)
        (scrollview-test--restore-face-state 'scrollview-thumb-face
                                             old-thumb)))))

(ert-deftest scrollview-scroll-hook-refreshes-immediately ()
  (scrollview-test--reset-state)
  (let ((window (selected-window))
        called)
    (cl-letf (((symbol-function 'scrollview--refresh-now)
               (lambda (&optional refreshed-window reuse-signs)
                 (setq called (list refreshed-window reuse-signs)))))
      (scrollview--after-window-scroll window nil))
    (should (equal called (list window t)))
    (should-not (timerp scrollview--refresh-timer))))

(ert-deftest scrollview-scroll-refresh-reuses-sign-cache ()
  (scrollview-test--reset-state)
  (scrollview-test--with-displayed-buffer
    (scrollview-test--insert-lines 200)
    (goto-char (point-min))
    (let ((scrollview-visibility 'info)
          (scrollview-signs-on-startup nil)
          (scrollview-line-limit -1)
          (scrollview-byte-limit -1)
          (calls 0))
      (cl-letf (((symbol-function 'scrollview--fringe-available-p)
                 (lambda (_window) t)))
        (scrollview-register-sign-group 'scrollview-test-cache t)
        (scrollview-register-sign-spec
         :group 'scrollview-test-cache
         :variant 'mock
         :priority 80
         :bitmap 'scrollview-search-bitmap
         :face 'scrollview-search-face
         :collector (lambda (_window)
                      (cl-incf calls)
                      '(1 100)))
        (scrollview-mode 1)
        (scrollview-refresh (selected-window))
        (should (= calls 1))
        (scrollview--after-window-scroll (selected-window) nil)
        (should (= calls 1))
        (scrollview-refresh (selected-window))
        (should (= calls 2))))))

(ert-deftest scrollview-refresh-reuses-overlay-objects ()
  (scrollview-test--reset-state)
  (scrollview-test--with-displayed-buffer
    (scrollview-test--insert-lines 200)
    (goto-char (point-min))
    (let ((scrollview-visibility 'overflow)
          (scrollview-signs-on-startup nil)
          (scrollview-line-limit -1)
          (scrollview-byte-limit -1))
      (cl-letf (((symbol-function 'scrollview--fringe-available-p)
                 (lambda (_window) t))
                ((symbol-function 'scrollview--window-line-height)
                 (lambda (_window) 10)))
        (scrollview-mode 1)
        (scrollview-refresh (selected-window))
        (let ((overlays (copy-sequence
                         (gethash (selected-window)
                                  scrollview--window-overlays))))
          (should overlays)
          (scrollview--after-window-scroll (selected-window) nil)
          (should (cl-every #'eq overlays
                            (gethash (selected-window)
                                     scrollview--window-overlays))))))))

(ert-deftest scrollview-buffer-change-invalidates-sign-cache ()
  (scrollview-test--reset-state)
  (scrollview-test--with-displayed-buffer
    (scrollview-test--insert-lines 200)
    (goto-char (point-min))
    (let ((scrollview-visibility 'info)
          (scrollview-signs-on-startup nil)
          (scrollview-line-limit -1)
          (scrollview-byte-limit -1)
          (calls 0))
      (cl-letf (((symbol-function 'scrollview--fringe-available-p)
                 (lambda (_window) t)))
        (scrollview-register-sign-group 'scrollview-test-change t)
        (scrollview-register-sign-spec
         :group 'scrollview-test-change
         :variant 'mock
         :priority 80
         :bitmap 'scrollview-search-bitmap
         :face 'scrollview-search-face
         :collector (lambda (_window)
                      (cl-incf calls)
                      '(1 100)))
        (scrollview-mode 1)
        (scrollview-refresh (selected-window))
        (should (= calls 1))
        (goto-char (point-max))
        (insert "\nnew")
        (scrollview--after-window-scroll (selected-window) nil)
        (should (= calls 2))))))

(ert-deftest scrollview-search-collector-follows-isearch-highlights ()
  (scrollview-test--reset-state)
  (with-temp-buffer
    (insert "foo\nbar\nfoo")
    (setq scrollview--last-search-pattern "foo")
    (setq scrollview--last-search-regexp nil)
    (should-not (scrollview--collect-search-lines nil))
    (let ((isearch-mode t)
          (isearch-success t)
          (isearch-string "foo")
          (isearch-regexp nil))
      (should (equal (scrollview--collect-search-lines nil) '(1 3)))
      (goto-char (point-max))
      (insert "\nfoo")
      (should (equal (scrollview--collect-search-lines nil) '(1 3 4))))
    (let ((overlay (make-overlay (point-min) (point-min))))
      (unwind-protect
          (let ((isearch-lazy-highlight-overlays (list overlay)))
            (should (equal (scrollview--collect-search-lines nil)
                           '(1 3 4)))
            (delete-overlay overlay)
            (should-not (scrollview--collect-search-lines nil)))
        (delete-overlay overlay)))
    (let ((isearch-mode t)
          (isearch-success nil)
          (isearch-string "foo")
          (isearch-regexp nil))
      (should-not (scrollview--collect-search-lines nil)))))

(ert-deftest scrollview-isearch-update-and-end-keep-retained-source ()
  (scrollview-test--reset-state)
  (with-temp-buffer
    (let ((isearch-mode t)
          (isearch-success t)
          (isearch-string "foo")
          (isearch-regexp nil))
      (cl-letf (((symbol-function 'scrollview--schedule-buffer-refresh)
                 #'ignore))
        (scrollview--after-isearch-update)))
    (should (equal scrollview--last-search-pattern "foo"))
    (cl-letf (((symbol-function 'scrollview--schedule-buffer-refresh)
               #'ignore))
      (scrollview--after-isearch-end))
    (should (equal scrollview--last-search-pattern "foo"))
    (let ((overlay (make-overlay (point-min) (point-min))))
      (unwind-protect
          (let ((isearch-lazy-highlight-overlays (list overlay)))
            (should (equal (scrollview--search-source) '("foo" nil)))
            (delete-overlay overlay)
            (should-not (scrollview--search-source)))
        (delete-overlay overlay)))))

(ert-deftest scrollview-isearch-update-clears-failed-search ()
  (scrollview-test--reset-state)
  (with-temp-buffer
    (setq scrollview--last-search-pattern "foo")
    (setq scrollview--last-search-regexp nil)
    (let ((isearch-mode t)
          (isearch-success nil)
          (isearch-string "missing")
          (isearch-regexp nil))
      (cl-letf (((symbol-function 'scrollview--schedule-buffer-refresh)
                 #'ignore))
        (scrollview--after-isearch-update)))
    (should-not scrollview--last-search-pattern)))

(ert-deftest scrollview-highlight-symbol-collector ()
  (scrollview-test--reset-state)
  (with-temp-buffer
    (insert "alpha\nbeta alpha\ngamma beta\n")
    (let ((highlight-symbol-keyword-alist
           '(("\\_<alpha\\_>" 0 highlight prepend)))
          (highlight-symbol "\\_<beta\\_>"))
      (should (equal (scrollview--collect-highlight-symbol-lines nil)
                     '(1 2 3))))
    (let ((highlight-symbol-keyword-alist nil)
          (highlight-symbol nil))
      (cl-incf scrollview--highlight-symbol-state-generation)
      (should-not (scrollview--collect-highlight-symbol-lines nil)))))

(ert-deftest scrollview-symbol-overlay-collector-uses-overlays ()
  (scrollview-test--reset-state)
  (with-temp-buffer
    (insert "alpha\nplain\nalpha\n")
    (goto-char (point-min))
    (let ((first (make-overlay (point-min) (line-end-position 1)))
          (second (make-overlay (line-beginning-position 3)
                                (line-end-position 3))))
      (unwind-protect
          (progn
            (overlay-put first 'symbol "alpha")
            (overlay-put second 'symbol "alpha")
            (should (equal (scrollview--collect-symbol-overlay-lines nil)
                           '(1 3)))
            (delete-overlay first)
            (cl-incf scrollview--symbol-overlay-state-generation)
            (should (equal (scrollview--collect-symbol-overlay-lines nil)
                           '(3))))
        (delete-overlay first)
        (delete-overlay second)))))

(ert-deftest scrollview-bookmark-collector-uses-file-bookmarks ()
  (scrollview-test--reset-state)
  (require 'bookmark)
  (let ((file (make-temp-file "scrollview-bookmark")))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name file)
          (insert "one\ntwo\nthree\n")
          (let ((line-three (save-excursion
                              (goto-char (point-min))
                              (forward-line 2)
                              (point)))
                (bookmark-alist nil))
            (setq bookmark-alist
                  `(("first" . ((filename . ,file) (position . 1)))
                    ("third" . ((filename . ,file)
                                (position . ,line-three)))
                    ("other" . ((filename . "/tmp/scrollview-other")
                                (position . 1)))))
            (should (equal (scrollview--collect-bookmark-lines nil)
                           '(1 3)))))
      (delete-file file))))

(ert-deftest scrollview-eglot-collector-uses-highlight-overlays ()
  (scrollview-test--reset-state)
  (with-temp-buffer
    (insert "alpha\nplain\nalpha\n")
    (goto-char (point-min))
    (let ((first (make-overlay (point-min) (line-end-position 1)))
          (second (make-overlay (line-beginning-position 3)
                                (line-end-position 3))))
      (unwind-protect
          (let ((eglot--highlights (list first second)))
            (should (equal (scrollview--collect-eglot-highlight-lines nil)
                           '(1 3))))
        (delete-overlay first)
        (delete-overlay second)))))

(ert-deftest scrollview-eglot-post-command-refreshes-on-highlight-change ()
  (scrollview-test--reset-state)
  (with-temp-buffer
    (insert "alpha\n")
    (let ((scrollview-signs-on-startup '(eglot))
          (invalidated nil)
          (scheduled nil))
      (scrollview--initialize-builtins)
      (setq scrollview-mode t)
      (let ((overlay (make-overlay (point-min) (line-end-position))))
        (unwind-protect
            (let ((eglot--highlights (list overlay)))
              (cl-letf (((symbol-function
                          'scrollview--invalidate-buffer-sign-cache)
                         (lambda (&optional _buffer)
                           (setq invalidated t)))
                        ((symbol-function 'scrollview--schedule-buffer-refresh)
                         (lambda (&optional _buffer)
                           (setq scheduled t))))
                (scrollview--after-eglot-post-command)
                (should invalidated)
                (should scheduled)
                (setq invalidated nil
                      scheduled nil)
                (scrollview--after-eglot-post-command)
                (should-not invalidated)
                (should-not scheduled)))
          (delete-overlay overlay))))))

(ert-deftest scrollview-conflict-collector ()
  (scrollview-test--reset-state)
  (with-temp-buffer
    (insert "ok\n<<<<<<< ours\nleft\n=======\nright\n>>>>>>> theirs\n")
    (should (equal (scrollview--collect-conflict-lines 'top) '(2)))
    (should (equal (scrollview--collect-conflict-lines 'middle) '(4)))
    (should (equal (scrollview--collect-conflict-lines 'bottom) '(6)))))

(ert-deftest scrollview-keyword-collector ()
  (scrollview-test--reset-state)
  (with-temp-buffer
    (insert "TODO one\nplain\nFIXME two\n")
    (let ((hl-todo-keyword-faces '(("TODO" . "red")
                                   ("FIXME" . "orange"))))
      (cl-letf (((symbol-function 'scrollview--hl-todo-available-p)
                 (lambda () t))
                ((symbol-function 'hl-todo--search)
                 (lambda (&optional _regexp bound _backward)
                   (re-search-forward "\\(\\(TODO\\|FIXME\\)\\)"
                                      bound t))))
        (should (equal (scrollview--collect-keyword-lines 'todo) '(1)))
        (should (equal (scrollview--collect-keyword-lines 'fixme) '(3)))))))

(ert-deftest scrollview-spell-collector-uses-flyspell-overlays ()
  (scrollview-test--reset-state)
  (with-temp-buffer
    (insert "good\nbadword\nagain\n")
    (goto-char (point-min))
    (let ((overlay (make-overlay (line-beginning-position 2)
                                 (line-end-position 2))))
      (overlay-put overlay 'flyspell-overlay t)
      (should (equal (scrollview--collect-spell-lines nil) '(2)))
      (delete-overlay overlay)
      (cl-incf scrollview--spell-state-generation)
      (should-not (scrollview--collect-spell-lines nil)))))

(ert-deftest scrollview-vc-collector-uses-diff-hl-changes ()
  (scrollview-test--reset-state)
  (with-temp-buffer
    (insert "one\ntwo\nthree\nfour\nfive\n")
    (cl-letf (((symbol-function 'scrollview--diff-hl-available-p)
               (lambda () t))
              ((symbol-function 'diff-hl-changes)
               (lambda ()
                 '((:working . ((2 2 0 insert)
                                (4 1 1 change)
                                (5 0 2 delete)))))))
      (should (equal (scrollview--collect-vc-lines 'add) '(2 3)))
      (should (equal (scrollview--collect-vc-lines 'change) '(4)))
      (should (equal (scrollview--collect-vc-lines 'delete) '(5))))))

(provide 'scrollview-test)

;;; scrollview-test.el ends here
