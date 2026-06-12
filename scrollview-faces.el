;;; scrollview-faces.el --- Faces and bitmaps for scrollview -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Internal module for scrollview.el.

;;; Code:

(require 'cl-lib)
(require 'diff-mode)
(require 'fringe)
(require 'subr-x)
(require 'scrollview-custom)

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
  '((t (:inherit font-lock-function-name-face)))
  "Face for search signs."
  :group 'scrollview)

(defface scrollview-highlight-symbol-face
  '((t (:inherit (highlight-symbol-face highlight))))
  "Face for highlight-symbol signs."
  :group 'scrollview)

(defface scrollview-symbol-overlay-face
  '((t (:inherit (symbol-overlay-default-face highlight))))
  "Face for symbol-overlay signs."
  :group 'scrollview)

(defface scrollview-bookmark-face
  '((t (:inherit font-lock-constant-face)))
  "Face for bookmark signs."
  :group 'scrollview)

(defface scrollview-eglot-face
  '((t (:inherit (eglot-highlight-symbol-face highlight))))
  "Face for Eglot highlight signs."
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
  "Face for added-line VC signs.
The foreground is synchronized from diff faces, with a green fallback."
  :group 'scrollview)

(defface scrollview-vc-change-face
  '((t (:inherit diff-changed)))
  "Face for changed-line VC signs.
The foreground is synchronized from diff faces, with a warning fallback."
  :group 'scrollview)

(defface scrollview-vc-delete-face
  '((t (:inherit diff-removed)))
  "Face for deleted-line VC signs.
The foreground is synchronized from diff faces, with an error fallback."
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

(defvar scrollview--sign-render-face-cache (make-hash-table :test #'equal)
  "Hash table mapping source sign faces to cached render face data.")

(defvar scrollview--thumb-face-state nil
  "Cached state for `scrollview-thumb-face' synchronization.")

(defvar scrollview--diagnostic-face-state nil
  "Cached state for diagnostic face synchronization.")

(defvar scrollview--vc-face-state nil
  "Cached state for VC face synchronization.")

(defun scrollview--selection-color ()
  "Return the best available color for the current selection."
  (cl-labels ((color (face attribute)
                (let ((value (face-attribute face attribute nil t)))
                  (when (scrollview--usable-color-p value)
                    value))))
    (or (color 'region :background)
        (color 'highlight :background)
        (color 'region :foreground)
        (color 'highlight :foreground))))

(defun scrollview--usable-color-p (color)
  "Return non-nil if COLOR is suitable for a fringe face."
  (and color
       (not (eq color 'unspecified))
       (not (and (stringp color)
                 (string-prefix-p "unspecified-" color)))))

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
                            :inverse-video t))
      (clrhash scrollview--sign-render-face-cache))))

(defun scrollview--face-color (face attributes)
  "Return FACE color from ATTRIBUTES, honoring inheritance."
  (when (facep face)
    (cl-loop for attribute in attributes
             for value = (face-attribute face attribute nil t)
             when (scrollview--usable-color-p value)
             return value)))

(defun scrollview--face-direct-color (face attributes)
  "Return FACE color from ATTRIBUTES without inherited values."
  (when (facep face)
    (cl-loop for attribute in attributes
             for value = (face-attribute face attribute nil nil)
             when (scrollview--usable-color-p value)
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

(defun scrollview--foreground-source-color (faces)
  "Return the best non-background source color from FACES."
  (cl-loop for face in faces
           thereis (or (scrollview--face-color face '(:foreground))
                       (scrollview--face-underline-color face))))

(defun scrollview--sync-diagnostic-faces (&rest _)
  "Synchronize diagnostic sign faces with diagnostic source faces."
  (let ((state
         (cl-loop for (key target inherit source-faces)
                  in '((:error scrollview-diagnostic-error-face
                        flymake-error (flymake-error flycheck-error error))
                       (:warning scrollview-diagnostic-warning-face
                        flymake-warning (flymake-warning flycheck-warning warning))
                       (:info scrollview-diagnostic-info-face
                        flymake-note (flymake-note flycheck-info success warning)))
                  collect (list key target inherit
                                (scrollview--diagnostic-source-color source-faces)))))
    (unless (equal state scrollview--diagnostic-face-state)
      (setq scrollview--diagnostic-face-state state)
      (pcase-dolist (`(,_ ,target ,inherit ,color) state)
        (set-face-attribute target nil
                            :inherit inherit
                            :foreground (or color 'unspecified)
                            :background 'unspecified
                            :inverse-video nil))
      (clrhash scrollview--sign-render-face-cache))))

(defun scrollview--sync-vc-faces (&rest _)
  "Synchronize VC sign faces with diff source faces."
  (let ((state
         (cl-loop for (key target inherit source-faces fallback)
                  in '((:add scrollview-vc-add-face
                        diff-added (diff-added diff-refine-added success)
                        "green3")
                       (:change scrollview-vc-change-face
                        diff-changed (diff-changed diff-refine-changed warning)
                        "goldenrod")
                       (:delete scrollview-vc-delete-face
                        diff-removed (diff-removed diff-refine-removed error)
                        "red3"))
                  collect (list key target inherit
                                (or (scrollview--foreground-source-color
                                     source-faces)
                                    fallback)))))
    (unless (equal state scrollview--vc-face-state)
      (setq scrollview--vc-face-state state)
      (pcase-dolist (`(,_ ,target ,inherit ,color) state)
        (set-face-attribute target nil
                            :inherit inherit
                            :foreground color
                            :background 'unspecified
                            :inverse-video nil))
      (clrhash scrollview--sign-render-face-cache))))

(defun scrollview--sync-faces (&rest _)
  "Synchronize scrollview faces with theme and source faces."
  (scrollview--sync-thumb-face)
  (scrollview--sync-diagnostic-faces)
  (scrollview--sync-vc-faces))

(defun scrollview--sign-foreground (face)
  "Return the best foreground color for rendering sign FACE.
Highlight-style faces often carry their visible color in the background, so
explicit sign colors are preferred before inherited highlight backgrounds."
  (or (scrollview--face-direct-color face '(:foreground :background))
      (scrollview--face-color face '(:background :foreground))
      (let ((foreground (face-foreground 'default nil t)))
        (when (scrollview--usable-color-p foreground)
          foreground))
      "black"))

(defun scrollview--thumb-background ()
  "Return the current scrollbar thumb background color."
  (or (scrollview--face-color 'scrollview-thumb-face '(:background :foreground))
      (scrollview--selection-color)
      'unspecified))

(defun scrollview--sign-render-face (face highlighted)
  "Return a render face for sign FACE.
When HIGHLIGHTED is non-nil, use the scrollbar thumb background.  Otherwise,
render without painting a background."
  (if (not (symbolp face))
      face
    (let* ((foreground (scrollview--sign-foreground face))
           (background (if highlighted
                           (scrollview--thumb-background)
                         'unspecified))
           (key (list face highlighted foreground background))
           (render-face (gethash key scrollview--sign-render-face-cache)))
      (unless render-face
        (setq render-face
              (intern (format "scrollview--render-sign-%s-%s"
                              face
                              (if highlighted "highlight" "plain"))))
        (unless (facep render-face)
          (make-face render-face))
        (set-face-attribute render-face nil
                            :inherit nil
                            :foreground foreground
                            :background background
                            :inverse-video nil)
        (puthash key render-face scrollview--sign-render-face-cache))
      render-face)))

(scrollview--sync-faces)
(advice-add 'enable-theme :after #'scrollview--sync-faces)

(with-eval-after-load 'flymake
  (scrollview--sync-diagnostic-faces))

(with-eval-after-load 'flycheck
  (scrollview--sync-diagnostic-faces))



(provide 'scrollview-faces)

;;; scrollview-faces.el ends here
