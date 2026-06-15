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

(defface scrollview-highlight-changes-face
  '((t (:inherit highlight-changes)))
  "Face for Highlight Changes change signs."
  :group 'scrollview)

(defface scrollview-highlight-changes-delete-face
  '((t (:inherit highlight-changes-delete)))
  "Face for Highlight Changes deletion signs."
  :group 'scrollview)

(defface scrollview-compilation-error-face
  '((t (:inherit (compilation-error error))))
  "Face for compilation error signs."
  :group 'scrollview)

(defface scrollview-compilation-warning-face
  '((t (:inherit (compilation-warning warning))))
  "Face for compilation warning signs."
  :group 'scrollview)

(defface scrollview-compilation-info-face
  '((t (:inherit (compilation-info success))))
  "Face for compilation info signs."
  :group 'scrollview)

(defface scrollview-diagnostic-error-face
  '((t (:inherit error)))
  "Face for diagnostic error signs."
  :group 'scrollview)

(defface scrollview-diagnostic-warning-face
  '((t (:inherit warning)))
  "Face for diagnostic warning signs."
  :group 'scrollview)

(defface scrollview-diagnostic-info-face
  '((t (:inherit success)))
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

(defface scrollview-keyword-workaround-face
  '((t (:inherit warning)))
  "Face for WORKAROUND keyword signs."
  :group 'scrollview)

(defface scrollview-keyword-trick-r-face
  '((t (:inherit warning)))
  "Face for TRICK(R) keyword signs."
  :group 'scrollview)

(defface scrollview-keyword-defect-face
  '((t (:inherit error)))
  "Face for DEFECT keyword signs."
  :group 'scrollview)

(defface scrollview-keyword-issue-face
  '((t (:inherit error)))
  "Face for ISSUE keyword signs."
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
The foreground is synchronized from `diff-added'."
  :group 'scrollview)

(defface scrollview-vc-change-face
  '((t (:inherit diff-changed)))
  "Face for changed-line VC signs.
The foreground is synchronized from `diff-changed'."
  :group 'scrollview)

(defface scrollview-vc-delete-face
  '((t (:inherit diff-removed)))
  "Face for deleted-line VC signs.
The foreground is synchronized from `diff-removed'."
  :group 'scrollview)

(define-fringe-bitmap 'scrollview-search-bitmap
  [0 0 0 126 126 0 0 0] nil nil 'center)

(define-fringe-bitmap 'scrollview-symbol-bitmap
  [0 24 24 126 24 24 0 0] nil nil 'center)

(define-fringe-bitmap 'scrollview-diagnostic-bitmap
  [0 60 126 126 126 126 60 0] nil nil 'center)

(define-fringe-bitmap 'scrollview-sign-dot-bitmap
  [0 24 60 126 126 60 24 0] nil nil 'center)

(define-fringe-bitmap 'scrollview-bookmark-bitmap
  [0 96 102 12 24 48 102 6] nil nil 'center)

(defconst scrollview--vc-bar-bitmap-vector
  [24 24 24 24 24 24 24 24
   24 24 24 24 24 24 24 24
   24 24 24 24 24 24 24 24
   24 24 24 24 24 24 24 24]
  "Bitmap vector for VC add/change vertical bar signs.")

(define-fringe-bitmap 'scrollview-sign-bar-bitmap
  scrollview--vc-bar-bitmap-vector nil nil 'center)

(define-fringe-bitmap 'scrollview-sign-delete-bitmap
  [0 0 0 126 126 0 0 0] nil nil 'center)

(define-fringe-bitmap 'scrollview-highlight-changes-bitmap
  [0 60 102 96 96 96 102 60] nil nil 'center)

(define-fringe-bitmap 'scrollview-highlight-changes-delete-bitmap
  [0 102 60 24 24 60 102 0] nil nil 'center)

(defconst scrollview--spell-bitmap-vector
  [0 0 0 54 126 108 0 0]
  "Bitmap vector for spell signs.")

(define-fringe-bitmap 'scrollview-spell-bitmap
  scrollview--spell-bitmap-vector nil nil 'center)

(define-fringe-bitmap 'scrollview-keyword-todo-bitmap
  [0 126 126 24 24 24 24 0] nil nil 'center)

(define-fringe-bitmap 'scrollview-keyword-fixme-bitmap
  [0 126 126 96 124 124 96 0] nil nil 'center)

(define-fringe-bitmap 'scrollview-keyword-hack-bitmap
  [0 102 102 126 126 102 102 0] nil nil 'center)

(define-fringe-bitmap 'scrollview-keyword-note-bitmap
  [0 102 118 126 126 110 102 0] nil nil 'center)

(define-fringe-bitmap 'scrollview-keyword-workaround-bitmap
  [0 102 102 126 126 126 60 0] nil nil 'center)

(define-fringe-bitmap 'scrollview-keyword-trick-r-bitmap
  [0 124 102 124 120 108 102 0] nil nil 'center)

(define-fringe-bitmap 'scrollview-keyword-defect-bitmap
  [0 120 108 102 102 108 120 0] nil nil 'center)

(define-fringe-bitmap 'scrollview-keyword-issue-bitmap
  [0 126 24 24 24 24 126 0] nil nil 'center)

(define-fringe-bitmap 'scrollview-keyword-bitmap
  [0 24 60 126 60 24 0 0] nil nil 'center)

(defconst scrollview--keyword-metadata
  '((todo       :priority 30 :bitmap scrollview-keyword-todo-bitmap
                :face scrollview-keyword-todo-face)
    (fixme      :priority 20 :bitmap scrollview-keyword-fixme-bitmap
                :face scrollview-keyword-fixme-face)
    (hack       :priority 20 :bitmap scrollview-keyword-hack-bitmap
                :face scrollview-keyword-hack-face)
    (note       :priority 15 :bitmap scrollview-keyword-note-bitmap
                :face scrollview-keyword-note-face)
    (workaround :priority 20 :bitmap scrollview-keyword-workaround-bitmap
                :face scrollview-keyword-workaround-face)
    (trick-r    :priority 20 :bitmap scrollview-keyword-trick-r-bitmap
                :face scrollview-keyword-trick-r-face)
    (defect     :priority 20 :bitmap scrollview-keyword-defect-bitmap
                :face scrollview-keyword-defect-face)
    (issue      :priority 25 :bitmap scrollview-keyword-issue-bitmap
                :face scrollview-keyword-issue-face))
  "Metadata for built-in keyword sign variants.
Each entry is (VARIANT :priority N :bitmap SYMBOL :face FACE).  Variants
not listed here use `scrollview-keyword-bitmap', `scrollview-keyword-face',
and priority 10.")

(defun scrollview--keyword-attr (variant attr)
  "Return ATTR for keyword VARIANT, falling back to a default."
  (or (plist-get (cdr (assq variant scrollview--keyword-metadata)) attr)
      (pcase attr
        (:priority 10)
        (:bitmap 'scrollview-keyword-bitmap)
        (:face 'scrollview-keyword-face))))

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
                (scrollview--face-color face attribute)))
    (or (color 'region :background)
        (color 'highlight :background))))

(defun scrollview--usable-color-p (color)
  "Return non-nil if COLOR is suitable for a fringe face."
  (and color
       (not (eq color 'unspecified))
       (not (and (stringp color)
                 (string-prefix-p "unspecified-" color)))))

(defun scrollview--sync-thumb-face (&rest _)
  "Synchronize the scrollbar thumb with the current selection color."
  (let* ((color (scrollview--selection-color))
         (state (if color (list :color color) (list :fallback t))))
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

(defun scrollview--face-color (face attribute)
  "Return FACE color from ATTRIBUTE, honoring inheritance."
  (let ((value (face-attribute face attribute nil t)))
    (when (scrollview--usable-color-p value)
      value)))

(defun scrollview--sync-source-faces (entries state-symbol)
  "Synchronize sign faces from ENTRIES, caching state in STATE-SYMBOL."
  (let ((state
         (cl-loop for (key target inherit source-face) in entries
                  collect (list key target inherit
                                (scrollview--face-color
                                 source-face :foreground)))))
    (unless (equal state (symbol-value state-symbol))
      (set state-symbol state)
      (pcase-dolist (`(,_ ,target ,inherit ,color) state)
        (set-face-attribute target nil
                            :inherit inherit
                            :foreground color
                            :background 'unspecified
                            :inverse-video nil))
      (clrhash scrollview--sign-render-face-cache))))

(defun scrollview--sync-diagnostic-faces (&rest _)
  "Synchronize diagnostic sign faces with diagnostic source faces."
  (scrollview--sync-source-faces
   '((:error scrollview-diagnostic-error-face error error)
     (:warning scrollview-diagnostic-warning-face warning warning)
     (:info scrollview-diagnostic-info-face success success))
   'scrollview--diagnostic-face-state))

(defun scrollview--sync-vc-faces (&rest _)
  "Synchronize VC sign faces with diff source faces."
  (scrollview--sync-source-faces
   '((:add scrollview-vc-add-face diff-added diff-added)
     (:change scrollview-vc-change-face diff-changed diff-changed)
     (:delete scrollview-vc-delete-face diff-removed diff-removed))
   'scrollview--vc-face-state))

(defun scrollview--sync-faces (&rest _)
  "Synchronize scrollview faces with theme and source faces."
  (scrollview--sync-thumb-face)
  (scrollview--sync-diagnostic-faces)
  (scrollview--sync-vc-faces))

(defun scrollview--sign-render-face (face highlighted)
  "Return a render face for sign FACE.
When HIGHLIGHTED is non-nil, use the scrollbar thumb background.  Otherwise,
render without painting a background."
  (if (not (symbolp face))
      face
    (let* ((foreground (scrollview--face-color face :foreground))
           (background (or (and highlighted
                                (or (scrollview--face-color 'scrollview-thumb-face :background)
                                    (scrollview--selection-color)))
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
