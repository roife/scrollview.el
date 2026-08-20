;;; scrollview.el --- Scrollbars and document signs -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: scrollview.el contributors
;; Keywords: convenience
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; scrollview.el displays a vertical scrollbar and document signs in the
;; selected fringe or window margin.  It is implemented with ordinary overlays
;; and display specs, not child frames.

;;; Code:

(require 'scrollview-core)
(require 'scrollview-signs)
(require 'scrollview-list)


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

(provide 'scrollview)

;;; scrollview.el ends here
