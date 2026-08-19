;;; scrollview-list.el --- Buffer listings for scrollview signs -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tabulated buffer listing for the signs visible in a scrollview window.

;;; Code:

(require 'subr-x)
(require 'tabulated-list)
(require 'scrollview-core)

(defvar-local scrollview--signs-buffer-source nil
  "Source buffer represented by the current scrollview signs buffer.")

(defvar-local scrollview--signs-buffer-source-window nil
  "Preferred source window for the current scrollview signs buffer.")

(defun scrollview--signs-buffer-line-less-p (entry-a entry-b)
  "Return non-nil when ENTRY-A belongs before ENTRY-B by source line."
  (< (plist-get (car entry-a) :line)
     (plist-get (car entry-b) :line)))

(defun scrollview--signs-buffer-priority-less-p (entry-a entry-b)
  "Return non-nil when ENTRY-A has lower priority than ENTRY-B."
  (< (plist-get (car entry-a) :priority)
     (plist-get (car entry-b) :priority)))

(defun scrollview--signs-buffer-window ()
  "Return a live window showing the current listing's source buffer."
  (let ((source scrollview--signs-buffer-source)
        (window scrollview--signs-buffer-source-window))
    (cond
     ((and (window-live-p window)
           (eq (window-buffer window) source))
      window)
     ((and (buffer-live-p source)
           (get-buffer-window source t)))
     (t nil))))

(defun scrollview--signs-buffer-line-data (source line)
  "Return a marker and display text for LINE in SOURCE."
  (with-current-buffer source
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- line))
        (list (copy-marker (line-beginning-position))
              (string-trim-right
               (buffer-substring-no-properties
                (line-beginning-position) (line-end-position))))))))

(defun scrollview--signs-buffer-item-less-p (item-a item-b)
  "Return non-nil when sign ITEM-A should precede ITEM-B."
  (let* ((line-a (car item-a))
         (line-b (car item-b))
         (spec-a (cdr item-a))
         (spec-b (cdr item-b))
         (priority-a (scrollview--sign-spec-priority spec-a))
         (priority-b (scrollview--sign-spec-priority spec-b))
         (group-a (symbol-name (scrollview--sign-spec-group spec-a)))
         (group-b (symbol-name (scrollview--sign-spec-group spec-b)))
         (variant-a (format "%s" (or (scrollview--sign-spec-variant spec-a) "")))
         (variant-b (format "%s" (or (scrollview--sign-spec-variant spec-b) ""))))
    (cond
     ((/= line-a line-b) (< line-a line-b))
     ((/= priority-a priority-b) (> priority-a priority-b))
     ((not (string= group-a group-b)) (string< group-a group-b))
     ((not (string= variant-a variant-b)) (string< variant-a variant-b))
     (t (< (scrollview--sign-spec-id spec-a)
           (scrollview--sign-spec-id spec-b))))))

(defun scrollview--signs-buffer-same-item-p (item-a item-b)
  "Return non-nil when ITEM-A and ITEM-B are the same collected sign."
  (and (= (car item-a) (car item-b))
       (= (scrollview--sign-spec-id (cdr item-a))
          (scrollview--sign-spec-id (cdr item-b)))))

(defun scrollview--signs-buffer-entries ()
  "Return tabulated entries for the current scrollview signs buffer."
  (let ((source scrollview--signs-buffer-source)
        (window (scrollview--signs-buffer-window))
        entries)
    (when (and (buffer-live-p source)
               (buffer-local-value 'scrollview-mode source))
      (unless window
        (user-error "The scrollview source buffer is not displayed"))
      (setq scrollview--signs-buffer-source-window window)
      (let (previous)
        (dolist (item (sort (copy-sequence
                             (scrollview--collect-sign-items window))
                            #'scrollview--signs-buffer-item-less-p))
          (unless (and previous
                       (scrollview--signs-buffer-same-item-p previous item))
            (let* ((line (car item))
                   (spec (cdr item))
                   (group (scrollview--sign-spec-group spec))
                   (variant (scrollview--sign-spec-variant spec))
                   (priority (scrollview--sign-spec-priority spec))
                   (face (scrollview--sign-spec-face spec))
                   (line-data (scrollview--signs-buffer-line-data source line))
                   (marker (car line-data))
                   (text (cadr line-data))
                   (id (list :marker marker
                             :line line
                             :priority priority
                             :spec-id (scrollview--sign-spec-id spec))))
              (push (list id
                          (vector (number-to-string line)
                                  (symbol-name group)
                                  (propertize (format "%s" (or variant ""))
                                              'face face)
                                  (number-to-string priority)
                                  text))
                    entries)))
          (setq previous item))))
    (nreverse entries)))

(defun scrollview--signs-buffer-entry-marker (&optional position)
  "Return the source marker for the entry at POSITION."
  (let* ((id (tabulated-list-get-id position))
         (marker (plist-get id :marker)))
    (unless id
      (user-error "No scrollview sign at point"))
    (unless (and (markerp marker) (marker-buffer marker))
      (user-error "The scrollview source position is no longer available"))
    marker))

;;;###autoload
(defun scrollview-signs-buffer-show (&optional position other-window)
  "Show the source of the scrollview sign at POSITION.
When OTHER-WINDOW is non-nil, keep the signs buffer selected."
  (interactive (list (point) t))
  (let* ((listing (current-buffer))
         (marker (scrollview--signs-buffer-entry-marker position))
         (source (marker-buffer marker))
         (source-position (marker-position marker))
         (window (display-buffer source (and other-window t))))
    (unless (window-live-p window)
      (user-error "Unable to display scrollview source buffer"))
    (with-selected-window window
      (goto-char source-position))
    (setq next-error-last-buffer listing)
    source))

;;;###autoload
(defun scrollview-signs-buffer-goto (&optional position)
  "Visit the source of the scrollview sign at POSITION."
  (interactive (list (point)))
  (pop-to-buffer (scrollview-signs-buffer-show position)))

(defvar scrollview-signs-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'scrollview-signs-buffer-goto)
    (define-key map (kbd "SPC") #'scrollview-signs-buffer-show)
    (define-key map (kbd "C-o") #'scrollview-signs-buffer-show)
    map)
  "Keymap for `scrollview-signs-buffer-mode'.")

(define-derived-mode scrollview-signs-buffer-mode tabulated-list-mode
  "Scrollview Signs"
  "Major mode for listing signs from a scrollview source buffer."
  :interactive nil
  (setq tabulated-list-format
        `[("Line" 7 scrollview--signs-buffer-line-less-p :right-align t)
          ("Group" 18 t)
          ("Variant" 14 t)
          ("Priority" 8 scrollview--signs-buffer-priority-less-p
           :right-align t)
          ("Text" 0 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Line" . nil))
  (setq tabulated-list-entries #'scrollview--signs-buffer-entries)
  (tabulated-list-init-header))

(defun scrollview--signs-buffer-name (source)
  "Return the signs buffer name for SOURCE."
  (format "*Scrollview signs for `%s'*" (buffer-name source)))

(defun scrollview--fit-signs-window (window)
  "Fit scrollview signs WINDOW to a useful height."
  (fit-window-to-buffer window 15 8))

;;;###autoload
(defun scrollview-show-buffer-signs ()
  "Show a refreshable listing of visible scrollview signs.
The listing belongs to the current buffer.  Press `g' in it to recollect
signs, `RET' to visit a sign, or `SPC' to preview one without leaving the
listing."
  (interactive)
  (unless scrollview-mode
    (user-error "Scrollview mode is not enabled in the current buffer"))
  (let* ((source (current-buffer))
         (source-window (if (eq (window-buffer (selected-window)) source)
                            (selected-window)
                          (get-buffer-window source t)))
         (name (scrollview--signs-buffer-name source))
         (target (get-buffer-create name)))
    (unless source-window
      (user-error "The scrollview source buffer is not displayed"))
    (with-current-buffer target
      (unless (derived-mode-p 'scrollview-signs-buffer-mode)
        (scrollview-signs-buffer-mode))
      (setq scrollview--signs-buffer-source source)
      (setq scrollview--signs-buffer-source-window source-window)
      (setq next-error-last-buffer target)
      (revert-buffer)
      (display-buffer
       target
       '((display-buffer-reuse-window display-buffer-below-selected)
         (window-height . scrollview--fit-signs-window))))
    target))

(provide 'scrollview-list)

;;; scrollview-list.el ends here
