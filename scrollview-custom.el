;;; scrollview-custom.el --- Customization for scrollview -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Internal module for scrollview.el.

;;; Code:

;;; Customization

(defgroup scrollview nil
  "Scrollbars and document signs."
  :group 'convenience
  :prefix "scrollview-")

(defcustom scrollview-area 'fringe
  "Display area used by scrollview.
The value `fringe' renders bitmap indicators in the fringe.  The value
`margin' renders one-column text indicators in the window margin, which also
works in terminal frames."
  :type '(choice (const :tag "Fringe" fringe)
                 (const :tag "Margin" margin))
  :group 'scrollview)

(defcustom scrollview-side 'right
  "Side used by scrollview.
The value must be either `right' or `left'."
  :type '(choice (const :tag "Right side" right)
                 (const :tag "Left side" left))
  :group 'scrollview)

(defcustom scrollview-margin-width 1
  "Minimum window margin width used when `scrollview-area' is `margin'."
  :type 'natnum
  :group 'scrollview)

(defcustom scrollview-visibility 'always
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

(defcustom scrollview-signs-on-startup 'all
  "Built-in sign groups enabled when scrollview is first used.
Use `all' to enable all built-in groups."
  :type '(choice (const :tag "All built-in groups" all)
                 (repeat :tag "Selected groups" symbol))
  :group 'scrollview)

(defcustom scrollview-refresh-delay 0.03
  "Idle delay, in seconds, before a scheduled refresh runs."
  :type 'number
  :group 'scrollview)

(defcustom scrollview-scrollbar-priority 0
  "Priority of the scrollbar when it conflicts with signs.
Higher priority signs replace the scrollbar for that display row."
  :type 'integer
  :group 'scrollview)

(defcustom scrollview-overlay-priority 1000
  "Overlay priority used for rendered scrollview indicators."
  :type 'integer
  :group 'scrollview)

(defcustom scrollview-wrap-navigation t
  "When non-nil, sign navigation wraps around buffer ends."
  :type 'boolean
  :group 'scrollview)



(provide 'scrollview-custom)

;;; scrollview-custom.el ends here
