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

(provide 'scrollview)

;;; scrollview.el ends here
