# scrollview.el

Scrollbar and document signs for Emacs.

`scrollview.el` renders into the left or right fringe, or into a one-column
window margin for terminal frames, with ordinary overlays and display specs.  It
does not use child frames.

**Requirements**: Emacs 29.1 or newer

## Installation

```elisp
(add-to-list 'load-path "/path/to/scrollview.el")
(require 'scrollview)
(global-scrollview-mode 1)
```

For one buffer only:

```elisp
(scrollview-mode 1)
```

## Configuration

```elisp
(setq scrollview-area 'fringe
      scrollview-side 'right
      scrollview-visibility 'always
      scrollview-signs-on-startup 'all)
```

Common alternatives:

```elisp
;; Show only selected sign groups on startup.
(setq scrollview-signs-on-startup '(search diagnostics vc))

;; Start without signs; enable groups later with commands.
(setq scrollview-signs-on-startup nil)

;; Hide signs in very large buffers.
(setq scrollview-line-limit 20000
      scrollview-byte-limit 1000000)

;; Use one-column margin indicators in terminal frames.
(setq scrollview-area 'margin
      scrollview-side 'right)
```

### Options

| Option | Default | Meaning |
| --- | --- | --- |
| `scrollview-area` | `fringe` | Display area.  Use `fringe` for bitmap indicators or `margin` for terminal-friendly text indicators. |
| `scrollview-side` | `right` | Display side.  Use `right` or `left`. |
| `scrollview-visibility` | `always` | `always`, `overflow`, or `info`.  `info` shows indicators when the buffer overflows or signs exist. |
| `scrollview-current-window-only` | `nil` | Show only in the selected window. |
| `scrollview-excluded-modes` | `(image-mode doc-view-mode pdf-view-mode)` | Major modes, including derived modes, where scrollview is disabled. |
| `scrollview-line-limit` | `20000` | Above this line count, restricted mode disables signs.  Set to `-1` to disable the limit. |
| `scrollview-byte-limit` | `1000000` | Above this buffer size, restricted mode disables signs.  Set to `-1` to disable the limit. |
| `scrollview-signs-on-startup` | `all` | Built-in sign groups enabled on first use.  Use `all`, `nil`, or a list of group symbols. |
| `scrollview-refresh-delay` | `0.03` | Idle delay, in seconds, for scheduled refreshes. |
| `scrollview-scrollbar-priority` | `0` | Scrollbar priority when a sign lands on the same display row. |
| `scrollview-overlay-priority` | `1000` | Overlay priority used for rendered indicators. |
| `scrollview-wrap-navigation` | `t` | `scrollview-next` and `scrollview-prev` wrap around buffer ends. |

Restricted mode keeps the scrollbar and skips sign collection.

## Built-In Signs

| Group | Source | Variants |
| --- | --- | --- |
| `search` | Active isearch, or retained lazy-highlight overlays after isearch exits | `match` |
| `highlight-symbol` | `highlight-symbol` highlighted regexps, when `highlight-symbol` is loaded | `match` |
| `symbol-overlay` | `symbol-overlay` overlays, when `symbol-overlay` is loaded | `match` |
| `bookmarks` | File bookmarks from `bookmark-alist` | `bookmark` |
| `eglot` | Existing Eglot document-highlight overlays | `highlight` |
| `diagnostics` | Flymake diagnostics and Flycheck errors when Flycheck is loaded | `error`, `warning`, `info` |
| `compilation` | Parsed `compilation-mode` messages, excluding `grep-mode` buffers | `error`, `warning`, `info` |
| `conflicts` | `smerge-mode` conflict markers | `top`, `middle`, `bottom` |
| `keywords` | `hl-todo` keywords from `hl-todo-keyword-faces` | One variant per configured keyword |
| `spell` | Flyspell overlays | `misspelled` |
| `vc` | `diff-hl` hunks | `add`, `change`, `delete` |

All built-in groups are enabled by default.  Groups backed by optional packages
produce signs only when their package is available and has data for the current
buffer.

Enable, disable, or toggle groups at runtime:

```elisp
(scrollview-enable-sign-group 'vc)
(scrollview-disable-sign-group 'spell)
(scrollview-toggle-sign-group 'all)
```

## Commands

| Command | Action |
| --- | --- |
| `scrollview-mode` | Toggle scrollview in the current buffer. |
| `global-scrollview-mode` | Toggle scrollview for eligible buffers. |
| `scrollview-refresh` | Rebuild rendered overlays. |
| `scrollview-next` | Jump to the next visible sign. |
| `scrollview-prev` | Jump to the previous visible sign. |
| `scrollview-first` | Jump to the first visible sign. |
| `scrollview-last` | Jump to the last visible sign. |
| `scrollview-click` | Mouse command for fringe or margin clicks. |
| `scrollview-legend` | Show registered sign specs, priorities, faces, and states. |
| `scrollview-enable-sign-group` | Enable a sign group. |
| `scrollview-disable-sign-group` | Disable a sign group. |
| `scrollview-toggle-sign-group` | Toggle a sign group. |

`scrollview-next`, `scrollview-prev`, `scrollview-first`, and
`scrollview-last` accept an optional group or group list from Lisp.

## Mouse

Click the configured fringe or margin to jump to the corresponding document
position.
Click a visible sign to jump to that sign's line.

Mouse drag is not implemented.

## Rendering Rules

- One display slot is used per window row.
- Higher priority wins when multiple items map to the same row.
- At equal priority, the earlier registered sign spec wins.
- A sign that replaces the scrollbar thumb uses the thumb background.
- A sign outside the thumb is rendered without a background.
- The scrollbar thumb follows the current `region` face color.

## Custom Signs

Register a group, then register one or more sign specs.  A collector is called
with a window and returns line numbers or markers in that window's buffer.

```elisp
(scrollview-register-sign-group 'todo t)

(defvar my-scrollview-todo-sign
  (scrollview-register-sign-spec
   :group 'todo
   :variant 'todo
   :priority 55
   :bitmap 'scrollview-search-bitmap
   :face 'font-lock-warning-face
   :collector
   (lambda (_window)
     (let (lines)
       (save-excursion
         (goto-char (point-min))
         (while (re-search-forward "\\<TODO\\>" nil t)
           (push (line-number-at-pos (match-beginning 0) t) lines)))
       (nreverse lines)))))
```

Remove a spec with:

```elisp
(scrollview-deregister-sign-spec my-scrollview-todo-sign)
```
