# scrollview.el

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
      scrollview-fallback-to-margin t
      scrollview-side 'right
      scrollview-visibility 'always
      scrollview-signs-on-startup 'all
      scrollview-spell-checker 'flyspell)
```

Common alternatives:

```elisp
;; Show only selected sign groups on startup.
(setq scrollview-signs-on-startup '(search diagnostics vc))

;; Start without signs; enable groups later with commands.
(setq scrollview-signs-on-startup nil)

;; Collect spelling signs from Jinx instead of Flyspell.
(setq scrollview-spell-checker 'jinx)

;; Hide signs in very large buffers.
(setq scrollview-line-limit 20000
      scrollview-byte-limit 1000000)

;; Use one-column margin indicators in terminal frames.
(setq scrollview-area 'margin
      scrollview-side 'right)

;; Use margin indicators in the current buffer only.
(scrollview-margin-local-mode 1)

;; Make new and existing buffers use margin indicators by default.
(scrollview-margin-mode 1)
```

### Options

| Option | Default | Meaning |
| --- | --- | --- |
| `scrollview-area` | `fringe` | Display area.  Use `fringe` for bitmap indicators or `margin` for terminal-friendly text indicators. |
| `scrollview-fallback-to-margin` | `t` | Use margin indicators instead of fringe indicators on non-graphic displays. |
| `scrollview-side` | `right` | Display side.  Use `right` or `left`. |
| `scrollview-visibility` | `always` | `always`, `overflow`, or `info`.  `info` shows indicators when the buffer overflows or signs exist. |
| `scrollview-current-window-only` | `nil` | Show only in the selected window. |
| `scrollview-excluded-modes` | `(image-mode doc-view-mode pdf-view-mode scrollview-signs-buffer-mode)` | Major modes, including derived modes, where scrollview is disabled. |
| `scrollview-line-limit` | `20000` | Above this line count, restricted mode disables signs.  Set to `-1` to disable the limit. |
| `scrollview-byte-limit` | `1000000` | Above this buffer size, restricted mode disables signs.  Set to `-1` to disable the limit. |
| `scrollview-signs-on-startup` | `all` | Built-in sign groups enabled on first use.  Use `all`, `nil`, or a list of group symbols. |
| `scrollview-spell-checker` | `flyspell` | Spell checker used by the `spell` sign group.  Use `flyspell` or `jinx`. |
| `scrollview-refresh-delay` | `0.03` | Idle delay, in seconds, for scheduled refreshes. |
| `scrollview-update-interval` | `0` | Minimum interval for scroll-driven refreshes.  Zero updates synchronously; a positive value coalesces rapid events per window. |

Restricted mode keeps the scrollbar and skips sign collection.

## Built-In Signs

| Group | Default priority | Default face | Fringe symbol | Margin glyph |
| --- | --- | --- | --- | --- |
| `search` | `100` | `scrollview-search-face` | `=` | `=` |
| `highlight-symbol` | `70` | `scrollview-highlight-symbol-face` | `=` | `=` |
| `highlight-changes` | `80` | `change` `scrollview-highlight-changes-face`, `delete` `scrollview-highlight-changes-delete-face` | `change` `C`, `delete` `X` | `change` `C`, `delete` `X` |
| `symbol-overlay` | `90` | `scrollview-symbol-overlay-face` | `=` | `=` |
| `bookmarks` | `30` | `scrollview-bookmark-face` | `%` | `%` |
| `eglot` | `90` | `scrollview-eglot-face` | `=` | `=` |
| `diagnostics` | `error` `60`, `warning` `58`, `info` `35` | `error` `scrollview-diagnostic-error-face`, `warning` `scrollview-diagnostic-warning-face`, `info` `scrollview-diagnostic-info-face` | `o` | `!` |
| `conflicts` | `70` | `top` `scrollview-conflict-top-face`, `middle` `scrollview-conflict-middle-face`, `bottom` `scrollview-conflict-bottom-face` | `*` | `top` `<`, `middle` `=`, `bottom` `>` |
| `spell` | `50` | `scrollview-spell-face` | `~` | `~` |
| `vc` | `40` | `add` `scrollview-vc-add-face`, `change` `scrollview-vc-change-face`, `delete` `scrollview-vc-delete-face` | `add` and `change` <code>&#124;</code>, `delete` `=` | `add` `+`, `change` <code>&#124;</code>, `delete` `-` |

All built-in groups are enabled by default.  Groups backed by optional packages
produce signs only when their package is available and has data for the current
buffer.

The `diagnostics` group reads diagnostics from Flymake.

The `spell` group reads overlays from `scrollview-spell-checker`; set it to
`flyspell` (the default) or `jinx` to match the spell checker you use.

The `conflicts` group reuses conflict overlays already created by `smerge-mode`.
It does not scan buffer text, so it shows only conflicts that smerge has
discovered.

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
| `scrollview-margin-local-mode` | Use margin indicators in the current buffer. |
| `scrollview-margin-mode` | Use margin indicators in all suitable buffers. |
| `scrollview-refresh` | Rebuild rendered overlays. |
| `scrollview-next` | Jump to the next visible sign. |
| `scrollview-prev` | Jump to the previous visible sign. |
| `scrollview-first` | Jump to the first visible sign. |
| `scrollview-last` | Jump to the last visible sign. |
| `scrollview-show-buffer-signs` | Show all visible signs in a refreshable buffer. |
| `scrollview-click` | Mouse command for fringe or margin clicks. |
| `scrollview-enable-sign-group` | Enable a sign group. |
| `scrollview-disable-sign-group` | Disable a sign group. |
| `scrollview-toggle-sign-group` | Toggle a sign group. |

`scrollview-next`, `scrollview-prev`, `scrollview-first`, and
`scrollview-last` accept an optional group or group list from Lisp.

The buffer created by `scrollview-show-buffer-signs` lists each sign's line,
group, variant, priority, and source text.  Press `g` to refresh the list,
`RET` to visit a sign, or `SPC` to preview its source.

## Mouse

Click the configured fringe or margin to jump to the corresponding document
position.
Click a visible sign to jump to that sign's line.

Mouse drag is not implemented.

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
