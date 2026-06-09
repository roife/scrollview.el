# scrollview.el

`scrollview.el` is an Emacs fringe scrollbar inspired by
`nvim-scrollview`.  It renders with window-local overlays and fringe display
specs, following the same general mechanism used by `yascroll`; it does not use
child frames.

## Requirements

- Emacs 29.1 or newer
- A window with a usable left or right fringe

## Installation

Place `scrollview.el` on your `load-path`, then enable it:

```elisp
(add-to-list 'load-path "/path/to/scrollview.el")
(require 'scrollview)
(global-scrollview-mode 1)
```

For a single buffer:

```elisp
(scrollview-mode 1)
```

## Configuration

```elisp
(setq scrollview-side 'right)              ; or 'left
(setq scrollview-visibility 'overflow)     ; overflow, always, info
(setq scrollview-current-window-only nil)
(setq scrollview-signs-on-startup '(search diagnostics))
(setq scrollview-signs-no-background nil)
(setq scrollview-keywords-comments-only nil)
```

Large buffers enter restricted mode and skip signs:

```elisp
(setq scrollview-line-limit 20000)
(setq scrollview-byte-limit 1000000)
```

The scrollbar thumb uses the current `region` face background, matching the
theme's selection color.

Refreshes are debounced for configuration and buffer changes.  Scroll refreshes
run synchronously to avoid stale fringe rows during redisplay, but reuse cached
sign data until the buffer, search state, diagnostics, spelling state, or sign
registration changes.

## Commands

- `scrollview-mode`
- `global-scrollview-mode`
- `scrollview-refresh`
- `scrollview-next`
- `scrollview-prev`
- `scrollview-first`
- `scrollview-last`
- `scrollview-legend`
- `scrollview-enable-sign-group`
- `scrollview-disable-sign-group`
- `scrollview-toggle-sign-group`

## Built-In Signs

- `search`: matches from active isearch, or retained isearch lazy highlights
- `diagnostics`: Flymake diagnostics and loaded Flycheck errors, using dot
  signs colored by the theme's diagnostic faces
- `conflicts`: optional, disabled by default; Git conflict marker lines
  (`<<<<<<<`, `=======`, `>>>>>>>`)
- `keywords`: optional, disabled by default; configurable TODO/FIXME/HACK/NOTE
  style regexps
- `spell`: optional, disabled by default; current Flyspell incorrect-word
  overlays
- `vc`: optional, disabled by default; Git add/change/delete signs computed
  from the current buffer contents against `HEAD`

Built-in sign shapes:

- `search`: horizontal block
- `diagnostics`: round dot
- `conflicts`: diamond-like dot
- `keywords`: first-letter bitmap, such as `H` for HACK
- `spell`: `~`
- `vc` add/change: vertical bar
- `vc` delete: bottom block

When signs and the scrollbar map to the same fringe row, the item with higher
priority wins.  The v1 renderer uses one fringe slot per row.
Signs render with their face background by default.  Set
`scrollview-signs-no-background' to non-nil to render signs without painting a
background; highlight-style faces then use their original background color as
the sign foreground.

## Sign Extension Example

```elisp
(scrollview-register-sign-group 'todo t)

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
     (nreverse (delete-dups lines)))))
```

## Limitations

- No mouse click or drag support in v1
- No childframe or text-area fallback
- No multi-column sign overflow
- Positioning uses simple line-based mapping; fold/wrap-accurate proper mode is
  deferred
- VC signs currently support local Git-backed files.  They do not use a
  childframe, do not require saving the buffer, and do not render non-Git VC
  backends in v1.

## Tests

```sh
/Applications/Emacs.2026-04-18.8f53537.emacs-30.macOS-26.arm64/Emacs.app/Contents/MacOS/Emacs \
  -Q --batch -L . -f batch-byte-compile scrollview.el

/Applications/Emacs.2026-04-18.8f53537.emacs-30.macOS-26.arm64/Emacs.app/Contents/MacOS/Emacs \
  -Q --batch -L . -l test/scrollview-test.el -f ert-run-tests-batch-and-exit
```
