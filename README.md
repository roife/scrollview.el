# scrollview.el

`scrollview.el` is an Emacs fringe scrollbar inspired by
`nvim-scrollview`.  It renders with window-local overlays and fringe display
specs, following the same general mechanism used by `yascroll`; it does not use
child frames.

## Requirements

- Emacs 29.1 or newer
- A window with a usable left or right fringe

## Installation

Place the `scrollview.el` directory on your `load-path`, then enable it:

```elisp
(add-to-list 'load-path "/path/to/scrollview.el")
(require 'scrollview)
(global-scrollview-mode 1)
```

For a single buffer:

```elisp
(scrollview-mode 1)
```

The package is split into small internal modules:

- `scrollview-custom.el`: user options
- `scrollview-faces.el`: faces, fringe bitmaps, and sign render faces
- `scrollview-core.el`: sign registry, rendering, scheduling, navigation, modes
- `scrollview-signs.el`: built-in sign collectors and their update hooks
- `scrollview.el`: public entry point

## Configuration

```elisp
(setq scrollview-side 'right)              ; or 'left
(setq scrollview-visibility 'always)       ; overflow, always, info
(setq scrollview-current-window-only nil)
(setq scrollview-signs-on-startup '(search diagnostics)) ; or 'all
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
- `scrollview-click`
- `scrollview-legend`
- `scrollview-enable-sign-group`
- `scrollview-disable-sign-group`
- `scrollview-toggle-sign-group`

## Mouse

Click the configured fringe side to jump to the corresponding document
position.  Clicking a visible sign jumps to that sign's line.

## Built-In Signs

- `search`: matches from active isearch, or retained isearch lazy highlights
- `diagnostics`: Flymake diagnostics and loaded Flycheck errors, using dot
  signs colored by the theme's diagnostic faces
- `conflicts`: optional, disabled by default; conflict markers found through
  `smerge-mode`
- `keywords`: optional, disabled by default; keyword matches found through
  `hl-todo`
- `spell`: optional, disabled by default; misspellings highlighted by
  `flyspell`
- `vc`: optional, disabled by default; add/change/delete hunks reported by
  `diff-hl`

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
Signs render without a background when they do not overlap the scrollbar
thumb.  When a sign replaces the scrollbar thumb on the same row, it uses the
thumb background so the scrollbar remains visually continuous.

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

- No mouse drag support in v1
- No childframe or text-area fallback
- No multi-column sign overflow
- Positioning uses simple line-based mapping; fold/wrap-accurate proper mode is
  deferred
- VC signs follow `diff-hl` support.  Backends and refresh behavior are the
  same ones `diff-hl` can report for the current buffer.

## Tests

```sh
/Applications/Emacs.2026-04-18.8f53537.emacs-30.macOS-26.arm64/Emacs.app/Contents/MacOS/Emacs \
  -Q --batch -L . -f batch-byte-compile \
  scrollview-custom.el scrollview-faces.el scrollview-core.el \
  scrollview-signs.el scrollview.el

/Applications/Emacs.2026-04-18.8f53537.emacs-30.macOS-26.arm64/Emacs.app/Contents/MacOS/Emacs \
  -Q --batch -L . -l test/scrollview-test.el -f ert-run-tests-batch-and-exit
```

## Performance Metrics

Use the batch benchmark harness to evaluate `scrollview` under a repeatable
stress workload instead of a micro-benchmark:

```sh
/Applications/Emacs.2026-04-18.8f53537.emacs-30.macOS-26.arm64/Emacs.app/Contents/MacOS/Emacs \
  -Q --batch -L . -l test/scrollview-benchmark.el -f scrollview-benchmark-run
```

Optional environment variables:

- `SCROLLVIEW_BENCH_ITERATIONS`: iterations per scenario, default `1`
- `SCROLLVIEW_BENCH_LINES`: line count for standard scenarios, default `20000`
- `SCROLLVIEW_BENCH_LARGE_LINES`: line count for restricted mode, default `100000`

The benchmark prints JSON with these scenarios:

- `cold_refresh_plain`: first full refresh cost without signs
- `warm_refresh_plain`: repeated full refresh cost without signs
- `scroll_refresh_plain`: synchronous scroll refresh cost without signs
- `full_refresh_with_signs`: full refresh cost with synthetic signs enabled
- `scroll_refresh_with_signs`: scroll refresh cost while reusing sign cache
- `full_refresh_stress_signs`: full refresh cost with dense signs and multiple
  regex-scanning collectors
- `restricted_refresh`: refresh cost once line limits force restricted mode

Each metric record includes:

- `total_s`: total wall-clock seconds across all iterations
- `mean_s`: average wall-clock seconds per iteration
- `mean_ms`: average wall-clock time per iteration; primary regression signal
- `gc_mean_ms`: average GC time per iteration; useful when allocations grow
- `gc_count`: total collections during the scenario
- `overlay_count`: fringe overlay count after the benchmarked operation
- `line_count`: buffer size used by the scenario
- `sign_count`: synthetic sign count used by the scenario
- `restricted`: whether the scenario ran in restricted mode

Recommended regression gates:

- Keep `scroll_refresh_plain` and `scroll_refresh_with_signs` stable first.
  Those are the user-visible scrolling paths.
- Use `full_refresh_stress_signs` as the upper-bound regression check.  It is
  intentionally hostile and should land in the seconds range with the default
  stress profile on typical developer machines.
- Watch `gc_mean_ms` together with `mean_ms`.  If both rise, the change likely
  adds allocation churn.
- Track `overlay_count` to catch accidental over-rendering.
- Compare JSON outputs between commits instead of relying on one absolute
  number from a single run.
