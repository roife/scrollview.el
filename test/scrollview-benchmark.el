;;; scrollview-benchmark.el --- Benchmarks for scrollview.el -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'scrollview)

(defgroup scrollview-benchmark nil
  "Benchmark settings for scrollview."
  :group 'scrollview)

(defcustom scrollview-benchmark-lines 20000
  "Default number of lines used by standard benchmark scenarios."
  :type 'integer
  :group 'scrollview-benchmark)

(defcustom scrollview-benchmark-large-lines 100000
  "Line count used by the restricted-mode benchmark scenario."
  :type 'integer
  :group 'scrollview-benchmark)

(defcustom scrollview-benchmark-sign-step 10
  "Line spacing used when generating synthetic benchmark signs."
  :type 'integer
  :group 'scrollview-benchmark)

(defcustom scrollview-benchmark-iterations 1
  "Default number of iterations for each benchmark scenario."
  :type 'integer
  :group 'scrollview-benchmark)

(defun scrollview-benchmark--env-int (name fallback)
  "Return integer from environment variable NAME, or FALLBACK."
  (let ((value (getenv name)))
    (if (and value (string-match-p "\\`[0-9]+\\'" value))
        (string-to-number value)
      fallback)))

(defun scrollview-benchmark--iterations ()
  "Return configured benchmark iteration count."
  (scrollview-benchmark--env-int
   "SCROLLVIEW_BENCH_ITERATIONS"
   scrollview-benchmark-iterations))

(defun scrollview-benchmark--reset-state ()
  "Reset global scrollview state between scenarios."
  (maphash (lambda (_window overlays)
             (mapc #'delete-overlay overlays))
           scrollview--window-overlays)
  (setq scrollview--window-overlays (make-hash-table :test #'eq))
  (setq scrollview--pending-windows (make-hash-table :test #'eq))
  (setq scrollview--pending-all nil)
  (when (timerp scrollview--refresh-timer)
    (cancel-timer scrollview--refresh-timer))
  (setq scrollview--refresh-timer nil)
  (remove-hook 'window-configuration-change-hook
               #'scrollview--window-configuration-change)
  (remove-hook 'window-size-change-functions
               #'scrollview--window-size-change)
  (remove-hook 'post-command-hook #'scrollview--post-command)
  (advice-remove 'lazy-highlight-cleanup
                 #'scrollview--after-lazy-highlight-cleanup)
  (setq scrollview--global-hooks-installed nil)
  (setq scrollview--last-selected-window nil)
  (setq scrollview--sign-groups (make-hash-table :test #'eq))
  (setq scrollview--sign-specs (make-hash-table :test #'eql))
  (setq scrollview--window-sign-cache (make-hash-table :test #'eq))
  (setq scrollview--sign-cache-generation 0)
  (setq scrollview--sign-render-face-cache (make-hash-table :test #'equal))
  (setq scrollview--thumb-face-state nil)
  (setq scrollview--diagnostic-face-state nil)
  (setq scrollview--vc-face-state nil)
  (setq scrollview--next-sign-id 0)
  (setq scrollview--builtins-initialized nil)
  (setq scrollview--refreshing nil)
  (setq scrollview--last-search-pattern nil)
  (setq scrollview--last-search-regexp nil)
  (setq scrollview--diagnostic-state-generation 0)
  (setq scrollview--spell-state-generation 0)
  (setq scrollview--ispell-misspelling-markers nil)
  (setq scrollview--vc-state-generation 0))

(defun scrollview-benchmark--insert-lines (count &optional prefix)
  "Insert COUNT lines using PREFIX."
  (dotimes (i count)
    (insert (format "%s%d" (or prefix "line ") (1+ i)))
    (when (< i (1- count))
      (insert "\n"))))

(defun scrollview-benchmark--insert-stress-lines (count)
  "Insert COUNT synthetic lines that exercise multiple collector styles."
  (dotimes (i count)
    (insert
     (format
      "line %d TODO alpha-%d FIXME beta-%d HACK gamma-%d NOTE delta-%d marker-%06d"
      (1+ i) (mod i 97) (mod i 193) (mod i 389) (mod i 53) i))
    (when (= (mod i 25) 0)
      (insert " repeated-token repeated-token repeated-token"))
    (when (< i (1- count))
      (insert "\n"))))

(defmacro scrollview-benchmark--with-displayed-buffer (&rest body)
  "Run BODY in a temporary displayed buffer."
  (declare (indent 0) (debug t))
  `(let ((buffer (generate-new-buffer " *scrollview-benchmark*"))
         (original-buffer (current-buffer)))
     (unwind-protect
         (progn
           (switch-to-buffer buffer)
           ,@body)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (bound-and-true-p scrollview-mode)
             (scrollview-mode -1)))
         (when (eq (current-buffer) buffer)
           (switch-to-buffer original-buffer))
         (kill-buffer buffer))
       (scrollview-benchmark--reset-state))))

(defun scrollview-benchmark--overlay-count (window)
  "Return current overlay count for WINDOW."
  (length (gethash window scrollview--window-overlays)))

(defun scrollview-benchmark--sign-lines (line-count)
  "Return synthetic sign lines for LINE-COUNT."
  (let ((step (max 1 scrollview-benchmark-sign-step))
        lines)
    (cl-loop for line from 1 to line-count by step
             do (push line lines))
    (nreverse lines)))

(defun scrollview-benchmark--register-signs (line-count)
  "Register a synthetic sign group for LINE-COUNT."
  (let ((lines (scrollview-benchmark--sign-lines line-count)))
    (scrollview-register-sign-group 'benchmark t)
    (scrollview-register-sign-spec
     :group 'benchmark
     :variant 'mock
     :priority 80
     :bitmap 'scrollview-search-bitmap
     :face 'scrollview-search-face
     :collector (lambda (_window) lines))
    (length lines)))

(defun scrollview-benchmark--require-hl-todo ()
  "Load `hl-todo' or signal a benchmark error."
  (dolist (dir '("~/.emacs.d/straight/repos/hl-todo"
                 "~/.emacs.d/straight/repos/cond-let"
                 "~/.emacs.d/straight/build/hl-todo"
                 "~/.emacs.d/straight/build/cond-let"))
    (let ((path (expand-file-name dir)))
      (when (file-directory-p path)
        (add-to-list 'load-path path))))
  (unless (require 'hl-todo nil t)
    (error "full_refresh_stress_signs requires hl-todo on load-path")))

(defun scrollview-benchmark--register-stress-signs (line-count)
  "Register stress sign collectors for LINE-COUNT using hl-todo results."
  (scrollview-benchmark--require-hl-todo)
  (let ((dense-lines (scrollview-benchmark--sign-lines line-count))
        (groups '((todo stress-a 90 scrollview-search-bitmap scrollview-search-face)
                  (fixme stress-b 80 scrollview-diagnostic-bitmap scrollview-diagnostic-warning-face)
                  (hack stress-c 70 scrollview-sign-dot-bitmap scrollview-keyword-hack-face)
                  (note stress-d 60 scrollview-sign-bar-bitmap scrollview-vc-change-face))))
    (setq-local hl-todo-keyword-faces
                '(("TODO" . "red")
                  ("FIXME" . "orange")
                  ("HACK" . "goldenrod")
                  ("NOTE" . "steel blue")))
    (scrollview-register-sign-group 'benchmark-stress t)
    (scrollview-register-sign-spec
     :group 'benchmark-stress
     :variant 'dense
     :priority 95
     :bitmap 'scrollview-search-bitmap
     :face 'scrollview-search-face
     :collector (lambda (_window) dense-lines))
    (dolist (entry groups)
      (pcase-let ((`(,keyword-variant ,variant ,priority ,bitmap ,face) entry))
        (scrollview-register-sign-spec
         :group 'benchmark-stress
         :variant variant
         :priority priority
         :bitmap bitmap
         :face face
         :collector (apply-partially
                     #'scrollview--collect-keyword-lines
                     keyword-variant))))
    (+ (length dense-lines)
       (* line-count (length groups)))))

(defun scrollview-benchmark--stats (name iterations benchmark overlay-count
                                         line-count sign-count restricted)
  "Build a metrics plist for NAME from BENCHMARK output."
  (let* ((elapsed (nth 0 benchmark))
         (gc-count (nth 1 benchmark))
         (gc-elapsed (nth 2 benchmark))
         (total-s elapsed)
         (mean-s (/ elapsed iterations))
         (mean-ms (* 1000.0 (/ elapsed iterations)))
         (gc-mean-ms (* 1000.0 (/ gc-elapsed iterations))))
    (list :name name
          :iterations iterations
          :total_s total-s
          :mean_s mean-s
          :total_ms (* 1000.0 elapsed)
          :mean_ms mean-ms
          :gc_total_ms (* 1000.0 gc-elapsed)
          :gc_mean_ms gc-mean-ms
          :gc_count gc-count
          :overlay_count overlay-count
          :line_count line-count
          :sign_count sign-count
          :restricted restricted)))

(defun scrollview-benchmark--run-scenario (name line-count setup benchmark-fn)
  "Run benchmark NAME on LINE-COUNT using SETUP and BENCHMARK-FN."
  (scrollview-benchmark--with-displayed-buffer
    (scrollview-benchmark--insert-lines line-count)
    (goto-char (point-min))
    (let* ((iterations (scrollview-benchmark--iterations))
           (scrollview-visibility 'always)
           (scrollview-signs-on-startup nil)
           (scrollview-refresh-delay 0)
           (scrollview-line-limit -1)
           (scrollview-byte-limit -1)
           (sign-count 0)
           (compiled-fn (byte-compile benchmark-fn))
           benchmark
           overlay-count
           restricted)
      (cl-letf (((symbol-function 'scrollview--fringe-available-p)
                 (lambda (_window) t)))
        (when setup
          (setq sign-count (or (funcall setup) 0)))
        (scrollview-mode 1)
        (setq benchmark (benchmark-call compiled-fn iterations))
        (setq overlay-count
              (scrollview-benchmark--overlay-count (selected-window)))
        (setq restricted (scrollview--restricted-p))
        (scrollview-benchmark--stats
         name iterations benchmark overlay-count
         line-count sign-count restricted)))))

(defun scrollview-benchmark--cold-refresh ()
  "Benchmark a full refresh without signs."
  (scrollview-refresh (selected-window)))

(defun scrollview-benchmark--warm-refresh ()
  "Benchmark a repeated full refresh without signs."
  (scrollview-refresh (selected-window)))

(defun scrollview-benchmark--scroll-refresh ()
  "Benchmark synchronous scroll refresh."
  (let ((window (selected-window)))
    (set-window-start
     window
     (save-excursion
       (goto-char (window-start window))
       (forward-line 10)
       (point))
     t)
    (scrollview--after-window-scroll window nil)))

(defun scrollview-benchmark--collector-refresh ()
  "Benchmark a full refresh with synthetic signs enabled."
  (scrollview-refresh (selected-window)))

(defun scrollview-benchmark--restricted-refresh ()
  "Benchmark refresh cost in restricted mode."
  (scrollview-refresh (selected-window)))

(defun scrollview-benchmark-collect ()
  "Return a list of benchmark metric plists."
  (let ((standard-lines
         (scrollview-benchmark--env-int
          "SCROLLVIEW_BENCH_LINES"
          scrollview-benchmark-lines))
        (large-lines
         (scrollview-benchmark--env-int
          "SCROLLVIEW_BENCH_LARGE_LINES"
          scrollview-benchmark-large-lines)))
    (list
     (scrollview-benchmark--run-scenario
      "cold_refresh_plain"
      standard-lines
      nil
      (lambda () (scrollview-benchmark--cold-refresh)))
     (scrollview-benchmark--run-scenario
      "warm_refresh_plain"
      standard-lines
      (lambda ()
        (scrollview-refresh (selected-window))
        0)
      (lambda () (scrollview-benchmark--warm-refresh)))
     (scrollview-benchmark--run-scenario
      "scroll_refresh_plain"
      standard-lines
      (lambda ()
        (scrollview-refresh (selected-window))
        0)
      (lambda () (scrollview-benchmark--scroll-refresh)))
     (scrollview-benchmark--run-scenario
      "full_refresh_with_signs"
      standard-lines
      (lambda ()
        (scrollview-benchmark--register-signs standard-lines))
      (lambda () (scrollview-benchmark--collector-refresh)))
     (scrollview-benchmark--run-scenario
      "scroll_refresh_with_signs"
      standard-lines
      (lambda ()
        (let ((count (scrollview-benchmark--register-signs standard-lines)))
          (scrollview-refresh (selected-window))
          count))
      (lambda () (scrollview-benchmark--scroll-refresh)))
     (scrollview-benchmark--with-displayed-buffer
       (scrollview-benchmark--insert-stress-lines standard-lines)
       (goto-char (point-min))
       (let ((iterations (scrollview-benchmark--iterations))
             (scrollview-visibility 'always)
             (scrollview-signs-on-startup nil)
             (scrollview-refresh-delay 0)
             (scrollview-line-limit -1)
             (scrollview-byte-limit -1)
             (compiled-fn (byte-compile #'scrollview-benchmark--collector-refresh))
             benchmark
             overlay-count
             sign-count
             restricted)
         (cl-letf (((symbol-function 'scrollview--fringe-available-p)
                    (lambda (_window) t)))
           (setq sign-count
                 (scrollview-benchmark--register-stress-signs standard-lines))
           (scrollview-mode 1)
           (setq benchmark (benchmark-call compiled-fn iterations))
           (setq overlay-count
                 (scrollview-benchmark--overlay-count (selected-window)))
           (setq restricted (scrollview--restricted-p))
           (scrollview-benchmark--stats
            "full_refresh_stress_signs"
            iterations benchmark overlay-count
            standard-lines sign-count restricted))))
     (scrollview-benchmark--with-displayed-buffer
       (scrollview-benchmark--insert-lines large-lines)
       (goto-char (point-min))
       (let* ((iterations (scrollview-benchmark--iterations))
              (scrollview-visibility 'always)
              (scrollview-signs-on-startup nil)
              (scrollview-refresh-delay 0)
              (scrollview-line-limit (max 1 (/ large-lines 2)))
              (scrollview-byte-limit -1)
              (compiled-fn
               (byte-compile #'scrollview-benchmark--restricted-refresh))
              benchmark
              overlay-count
              restricted)
         (cl-letf (((symbol-function 'scrollview--fringe-available-p)
                    (lambda (_window) t)))
           (scrollview-mode 1)
           (setq benchmark (benchmark-call compiled-fn iterations))
           (setq overlay-count
                 (scrollview-benchmark--overlay-count (selected-window)))
           (setq restricted (scrollview--restricted-p))
           (scrollview-benchmark--stats
            "restricted_refresh"
            iterations benchmark overlay-count
            large-lines 0 restricted)))))))

;;;###autoload
(defun scrollview-benchmark-run ()
  "Run scrollview benchmarks and print metrics as JSON."
  (interactive)
  (let ((json-encoding-pretty-print t))
    (princ
     (json-encode
      (list :generated_at (format-time-string "%FT%T%z")
            :emacs_version emacs-version
            :iterations (scrollview-benchmark--iterations)
            :metrics (vconcat (scrollview-benchmark-collect)))))))

(provide 'scrollview-benchmark)

;;; scrollview-benchmark.el ends here
