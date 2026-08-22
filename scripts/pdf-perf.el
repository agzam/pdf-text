;;; scripts/pdf-perf.el --- time the render over a real book -*- lexical-binding: t; -*-

;; The timing harness: where a render's time goes, so a performance
;; claim is a number that reruns instead of a guess.  `bb perf' walks
;; the pipeline stage by stage over a whole book and prints seconds,
;; ms/page and share per stage; `bb perf-ipc' splits the charlayout
;; stage into epdfinfo's own work and pdf-info's parsing of the
;; response, which is what tells a poppler cost from an elisp one.
;;
;; The staging here mirrors `pdf-text-render-lines' call for call, and
;; a run checks itself against the product: it fails when the staged
;; render disagrees with `pdf-text-render-pages' output.
;;
;; Shares the corpus script's plumbing: the pdf-tools lookup
;; (PDF_TOOLS overrides), the book-by-fragment lookup (BOOKS
;; overrides).  FORM=source runs the interpreted source instead of the
;; byte-compiled form the package ships as; PROFILE=1 adds a CPU
;; self-time profile to `bb perf'.

(require 'cl-lib)
(require 'seq)
(require 'bytecomp)

(load (expand-file-name "pdf-corpus.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil 'nomessage)

;; A definition swapped by deferred native compilation mid-run would
;; time two forms of the code in one table.
(when (boundp 'native-comp-jit-compilation)
  (setq native-comp-jit-compilation nil))
(when (boundp 'native-comp-deferred-compilation)
  (setq native-comp-deferred-compilation nil))

(defun pdf-perf--load-form ()
  "Load pdf-text in the form under measurement.
The corpus script loads the source; the default form byte-compiles it
under .local/ and loads the result on top, because elpaca ships the
package byte-compiled and 2.6x separates the two."
  (unless (equal (getenv "FORM") "source")
    (let* ((src (expand-file-name "pdf-text.el" pdf-corpus-script-root))
           (dest (expand-file-name ".local/elc/pdf-text.elc" pdf-corpus-script-root)))
      (make-directory (file-name-directory dest) t)
      (when (file-newer-than-file-p src dest)
        (let ((byte-compile-dest-file-function (lambda (_) dest)))
          (unless (byte-compile-file src)
            (error "pdf-text.el does not byte-compile"))))
      (load dest nil 'nomessage))))

(defun pdf-perf--form-name ()
  "The form the pipeline actually runs as, read off a loaded function."
  (let ((f (symbol-function 'pdf-text--quantile)))
    (cond ((subrp f) "native")
          ((byte-code-function-p f) "byte-code")
          (t "interpreted source"))))

(defvar pdf-perf--stages nil
  "Stage timings of the run in progress, newest first.")

(defmacro pdf-perf--stage (name &rest body)
  "Time BODY as the stage NAME and return its value."
  (declare (indent 1))
  `(let ((start (float-time)) (result (progn ,@body)))
     (push (cons ,name (- (float-time) start)) pdf-perf--stages)
     result))

(defun pdf-perf--print-profile (log)
  "Top self-time frames of the CPU profiler LOG."
  (let ((self (make-hash-table :test #'equal)) (samples 0) rows)
    (maphash (lambda (backtrace count)
               (cl-incf samples count)
               (let ((frame (aref backtrace 0)))
                 (when frame
                   (puthash frame (+ count (gethash frame self 0)) self))))
             log)
    (maphash (lambda (frame count) (push (cons frame count) rows)) self)
    (princ "\nself time, top 20 frames:\n")
    (dolist (row (seq-take (sort rows (lambda (a b) (< (cdr b) (cdr a)))) 20))
      (princ (format "  %5.1f%%  %s\n" (* 100 (/ (float (cdr row)) samples))
                     (let ((f (car row)))
                       (cond ((symbolp f) f)
                             ((and (consp f) (symbolp (car f))) (car f))
                             (t "<anonymous>"))))))))

(defun pdf-perf-stages (book)
  "Print the render's stage table over BOOK: seconds, ms/page, share.
The stages compose exactly as `pdf-view-as-text' renders - every
layout held at once, the reflow over all of it - so the numbers price
what the reader pays."
  (pdf-corpus-script-connect)
  (pdf-perf--load-form)
  (let* ((file (pdf-corpus-script-book book))
         (pages (pdf-info-number-of-pages file))
         (whole (float-time))
         (pdf-perf--stages nil)
         layouts raw records cleaned profile profiles marginal vocabulary
         outline heads rendered composed)
    (princ (format "%s: %d pages, %s\n"
                   (file-name-base file) pages (pdf-perf--form-name)))
    (when (getenv "PROFILE") (profiler-start 'cpu))
    (setq layouts (pdf-perf--stage "charlayout (epdfinfo)"
                    (cl-loop for p from 1 to pages
                             collect (condition-case nil
                                         (pdf-info-charlayout p nil file)
                                       (error nil)))))
    (setq raw (pdf-perf--stage "text from layout"
                (cl-loop for layout in layouts
                         collect (pdf-text--layout-text layout))))
    (setq records (pdf-perf--stage "line records"
                    (cl-loop for text in raw for layout in layouts
                             collect (pdf-text--page-lines text layout))))
    (setq cleaned (pdf-perf--stage "clean-pages" (pdf-text-clean-pages records)))
    (setq profile (pdf-perf--stage "document profile" (pdf-text--profile cleaned)))
    (setq profiles (pdf-perf--stage "page profiles"
                     (mapcar (lambda (l) (pdf-text--page-profile l profile))
                             cleaned)))
    (setq outline (pdf-perf--stage "outline (epdfinfo)" (pdf-info-outline file)))
    (setq heads (pdf-perf--stage "page-headings"
                  (pdf-text-page-headings outline 1 pages)))
    (setq marginal (pdf-perf--stage "marginal lines"
                     (pdf-text-remove-marginal-lines cleaned profiles heads)))
    (setq vocabulary (pdf-perf--stage "hyphen vocabulary"
                       (pdf-text--hyphenated-words marginal)))
    (setq rendered
          (pdf-perf--stage "blocks + render + escape"
            (cl-loop for lines in marginal
                     for page-profile in profiles
                     for hs = heads then (cdr hs)
                     collect (pdf-text--escape-org-lines
                              (pdf-text--render-blocks
                               (pdf-text--blocks lines page-profile)
                               page-profile vocabulary (car hs))
                              (car hs)))))
    (setq composed (pdf-perf--stage "interleave outline"
                     (if outline (pdf-text--interleave-outline rendered outline)
                       (pdf-text--synthesize-headings rendered))))
    (let ((total (- (float-time) whole)))
      (princ (format "%-26s %8s %8s %6s\n" "stage" "seconds" "ms/page" "share"))
      (dolist (stage (nreverse pdf-perf--stages))
        (princ (format "%-26s %8.2f %8.1f %5.1f%%\n" (car stage) (cdr stage)
                       (* 1000 (/ (cdr stage) pages)) (* 100 (/ (cdr stage) total)))))
      (princ (format "%-26s %8.2f %8.1f\n" "TOTAL" total (* 1000 (/ total pages))))
      (princ (format "text: %d chars, %d lines\n"
                     (apply #'+ (mapcar #'length composed))
                     (apply #'+ (mapcar (lambda (p) (length (split-string p "\n")))
                                        composed)))))
    (when (getenv "PROFILE")
      ;; profiler-stop drains the log; read it first
      (let ((log (profiler-cpu-log)))
        (profiler-stop)
        (pdf-perf--print-profile log)))
    (unless (equal rendered (pdf-text-render-pages raw layouts heads))
      (error "The staging no longer mirrors pdf-text-render-pages; realign the stages"))))

(defun pdf-perf-ipc (book &optional pages)
  "Split BOOK's charlayout cost: epdfinfo's work against pdf-info's parsing.
Walks up to PAGES pages (default 120) under `elp' instrumentation of
the pdf-info package; wire time plus poppler's own work is what the
per-call total leaves unexplained."
  (pdf-corpus-script-connect)
  (pdf-perf--load-form)
  (require 'elp)
  (let* ((file (pdf-corpus-script-book book))
         (limit (min (or (and pages (if (stringp pages) (string-to-number pages)
                                      pages))
                         120)
                     (pdf-info-number-of-pages file)))
         (glyphs 0))
    ;; warm the server, so the first-request cost is not counted
    (pdf-info-charlayout 1 nil file)
    (elp-instrument-package "pdf-info")
    (dolist (p (number-sequence 1 limit))
      (cl-incf glyphs (length (condition-case nil
                                  (pdf-info-charlayout p nil file)
                                (error nil)))))
    (princ (format "%s: %d pages, %d glyphs\n\n"
                   (file-name-base file) limit glyphs))
    ;; in batch elp-results princs the table itself
    (elp-results)))
