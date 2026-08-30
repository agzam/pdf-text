;;; scripts/pdf-perf.el --- time the render over a real book -*- lexical-binding: t; -*-

;; The timing harness: where a render's time goes, so a performance
;; claim is a number that reruns instead of a guess.  `bb perf' walks
;; the pipeline stage by stage over a whole book and prints seconds,
;; ms/page and share per stage; the mutool and parse stages separate
;; MuPDF's own work from the elisp read of its records.
;;
;; The staging here mirrors `pdf-text-render-lines' call for call, and
;; a run checks itself against the product: it fails when the staged
;; render disagrees with a fresh `pdf-text-render-lines' pass.
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

(defun pdf-perf--records (output file pages)
  "Per-page records from walker OUTPUT over FILE's PAGES.
Gettext fills a page the walker found no text on, the way
`pdf-view-as-text' fills it."
  (cl-loop for p from 1
           for lines in (pdf-text--mupdf-parse output 1 pages)
           collect (or lines
                       (pdf-text--page-lines
                        (pdf-info-gettext p '(0 0 1 1) nil file)))))

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
         output records cleaned repaired profile profiles marginal
         vocabulary outline heads rendered composed)
    (princ (format "%s: %d pages, %s\n"
                   (file-name-base file) pages (pdf-perf--form-name)))
    (when (getenv "PROFILE") (profiler-start 'cpu))
    (setq output (pdf-perf--stage "mutool stext"
                   (pdf-text--mupdf-output file 1 pages)))
    (setq records (pdf-perf--stage "line records"
                    (pdf-perf--records output file pages)))
    (setq cleaned (pdf-perf--stage "clean-pages" (pdf-text-clean-pages records)))
    (setq repaired (pdf-perf--stage "reading-order repair"
                     (let ((pre (pdf-text--profile cleaned)))
                       (mapcar (lambda (lines)
                                 (pdf-text--mark-entry-runs
                                  (pdf-text--defer-margin-notes
                                   (pdf-text--reassemble-zones
                                    (pdf-text--mark-lanes
                                     (pdf-text--merge-script-fragments
                                      (pdf-text--join-split-lines
                                       (pdf-text--float-drop-caps lines pre)
                                       pre)
                                      pre)
                                     pre)
                                    pre)
                                   pre)))
                               cleaned))))
    (setq profile (pdf-perf--stage "document profile"
                    (pdf-text--profile repaired)))
    (setq profiles (pdf-perf--stage "page profiles"
                     (mapcar (lambda (l) (pdf-text--page-profile l profile))
                             repaired)))
    (setq outline (pdf-perf--stage "outline (epdfinfo)" (pdf-info-outline file)))
    (setq heads (pdf-perf--stage "page-headings"
                  (pdf-text-page-headings outline 1 pages)))
    (setq marginal (pdf-perf--stage "marginal lines"
                     (pdf-text-remove-marginal-lines repaired profiles heads)))
    (setq vocabulary (pdf-perf--stage "hyphen vocabulary"
                       (pdf-text--hyphenated-words marginal)))
    (let* ((blocks (pdf-perf--stage "blocks"
                     (cl-loop for lines in marginal
                              for page-profile in profiles
                              collect (pdf-text--blocks lines page-profile))))
           (assignments
            (pdf-perf--stage "heading synthesis"
              (and (null outline)
                   (cl-some (lambda (lines) (cl-some #'pdf-text-line-x0 lines))
                            marginal)
                   (pdf-text--synth-assignments blocks profiles vocabulary)))))
      (setq rendered
            (pdf-perf--stage "render + escape"
              (cl-loop for page-blocks in blocks
                       for page-profile in profiles
                       for number from 1
                       for hs = heads then (cdr hs)
                       for assigned = assignments then (cdr assigned)
                       collect (pdf-text--escape-org-lines
                                (pdf-text--render-blocks
                                 page-blocks page-profile vocabulary (car hs)
                                 number (caar assigned) (cdar assigned))
                                (append (mapcar #'cadr (caar assigned))
                                        (car hs)))))))
    (setq composed (pdf-perf--stage "interleave outline"
                     (if outline (pdf-text--interleave-outline rendered outline)
                       rendered)))
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
    ;; a fresh parse, because the staged render mutated its records
    (unless (equal rendered (pdf-text-render-lines
                             (pdf-perf--records output file pages)
                             heads nil (null outline)))
      (error "The staging no longer mirrors pdf-text-render-lines; realign the stages"))))
