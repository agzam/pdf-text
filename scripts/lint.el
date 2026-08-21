;;; scripts/lint.el --- parens, byte-compile and checkdoc -*- lexical-binding: t; -*-

;; Usage: emacs -Q --batch -l scripts/lint.el PACKAGE-FILE FILE...
;;
;; check-parens runs over every file given; the byte compiler (warnings as
;; errors) and checkdoc run over the first, the file the package ships.
;; Compilation must stay clean without pdf-tools on the load path - every
;; symbol of it resolves at call time - which is what lets CI skip epdfinfo.

(require 'cl-lib)
(require 'checkdoc)

(defun lint-parens (file)
  "Whether FILE's parens balance, reporting the position they do not."
  (with-temp-buffer
    (insert-file-contents file)
    (emacs-lisp-mode)
    (condition-case err
        (progn (check-parens) t)
      (error (message "PARENS %s:%d: %s"
                      file (line-number-at-pos) (error-message-string err))
             nil))))

(defun lint-compiles (file)
  "Whether FILE byte-compiles with no warning, leaving no .elc behind."
  (let ((byte-compile-error-on-warn t)
        (byte-compile-dest-file-function
         (lambda (_) (make-temp-file "lint" nil ".elc"))))
    (byte-compile-file file)))

(defun lint-checkdoc (file)
  "Whether checkdoc has nothing to say about FILE.
`checkdoc-file' reports through `warn', so the warning buffer is the
verdict: it exists only once something failed.  The experimental verb
check stays off - Emacs 29 and 30 default it on and 32 does not, and it
reads any word from its list as a leading verb, so \"the runs it leaves
behind\" comes back as a mood error."
  (let ((checkdoc-verb-check-experimental-flag nil))
    (when-let* ((buffer (get-buffer "*Warnings*")))
      (kill-buffer buffer))
    (checkdoc-file file))
  (if-let* ((buffer (get-buffer "*Warnings*")))
      (progn (message "CHECKDOC %s:\n%s" file
                      (with-current-buffer buffer (buffer-string)))
             nil)
    t))

(let* ((files command-line-args-left)
       (package (car files))
       (failures 0))
  (dolist (file files)
    (unless (lint-parens file) (cl-incf failures)))
  (unless (lint-compiles package) (cl-incf failures))
  (unless (lint-checkdoc package) (cl-incf failures))
  (message "lint: %d file(s) checked, %d failure(s)" (length files) failures)
  (kill-emacs (if (zerop failures) 0 1)))
