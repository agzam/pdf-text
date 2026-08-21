;;; tests/helper.el --- shared buttercup bootstrap -*- lexical-binding: t; -*-

;; Point user-emacs-directory at a temp dir before anything derives a path
;; from it: neither -Q nor --batch relocates it, so an unadorned batch Emacs
;; writes into whatever config the developer actually runs.
(defvar test-sandbox-dir
  (file-name-as-directory (make-temp-file "pdf-text-tests" t)))

(setq user-emacs-directory test-sandbox-dir)

;; Batch runs skip early-init.el, so no eln redirect ever fires - and specs
;; that cl-letf primitives make Emacs synthesize trampoline .eln files, which
;; would land in the real eln-cache.  Point them at the sandbox.
(when (and (featurep 'native-compile)
           (fboundp 'startup-redirect-eln-cache))
  (startup-redirect-eln-cache (expand-file-name "eln-cache/" test-sandbox-dir)))

(defvar test-package-root
  (expand-file-name "../" (file-name-directory (or load-file-name buffer-file-name)))
  "Root of the package being tested, derived from this file's location.")

(add-to-list 'load-path test-package-root)

(defun load-package-file (relpath)
  "Load RELPATH relative to the package root, without load-path pollution."
  (load (expand-file-name relpath test-package-root) nil 'nomessage))

(provide 'test-helper)
