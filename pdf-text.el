;;; modules/pdf/autoload/pdf-text.el -*- lexical-binding: t; -*-

;; Reflowed plain-text reading view for PDFs.  Text comes from the
;; already-running epdfinfo (`pdf-info-gettext' per page); everything
;; below the two entry commands is pure text transformation, testable
;; without a PDF or pdf-tools on the load path.

(require 'cl-lib)

(defvar pdf-text-preformatted-indent 4
  "Leading spaces at which a line counts as preformatted.")

(defvar pdf-text-preformatted-space-run 3
  "Interior space-run length that marks a line preformatted.
Three, not two: justified PDFs emit double spaces inside ordinary
sentences, and those lines must stay joinable.")

(defun pdf-text--preformatted-p (line)
  "Whether LINE looks preformatted: code, table, or aligned layout."
  (or (string-match-p (format "\\`\\(?: \\{%d\\}\\|\t\\)"
                              pdf-text-preformatted-indent)
                      line)
      (string-match-p (format " \\{%d\\}" pdf-text-preformatted-space-run)
                      (string-trim line))))

(defun pdf-text--join-lines (para line)
  "Append LINE to PARA, absorbing a wrap hyphen.
A word-attached trailing hyphen means the wrap split mid-word: a
lowercase continuation is the split word's tail (drop the hyphen), any
other continuation is a compound broken at its own hyphen (keep it);
neither wants a space.  A dangling hyphen is ordinary text."
  (let ((case-fold-search nil))         ; [[:lower:]] must not match S
    (cond
     ((not (string-match-p "[[:alnum:]]-\\'" para))
      (concat para " " line))
     ((string-match-p "\\`[[:lower:]]" line)
      (concat (substring para 0 -1) line))
     (t (concat para line)))))

(defun pdf-text-unfill (text)
  "Reflow hard-wrapped TEXT into one line per paragraph.
Blank lines separate paragraphs and pass through; preformatted-looking
lines (see `pdf-text--preformatted-p') pass through verbatim."
  (let (out para)
    (dolist (line (split-string text "\n"))
      (cond
       ((string-blank-p line)
        (when para (push para out) (setq para nil))
        (push "" out))
       ((pdf-text--preformatted-p line)
        (when para (push para out) (setq para nil))
        (push line out))
       (t
        (setq para (if para
                       (pdf-text--join-lines para (string-trim line))
                     (string-trim-right line))))))
    (when para (push para out))
    (string-join (nreverse out) "\n")))

(defvar pdf-text-recurring-min-count 3
  "Occurrences at page edges before a line counts as a running header/footer.")

(defun pdf-text--normalize-line (line)
  "LINE with digit runs collapsed to #, for header/footer matching.
\"INTRODUCTION │ 7\" and \"INTRODUCTION │ 9\" must count as one form."
  (string-trim (replace-regexp-in-string "[0-9]+" "#" line)))

(defun pdf-text--edge-lines (lines)
  "First and last non-blank line of LINES, once each."
  (let ((nb (cl-remove-if #'string-blank-p lines)))
    (cl-remove-duplicates (list (car nb) (car (last nb))) :test #'equal)))

(defun pdf-text-remove-recurring-lines (pages)
  "PAGES (raw strings) without running-header/footer lines.
A line whose digit-normalized form shows up as the first or last
non-blank line of `pdf-text-recurring-min-count' pages is layout, not
text.  Qualified forms are then dropped anywhere they appear: 2-up
spread pages embed whole book pages, headers included, mid-text."
  (let ((page-lines (mapcar (lambda (p) (split-string p "\n")) pages))
        (counts (make-hash-table :test #'equal)))
    (dolist (lines page-lines)
      (dolist (l (pdf-text--edge-lines lines))
        (when l
          (cl-incf (gethash (pdf-text--normalize-line l) counts 0)))))
    (let (recurring)
      (maphash (lambda (form n)
                 (when (<= pdf-text-recurring-min-count n)
                   (push form recurring)))
               counts)
      (mapcar (lambda (lines)
                (string-join
                 (cl-remove-if (lambda (l)
                                 (and (not (string-blank-p l))
                                      (member (pdf-text--normalize-line l)
                                              recurring)))
                               lines)
                 "\n"))
              page-lines))))

(defun pdf-text--collapse-doubled (line)
  "Collapse LINE when it reads as the same string twice.
Slide-style titles painted twice for a shadow effect reach gettext as
\"PATTERNS OF CONFLICT PATTERNS OF CONFLICT\"; a body line that is one
string doubled around a single space is that artifact, not prose."
  (let* ((trimmed (string-trim line))
         (len (length trimmed))
         (mid (/ len 2)))
    (if (and (< 2 len)
             (cl-oddp len)
             (eq ?\s (aref trimmed mid))
             (equal (substring trimmed 0 mid) (substring trimmed (1+ mid))))
        (substring trimmed 0 mid)
      line)))

(defun pdf-text--dedup-adjacent (lines)
  "LINES with runs of identical non-blank neighbors collapsed to one.
The other face of the shadow-draw artifact: the second paint lands a
point lower, so gettext emits the same title on two adjacent lines."
  (let (out)
    (dolist (line lines (nreverse out))
      (unless (and out
                   (not (string-blank-p line))
                   (equal (string-trim line) (string-trim (car out))))
        (push line out)))))

(defun pdf-text-clean-pages (pages)
  "PAGES with running headers/footers stripped and adjacent dupes dropped."
  (mapcar (lambda (page)
            (string-join (pdf-text--dedup-adjacent (split-string page "\n")) "\n"))
          (pdf-text-remove-recurring-lines pages)))

(defun pdf-text-render-pages (pages)
  "Raw PAGES cleaned, unfilled, and de-shadowed, ready for insertion.
The doubled-title collapse runs after unfill: the second shadow paint
can split across raw lines (\"PATTERNS OF\" / \"CONFLICT\"), so the
doubling only becomes a matchable line once the paragraph is joined."
  (mapcar (lambda (page)
            (string-join (mapcar #'pdf-text--collapse-doubled
                                 (split-string (pdf-text-unfill page) "\n"))
                         "\n"))
          (pdf-text-clean-pages pages)))

(defvar-local pdf-text--page-starts nil
  "Vector of buffer positions; element N-1 is where page N starts.")

(defvar-local pdf-text--pdf-buffer nil
  "The `pdf-view-mode' buffer this text view mirrors.")

(defun pdf-text--insert-pages (pages)
  "Insert PAGES (list of strings) at point, a form feed between pages.
Fills `pdf-text--page-starts' with each page's start position."
  (let (starts)
    (while pages
      (push (point) starts)
      (insert (string-trim (pop pages)) "\n")
      (when pages (insert "\f\n")))
    (setq pdf-text--page-starts (vconcat (nreverse starts)))))

(defun pdf-text--page-start (page)
  "Buffer position where PAGE starts, clamped to the known range."
  (let ((starts pdf-text--page-starts))
    (aref starts (min (max 0 (1- page)) (1- (length starts))))))

(defun pdf-text-page-at-point ()
  "Page number at point: one more than the form feeds above it."
  (save-excursion
    (let ((pos (point))
          (n 1))
      (goto-char (point-min))
      (while (search-forward "\f" pos t)
        (setq n (1+ n)))
      n)))

(define-derived-mode pdf-text-mode text-mode "pdf-text"
  "Reflowed plain-text reading view of a PDF."
  (setq buffer-read-only t)
  (visual-line-mode 1)
  (goto-address-mode 1)
  ;; Render each page-delimiting ^L as a rule instead of a glyph.
  (setq-local buffer-display-table (make-display-table))
  (aset buffer-display-table ?\f
        (vconcat (make-list 64 (make-glyph-code ?─ 'shadow)))))

;;;###autoload
(defun pdf-view-as-text ()
  "Read the current PDF as reflowed text in a companion buffer.
Extracts every page through epdfinfo and lands on the page the
`pdf-view-mode' buffer is showing.  Re-invoking re-extracts."
  (interactive)
  (unless (derived-mode-p 'pdf-view-mode)
    (user-error "Not in a pdf-view buffer"))
  (let* ((pdf-buf (current-buffer))
         (page (pdf-view-current-page))
         (pages (pdf-text-render-pages
                 (mapcar (lambda (p) (pdf-info-gettext p '(0 0 1 1)))
                         (number-sequence 1 (pdf-info-number-of-pages)))))
         (buf (get-buffer-create (format "*pdf-text: %s*" (buffer-name)))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (pdf-text-mode)
        (pdf-text--insert-pages pages)
        (setq pdf-text--pdf-buffer pdf-buf)))
    (pop-to-buffer buf)
    (goto-char (pdf-text--page-start page))
    (recenter 0)))

(defun pdf-text-show-in-pdf ()
  "Jump the source PDF buffer to the page at point and focus it."
  (interactive)
  (let ((page (pdf-text-page-at-point))
        (buf pdf-text--pdf-buffer))
    (unless (buffer-live-p buf)
      (user-error "The source PDF buffer is gone"))
    (pop-to-buffer buf)
    (pdf-view-goto-page page)))
