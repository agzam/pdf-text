;;; modules/pdf/autoload/pdf-text.el -*- lexical-binding: t; -*-

;; Reflowed reading view for PDFs.  Text comes from the already-running
;; epdfinfo (`pdf-info-gettext' per page), the document outline
;; (`pdf-info-outline') becomes foldable org headings; everything below
;; the two entry commands is pure text transformation, testable without
;; a PDF or pdf-tools on the load path.

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
  "Append LINE to PARA, absorbing a wrap hyphen or a drop cap.
A word-attached trailing hyphen means the wrap split mid-word: a
lowercase continuation is the split word's tail (drop the hyphen), any
other continuation is a compound broken at its own hyphen (keep it);
neither wants a space.  A dangling hyphen is ordinary text.  A PARA
that is one capital letter is a drop cap - the oversized initial
extracts as its own line - and rejoins its word without a space."
  (let ((case-fold-search nil))         ; [[:lower:]] must not match S
    (cond
     ((string-match-p "\\`[[:upper:]]\\'" para)
      (concat para line))
     ((not (string-match-p "[[:alnum:]]-\\'" para))
      (concat para " " line))
     ((string-match-p "\\`[[:lower:]]" line)
      (concat (substring para 0 -1) line))
     (t (concat para line)))))

(defvar pdf-text-full-line-fraction 0.7
  "Fraction of the page's widest line at which a line counts as full.
Only a full line was wrapped by the renderer mid-paragraph; a shorter
one ended its paragraph - or its TOC entry - so the next line starts
fresh.")

(defun pdf-text--page-width (lines)
  "Widest trimmed line length in LINES, the page's wrap-column estimate.
Preformatted lines are layout, not prose, and stay out of the
estimate."
  (apply #'max 0 (mapcar (lambda (l) (length (string-trim l)))
                         (cl-remove-if #'pdf-text--preformatted-p lines))))

(defun pdf-text-unfill (text &optional geometry doc-right)
  "Reflow hard-wrapped TEXT into one line per paragraph.
Blank lines separate paragraphs and pass through; preformatted-looking
lines (see `pdf-text--preformatted-p') pass through verbatim.  A line
continues the open paragraph only while the previous line was full or
ended mid-word in a wrap hyphen; anything after a shorter line - a
TOC entry, a heading, a paragraph's natural end - starts fresh.  So
does an indented line (a first-line-indented paragraph, rendered with
a two-space prefix) or one opening with a number and a capitalized
word (a numbered entry or section heading; a lowercase continuation
like \"10 cover it\" still joins).

GEOMETRY, when given, is the page's `pdf-text--page-geometry' table
and DOC-RIGHT the document-wide `pdf-text--doc-right-edge' profile.
Glyph edges then decide.  Full means the line's right edge reaches
the column margin minus the profile's slack - immune to the
char-count inflation that small-type footnote lines cause.  Indented
means the left edge sits `pdf-text-indent-min' to
`pdf-text-indent-max' past the modal margin, restoring the
paragraph-start signal gettext strips from the print - except when
the previous line starts at the same inset (a drop-cap or block-quote
body, not a paragraph start) or when a full line that is itself inset
hangs a deeper continuation under itself (a wrapped list item).  A
lone capital letter with an inset next line is a drop cap and rejoins
its word.  A line dedenting out of a two-line-or-longer inset run
ends that block (a quote, a listing) rather than continuing its
paragraph - except the run a drop cap indents, whose paragraph really
does resume at the margin.  Lines without geometry fall back to
character counts against `pdf-text-full-line-fraction' of the page's
widest line."
  (let* ((lines (split-string text "\n"))
         (full-chars (* pdf-text-full-line-fraction (pdf-text--page-width lines)))
         (geos (and geometry
                    (mapcar (lambda (l) (gethash (string-trim l) geometry)) lines)))
         (col-left (and geos (pdf-text--modal-edge geos #'car)))
         (slack (or (cdr-safe doc-right) pdf-text-full-slack))
         (col-right (let ((page (and geos (pdf-text--modal-edge geos #'cdr))))
                      (if (and page doc-right) (max page (car doc-right))
                        (or page (car-safe doc-right)))))
         (case-fold-search nil)
         out para open prev-geo prev-full (inset-run 0) para-drop-cap)
    (cl-loop
     for line in lines
     for rest = geos then (cdr rest)
     for geo = (car rest)
     do
     (cond
      ((string-blank-p line)
       (when para (push para out) (setq para nil para-drop-cap nil))
       (push "" out))
      ((pdf-text--preformatted-p line)
       (when para (push para out) (setq para nil para-drop-cap nil))
       (push line out))
      (t
       (let* ((trimmed (string-trim line))
              (geo-full (and geo col-right
                             (<= (- col-right slack) (cdr geo))))
              (indented
               (if (and geo col-left)
                   (let ((delta (- (car geo) col-left)))
                     (and (< pdf-text-indent-min delta)
                          (<= delta pdf-text-indent-max)
                          ;; same inset as the previous line: the body of a
                          ;; drop cap or block quote, not a paragraph start
                          (not (and prev-geo
                                    (< (abs (- (car geo) (car prev-geo))) 0.003)))
                          ;; deeper than a full line that is itself inset (a
                          ;; wrapped list item): its hanging continuation.  A
                          ;; full line at the margin is just a paragraph
                          ;; ending flush, no bar to the next one's indent.
                          (not (and prev-geo prev-full
                                    (< (+ col-left 0.003) (car prev-geo))
                                    (< (+ (car prev-geo) 0.003) (car geo))))))
                 (string-match-p "\\`[ \t]" line)))
              (dedent (and geo prev-geo
                           (< (car geo) (- (car prev-geo) 0.003))
                           (<= 2 inset-run)
                           (not para-drop-cap)))
              (drop-cap (and para geo indented
                             (string-match-p "\\`[[:upper:]]\\'" para))))
         (if (and para
                  (or drop-cap
                      (and open
                           (not indented)
                           (not dedent)
                           (not (string-match-p
                                 "\\`[0-9]+\\(?:\\.[0-9]+\\)*\\.? +[[:upper:]]"
                                 line)))))
             (progn
               (setq para (pdf-text--join-lines para trimmed))
               (when drop-cap (setq para-drop-cap t)))
           (when para (push para out))
           (setq para (if (and geo indented)
                          (concat "  " trimmed)
                        (string-trim-right line))
                 para-drop-cap nil))
         (setq inset-run (if (and geo prev-geo
                                  (< (abs (- (car geo) (car prev-geo))) 0.003))
                             (1+ inset-run)
                           1))
         (setq open (or geo-full
                        (<= full-chars (length trimmed))
                        (string-match-p "[[:alnum:]]-\\'" trimmed)
                        ;; a drop cap's lone capital must stay joinable
                        (and geo (string-match-p "\\`[[:upper:]]\\'" trimmed)))
               prev-geo geo
               prev-full geo-full)))))
    (when para (push para out))
    (string-join (nreverse out) "\n")))

(defvar pdf-text-indent-min 0.01
  "Least x-offset past the column edge that reads as a first-line indent.
Relative to page width.  Print indents measure 0.02-0.04; font-metric
jitter (an inline code glyph opening a wrapped line) reaches 0.006,
so the floor sits between the two.")

(defvar pdf-text-indent-max 0.05
  "Largest x-offset past the column edge that still reads as an indent.
Lines starting farther right - centered headings, a second column -
carry no paragraph signal.")

(defun pdf-text--layout-lines (layout)
  "Charlayout LAYOUT split at newline glyphs into per-line glyph lists.
Entries are (CHAR (X0 Y0 X1 Y1)); the newline glyphs poppler emits
mirror the line breaks of the gettext stream."
  (let (lines cur)
    (dolist (e layout)
      (if (eq (car e) ?\n)
          (progn (push (nreverse cur) lines) (setq cur nil))
        (push e cur)))
    (when cur (push (nreverse cur) lines))
    (nreverse lines)))

(defvar pdf-text-full-slack 0.025
  "How far short of the column's right edge a full line may still end.
Relative to page width.  Justified text lands within 0.002 of the
edge on every wrapped line; a paragraph's last line falls short by an
order of magnitude more.")

(defvar pdf-text-full-slack-ragged 0.12
  "The `pdf-text-full-slack' for ragged-right documents.
Unjustified lines stop wherever the next word no longer fits, up to a
long word short of the margin, so calling them full takes an order of
magnitude more slack.")

(defun pdf-text--page-geometry (text layout)
  "Trimmed-line-text -> (X0 . X1) table for TEXT's lines, from LAYOUT.
LAYOUT is `pdf-info-charlayout' output.  Content lookup, not line
position, lets the geometry survive the line-dropping cleanups
between extraction and `pdf-text-unfill'; a layout line that does not
match its gettext line verbatim contributes nothing, so drift fails
open to the character heuristics."
  (when layout
    (let ((table (make-hash-table :test #'equal)))
      (cl-loop for lline in (pdf-text--layout-lines layout)
               for tline in (split-string text "\n")
               when (and lline
                         (equal tline (apply #'string (mapcar #'car lline))))
               do (puthash (string-trim tline)
                           (cons (nth 0 (cadr (car lline)))
                                 (nth 2 (cadr (car (last lline)))))
                           table))
      table)))

(defun pdf-text--modal-edge (geos accessor)
  "Most common ACCESSOR-side edge among GEOS entries; a column margin.
The mode, not an extremum: a stray page number or margin note outside
the column cannot pose as the text edge, on either side."
  (let ((counts (make-hash-table :test #'eql)) (mode nil) (best 0))
    (dolist (g geos)
      (when g
        (let* ((key (round (* 200 (funcall accessor g))))
               (n (1+ (gethash key counts 0))))
          (puthash key n counts)
          (when (< best n) (setq best n mode key)))))
    (and mode (/ mode 200.0))))

(defun pdf-text--doc-right-edge (geometries)
  "Right-margin profile (RIGHT . SLACK) across GEOMETRIES' lines.
RIGHT is the modal right edge - document-wide, because a page
dominated by a code listing or display material has no prevailing
right edge of its own.  SLACK is `pdf-text-full-slack' when the lines
near the margin concentrate tightly on it (justified type), else
`pdf-text-full-slack-ragged': unjustified lines wrap a variable word
short of the margin and need the room."
  (let (all)
    (dolist (table geometries)
      (when table
        (maphash (lambda (_ geo) (push geo all)) table)))
    (when-let* ((right (pdf-text--modal-edge all #'cdr)))
      (let ((near 0) (tight 0))
        (dolist (geo all)
          (let ((short (- right (cdr geo))))
            (when (<= (abs short) 0.15) (cl-incf near))
            (when (<= (abs short) 0.005) (cl-incf tight))))
        (cons right
              (if (and (< 0 near) (<= 0.5 (/ (float tight) near)))
                  pdf-text-full-slack
                pdf-text-full-slack-ragged))))))

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

(defun pdf-text--drop-split-echoes (lines)
  "LINES without runs that only repeat the preceding line in pieces.
The shadow paint's second copy can also split across lines: after
\"PATTERNS OF CONFLICT\" come \"PATTERNS OF\" and \"CONFLICT\".  When
the space-join of the following lines equals the previous line, they
are that echo, not text.  A blank line ends the candidate run."
  (let (out)
    (while lines
      (let* ((line (pop lines))
             (trimmed (string-trim line)))
        (push line out)
        (unless (string-blank-p line)
          (let ((acc "") (rest lines) (n 0) matched)
            (while (and rest
                        (not matched)
                        (not (string-blank-p (car rest)))
                        (< (length acc) (length trimmed)))
              (setq acc (string-trim (concat acc " " (string-trim (car rest))))
                    n (1+ n)
                    rest (cdr rest))
              (when (equal acc trimmed) (setq matched t)))
            (when matched (setq lines (nthcdr n lines)))))))
    (nreverse out)))

(defun pdf-text-join-small-caps (line)
  "LINE with small-caps extraction gaps closed.
A word typeset in small caps reaches gettext as its full-size initial,
a spurious space from the font-size change, then the tail: \"S ECOND
E DITION\".  Word gaps look identical, so joining takes line-level
evidence: no lowercase anywhere (small caps extract as capitals), and
the initial-gap pattern either twice or filling the whole line.  A
lone \"A\" or \"I\" pair stays: those are the two real one-letter
words (\"A DISCOURSE\"), while a heading like \"P REFACE\" cannot be
anything but the artifact."
  (let* ((case-fold-search nil)         ; [A-Z] must not match lowercase
         (trimmed (string-trim line))
         (pairs (let ((n 0) (start 0))
                  (while (string-match "\\b[A-Z] [A-Z]\\{2,\\}\\b" trimmed start)
                    (setq n (1+ n) start (match-end 0)))
                  n)))
    (if (and (not (string-match-p "[a-z]" trimmed))
             (or (<= 2 pairs)
                 (string-match-p "\\`[B-HJ-Z] [A-Z]\\{2,\\}\\'" trimmed)))
        (replace-regexp-in-string "\\b\\([A-Z]\\) \\([A-Z]+\\)\\b" "\\1\\2" line)
      line)))

(defun pdf-text-clean-pages (pages)
  "PAGES with headers stripped, dupes dropped, small-caps gaps closed."
  (mapcar (lambda (page)
            (string-join (mapcar #'pdf-text-join-small-caps
                                 (pdf-text--drop-split-echoes
                                  (pdf-text--dedup-adjacent (split-string page "\n"))))
                         "\n"))
          (pdf-text-remove-recurring-lines pages)))

(defvar pdf-text-org-escape-re
  (rx bos (or (seq (+ "*") " ")
              (seq (* (in " \t")) "#+")
              (seq (* (in " \t")) ":" (+ (in alnum "_@#%-")) ":" (* (in " \t")) eos)))
  "Extracted lines that org would parse as document structure.
Headlines, keyword/block lines, drawer and property lines.")

(defun pdf-text--escape-org-lines (text)
  "TEXT with org-structural lines neutralized by a zero-width space.
The buffer derives from `org-mode' only so the interleaved outline
headings fold; a PDF bullet line starting `* ' must not become a real
headline and corrupt that folding.  The invisible prefix keeps the
line visually identical, and a plain-text search still matches it
whole."
  (string-join
   (mapcar (lambda (line)
             (if (string-match-p pdf-text-org-escape-re line)
                 (concat "\u200B" line)
               line))
           (split-string text "\n"))
   "\n"))

(defun pdf-text-render-pages (pages &optional geometries)
  "Raw PAGES cleaned, unfilled, de-shadowed, and org-escaped.
GEOMETRIES, when given, holds one `pdf-text--page-geometry' table (or
nil) per page for the unfill's glyph-edge decisions.  Split-across-
lines shadow echoes die in `pdf-text-clean-pages'; the doubled-title
collapse after unfill catches the same-line form, which a paragraph
join can also assemble.  Escaping runs last, so the zero-width prefix
cannot skew the doubled check."
  (let ((doc-right (and geometries (pdf-text--doc-right-edge geometries))))
    (cl-loop for page in (pdf-text-clean-pages pages)
             for rest = geometries then (cdr rest)
             collect
             (pdf-text--escape-org-lines
              (string-join (mapcar #'pdf-text--collapse-doubled
                                   (split-string
                                    (pdf-text-unfill page (car rest) doc-right)
                                    "\n"))
                           "\n")))))

(defun pdf-text--interleave-outline (pages outline)
  "PAGES with a heading line per OUTLINE entry at its page's start.
OUTLINE is `pdf-info-outline' output: alists with depth, title, and -
for goto-dest entries - page.  Entries without a usable page (URI
links, unresolved destinations reported as page 0) or without a title
are dropped.  A lone top-level entry is the book's own title, not a
chapter - as a headline it would fold the entire book into one line -
so it renders as a #+TITLE keyword and every deeper entry promotes to
close the gap.  A nil OUTLINE returns PAGES unchanged: PDFs without
an outline degrade to the flat view."
  (let* ((usable (cl-remove-if-not
                  (lambda (entry)
                    (let-alist entry
                      (and (integerp .page) (<= 1 .page)
                           (stringp .title) (not (string-blank-p .title)))))
                  outline))
         (depth-of (lambda (entry) (max 1 (or (alist-get 'depth entry) 1))))
         (min-depth (and usable (apply #'min (mapcar depth-of usable))))
         (root (let ((top (cl-remove-if-not
                           (lambda (e) (eql min-depth (funcall depth-of e)))
                           usable)))
                 (and (eql 1 (length top)) (car top))))
         (heads (make-hash-table)))
    (dolist (entry usable)
      (let-alist entry
        (push (cond
               ((eq entry root) (concat "#+TITLE: " (string-trim .title)))
               (root (concat (make-string (max 1 (- (funcall depth-of entry)
                                                    min-depth))
                                          ?*)
                             " " (string-trim .title)))
               (t (concat (make-string (funcall depth-of entry) ?*)
                          " " (string-trim .title))))
              (gethash .page heads))))
    (let ((n 0))
      (mapcar (lambda (page)
                (cl-incf n)
                (if-let* ((lines (nreverse (gethash n heads))))
                    (concat (string-join lines "\n") "\n" page)
                  page))
              pages))))

(defvar pdf-text-synth-heading-max-fraction 0.6
  "Widest fraction of the page's wrap column a synthesized heading fills.")

(defun pdf-text--synthesize-headings (pages)
  "PAGES with short numbered section lines promoted to org headings.
The fallback for documents carrying no outline metadata: a line like
\"2.2 Arguments\" - a dotted section number, then a capitalized word,
well short of the page's wrap column, with no page number at the end
the way TOC entries have - reads as a section heading, its dot count
as the org level.  Prose and TOC pages pass through untouched."
  (let ((case-fold-search nil))
    (mapcar
     (lambda (page)
       (let* ((lines (split-string page "\n"))
              (limit (* pdf-text-synth-heading-max-fraction
                        (pdf-text--page-width lines))))
         (string-join
          (mapcar
           (lambda (line)
             (let ((trimmed (string-trim line)))
               (if (and (string-match "\\`\\([0-9]+\\(?:\\.[0-9]+\\)*\\)\\.? +[[:upper:]]"
                                      trimmed)
                        (<= (length trimmed) limit)
                        (not (string-match-p "[0-9]\\'" trimmed)))
                   (concat (make-string (1+ (cl-count ?. (match-string 1 trimmed))) ?*)
                           " " trimmed)
                 line)))
           lines)
          "\n")))
     pages)))

(defvar pdf-text-min-text-fraction 0.05
  "Fraction of pages that must carry text before extraction proceeds.
Below it the document is a scan: a page or two of stray text in an
otherwise image-only book does not make a readable view.")

(defun pdf-text--scanned-p (pages)
  "Whether raw PAGES look like a scan: nearly no page carries text."
  (< (cl-count-if-not #'string-blank-p pages)
     (* pdf-text-min-text-fraction (length pages))))

(defvar-local pdf-text--page-starts nil
  "Vector of buffer positions; element N-1 is where page N starts.")

(defvar-local pdf-text--pdf-buffer nil
  "The `pdf-view-mode' buffer this text view mirrors.")

(defvar-local pdf-text--companion nil
  "The companion pdf-text buffer, on the `pdf-view-mode' side.")

(defvar-local pdf-text--source-stamp nil
  "The rendered PDF's `pdf-text--file-stamp'; freshness key for reuse.")

(defvar-local pdf-text--has-outline nil
  "Whether the rendered document carried an outline, so folds exist.")

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

(defun pdf-text--page-end (page)
  "Buffer position where PAGE's text ends, clamped like its start.
For every page but the last that is the newline before the \"\\n\\f\\n\"
delimiter `pdf-text--insert-pages' writes; the last page runs to the
end of the buffer."
  (let* ((starts pdf-text--page-starts)
         (page (min (max 1 page) (length starts))))
    (if (< page (length starts))
        (- (aref starts page) 3)
      (point-max))))

(defun pdf-text--page-position (page fraction)
  "Buffer position FRACTION of the way into PAGE's text.
Character-based interpolation: reflow already re-shapes lines, so a
finer mapping from the image's geometry would be false precision."
  (let* ((start (pdf-text--page-start page))
         (end (pdf-text--page-end page)))
    (min (max start (+ start (round (* fraction (- end start))))) end)))

(defun pdf-text-page-at-point ()
  "Page number at point: one more than the form feeds above it."
  (save-excursion
    (let ((pos (point))
          (n 1))
      (goto-char (point-min))
      (while (search-forward "\f" pos t)
        (setq n (1+ n)))
      n)))

(define-derived-mode pdf-text-mode org-mode "pdf-text"
  "Reflowed reading view of a PDF.
Derives from `org-mode' so the document outline, interleaved as
headings, gives folding, sparse trees, and heading-addressable
positions; the extracted text itself is escaped so none of it reads
as org structure.  Without an outline the buffer is the same flat
text it always was."
  (setq buffer-read-only t)
  (visual-line-mode 1)
  (goto-address-mode 1)
  ;; Render each page-delimiting ^L as a rule instead of a glyph.
  (setq-local buffer-display-table (make-display-table))
  (aset buffer-display-table ?\f
        (vconcat (make-list 64 (make-glyph-code ?─ 'shadow)))))

(defconst pdf-text-render-version 2
  "Version of the rendering pipeline, part of the freshness stamp.
Bumping it stales every companion rendered by older code, so reuse
cannot serve output the current transforms would no longer produce.")

(defun pdf-text--file-stamp (file)
  "FILE's identity - path, mtime, render version - as the freshness key.
Nil (never fresh) without a file or with a vanished one."
  (when-let* ((attrs (and file (file-attributes file))))
    (list file (file-attribute-modification-time attrs)
          pdf-text-render-version)))

(defun pdf-text--view-fraction ()
  "How far down the page the `pdf-view' window's top edge sits, 0..1.
Pixel vscroll over the displayed image's height, the same quantities
`pdf-util-image-displayed-edges' derives its visible top from.  Zero
when the whole page fits the window; approximate under
`pdf-view-roll-minor-mode', where vscroll spans stacked pages."
  (condition-case nil
      (let ((height (cdr (pdf-view-image-size t))))
        (if (< 0 height)
            (min 1.0 (/ (window-vscroll nil t) (float height)))
          0.0))
    (error 0.0)))

;;;###autoload
(defun pdf-view-as-text ()
  "Read the current PDF as reflowed text in a companion buffer.
Lands where the `pdf-view-mode' window is: same page, proportionally
as far into the page's text as the window top sits down the image.
The companion is reused as long as the PDF file on disk is unchanged;
a stale or missing one is re-extracted through epdfinfo.  The PDF
outline becomes org headings; without one, numbered section lines
found in the text stand in.  A document whose pages carry almost no
text - a scan - signals an error instead of an empty buffer."
  (interactive)
  (unless (derived-mode-p 'pdf-view-mode)
    (user-error "Not in a pdf-view buffer"))
  (let* ((pdf-buf (current-buffer))
         (page (pdf-view-current-page))
         (fraction (pdf-text--view-fraction))
         (stamp (pdf-text--file-stamp buffer-file-name))
         (name (format "*pdf-text: %s*" (buffer-name)))
         (buf (get-buffer name)))
    (unless (and buf stamp
                 (equal stamp (buffer-local-value 'pdf-text--source-stamp buf)))
      (let* ((raw (mapcar (lambda (p) (pdf-info-gettext p '(0 0 1 1)))
                          (number-sequence 1 (pdf-info-number-of-pages))))
             (geometries
              (cl-loop for p from 1 for text in raw
                       collect (condition-case nil
                                   (pdf-text--page-geometry
                                    text (pdf-info-charlayout p))
                                 (error nil)))))
        (when (pdf-text--scanned-p raw)
          (user-error "%s has no text layer (%d of %d pages carry text)"
                      (buffer-name)
                      (cl-count-if-not #'string-blank-p raw) (length raw)))
        (let* ((outline (pdf-info-outline))
               (rendered (pdf-text-render-pages raw geometries))
               (pages (if outline
                          (pdf-text--interleave-outline rendered outline)
                        (pdf-text--synthesize-headings rendered))))
          (setq buf (get-buffer-create name))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (pdf-text-mode)
              (pdf-text--insert-pages pages)
              (setq pdf-text--source-stamp stamp
                    pdf-text--has-outline
                    (and (save-excursion
                           (goto-char (point-min))
                           (re-search-forward "^\\*+ " nil t))
                         t))
              (when pdf-text--has-outline (org-cycle-overview)))))))
    (with-current-buffer buf
      (setq pdf-text--pdf-buffer pdf-buf))
    (setq pdf-text--companion buf)
    (pop-to-buffer buf)
    (goto-char (pdf-text--page-position page fraction))
    (when pdf-text--has-outline (org-fold-show-set-visibility 'lineage))
    (beginning-of-visual-line)
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

(defvar pdf-text-sync--inhibit nil
  "Non-nil while one side of the sync moves the other; breaks the loop.")

(defvar-local pdf-text-sync--last-page nil
  "Page the sync last settled on; motions within it stay silent.")

(defun pdf-text-sync--follow-text ()
  "Post-command in the companion: show the page at point in the PDF.
Only a visible PDF window follows; the sync is about reading side by
side, not about flipping pages in a buried buffer."
  (let ((page (pdf-text-page-at-point))
        (pdf pdf-text--pdf-buffer))
    (unless (or pdf-text-sync--inhibit (eql page pdf-text-sync--last-page))
      (setq pdf-text-sync--last-page page)
      (when-let* (((buffer-live-p pdf))
                  (win (get-buffer-window pdf t)))
        (let ((pdf-text-sync--inhibit t))
          (with-selected-window win
            (unless (eql page (pdf-view-current-page))
              (pdf-view-goto-page page))))))))

(defun pdf-text-sync--follow-pdf ()
  "Page-change hook in the PDF buffer: move the companion's point along.
Removes itself once the companion is gone or dropped the mode - the
hook must not outlive its buffer.  A companion already on the page
stays put, so the explicit RET jump keeps its exact position."
  (let ((companion pdf-text--companion)
        (page (pdf-view-current-page)))
    (cond
     ((not (and (buffer-live-p companion)
                (buffer-local-value 'pdf-text-sync-mode companion)))
      (remove-hook 'pdf-view-after-change-page-hook #'pdf-text-sync--follow-pdf t))
     ((not pdf-text-sync--inhibit)
      (let ((pdf-text-sync--inhibit t))
        (with-current-buffer companion
          (setq pdf-text-sync--last-page page)
          (unless (eql page (pdf-text-page-at-point))
            (let ((pos (pdf-text--page-position page 0))
                  (win (get-buffer-window companion t)))
              (if win
                  (with-selected-window win
                    (goto-char pos)
                    (when pdf-text--has-outline
                      (org-fold-show-set-visibility 'lineage))
                    (recenter 0))
                (goto-char pos)
                (when pdf-text--has-outline
                  (org-fold-show-set-visibility 'lineage)))))))))))

(define-minor-mode pdf-text-sync-mode
  "Keep the companion and its PDF on the same page, both directions.
Point moving onto another page here scrolls the `pdf-view' window
there; flipping the PDF's page moves point here.  The explicit jump
on RET works without the mode."
  :lighter " pdf-sync"
  (unless (derived-mode-p 'pdf-text-mode)
    (setq pdf-text-sync-mode nil)
    (user-error "Not in a pdf-text buffer"))
  (let ((companion (current-buffer))
        (pdf pdf-text--pdf-buffer))
    (if pdf-text-sync-mode
        (progn
          (unless (buffer-live-p pdf)
            (setq pdf-text-sync-mode nil)
            (user-error "The source PDF buffer is gone"))
          (setq pdf-text-sync--last-page (pdf-text-page-at-point))
          (add-hook 'post-command-hook #'pdf-text-sync--follow-text nil t)
          (with-current-buffer pdf
            (setq pdf-text--companion companion)
            (add-hook 'pdf-view-after-change-page-hook
                      #'pdf-text-sync--follow-pdf nil t)))
      (remove-hook 'post-command-hook #'pdf-text-sync--follow-text t)
      (when (buffer-live-p pdf)
        (with-current-buffer pdf
          (remove-hook 'pdf-view-after-change-page-hook
                       #'pdf-text-sync--follow-pdf t))))))
