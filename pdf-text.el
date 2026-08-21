;;; modules/pdf/autoload/pdf-text.el -*- lexical-binding: t; -*-

;; Reflowed reading view for PDFs.  Text and glyph geometry both come
;; from the already-running epdfinfo (`pdf-info-charlayout' per page,
;; `pdf-info-gettext' as the fallback for a page without one), and the
;; document outline (`pdf-info-outline') becomes foldable org headings.
;; Everything below the entry commands is pure transformation, testable
;; without a PDF or pdf-tools on the load path.
;;
;; poppler hands out lines, never blocks: it computes paragraphs and
;; columns internally and then discards them while serving text, so the
;; reflow has to rebuild that structure from the glyph boxes - where a
;; line ends, how far the next one starts in, how much air sits between
;; their baselines.

(require 'cl-lib)

;;; Statistics over glyph measurements

(defun pdf-text--quantile (values fraction)
  "Order statistic of VALUES at FRACTION; nil when VALUES is empty."
  (when values
    (let* ((sorted (sort (copy-sequence values) #'<))
           (index (min (1- (length sorted))
                       (floor (* fraction (length sorted))))))
      (nth index sorted))))

(defun pdf-text--variation (values)
  "Coefficient of variation of VALUES; nil below two values."
  (when (< 1 (length values))
    (let ((mean (/ (apply #'+ values) (float (length values)))))
      (when (< 0 mean)
        (/ (sqrt (/ (apply #'+ (mapcar (lambda (v) (expt (- v mean) 2)) values))
                    (float (length values))))
           mean)))))

(defun pdf-text--mode-value (values bucket &optional weights)
  "Most common of VALUES once rounded into BUCKET-wide steps.
The mode, not an extreme: a page number outside the column, a heading
at twice the body size, or a stray wide line cannot move it.  WEIGHTS,
a parallel list, says how much each value counts - by ink width, a
column of page numbers no longer outvotes a column of prose it happens
to match line for line."
  (let ((counts (make-hash-table :test #'eql)) (best 0) mode)
    (cl-loop for v in values
             for w in (or weights (make-list (length values) 1))
             do (let* ((key (round (/ v bucket)))
                       (n (+ w (gethash key counts 0))))
                  (puthash key n counts)
                  (when (< best n) (setq best n mode key))))
    (and mode (* mode bucket))))

;;; Line records

(cl-defstruct (pdf-text-line (:constructor pdf-text-line-create)
                             (:copier nil))
  "One line of a page: its text and the geometry of the glyphs that drew it.
Coordinates are page-relative.  The geometry slots are nil for a line
built without a layout, and every rule that reads one falls back to
character heuristics."
  text
  kind                                  ; nil for prose, `mono' for a listing
  align                                 ; nil, `right' or `center'
  x0 x1                                 ; ink edges, left and right
  top bot                               ; ink extent, highest and lowest
  base                                  ; median glyph bottom: the baseline
  height                                ; glyph height, upper quantile of the ink
  space                                 ; median width of the line's spaces
  cv                                    ; advance variation; ~0 is monospaced
  first-width)                          ; width of the first word

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

(defun pdf-text--glyph-line (glyphs)
  "Line record for GLYPHS, one line of `pdf-info-charlayout' output."
  (let* ((text (apply #'string (mapcar #'car glyphs)))
         (ink (cl-remove-if (lambda (g) (memq (car g) '(?\s ?\t))) glyphs))
         (boxes (mapcar #'cadr ink))
         (spaces (cl-loop for g in glyphs
                          when (eq (car g) ?\s)
                          collect (- (nth 2 (cadr g)) (nth 0 (cadr g)))))
         (advances (cl-loop for (a b) on glyphs while b
                            for step = (- (nth 0 (cadr b)) (nth 0 (cadr a)))
                            when (< 0 step) collect step))
         (opening (cl-position-if (lambda (g) (not (eq (car g) ?\s))) glyphs))
         (gap (and opening (cl-position ?\s glyphs :key #'car :start opening))))
    (if (null boxes)
        (pdf-text-line-create :text text)
      (pdf-text-line-create
       :text text
       :x0 (apply #'min (mapcar (lambda (b) (nth 0 b)) boxes))
       :x1 (apply #'max (mapcar (lambda (b) (nth 2 b)) boxes))
       :top (apply #'min (mapcar (lambda (b) (nth 1 b)) boxes))
       :bot (apply #'max (mapcar (lambda (b) (nth 3 b)) boxes))
       ;; the median glyph bottom is the baseline: descenders and
       ;; superscripts are too few to move it, unlike the extremes
       :base (pdf-text--quantile (mapcar (lambda (b) (nth 3 b)) boxes) 0.5)
       ;; the upper quantile of ink heights tracks the font size, where
       ;; the median would only report the x-height of the line's vowels
       :height (pdf-text--quantile
                (mapcar (lambda (b) (- (nth 3 b) (nth 1 b))) boxes) 0.8)
       :space (pdf-text--quantile spaces 0.5)
       :cv (pdf-text--variation advances)
       :first-width (- (nth 2 (cadr (nth (1- (or gap (length glyphs))) glyphs)))
                       (nth 0 (cadr (nth opening glyphs))))))))

(defun pdf-text--layout-text (layout)
  "The plain text of LAYOUT's glyph stream, newlines included."
  (apply #'string (delq nil (mapcar #'car layout))))

(defun pdf-text--page-lines (text &optional layout)
  "Line records for one page, from LAYOUT when it exists, else from TEXT.
Taking the text from the same glyph stream as its geometry is what
keeps the two aligned; the fallback path reflows on character
heuristics alone."
  (if layout
      (mapcar #'pdf-text--glyph-line (pdf-text--layout-lines layout))
    (mapcar (lambda (line) (pdf-text-line-create :text line))
            (split-string text "\n"))))

(defun pdf-text--profile (pages)
  "Modal body geometry of PAGES, each a list of `pdf-text-line'.
A plist: :height glyph height, :leading baseline step, :left and
:right column edges, :space word gap.  Document-wide, because a page
of listings or a page of table rows has no representative body
geometry of its own."
  (let (heights lefts rights widths spaces leadings)
    (dolist (lines pages)
      (let (prev)
        (dolist (line lines)
          (when (pdf-text-line-x0 line)
            (push (pdf-text-line-height line) heights)
            (push (pdf-text-line-x0 line) lefts)
            (push (pdf-text-line-x1 line) rights)
            (push (- (pdf-text-line-x1 line) (pdf-text-line-x0 line)) widths)
            (when (pdf-text-line-space line)
              (push (pdf-text-line-space line) spaces))
            (when-let* ((base (pdf-text-line-base line))
                        (previous (and prev (pdf-text-line-base prev)))
                        (step (- base previous))
                        ((< 0 step))
                        ((< step 0.1)))
              (push step leadings))
            (setq prev line)))))
    (let ((height (pdf-text--mode-value heights 0.001)))
      (list :height height
            :leading (pdf-text--mode-value leadings 0.002)
            :left (pdf-text--mode-value lefts 0.005 widths)
            :right (pdf-text--mode-value rights 0.005 widths)
            :space (let ((median (pdf-text--quantile spaces 0.5)))
                     (cond ((and median height) (max median (/ height 3.0)))
                           (median)
                           (height (/ height 3.0))))))))

(defvar pdf-text-page-profile-min-lines 8
  "Body lines a page needs before its own column edges are trusted.")

(defun pdf-text--page-profile (lines profile)
  "PROFILE with the column edges this page's own body lines establish.
Mirrored margins put the body column at a different offset on facing
pages, so one document-wide edge is wrong for half the book.  Only
lines set at the body size have a say: running heads, margin notes and
figure captions have margins of their own.  A page too thin to speak
for itself keeps the document's edges."
  (let* ((height (plist-get profile :height))
         (body (cl-remove-if-not
                (lambda (line)
                  (and (pdf-text-line-x0 line)
                       (or (null height)
                           (null (pdf-text-line-height line))
                           (< (abs (- (pdf-text-line-height line) height))
                              (* 0.15 height)))))
                lines)))
    (if (< (length body) pdf-text-page-profile-min-lines)
        profile
      (let ((page (copy-sequence profile))
            (widths (mapcar (lambda (line)
                              (- (pdf-text-line-x1 line) (pdf-text-line-x0 line)))
                            body)))
        (plist-put page :left
                   (pdf-text--mode-value (mapcar #'pdf-text-line-x0 body)
                                         0.005 widths))
        (plist-put page :right
                   (pdf-text--mode-value (mapcar #'pdf-text-line-x1 body)
                                         0.005 widths))
        page))))

;;; Line classification

(defvar pdf-text-monospace-variation 0.05
  "Advance variation below which a line reads as monospaced.
A listing's glyph advances are identical, proportional type varies by
0.2 and more, so the two never meet in the middle.")

(defvar pdf-text-monospace-min-glyphs 8
  "Glyphs a line needs before its advances can call it monospaced.
Proportional fonts set digits to one width, so a page number or a
table cell of figures reads as monospaced until enough letters have
had their say.")

(defun pdf-text--similar-height-p (a b)
  "Whether lines A and B were set in the same size."
  (let ((ha (pdf-text-line-height a))
        (hb (pdf-text-line-height b)))
    (and ha hb (< 0 (max ha hb))
         (< (/ (abs (- ha hb)) (max ha hb)) 0.15))))

(defun pdf-text--mark-monospace (lines)
  "Tag the monospaced LINES, which are listings and must not reflow.
A line too short to judge on its own takes the tag from a neighbour of
the same size: a closing brace alone on its line belongs to the
listing above it."
  (dolist (line lines)
    (when-let* ((cv (pdf-text-line-cv line)))
      (when (and (< cv pdf-text-monospace-variation)
                 (<= pdf-text-monospace-min-glyphs
                     (length (string-trim (pdf-text-line-text line)))))
        (setf (pdf-text-line-kind line) 'mono))))
  (let ((vec (vconcat lines)))
    (dotimes (i (length vec))
      (let ((line (aref vec i)))
        (unless (or (pdf-text-line-kind line)
                    (<= pdf-text-monospace-min-glyphs
                        (length (string-trim (pdf-text-line-text line)))))
          (when (cl-some (lambda (j)
                           (and (<= 0 j) (< j (length vec))
                                (eq 'mono (pdf-text-line-kind (aref vec j)))
                                (pdf-text--similar-height-p line (aref vec j))))
                         (list (1- i) (1+ i)))
            (setf (pdf-text-line-kind line) 'mono))))))
  lines)

(defun pdf-text--aligned-pair (line other profile)
  "How LINE sits against neighbouring OTHER: `right', `center' or nil.
A run whose right edges agree while its left edges move is set flush
right; one whose centres agree while both edges move is centred.
Either way the left edge means nothing about paragraphs.  Justified
prose looks flush right too, so the run must also stay clear of the
column's own right edge."
  (let ((space (or (plist-get profile :space) 0.005))
        (column-right (plist-get profile :right))
        (column-left (plist-get profile :left))
        (leading (or (plist-get profile :leading) 0.02)))
    (when (and (pdf-text-line-x0 other)
               (pdf-text--similar-height-p line other)
               (pdf-text-line-base line) (pdf-text-line-base other)
               (< (abs (- (pdf-text-line-base line) (pdf-text-line-base other)))
                  (* 2 leading))
               column-right
               (< (pdf-text-line-x1 line) (- column-right (* 2 space))))
      (let ((left-step (abs (- (pdf-text-line-x0 line) (pdf-text-line-x0 other))))
            (right-step (abs (- (pdf-text-line-x1 line) (pdf-text-line-x1 other))))
            (centre-step (abs (- (+ (pdf-text-line-x0 line) (pdf-text-line-x1 line))
                                 (+ (pdf-text-line-x0 other) (pdf-text-line-x1 other))))))
        (cond
         ((and (< right-step space) (< space left-step)) 'right)
         ((and (< centre-step (* 2 space))
               (< space left-step)
               (< space right-step)
               column-left
               (< (+ column-left (* 2 space)) (pdf-text-line-x0 line)))
          'center))))))

(defun pdf-text--shares-measure-p (line other align profile)
  "Whether LINE is set to the same ALIGN measure as neighbouring OTHER."
  (let ((space (or (plist-get profile :space) 0.005))
        (leading (or (plist-get profile :leading) 0.02)))
    (and (pdf-text-line-x0 line) (pdf-text-line-x0 other)
         (pdf-text--similar-height-p line other)
         (pdf-text-line-base line) (pdf-text-line-base other)
         (< (abs (- (pdf-text-line-base line) (pdf-text-line-base other)))
            (* 2 leading))
         (pcase align
           ('right (< (abs (- (pdf-text-line-x1 line) (pdf-text-line-x1 other)))
                      space))
           ('center (< (abs (- (+ (pdf-text-line-x0 line) (pdf-text-line-x1 line))
                               (+ (pdf-text-line-x0 other) (pdf-text-line-x1 other))))
                       (* 2 space)))))))

(defun pdf-text--mark-alignment (lines profile)
  "Tag the LINES of right-aligned and centred runs.
A pair of lines establishes the run, then it spreads to the neighbours
sharing its measure: one line of a flush-right note can start where
the line above it did by coincidence, and that is no reason to read it
as prose."
  (let ((vec (vconcat lines)))
    (dotimes (i (length vec))
      (let ((line (aref vec i)))
        (when (pdf-text-line-x0 line)
          (dolist (j (list (1- i) (1+ i)))
            (when (and (<= 0 j) (< j (length vec))
                       (null (pdf-text-line-align line)))
              (setf (pdf-text-line-align line)
                    (pdf-text--aligned-pair line (aref vec j) profile)))))))
    (dolist (step '(1 -1))
      (dotimes (k (length vec))
        (let* ((i (if (< 0 step) k (- (length vec) 1 k)))
               (j (- i step))
               (line (aref vec i)))
          (when (and (<= 0 j) (< j (length vec))
                     (null (pdf-text-line-align line)))
            (let ((align (pdf-text-line-align (aref vec j))))
              (when (and align
                         (pdf-text--shares-measure-p line (aref vec j) align profile))
                (setf (pdf-text-line-align line) align))))))))
  lines)

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

(defconst pdf-text-bullet-re
  "[•‣▪▫◦∙·◆◇○●□■▶▸✓✔➤]"
  "Glyphs that open a list item and mean nothing else.")

(defconst pdf-text-weak-bullet-re
  "[-–—*+]"
  "Glyphs that open a list item only where the line is indented.
A dash or an asterisk at the margin is far more often a footnote
marker or a wrapped clause than a bullet.")

(defconst pdf-text-numeral-re
  "(?[0-9]\\{1,3\\}[.)]\\|(?[ivxIVX]\\{1,5\\})\\|(?[ivx]\\{1,5\\}[.)]\\|([a-zA-Z])"
  "Enumerators that open a list item: 1. 2) (3) iv. (a).")

(defun pdf-text--list-marker (text &optional indented)
  "The list marker TEXT opens with, else nil.
INDENTED reports that the line starts in from the column margin, which
is what tells a bullet dash from a wrapped clause."
  (let ((trimmed (string-trim-left text)))
    (when (string-match (format "\\`\\(%s\\|%s%s\\)[ \t]+[^ \t]"
                                pdf-text-bullet-re
                                (if indented
                                    (format "%s\\|" pdf-text-weak-bullet-re)
                                  "")
                                pdf-text-numeral-re)
                        trimmed)
      (match-string 1 trimmed))))

;;; Titles the outline names

(defconst pdf-text-ligature-alist
  '((?\ﬀ . "ff") (?\ﬁ . "fi") (?\ﬂ . "fl") (?\ﬃ . "ffi") (?\ﬄ . "ffl")
    (?\ﬅ . "st") (?\ﬆ . "st"))
  "Ligature glyphs and the letters they stand for.
A font substitutes them for the letter pairs it sets, so the line
carries the glyph where the outline entry carries the letters.")

(defun pdf-text--normalize-title (text)
  "TEXT reduced to the letters and digits it is made of, downcased.
An outline entry and the line it names agree on the words and on
little else: the line may be set in small caps, spaced out by the
extraction, or carry the ligatures its font substitutes."
  (let ((folded (mapconcat (lambda (char)
                             (or (cdr (assq char pdf-text-ligature-alist))
                                 (char-to-string char)))
                           text "")))
    (downcase (replace-regexp-in-string "[^[:alnum:]]" "" folded))))

(defun pdf-text--heading-title (head)
  "The title the org heading line HEAD carries, without its markers."
  (string-trim (replace-regexp-in-string "\\`\\(?:\\*+\\|#\\+TITLE:\\)[ \t]*" ""
                                         head)))

(defun pdf-text--heading-alist (headings)
  "HEADINGS as an alist from the normalised title to the heading line.
The book's own title renders as a keyword rather than a headline and
names no line of the page, so it stays out."
  (delq nil
        (mapcar (lambda (head)
                  (when (string-prefix-p "*" head)
                    (cons (pdf-text--normalize-title
                           (pdf-text--heading-title head))
                          head)))
                headings)))

;;; Cleanups over line records

(defun pdf-text--normalize-line (line)
  "LINE with digit runs collapsed to #, for header/footer matching.
\"INTRODUCTION │ 7\" and \"INTRODUCTION │ 9\" must count as one form."
  (string-trim (replace-regexp-in-string "[0-9]+" "#" line)))

(defun pdf-text--edge-lines (lines)
  "First and last non-blank line of LINES, once each.
The fallback for a page with no geometry: running heads open and close
the plain text stream."
  (let ((nb (cl-remove-if (lambda (line)
                            (string-blank-p (pdf-text-line-text line)))
                          lines)))
    (cl-remove-duplicates (delq nil (list (car nb) (car (last nb)))))))

(defvar pdf-text-margin-band 0.12
  "Fraction of the page, at top and bottom, where running heads sit.")

(defvar pdf-text-recurring-min-count 3
  "Occurrences in the margin band before a line counts as a running head.")

(defun pdf-text--neighbour-gap (vec index step)
  "Baseline distance from line INDEX of VEC to its neighbour along STEP.
Lines sharing a baseline are one visual line that poppler split at a
wide gap - a page number and its running head - so the scan walks past
them.  Nil at the end of the page."
  (let* ((line (aref vec index))
         (base (pdf-text-line-base line))
         (i (+ index step))
         found)
    (while (and base (<= 0 i) (< i (length vec)) (not found))
      (when-let* ((other (pdf-text-line-base (aref vec i)))
                  ((< 0.0001 (abs (- other base)))))
        (setq found (abs (- other base))))
      (setq i (+ i step)))
    found))

(defun pdf-text--margin-candidates (lines profile)
  "Lines of one page that sit apart in the top or bottom margin band.
Narrow, inside the band, and cut off from the body by more than two
leadings - a footnote block fails the last two tests, which is what
keeps it out of the running-head count."
  (let ((leading (plist-get profile :leading))
        (left (plist-get profile :left))
        (right (plist-get profile :right)))
    (if (not (and leading left right (cl-some #'pdf-text-line-base lines)))
        (pdf-text--edge-lines lines)
      (let ((vec (vconcat lines))
            (width (- right left))
            candidates)
        (dotimes (i (length vec))
          (let* ((line (aref vec i))
                 (base (pdf-text-line-base line))
                 (before (pdf-text--neighbour-gap vec i -1))
                 (after (pdf-text--neighbour-gap vec i 1)))
            (when (and base
                       (or (< base pdf-text-margin-band)
                           (< (- 1.0 pdf-text-margin-band) base))
                       (< (- (pdf-text-line-x1 line) (pdf-text-line-x0 line))
                          (* 0.6 width))
                       (or (null before) (< (* 2 leading) before))
                       (or (null after) (< (* 2 leading) after)))
              (push line candidates))))
        (nreverse candidates)))))

(defun pdf-text--page-marker-p (text)
  "Whether TEXT is a bare page marker: a number, a numeral, or a rule."
  (let ((trimmed (string-trim text)))
    (and (not (string-blank-p trimmed))
         (string-match-p
          "\\`\\(?:[Pp]age[ \t]*\\)?\\(?:[0-9]+\\|[ivxlcdmIVXLCDM]+\\|[|·—–-]+\\)\\'"
          trimmed))))

(defun pdf-text-remove-marginal-lines (pages profiles &optional headings)
  "PAGES without running heads, footers, and page numbers.
PROFILES holds each page's own layout profile.
A margin line goes when its digit-normalised form recurs across pages,
when it is nothing but a page marker, or when it shares a baseline
with one - the folio and the running head are set as one line, which
poppler splits at the gap between them.  Recurring forms are then
dropped wherever they appear: a two-up scan embeds whole book pages,
running heads included, in the middle of the text.

HEADINGS carries one entry per page: the heading lines the outline
puts on it.  A book that runs its section title in the page head
makes that title a recurring form, and the section's own heading line
is then dropped along with the head - so a line down in the body that
the outline names is spared.  The head itself sits in the margin band
and goes as it should."
  (let ((counts (make-hash-table :test #'equal))
        (tolerance (* 0.5 (or (plist-get (car profiles) :leading) 0.01)))
        (candidates (cl-loop for lines in pages
                             for profile in profiles
                             collect (pdf-text--margin-candidates lines profile)))
        recurring)
    (dolist (lines candidates)
      (dolist (line lines)
        (when (and line (not (string-blank-p (pdf-text-line-text line))))
          (cl-incf (gethash (pdf-text--normalize-line (pdf-text-line-text line))
                            counts 0)))))
    (maphash (lambda (form n)
               (when (<= pdf-text-recurring-min-count n) (push form recurring)))
             counts)
    (cl-loop
     for lines in pages
     for marginal in candidates
     for heads = headings then (cdr heads)
     collect
     (let ((folios (delq nil
                         (mapcar (lambda (line)
                                   (and line
                                        (pdf-text--page-marker-p (pdf-text-line-text line))
                                        (pdf-text-line-base line)))
                                 marginal)))
           (titles (mapcar #'car (pdf-text--heading-alist (car heads)))))
       (cl-remove-if
        (lambda (line)
          (let* ((text (pdf-text-line-text line))
                 (base (pdf-text-line-base line))
                 (in-margin (memq line marginal))
                 (named (and (not in-margin)
                             (member (pdf-text--normalize-title text) titles))))
            (and (not (string-blank-p text))
                 (not named)
                 (or (member (pdf-text--normalize-line text) recurring)
                     (and in-margin
                          (or (pdf-text--page-marker-p text)
                              (and base
                                   (cl-some (lambda (folio)
                                              (< (abs (- base folio)) tolerance))
                                            folios))))))))
        lines)))))

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
  "LINES with runs of identical non-blank neighbours collapsed to one.
The shadow-draw artifact: the second paint lands a point lower, so
gettext emits the same title on two adjacent lines."
  (let (out)
    (dolist (line lines (nreverse out))
      (unless (and out
                   (not (string-blank-p (pdf-text-line-text line)))
                   (equal (string-trim (pdf-text-line-text line))
                          (string-trim (pdf-text-line-text (car out)))))
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
             (trimmed (string-trim (pdf-text-line-text line))))
        (push line out)
        (unless (string-blank-p trimmed)
          (let ((acc "") (rest lines) (n 0) matched)
            (while (and rest
                        (not matched)
                        (not (string-blank-p (pdf-text-line-text (car rest))))
                        (< (length acc) (length trimmed)))
              (setq acc (string-trim
                         (concat acc " " (string-trim (pdf-text-line-text (car rest)))))
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
  "PAGES of line records with paint artifacts and small-caps gaps gone."
  (mapcar (lambda (lines)
            (let ((kept (pdf-text--drop-split-echoes
                         (pdf-text--dedup-adjacent lines))))
              (dolist (line kept kept)
                (setf (pdf-text-line-text line)
                      (pdf-text-join-small-caps (pdf-text-line-text line))))))
          pages))

;;; Joining lines into blocks

(defconst pdf-text-wrap-hyphen-re "[[:alnum:]][-\u2010\u2011\u00AD]\\'"
  "A word-attached hyphen at a line's end: the renderer split a word.
Books use the ASCII hyphen, the typographic one, and the soft hyphen
interchangeably for this.")

(defconst pdf-text-closed-dash-re "[[:alnum:]][\u2013\u2014]\\'"
  "An en or em dash at a line's end.
English typography sets both closed up against their neighbours, so
the wrap put no space there and the join must not add one - but the
dash itself is text and stays.")

(defun pdf-text--wrap-hyphen-p (text)
  "Whether TEXT ends in a hyphen that a line wrap put there."
  (string-match-p pdf-text-wrap-hyphen-re text))

(defvar pdf-text-extra-vocabulary nil
  "Hyphenated words known from outside the pages being rendered.
The document decides whether a wrap hyphen closes up or stays, and a
window of pages is not the document: a book can hyphenate
\"well-known\" once in chapter one and wrap it in chapter nine.  A
corpus case carries the compounds its own pages cannot show.")

(defun pdf-text--hyphenated-words (pages)
  "Words PAGES writes with an internal hyphen, downcased, as a set.
A wrap hyphen is ambiguous - \"well-\" plus \"known\" is a compound,
\"informa-\" plus \"tion\" is one split word - and the document itself
settles it: a compound it hyphenates elsewhere keeps its hyphen here."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (word pdf-text-extra-vocabulary)
      (puthash (downcase word) t table))
    (dolist (lines pages table)
      (dolist (line lines)
        (dolist (word (split-string (pdf-text-line-text line) "[ \t]+" t))
          (when (string-match "[[:alnum:]]-[[:alnum:]]" word)
            (puthash (downcase (string-trim word "[^[:alnum:]]+" "[^[:alnum:]]+"))
                     t table)))))))

(defun pdf-text--join-lines (para line &optional vocabulary)
  "Append LINE to PARA, absorbing a wrap hyphen or a drop cap.
A word-attached trailing hyphen means the wrap split mid-word: a
lowercase continuation is the split word's tail, so the hyphen goes -
unless VOCABULARY, the document's own hyphenated words, shows the two
halves belong to a compound.  Any other continuation is a compound
broken at its own hyphen and keeps it; neither wants a space.  An en
or em dash closes up the same way but survives, being text rather
than a wrap artifact.  A dangling hyphen is ordinary text.  A PARA
that is one capital letter
is a drop cap - the oversized initial extracts as its own line - and
rejoins its word without a space."
  (let ((case-fold-search nil))         ; [[:lower:]] must not match S
    (cond
     ((string-match-p "\\`[[:upper:]]\\'" para)
      (concat para line))
     ((string-match-p pdf-text-closed-dash-re para)
      (concat para line))
     ((not (pdf-text--wrap-hyphen-p para))
      (concat para " " line))
     ((not (string-match-p "\\`[[:lower:]]" line))
      (concat para line))
     ((and vocabulary
           (gethash (downcase
                     (concat (car (last (split-string (substring para 0 -1) "[ \t]+" t)))
                             "-"
                             (string-trim (car (split-string line "[ \t]+" t))
                                          "" "[^[:alnum:]]+")))
                    vocabulary))
      (concat (substring para 0 -1) "-" line))
     (t (concat (substring para 0 -1) line)))))

;;; Blocks

(cl-defstruct (pdf-text-block (:constructor pdf-text-block-create)
                              (:copier nil))
  "A run of lines that belong together: a paragraph, a list item, a listing."
  kind                                  ; para, item, mono, fixed or blank
  reason                                ; why the block before it ended
  lines                                 ; in reverse while it grows
  left body-x marker drop-cap)

(defvar pdf-text-gap-factor 1.35
  "Baseline step, in modal leadings, that opens a new block.
Renderers pad between paragraphs and around headings; within a
paragraph the step is the leading itself.")

(defvar pdf-text-blank-factor 1.25
  "Baseline step, in modal leadings, that renders as a blank line.
Below it the page put no air between the two blocks either, so a table
of contents or a stanza stays tight.")

(defvar pdf-text-size-tolerance 0.2
  "Relative glyph-height change that separates two blocks.")

(defvar pdf-text-full-line-fraction 0.7
  "Fraction of the page's widest line at which a line counts as full.
The fallback for a page with no geometry, where glyph edges cannot say
whether the renderer wrapped the line or the paragraph ended.")

(defun pdf-text--page-width (lines)
  "Widest trimmed line length in LINES, the page's wrap-column estimate.
Preformatted lines are layout, not prose, and stay out of the
estimate."
  (apply #'max 0
         (mapcar (lambda (line) (length (string-trim (pdf-text-line-text line))))
                 (cl-remove-if (lambda (line)
                                 (pdf-text--preformatted-p (pdf-text-line-text line)))
                               lines))))

(defun pdf-text--mono-p (line)
  "Whether LINE was set in a monospaced face."
  (eq 'mono (pdf-text-line-kind line)))

(defun pdf-text--drop-cap-p (line profile)
  "Whether LINE is an oversized initial standing alone on its line."
  (let ((case-fold-search nil)
        (height (pdf-text-line-height line))
        (body (plist-get profile :height)))
    (and (string-match-p "\\`[[:upper:]]\\'" (string-trim (pdf-text-line-text line)))
         (or (null height) (null body) (< (* 1.4 body) height)))))

(defun pdf-text--gap-break-p (line prev profile)
  "Whether the step from PREV's baseline to LINE's opens a new block."
  (when-let* ((leading (plist-get profile :leading))
              (base (pdf-text-line-base line))
              (previous (pdf-text-line-base prev))
              ((< 0 leading)))
    (let ((step (- base previous)))
      (or (< step (- (* 0.5 leading)))  ; back up the page: another column
          (< (* pdf-text-gap-factor leading) step)))))

(defun pdf-text--size-break-p (line prev profile)
  "Whether LINE and PREV differ enough in glyph size to be separate blocks."
  (when-let* ((body (plist-get profile :height))
              (this (pdf-text-line-height line))
              (that (pdf-text-line-height prev))
              ((< 0 body)))
    (< pdf-text-size-tolerance (/ (abs (- this that)) body))))

(defun pdf-text--supports-break-p (prev line)
  "Whether the words meeting at PREV and LINE read as a break in the text.
Sentence punctuation closing one line and a capital, a digit or an
opening quote starting the next is what a paragraph boundary looks
like in words.  Flush-right and centred runs need the confirmation,
because every line of theirs falls short of a margin without ending
anything."
  (let ((case-fold-search nil))
    (and (string-match-p "[.!?:;)”’\"']\\'" (string-trim (pdf-text-line-text prev)))
         (string-match-p "\\`[[:upper:][:digit:]“\"'(]"
                         (string-trim (pdf-text-line-text line))))))

(defun pdf-text--ends-short-p (line prev right profile page-width)
  "Whether PREV ended its block instead of wrapping into LINE.
The renderer breaks a line when the next word no longer fits, so a
line that left room for LINE's first word ended for a reason of its
own: the paragraph, the list item or the entry stopped there.  RIGHT
is the margin they wrap against.  PAGE-WIDTH carries the
character-count fallback for a page with no geometry."
  (let ((space (or (plist-get profile :space) 0))
        (x1 (pdf-text-line-x1 prev))
        (width (pdf-text-line-first-width line)))
    (if (and right x1 width)
        (< (+ x1 space width) right)
      (< (length (string-trim (pdf-text-line-text prev)))
         (* pdf-text-full-line-fraction page-width)))))

(defun pdf-text--indent-break-p (line block profile)
  "Whether LINE starts in from its block's body margin: a new paragraph.
Silent over a right-aligned or centred run, where the left edge moves
for reasons of typesetting."
  (let ((space (or (plist-get profile :space) 0))
        (body (pdf-text-block-body-x block))
        (x0 (pdf-text-line-x0 line)))
    (cond
     ((pdf-text-line-align line) nil)
     ((and body x0) (< (+ body space) x0))
     (t (and (null x0) (string-match-p "\\`[ \t]" (pdf-text-line-text line)))))))

(defun pdf-text--dedent-break-p (line block profile)
  "Whether LINE falls back left of its block's body margin, ending it.
A quotation, a listing or a list item runs inset; prose resuming at
the column margin is no longer part of it.  A drop cap is the
exception: it holds the first lines of its own paragraph inset, and
the paragraph really does go on once the text clears the initial."
  (let* ((space (or (plist-get profile :space) 0))
         (column (plist-get profile :left))
         (body (pdf-text-block-body-x block))
         (x0 (pdf-text-line-x0 line)))
    (and body x0 (< x0 (- body space))
         (null (pdf-text-line-align line))
         (not (and (pdf-text-block-drop-cap block)
                   column
                   (< (abs (- x0 column)) (* 2 space)))))))

(defun pdf-text--item-break-p (line prev block profile page-width)
  "Whether LINE opens a list item.
The marker alone is not enough - a wrapped clause can start with a
dash - so the line must also stand apart from its predecessor: a step
in from the margin, extra air above, a predecessor that ended short,
or a list already running."
  (let* ((space (or (plist-get profile :space) 0))
         (x0 (pdf-text-line-x0 line))
         (left (plist-get profile :left))
         (indented (and x0 left (< (+ left space) x0))))
    (and (pdf-text--list-marker (pdf-text-line-text line) (or indented (null x0)))
         (or (eq 'item (pdf-text-block-kind block))
             (and x0 (pdf-text-line-x0 prev)
                  (< space (abs (- x0 (pdf-text-line-x0 prev)))))
             (pdf-text--gap-break-p line prev profile)
             (pdf-text--ends-short-p line prev (plist-get profile :right)
                                     profile page-width)))))

(defun pdf-text--break-reason (line block profile page-width)
  "Why LINE cannot continue BLOCK, or nil when it can.
Ordered by strength of evidence: a wrap hyphen or a drop cap forces
the join, a change of face or a list marker forces the break, and the
geometry decides the rest."
  (let ((prev (car (pdf-text-block-lines block)))
        (kind (pdf-text-block-kind block)))
    (cond
     ((string-blank-p (pdf-text-line-text line)) 'blank)
     ((eq 'blank kind) 'text)
     ((not (eq (pdf-text--mono-p line) (eq 'mono kind))) 'face)
     ((eq 'mono kind) (and (pdf-text--gap-break-p line prev profile) 'gap))
     ((eq 'fixed kind)
      (if (pdf-text--preformatted-p (pdf-text-line-text line))
          (and (pdf-text--gap-break-p line prev profile) 'gap)
        'fixed))
     ((pdf-text--preformatted-p (pdf-text-line-text line)) 'fixed)
     ((pdf-text--drop-cap-p prev profile) nil)
     ((pdf-text--wrap-hyphen-p (pdf-text-line-text prev)) nil)
     ((pdf-text--item-break-p line prev block profile page-width) 'item)
     ((pdf-text--gap-break-p line prev profile) 'gap)
     ((pdf-text--size-break-p line prev profile) 'size)
     ((pdf-text--indent-break-p line block profile) 'indent)
     ((pdf-text--dedent-break-p line block profile) 'dedent))))

(defun pdf-text--block-margin (block profile)
  "Right margin BLOCK's lines wrap against.
A block starting at the column margin wraps against the column's own
right edge.  One starting elsewhere - a margin note, a pull quote, an
indented list item - wraps against its own widest line instead;
measured against the column's, every line of it would read as a
paragraph end.  Knowing this takes the whole block, which is why the
short-line rule runs over grouped blocks rather than line by line."
  (let* ((edges (delq nil (mapcar #'pdf-text-line-x1 (pdf-text-block-lines block))))
         (widest (and edges (apply #'max edges)))
         (left (pdf-text-block-left block))
         (column-left (plist-get profile :left))
         (column-right (plist-get profile :right))
         (space (or (plist-get profile :space) 0)))
    (cond ((null widest) column-right)
          ((null column-right) widest)
          ((or (null left) (null column-left)) column-right)
          ((< (abs (- left column-left)) (* 2 space)) column-right)
          (t widest))))

(defun pdf-text--start-block (line reason profile)
  "A block opened by LINE, which broke off the one before it for REASON."
  (let* ((text (pdf-text-line-text line))
         (space (or (plist-get profile :space) 0))
         (left (pdf-text-line-x0 line))
         (column-left (plist-get profile :left))
         (indented (or (null left)
                       (null column-left)
                       (< (+ column-left space) left)))
         (marker (pdf-text--list-marker text indented))
         (kind (cond ((string-blank-p text) 'blank)
                     ((pdf-text--mono-p line) 'mono)
                     ((pdf-text--preformatted-p text) 'fixed)
                     (marker 'item)
                     (t 'para))))
    (pdf-text-block-create
     :kind kind
     :reason reason
     :lines (list line)
     :left left
     ;; every block, list item included, finds its body margin on its
     ;; second line: an item's continuation hangs under the marker in
     ;; one book and returns to the column margin in the next
     :marker marker
     :drop-cap (pdf-text--drop-cap-p line profile))))

(defun pdf-text--extend-block (block line)
  "Add LINE to BLOCK, tracking the margins its later lines establish."
  (push line (pdf-text-block-lines block))
  (when-let* ((x0 (pdf-text-line-x0 line)))
    (unless (pdf-text-block-body-x block)
      (setf (pdf-text-block-body-x block) x0))
    (when (or (null (pdf-text-block-left block))
              (< x0 (pdf-text-block-left block)))
      (setf (pdf-text-block-left block) x0)))
  block)

(defun pdf-text--group-lines (lines profile page-width)
  "LINES of one page grouped into blocks by every rule but the short line."
  (let (blocks current)
    (dolist (line lines)
      (let ((reason (and current
                         (pdf-text--break-reason line current profile page-width))))
        (if (and current (not reason))
            (pdf-text--extend-block current line)
          (when current
            (setf (pdf-text-block-lines current)
                  (nreverse (pdf-text-block-lines current)))
            (push current blocks))
          (setq current (pdf-text--start-block line (or reason 'start) profile)))))
    (when current
      (setf (pdf-text-block-lines current)
            (nreverse (pdf-text-block-lines current)))
      (push current blocks))
    (nreverse blocks)))

(defun pdf-text--split-short-lines (block profile page-width)
  "BLOCK split wherever one of its lines ended short of its own margin.
A short line is the end of a paragraph, an entry or an item; the rule
needs the block's measure, so it runs once the grouping settled it."
  (if (or (memq (pdf-text-block-kind block) '(mono fixed blank))
          (< (length (pdf-text-block-lines block)) 2))
      (list block)
    (let* ((right (pdf-text--block-margin block profile))
           ;; lines set flush right or centred fall short of any margin
           ;; by design, so their geometry alone cannot end a paragraph
           (aligned (cl-some #'pdf-text-line-align (pdf-text-block-lines block)))
           (lines (pdf-text-block-lines block))
           parts current)
      (dolist (line lines)
        (if (null current)
            (setq current (pdf-text--start-block
                           line (pdf-text-block-reason block) profile))
          (let ((prev (car (pdf-text-block-lines current))))
            (if (and (not (pdf-text--wrap-hyphen-p (pdf-text-line-text prev)))
                     (not (pdf-text--drop-cap-p prev profile))
                     (pdf-text--ends-short-p line prev right profile page-width)
                     (or (not aligned) (pdf-text--supports-break-p prev line)))
                (progn
                  (setf (pdf-text-block-lines current)
                        (nreverse (pdf-text-block-lines current)))
                  (push current parts)
                  (setq current (pdf-text--start-block line 'short profile)))
              (pdf-text--extend-block current line)))))
      (when current
        (setf (pdf-text-block-lines current)
              (nreverse (pdf-text-block-lines current)))
        (push current parts))
      (nreverse parts))))

(defun pdf-text--blocks (lines profile)
  "LINES of one page grouped into `pdf-text-block' records."
  (let ((page-width (pdf-text--page-width lines))
        (marked (pdf-text--mark-alignment (pdf-text--mark-monospace lines) profile)))
    (cl-mapcan (lambda (block)
                 (pdf-text--split-short-lines block profile page-width))
               (pdf-text--group-lines marked profile page-width))))

;;; Rendering blocks back to text

(defun pdf-text--join-block (block vocabulary)
  "BLOCK's lines joined into one paragraph line."
  (let (para)
    (dolist (line (pdf-text-block-lines block) (or para ""))
      (let ((text (string-trim (pdf-text-line-text line))))
        (setq para (if para
                       (pdf-text--join-lines para text vocabulary)
                     text))))))

(defun pdf-text--render-item (block vocabulary indent)
  "BLOCK rendered as an org list item at INDENT columns.
A bullet glyph becomes org's own dash, which costs nothing to read and
buys real list structure; an enumerator is already org syntax and
stays as the document wrote it."
  (let* ((text (pdf-text--join-block block vocabulary))
         (marker (pdf-text-block-marker block))
         (pad (make-string indent ?\s)))
    (if (and marker (string-match-p (concat "\\`" pdf-text-bullet-re) marker))
        (concat pad "- " (string-trim (substring text (length marker))))
      (concat pad text))))

(defun pdf-text--render-mono (block profile left)
  "BLOCK's listing lines, verbatim, with their own indentation restored.
LEFT is the margin of the listing this block belongs to, which spans
every block the vertical gaps inside a listing split it into - measure
each block against itself and the second half of a listing loses its
nesting."
  (let* ((lines (pdf-text-block-lines block))
         (unit (or (car (delq nil (mapcar #'pdf-text-line-space lines)))
                   (plist-get profile :space)
                   0.005)))
    (mapconcat (lambda (line)
                 (let ((step (if (and left (pdf-text-line-x0 line))
                                 (round (/ (- (pdf-text-line-x0 line) left) unit))
                               0)))
                   (concat "  " (make-string (max 0 step) ?\s)
                           (string-trim (pdf-text-line-text line)))))
               lines "\n")))

(defun pdf-text--inset-p (block profile)
  "Whether BLOCK is a passage set in from the column margin, as a quotation is.
One line in from the margin is a centred heading or an attribution and
reads better flush; the inset only means something over a passage."
  (let ((left (pdf-text-block-left block))
        (column (plist-get profile :left))
        (space (or (plist-get profile :space) 0)))
    (and left column
         (< 1 (length (pdf-text-block-lines block)))
         (< (+ column (* 2 space)) left))))

(defun pdf-text--block-height (block)
  "The glyph height BLOCK is set at, or nil where no line measured one."
  (when-let* ((heights (delq nil (mapcar #'pdf-text-line-height
                                         (pdf-text-block-lines block)))))
    (apply #'max heights)))

(defun pdf-text--sidebar-title-p (block next profile)
  "Whether BLOCK is the title of the boxed passage NEXT opens.
A sidebar is set in from the column and in smaller type than the body,
and its first line is its title.  On its own that line reads as a
centred heading and prints flush, which loses the box: the title
starts where the box starts and is set as the box is set, so it is
part of it."
  (let ((space (or (plist-get profile :space) 0))
        (column (plist-get profile :left))
        (body (plist-get profile :height))
        (left (pdf-text-block-left block))
        (other (pdf-text-block-left next))
        (height (pdf-text--block-height block))
        (box (pdf-text--block-height next)))
    (and left other column body height box
         (< (+ column (* 2 space)) left)
         (< (abs (- left other)) space)
         (< height body)
         (< box body)
         (pdf-text--inset-p next profile))))

(defun pdf-text--inset-blocks (blocks profile)
  "The blocks of BLOCKS that render set in from the column margin.
A passage of more than one line that runs inset is a quotation or the
body of a boxed sidebar; the sidebar's title line joins it, so the box
reads as the one unit the page sets."
  (let (out)
    (cl-loop for (block next) on blocks
             do (when (or (pdf-text--inset-p block profile)
                          (and next (pdf-text--sidebar-title-p block next profile)))
                  (push block out)))
    (nreverse out)))

(defun pdf-text--item-indent (block stack profile)
  "Indent columns for item BLOCK, and the nesting STACK it leaves behind.
Deeper markers nest, a marker back at an earlier column closes the
levels it left."
  (let ((left (pdf-text-block-left block))
        (space (or (plist-get profile :space) 0)))
    (if (null left)
        (cons 0 stack)
      (while (and stack (< left (- (car stack) space)))
        (pop stack))
      (when (or (null stack) (< (+ (car stack) space) left))
        (push left stack))
      (cons (* 2 (1- (length stack))) stack))))

(defun pdf-text--blank-between-p (block previous profile)
  "Whether a blank line belongs between PREVIOUS and BLOCK.
The page itself decides: blocks the renderer set apart get one, blocks
it stacked at the plain leading - a table of contents, a stanza, a
tight list - stay together."
  (let ((leading (plist-get profile :leading))
        (last (car (last (pdf-text-block-lines previous))))
        (first (car (pdf-text-block-lines block))))
    (cond
     ((memq (pdf-text-block-reason block) '(indent dedent size face fixed gap)) t)
     ((not (and leading last first
                (pdf-text-line-base last) (pdf-text-line-base first)))
      t)
     (t (< (* pdf-text-blank-factor leading)
           (- (pdf-text-line-base first) (pdf-text-line-base last)))))))

(defvar pdf-text-heading-height 1.15
  "Glyph height, in body heights, at which a line reads as a heading.")

(defvar pdf-text-heading-max-words 14
  "Words a block may carry and still read as a heading rather than prose.")

(defun pdf-text--heading-block-p (block profile text)
  "Whether BLOCK, joined up as TEXT, reads as a heading by how it is set.
Bigger type than the body, few words, nothing closing the line: what a
section title looks like on a page that spells it differently from the
outline - a display face the extraction reads letter by letter, a
title the page carries with its chapter number."
  (let ((body (plist-get profile :height))
        (heights (delq nil (mapcar #'pdf-text-line-height
                                   (pdf-text-block-lines block)))))
    (and body heights
         (< (* pdf-text-heading-height body) (apply #'max heights))
         (<= (length (split-string text)) pdf-text-heading-max-words)
         (not (string-match-p "[.,;:]\\'" (string-trim text))))))

(defun pdf-text--assign-headings (blocks profile vocabulary headings)
  "Where each of HEADINGS goes among BLOCKS, as an alist.
Every entry is (BLOCK HEAD REPLACE).  A title names a line, and the
block whose words are that title is where its section starts: the
heading is that line, so it replaces it.  A title no block spells out
falls back on the way the page is set - the blocks that read as
headings take what is left over, both in the order they run down the
page - and there the heading goes above the block, because the words
differ and the page's own are not the reflow's to drop.  A title that
finds no block at all stays for `pdf-text--interleave-outline', which
has only the page's start left to put it at."
  (when-let* ((pending (pdf-text--heading-alist headings)))
    (let ((texts (mapcar (lambda (block)
                           (and (memq (pdf-text-block-kind block) '(para item))
                                (pdf-text--join-block block vocabulary)))
                         blocks))
          assigned)
      (cl-loop for block in blocks
               for text in texts
               do (when-let* ((text)
                              (found (assoc (pdf-text--normalize-title text)
                                            pending)))
                    (push (list block (cdr found) t) assigned)
                    (setq pending (delq found pending))))
      (cl-loop for block in blocks
               for text in texts
               while pending
               do (when (and text
                             (not (assq block assigned))
                             (pdf-text--heading-block-p block profile text))
                    (push (list block (cdr (pop pending))) assigned)))
      assigned)))

(defun pdf-text--render-blocks (blocks profile vocabulary &optional headings)
  "BLOCKS as the page's reflowed text.
HEADINGS are the org heading lines the outline puts on this page.
`pdf-text--assign-headings' says which block each one belongs at: a
section starts where the page starts it, not where the page it sits
on does."
  (let ((placed (pdf-text--assign-headings blocks profile vocabulary headings))
        (inset (pdf-text--inset-blocks blocks profile))
        out previous stack listing-left)
    (dolist (block blocks)
      (let* ((kind (pdf-text-block-kind block))
             (placement (cdr (assq block placed)))
             (head (car placement))
             indent)
        (unless (eq 'item kind) (setq stack nil))
        (when (eq 'item kind)
          (let ((nesting (pdf-text--item-indent block stack profile)))
            (setq indent (car nesting) stack (cdr nesting))))
        (if (eq 'mono kind)
            (unless (eq 'mono (and previous (pdf-text-block-kind previous)))
              (setq listing-left (pdf-text-block-left block)))
          (setq listing-left nil))
        (unless (eq 'blank kind)
          (when (and previous (pdf-text--blank-between-p block previous profile))
            (push "" out))
          (when (and head (not (cadr placement)))
            (push head out)
            (push "" out))
          (push (or (and (cadr placement) head)
                    (pcase kind
                      ('mono (pdf-text--render-mono block profile listing-left))
                      ('fixed (mapconcat (lambda (line)
                                           (string-trim-right (pdf-text-line-text line)))
                                         (pdf-text-block-lines block) "\n"))
                      ('item (pdf-text--render-item block vocabulary indent))
                      (_ (let ((text (pdf-text--collapse-doubled
                                      (pdf-text--join-block block vocabulary))))
                           (if (memq block inset)
                               (concat "  " text)
                             text)))))
                out)
          (setq previous block))))
    (string-join (nreverse out) "\n")))

;;; Org structure

(defvar pdf-text-org-escape-re
  (rx bos (or (seq (+ "*") " ")
              (seq (* (in " \t")) "#+")
              (seq (* (in " \t")) ":" (+ (in alnum "_@#%-")) ":" (* (in " \t")) eos)))
  "Extracted lines that org would parse as document structure.
Headlines, keyword/block lines, drawer and property lines.")

(defun pdf-text--escape-org-lines (text &optional headings)
  "TEXT with org-structural lines neutralized by a zero-width space.
The buffer derives from `org-mode' only so the interleaved outline
headings fold; a PDF bullet line starting `* ' must not become a real
headline and corrupt that folding.  The invisible prefix keeps the
line visually identical, and a plain-text search still matches it
whole.  HEADINGS are the heading lines the render placed itself: they
are the structure the folding is for, and stay as they are."
  (string-join
   (mapcar (lambda (line)
             (if (and (string-match-p pdf-text-org-escape-re line)
                      (not (member line headings)))
                 (concat "\u200B" line)
               line))
           (split-string text "\n"))
   "\n"))

(defun pdf-text-render-lines (pages &optional headings)
  "PAGES of `pdf-text-line' records reflowed into readable text.
One string per page.  Body geometry, running heads and the
hyphenation vocabulary are all read across the whole of PAGES, so a
page never renders on its own: the same page reads differently
depending on what it arrives with.

HEADINGS carries one entry per page of PAGES - the org heading lines
the outline puts on it, from `pdf-text-page-headings'.  A page gets
its headings at the lines they name; only the caller knows which page
of the book each entry of PAGES is, which is why they arrive already
lined up."
  (let* ((page-lines (pdf-text-clean-pages pages))
         (profile (pdf-text--profile page-lines))
         (profiles (mapcar (lambda (lines) (pdf-text--page-profile lines profile))
                           page-lines))
         (page-lines (pdf-text-remove-marginal-lines page-lines profiles headings))
         (vocabulary (pdf-text--hyphenated-words page-lines)))
    (cl-loop for lines in page-lines
             for page-profile in profiles
             for heads = headings then (cdr heads)
             collect (pdf-text--escape-org-lines
                      (pdf-text--render-blocks (pdf-text--blocks lines page-profile)
                                               page-profile vocabulary (car heads))
                      (car heads)))))

(defun pdf-text-render-pages (pages &optional layouts headings)
  "Raw PAGES reflowed into readable text, one string per page.
PAGES are `pdf-info-gettext' strings and LAYOUTS the matching
`pdf-info-charlayout' output.  The layout is what carries paragraph
structure - indents, line fullness, the air between baselines - so a
page without one falls back to character heuristics.  HEADINGS is
what `pdf-text-render-lines' takes."
  (pdf-text-render-lines
   (cl-loop for text in pages
            for rest = layouts then (cdr rest)
            collect (pdf-text--page-lines text (car rest)))
   headings))

(defun pdf-text--outline-heads (outline)
  "OUTLINE as org heading lines, keyed by the page each one names.
OUTLINE is `pdf-info-outline' output: alists with depth, title, and -
for goto-dest entries - page.  Entries without a usable page (URI
links, unresolved destinations reported as page 0) or without a title
are dropped.  A lone top-level entry is the book's own title, not a
chapter - as a headline it would fold the entire book into one line -
so it renders as a #+TITLE keyword and every deeper entry promotes to
close the gap."
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
    (dolist (page (hash-table-keys heads) heads)
      (puthash page (nreverse (gethash page heads)) heads))))

(defun pdf-text-page-headings (outline first count)
  "The heading lines OUTLINE puts on COUNT pages starting at page FIRST.
One entry per page, which is how `pdf-text-render-lines' takes them:
the reflow places a heading at the line naming it, and only the caller
knows which page of the book each page it renders is."
  (let ((heads (pdf-text--outline-heads outline)))
    (cl-loop for page from first below (+ first count)
             collect (gethash page heads))))

(defun pdf-text--interleave-outline (pages outline)
  "PAGES with every OUTLINE heading its page does not already carry.
The reflow places a heading at the line the outline names, when the
page has that line; what is left over - a title no line of the page
reproduces - goes at the page's start, which is the only thing left
to say about where its section begins.  A nil OUTLINE returns PAGES
unchanged: PDFs without an outline degrade to the flat view."
  (let ((heads (pdf-text--outline-heads outline))
        (n 0))
    (mapcar (lambda (page)
              (cl-incf n)
              (if-let* ((placed (split-string page "\n"))
                        (missing (cl-remove-if (lambda (head) (member head placed))
                                               (gethash n heads))))
                  (concat (string-join missing "\n") "\n" page)
                page))
            pages)))

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
                        (apply #'max 0 (mapcar (lambda (l) (length (string-trim l)))
                                               lines)))))
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

;;; The companion buffer

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

(defconst pdf-text-render-version 4
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
      ;; the layout carries the text as well as its geometry, so gettext
      ;; only runs for a page epdfinfo lays out no glyphs for
      (let* ((layouts (cl-loop for p from 1 to (pdf-info-number-of-pages)
                               collect (condition-case nil
                                           (pdf-info-charlayout p)
                                         (error nil))))
             (raw (cl-loop for p from 1
                           for layout in layouts
                           collect (if layout
                                       (pdf-text--layout-text layout)
                                     (pdf-info-gettext p '(0 0 1 1))))))
        (when (pdf-text--scanned-p raw)
          (user-error "%s has no text layer (%d of %d pages carry text)"
                      (buffer-name)
                      (cl-count-if-not #'string-blank-p raw) (length raw)))
        (let* ((outline (pdf-info-outline))
               (rendered (pdf-text-render-pages
                          raw layouts
                          (pdf-text-page-headings outline 1 (length raw))))
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
