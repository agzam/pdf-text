;;; tests/corpus.el --- real-book cases for the pdf-text reflow -*- lexical-binding: t; -*-

;; Synthetic geometry proves a rule; it never proves a book renders.
;; Books cannot go in the repo and epdfinfo cannot run in CI, so a case
;; carries the geometry instead of the PDF: a window of pages as line
;; records, next to the rendering of one subject page that a human read
;; once.  Rendering a case needs neither, which is why the suite over
;; `pdf-corpus-directory' runs like any other spec.
;;
;; A case is a directory under corpus/, named for what it exercises
;; rather than for its book:
;;
;;   case.eld     what the case is about, its budgets and flags - written by hand
;;   lines.eld    the captured window, one line record per line - `bb corpus-add'
;;   golden.txt   the subject page as it renders - `bb corpus-add', `bb corpus-accept'
;;   dropped.txt  source lines the render is licensed to lose (running heads)
;;
;; The window matters: body geometry, running heads and the hyphenation
;; vocabulary are read across pages, so a page pulled out alone renders
;; differently from the same page inside its book.  What even a window
;; cannot show travels beside it: the whole document's outline, the
;; compounds the book hyphenates elsewhere, and the recurring-form and
;; folio-merged counts the running-head rules read book-wide - and
;; `bb corpus-add' proves the point by rendering the whole book and
;; refusing a case whose window disagrees with it on the subject page.

(require 'cl-lib)
(require 'seq)

(defvar pdf-corpus-directory
  (expand-file-name "corpus/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Directory holding the captured cases, one per subdirectory.")

;;; The capture format

(defun pdf-corpus--number (value)
  "VALUE as it is written into a case, or \"nil\" for a missing measurement.
Four decimals resolve a page to well under a point, an order finer
than any threshold the reflow compares against, and keep a case's
line records diffable."
  (if value (format "%.4f" value) "nil"))

(defun pdf-corpus-record (line)
  "LINE, a `pdf-text-line', as the list a case file stores."
  (list (pdf-text-line-text line)
        (pdf-text-line-x0 line) (pdf-text-line-x1 line)
        (pdf-text-line-top line) (pdf-text-line-bot line)
        (pdf-text-line-base line) (pdf-text-line-height line)
        (pdf-text-line-space line) (pdf-text-line-cv line)
        (pdf-text-line-first-width line)))

(defun pdf-corpus-line (record)
  "A fresh `pdf-text-line' for RECORD, as stored in a case file.
Fresh every time on purpose: the reflow tags records as it works, so a
record shared between two renders would carry the first one's verdict
into the second."
  (cl-destructuring-bind (text x0 x1 top bot base height space cv first-width) record
    (pdf-text-line-create :text text :x0 x0 :x1 x1 :top top :bot bot
                          :base base :height height :space space :cv cv
                          :first-width first-width)))

(defun pdf-corpus--print-record (line)
  "LINE as one line of a case file."
  (format "  (%s %s)"
          (prin1-to-string (pdf-text-line-text line))
          (mapconcat #'pdf-corpus--number (cdr (pdf-corpus-record line)) " ")))

(defun pdf-corpus--print-profile (profile)
  "PROFILE, a `pdf-text--profile' plist, rounded as a case stores it."
  (format "(:height %s :leading %s :left %s :right %s :space %s)"
          (pdf-corpus--number (plist-get profile :height))
          (pdf-corpus--number (plist-get profile :leading))
          (pdf-corpus--number (plist-get profile :left))
          (pdf-corpus--number (plist-get profile :right))
          (pdf-corpus--number (plist-get profile :space))))

(defun pdf-corpus--print-clusters (clusters)
  "CLUSTERS of heading heights, rounded as a case stores them."
  (format "(%s)"
          (mapconcat (lambda (cluster)
                       (format "(%s . %s)"
                               (pdf-corpus--number (car cluster))
                               (pdf-corpus--number (cdr cluster))))
                     clusters " ")))

(defun pdf-corpus-print-lines (window outline vocabulary facts pages)
  "The lines file for PAGES, a WINDOW of the book, with its OUTLINE.
WINDOW is (FIRST . LAST), PAGES an alist of (PAGE . LINES), and
OUTLINE a list of (DEPTH PAGE TITLE) covering the whole document -
the book's own title and the depth its chapters sit at decide how any
one page's headings render.  VOCABULARY carries the compounds the
book hyphenates elsewhere, and FACTS the document-wide readings a
window cannot establish on its own: the running-head forms and
folio-merged count, the modal body profile, and the heading-height
clusters, as `pdf-corpus-script-facts' collects them."
  (concat ";; Captured by `bb corpus-add'; regenerate rather than edit.\n"
          (format "(:window %S\n :vocabulary %S\n :recurring-forms %S\n :folio-merged %d\n :profile %s\n :heading-levels %s\n :outline\n (%s)\n :pages\n ("
                  window vocabulary
                  (plist-get facts :recurring-forms)
                  (or (plist-get facts :folio-merged) 0)
                  (pdf-corpus--print-profile (plist-get facts :profile))
                  (pdf-corpus--print-clusters (plist-get facts :heading-levels))
                  (mapconcat (lambda (entry) (format "%S" entry)) outline "\n  "))
          (mapconcat
           (lambda (page)
             (format "(%d\n%s)"
                     (car page)
                     (mapconcat #'pdf-corpus--print-record (cdr page) "\n")))
           pages "\n  ")
          "))\n"))

(defun pdf-corpus--read-form (file)
  "The single form FILE holds, or nil when it does not exist."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (read (current-buffer)))))

(defun pdf-corpus--read-text-lines (file)
  "FILE's lines, its blank and commented ones left out, or nil."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (seq-remove (lambda (line) (string-prefix-p ";;" line))
                  (split-string (buffer-string) "\n" t)))))

(defun pdf-corpus-slugs ()
  "Every case in `pdf-corpus-directory', by name."
  (when (file-directory-p pdf-corpus-directory)
    (seq-filter (lambda (slug)
                  (file-exists-p (expand-file-name (format "%s/lines.eld" slug)
                                                   pdf-corpus-directory)))
                (directory-files pdf-corpus-directory nil "\\`[^.]"))))

(defun pdf-corpus-read (slug)
  "The case named SLUG, as a plist.
:meta the hand-written description, :window the pages captured,
:outline the document's outline, :pages an alist of (PAGE . LINES),
:golden the reviewed rendering of the subject page, :drops the source
lines it is allowed to lose."
  (let* ((dir (file-name-as-directory (expand-file-name slug pdf-corpus-directory)))
         (lines (pdf-corpus--read-form (expand-file-name "lines.eld" dir)))
         (golden (expand-file-name "golden.txt" dir)))
    (list :slug slug
          :dir dir
          :meta (pdf-corpus--read-form (expand-file-name "case.eld" dir))
          :window (plist-get lines :window)
          :vocabulary (plist-get lines :vocabulary)
          :recurring-forms (plist-get lines :recurring-forms)
          :folio-merged (or (plist-get lines :folio-merged) 0)
          :profile (plist-get lines :profile)
          :heading-levels (plist-get lines :heading-levels)
          :outline (plist-get lines :outline)
          :pages (mapcar (lambda (page)
                           (cons (car page) (mapcar #'pdf-corpus-line (cdr page))))
                         (plist-get lines :pages))
          ;; the file ends in a newline, as a text file should; the
          ;; rendering of a page does not
          :golden (when (file-exists-p golden)
                    (with-temp-buffer
                      (insert-file-contents golden)
                      (replace-regexp-in-string "\n\\'" "" (buffer-string))))
          :drops (pdf-corpus--read-text-lines (expand-file-name "dropped.txt" dir)))))

(defun pdf-corpus-subject (case)
  "The page CASE is about."
  (plist-get (plist-get case :meta) :page))

;;; Rendering a case

(defun pdf-corpus--outline-entries (outline)
  "OUTLINE, stored as (DEPTH PAGE TITLE), in `pdf-info-outline' shape."
  (mapcar (lambda (entry)
            (list (cons 'depth (nth 0 entry))
                  (cons 'page (nth 1 entry))
                  (cons 'title (nth 2 entry))))
          outline))

(defun pdf-corpus-render (case)
  "CASE's window rendered, as a plist of two alists keyed by page number.
:reflowed is the reflow alone, which is what the text-survival rules
are about; :headed adds the outline headings, which is what the reader
sees and what the heading rules are about.  Pages before the window
render as empty strings so every page keeps the number it has in the
book: the outline is keyed by those numbers.

Everything the reflow reads beyond the page itself - the outline, the
hyphenation vocabulary, the recurring-form and folio-merged facts, the
document profile and its heading-height clusters, the pages either
side - comes out of the case, which is what makes this render the one
the book gets."
  (let* ((pages (plist-get case :pages))
         (start (car (plist-get case :window)))
         (outline (plist-get case :outline))
         (entries (pdf-corpus--outline-entries outline))
         (pdf-text-extra-vocabulary (plist-get case :vocabulary))
         (pdf-text-extra-recurring-forms (plist-get case :recurring-forms))
         (pdf-text-extra-folio-merged (or (plist-get case :folio-merged) 0))
         (pdf-text-extra-profile (plist-get case :profile))
         (pdf-text-extra-heading-levels (plist-get case :heading-levels))
         (reflowed (pdf-text-render-lines
                    (mapcar #'cdr pages)
                    (pdf-text-page-headings entries start (length pages))
                    start (null outline)))
         (padded (append (make-list (1- start) "") reflowed))
         (headed (if outline
                     (pdf-text--interleave-outline padded entries)
                   padded))
         (number (lambda (texts)
                   (cl-loop for page in pages
                            for text in texts
                            collect (cons (car page) text)))))
    (list :reflowed (funcall number reflowed)
          :headed (funcall number (nthcdr (1- start) headed)))))

(defun pdf-corpus-sources (pages)
  "Per-page source lines of PAGES, an alist of (PAGE . LINES), repaired.
The reflow reads a page in repaired reading order - script fragments
rejoined to their typeset line, scattered arrays back in rows - so
that order is what text survival is measured against.  The repairs
merge and reorder records, never drop one, which
`pdf-text-reading-order''s own specs hold it to."
  (cl-loop for entry in pages
           for lines in (pdf-text-reading-order (mapcar #'cdr pages))
           collect (cons (car entry) (mapcar #'pdf-text-line-text lines))))

;;; Invariants

(defvar pdf-corpus-insert-tolerance 200
  "Characters a source line may be found ahead of where it was expected.
The reflow inserts nothing of its own, so a line that turns up much
further on is another occurrence of the same words - a section title
that also runs in the page's head - and reading the gap as inserted
text would strand every line after it.")

(defun pdf-corpus-stream (text)
  "TEXT reduced to its letters and digits, downcased.
Re-wrapping, de-hyphenation, the indent of a quotation and the dashes
the render sets in front of list items all disappear here, so what is
left compares the words themselves.  A generated footnote label
disappears the same way: a numeric marker's label gives back the
digits the page wrote - 2005.^{1} renders as [fn:19-1], and the 1 is
the page's own - while a symbol marker's gives back nothing, the
page's star being no letter.  A literal [fn: off the page carries a
zero-width space and never reads as a label."
  (let ((bare (replace-regexp-in-string
               "\\[fn:[0-9]+-\\([0-9]+\\)\\]" "\\1"
               (replace-regexp-in-string "\\[fn:[0-9]+-[a-z]+\\]" "" text))))
    (downcase (replace-regexp-in-string "[^[:alnum:]]" "" bare))))

(defun pdf-corpus-diff (source render)
  "What RENDER lost of SOURCE and what it added, as a plist.
:lost holds the source lines RENDER does not carry, in order, and
:added the runs of text RENDER holds that no source line accounts
for.  Each side reduces to its letters and digits first, so
re-wrapping, de-hyphenation, a restored indent and the dash the
render sets in front of a list item are no change at all - what is
compared is the words.  The source's own lines are the unit of
comparison, because that is the unit the render drops: a running
head, an echo of a shadow-painted title.  A line that survives only
in part reads as lost, and the half that survived reads as added,
which is what a paragraph cut in two looks like from here."
  (let ((stream (pdf-corpus-stream render))
        (position 0)
        lost added)
    (dolist (line source)
      (let ((piece (pdf-corpus-stream line)))
        (unless (string-empty-p piece)
          (if (string-prefix-p piece (substring stream position))
              (setq position (+ position (length piece)))
            ;; not where the source says: either the render dropped the
            ;; line, or it inserted something just ahead of it.  A piece
            ;; of a character or two is not worth searching for - it
            ;; matches inside any word, and a false match strands the
            ;; walk - so it reads as lost where it stands
            (if-let* (((<= 3 (length piece)))
                      (found (string-search piece stream position))
                      ((< (- found position) pdf-corpus-insert-tolerance)))
                (progn (push (substring stream position found) added)
                       (setq position (+ found (length piece))))
              (push line lost))))))
    (when (< position (length stream))
      (push (substring stream position) added))
    (list :lost (nreverse lost) :added (nreverse added))))

(defun pdf-corpus-mid-sentence-breaks (text)
  "Pairs of rendered lines in TEXT that read as one sentence cut in two.
A line closing on a lowercase word or a comma, and the next opening
lowercase, is a paragraph the reflow broke - or display maths and code
inside prose, which is why cases carry a budget.  A pair whose either
side reads as mathematics is the page interrupting its own sentence
with a display, which is the correct rendering, not a break."
  (let ((case-fold-search nil)          ; batch defaults to t, where
        (lines (mapcar #'string-trim    ; [[:lower:]] matches capitals
                       (seq-remove #'string-blank-p (split-string text "\n"))))
        breaks)
    (cl-loop for (this next) on lines while next
             do (when (and (string-match-p "[[:lower:],;]\\'" this)
                           (string-match-p "\\`[[:lower:]]" next)
                           (not (pdf-text-mathish-text-p this))
                           (not (pdf-text-mathish-text-p next)))
                  (push (cons this next) breaks)))
    (nreverse breaks)))

(defun pdf-corpus-page-marker-lines (text)
  "Rendered lines of TEXT that are nothing but a page number.
A folio that reaches the reader is a running head the geometry missed."
  (seq-filter #'pdf-text--page-marker-p
              (seq-remove #'string-blank-p (split-string text "\n"))))

(defun pdf-corpus-heading-bodies (text)
  "Each org heading in TEXT as (TITLE LEVEL CHARS), in order.
CHARS is what the heading owns, which is what the reader gets when the
buffer is folded: everything down to the next heading.  A heading
escaped with a zero-width space is page text, not structure, and does
not count."
  (let ((chars 0) current level out)
    (dolist (line (split-string text "\n"))
      (if (string-match "\\`\\(\\*+\\) \\(.*\\)\\'" line)
          (progn (when current (push (list current level chars) out))
                 (setq current (match-string 2 line)
                       level (length (match-string 1 line))
                       chars 0))
        (when current (setq chars (+ chars (length (string-trim line)))))))
    (when current (push (list current level chars) out))
    (nreverse out)))

(defun pdf-corpus-empty-headings (text)
  "Headings in TEXT that own nothing, which fold into an empty section.
A heading whose next one is deeper owns that subtree - the reader who
folds it gets the child section, which is what a book that opens a
section with its first subsection looks like.  The last heading is
exempt too: its section continues on the page after this one."
  (let (out)
    (cl-loop for (this next) on (pdf-corpus-heading-bodies text)
             while next
             do (when (and (eql 0 (nth 2 this))
                           (<= (nth 1 next) (nth 1 this)))
                  (push (car this) out)))
    (nreverse out)))

(defun pdf-corpus--occurrences (needle haystack)
  "How many times NEEDLE occurs in HAYSTACK."
  (if (string-empty-p needle)
      0
    (let ((n 0) (position 0))
      (while (setq position (string-search needle haystack position))
        (setq n (1+ n) position (+ position (length needle))))
      n)))

(defun pdf-corpus-outline-placements (titles text)
  "TITLES that TEXT does not carry exactly once, with their count.
A section heading names a line of the page it opens: once the heading
sits at that line, the title reads once.  Twice means the heading was
prepended and the line it names is still down in the prose; none means
the outline points at something the page never says."
  (let ((stream (pdf-corpus-stream text)))
    (seq-remove (lambda (found) (eql 1 (cdr found)))
                (mapcar (lambda (title)
                          (cons title (pdf-corpus--occurrences
                                       (pdf-corpus-stream title) stream)))
                        titles))))

(cl-defun pdf-corpus-violations (&key source reflowed headed titles drops budget)
  "Every invariant the rendering of one page breaks, as (KIND . DETAIL).
SOURCE is its source lines, REFLOWED its reflow, HEADED the reflow
with the outline headings interleaved, TITLES the outline entries
naming a section on the page, DROPS the source lines the case
licenses the render to lose, and BUDGET how many mid-sentence breaks
it tolerates."
  (let ((diff (pdf-corpus-diff source reflowed))
        violations)
    (unless (equal (mapcar #'string-trim (plist-get diff :lost))
                   (mapcar #'string-trim drops))
      (push (cons 'lost (plist-get diff :lost)) violations))
    (when-let* ((added (plist-get diff :added)))
      (push (cons 'added added) violations))
    (let ((breaks (pdf-corpus-mid-sentence-breaks reflowed)))
      (when (< (or (plist-get budget :mid-sentence) 0) (length breaks))
        (push (cons 'mid-sentence breaks) violations)))
    (when-let* ((markers (pdf-corpus-page-marker-lines reflowed)))
      (push (cons 'page-marker markers) violations))
    (when-let* ((misplaced (pdf-corpus-outline-placements titles (or headed reflowed))))
      (push (cons 'outline misplaced) violations))
    (when-let* ((empty (pdf-corpus-empty-headings (or headed ""))))
      (push (cons 'heading empty) violations))
    (nreverse violations)))

(defun pdf-corpus-titles (outline page)
  "Titles OUTLINE puts on PAGE."
  (mapcar (lambda (entry) (nth 2 entry))
          (seq-filter (lambda (entry) (eql page (nth 1 entry))) outline)))

(defun pdf-corpus-case-violations (case &optional rendered)
  "Every invariant CASE's subject page breaks.
RENDERED is `pdf-corpus-render' output, computed when not supplied."
  (let* ((page (pdf-corpus-subject case))
         ;; before the render, whose cleanups mutate the records in place
         (source (alist-get page (pdf-corpus-sources (plist-get case :pages))))
         (rendered (or rendered (pdf-corpus-render case)))
         (meta (plist-get case :meta)))
    (pdf-corpus-violations
     :source source
     :reflowed (alist-get page (plist-get rendered :reflowed))
     :headed (alist-get page (plist-get rendered :headed))
     :titles (pdf-corpus-titles (plist-get case :outline) page)
     :drops (plist-get case :drops)
     :budget (plist-get meta :budget))))

(defun pdf-corpus-describe-violation (violation)
  "VIOLATION as one line of a report."
  (pcase-let ((`(,kind . ,detail) violation))
    (pcase kind
      ('lost (format "lost: %s" (string-join (mapcar #'string-trim detail) " / ")))
      ('added (format "added: %s" (string-join detail " / ")))
      ('mid-sentence (format "%d mid-sentence break(s): %s"
                             (length detail)
                             (mapconcat (lambda (pair)
                                          (format "%S -> %S" (car pair) (cdr pair)))
                                        (seq-take detail 3) "; ")))
      ('page-marker (format "page marker survived: %s" (string-join detail " ")))
      ('outline (format "outline title placed wrong: %s"
                        (mapconcat (lambda (found)
                                     (format "%S x%d" (car found) (cdr found)))
                                   detail "; ")))
      ('heading (format "heading folds to an empty section: %s"
                        (mapconcat (lambda (title) (format "%S" title)) detail "; ")))
      (_ (format "%s: %S" kind detail)))))

(provide 'pdf-corpus)
