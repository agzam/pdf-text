;;; scripts/pdf-corpus.el --- capture and sweep the pdf-text corpus -*- lexical-binding: t; -*-

;; The half of the corpus that wants a real book.  `bb corpus-add'
;; captures a window of pages through epdfinfo into a case; `bb
;; corpus-accept' re-renders captured cases into their goldens, after an
;; intended change, so the review happens in `git diff'; `bb
;; corpus-sweep' renders a whole book and ranks its pages by the
;; invariants they break, which is how the next botched page is found
;; instead of stumbled on.
;;
;; Only add and sweep open a PDF.  The suite over the captured cases
;; runs on the records alone, which is what lets it run in CI.

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defvar pdf-corpus-script-root
  (expand-file-name "../" (file-name-directory (or load-file-name buffer-file-name)))
  "Root of the config this script belongs to.")

(defvar pdf-corpus-books-directory
  (file-name-as-directory
   (expand-file-name (or (getenv "BOOKS") "~/SyncMobile/Books")))
  "Where a book named by a fragment rather than a path is looked for.")

(defvar pdf-corpus-window 3
  "Pages captured either side of a case's subject page.
The reflow reads body geometry, running heads and hyphenation across
pages, so a page captured alone would not render as it does in its
book.  Three each way gives the running-head rule the repetitions it
counts.")

(defvar pdf-corpus-sweep-body-words 8
  "Words a dropped line needs before a sweep calls its loss suspicious.
Running heads and folios are short; a dropped line this long is a
line of prose the cleanups ate.")

(load (expand-file-name "modules/pdf/autoload/pdf-text.el" pdf-corpus-script-root)
      nil 'nomessage)
(load (expand-file-name "tests/pdf/corpus.el" pdf-corpus-script-root)
      nil 'nomessage)

(defun pdf-corpus-script-connect ()
  "Put pdf-tools' epdfinfo within reach of this batch Emacs."
  (let ((build (expand-file-name ".local/elpaca/builds/pdf-tools/"
                                 pdf-corpus-script-root)))
    (unless (file-directory-p build)
      (error "pdf-tools is not built at %s" build))
    (add-to-list 'load-path build)
    (require 'pdf-info)
    (setq pdf-info-epdfinfo-program (expand-file-name "epdfinfo" build))
    (unless (file-executable-p pdf-info-epdfinfo-program)
      (error "epdfinfo is not built at %s" pdf-info-epdfinfo-program))))

(defun pdf-corpus-script-book (spec)
  "The PDF SPEC names: a path as it stands, or a fragment of one."
  (let ((path (expand-file-name spec)))
    (if (file-readable-p path)
        path
      (let ((found (seq-filter
                    (lambda (file) (string-match-p (regexp-quote (downcase spec))
                                                   (downcase file)))
                    (directory-files-recursively pdf-corpus-books-directory
                                                 "\\.pdf\\'"))))
        (pcase (length found)
          (0 (error "No PDF under %s matches %S" pdf-corpus-books-directory spec))
          (1 (car found))
          (_ (error "%d PDFs match %S:\n%s" (length found) spec
                    (mapconcat (lambda (file) (concat "  " file))
                               (seq-take found 10) "\n"))))))))

;;; Reading a book

(defun pdf-corpus-script-outline (file)
  "FILE's outline as the case format stores it: (DEPTH PAGE TITLE).
Entries without a page - URI links, destinations poppler reports as
page 0 - carry nothing a heading can be placed by."
  (delq nil
        (mapcar (lambda (entry)
                  (let-alist entry
                    (when (and (integerp .page) (<= 1 .page)
                               (stringp .title) (not (string-blank-p .title)))
                      (list (max 1 (or .depth 1)) .page (string-trim .title)))))
                (pdf-info-outline file))))

(defun pdf-corpus-script-pages (file first last)
  "Line records for pages FIRST to LAST of FILE, as (PAGE . LINES).
The layout carries the text as well as its geometry, so gettext only
runs for a page epdfinfo lays out no glyphs for - the same order
`pdf-view-as-text' uses."
  (cl-loop for page from first to last
           collect (let* ((layout (condition-case nil
                                      (pdf-info-charlayout page nil file)
                                    (error nil)))
                          (text (if layout
                                    (pdf-text--layout-text layout)
                                  (pdf-info-gettext page '(0 0 1 1) nil file))))
                     (cons page (pdf-text--page-lines text layout)))))

(defun pdf-corpus-script--compounds (lines)
  "Compounds the wrap hyphens in LINES could be closing up.
The word before the hyphen and the word after it, the pair
`pdf-text--join-lines' has to decide about."
  (delq nil
        (cl-loop for (this next) on lines while next
                 for text = (string-trim (pdf-text-line-text this))
                 when (pdf-text--wrap-hyphen-p text)
                 collect (let ((head (car (last (split-string (substring text 0 -1)
                                                              "[ \t]+" t))))
                               (tail (car (split-string (pdf-text-line-text next)
                                                        "[ \t]+" t))))
                           (when (and head tail)
                             (downcase (concat head "-"
                                               (string-trim tail "" "[^[:alnum:]]+"))))))))

(defun pdf-corpus-script-vocabulary (file pages)
  "Compounds FILE hyphenates elsewhere that PAGES wrap over a line end.
The whole book has the say, so the whole book is read - by gettext
alone, which is all the vocabulary needs and a fraction of the cost
of laying out every glyph."
  (let* ((total (pdf-info-number-of-pages file))
         (document (pdf-text--hyphenated-words
                    (cl-loop for page from 1 to total
                             collect (pdf-text--page-lines
                                      (pdf-info-gettext page '(0 0 1 1) nil file)))))
         (wanted (mapcan (lambda (page) (pdf-corpus-script--compounds (cdr page)))
                         pages)))
    (seq-uniq (seq-filter (lambda (word) (gethash word document)) wanted))))

;;; Writing a case

(defun pdf-corpus-script--write (file text)
  "Write TEXT to FILE, making its directory first."
  (make-directory (file-name-directory file) t)
  (with-temp-file file (insert text)))

(defun pdf-corpus-script--starter (file page)
  "The hand-owned half of a case for PAGE of FILE."
  (format ";; What this case exercises; edit by hand.  `bb corpus-accept' leaves it alone.\n(:book %S\n :source %S\n :page %d\n :exercises \"TODO: name the defect or the structure this page proves\"\n :budget (:mid-sentence 0))\n"
          (file-name-base file)
          (string-remove-prefix pdf-corpus-books-directory file)
          page))

(defun pdf-corpus-script-refresh (slug)
  "Render the stored case SLUG and write its golden and its drops.
The rendering comes from the records as the file holds them, rounded,
so the suite reproduces this golden exactly."
  (let* ((case (pdf-corpus-read slug))
         (page (pdf-corpus-subject case))
         (source (mapcar #'pdf-text-line-text
                         (alist-get page (plist-get case :pages))))
         (rendered (pdf-corpus-render case))
         (reflowed (alist-get page (plist-get rendered :reflowed)))
         (headed (alist-get page (plist-get rendered :headed)))
         (lost (plist-get (pdf-corpus-diff source reflowed) :lost))
         (dir (plist-get case :dir)))
    (unless page
      (error "Case %s has no :page in case.eld" slug))
    (pdf-corpus-script--write (expand-file-name "golden.txt" dir)
                              (concat headed "\n"))
    (if lost
        (pdf-corpus-script--write
         (expand-file-name "dropped.txt" dir)
         (concat ";; Source lines the render is allowed to lose: running heads,\n"
                 ";; folios, the echo of a title painted twice.  A line of prose\n"
                 ";; here is a defect, not a licence.\n"
                 (string-join lost "\n") "\n"))
      (delete-file (expand-file-name "dropped.txt" dir) t))
    ;; read the case back rather than reuse what is in hand: the verdict
    ;; then comes from the files the suite will read, goldens, declared
    ;; drops and rounded records alike
    (let ((written (pdf-corpus-read slug)))
      (list :page page
            :lost lost
            :violations (pdf-corpus-case-violations written)))))

(defun pdf-corpus-script-report (slug result)
  "Print what refreshing SLUG produced, RESULT."
  (princ (format "%s: page %d captured\n" slug (plist-get result :page)))
  (dolist (line (plist-get result :lost))
    (princ (format "  dropped: %s\n" (string-trim line))))
  (dolist (violation (plist-get result :violations))
    (princ (format "  ! %s\n" (pdf-corpus-describe-violation violation)))))

(defun pdf-corpus-add (book page &optional slug window)
  "Capture PAGE of BOOK, with its neighbours, as the case named SLUG.
WINDOW overrides how many pages either side travel with it.  The case
starts out passing whatever it renders today; a page captured for a
defect wants its golden corrected by hand and `:known-failing' set in
case.eld, which is what turns it into the acceptance test for the fix."
  (pdf-corpus-script-connect)
  (let* ((file (pdf-corpus-script-book book))
         (page (if (stringp page) (string-to-number page) page))
         (window (or window pdf-corpus-window))
         (total (pdf-info-number-of-pages file))
         (first (max 1 (- page window)))
         (last (min total (+ page window)))
         (slug (or slug (format "%s-p%d"
                                (replace-regexp-in-string
                                 "[^a-z0-9]+" "-"
                                 (downcase (substring (file-name-base file) 0
                                                      (min 20 (length (file-name-base file))))))
                                page)))
         (dir (file-name-as-directory (expand-file-name slug pdf-corpus-directory))))
    (unless (<= 1 page total)
      (error "%s has %d pages; %d is not one of them" file total page))
    (when (file-exists-p (expand-file-name "lines.eld" dir))
      (error "Case %s exists already: delete it, or run bb corpus-accept" slug))
    (princ (format "capturing %s pages %d-%d of %s\n" slug first last file))
    (let ((window (pdf-corpus-script-pages file first last)))
      (pdf-corpus-script--write
       (expand-file-name "lines.eld" dir)
       (pdf-corpus-print-lines (cons first last)
                               (pdf-corpus-script-outline file)
                               (pdf-corpus-script-vocabulary file window)
                               window)))
    (unless (file-exists-p (expand-file-name "case.eld" dir))
      (pdf-corpus-script--write (expand-file-name "case.eld" dir)
                                (pdf-corpus-script--starter file page)))
    (pdf-corpus-script-report slug (pdf-corpus-script-refresh slug))
    (princ (format "  %s\n  say in case.eld what it exercises, then read golden.txt\n"
                   (abbreviate-file-name dir)))))

(defun pdf-corpus-accept (&optional slug)
  "Rewrite the goldens of SLUG, or of every case, from the stored records.
Needs no PDF: an intended change to the reflow lands here, and the
review is the diff this leaves behind."
  (dolist (slug (if (and slug (not (string-empty-p slug)))
                    (list slug)
                  (pdf-corpus-slugs)))
    (pdf-corpus-script-report slug (pdf-corpus-script-refresh slug))))

;;; Sweeping a whole book

(defun pdf-corpus-script--body-line-p (line)
  "Whether LINE is too long to be the running head a page may lose."
  (<= pdf-corpus-sweep-body-words (length (split-string (string-trim line)))))

(defun pdf-corpus-script--sweep-violations (violations)
  "VIOLATIONS with the losses a running head explains taken out.
A sweep has no goldens and no declared drops, so every page would
report its own running head; what is worth ranking is a page that
lost prose."
  (delq nil
        (mapcar (lambda (violation)
                  (if (eq 'lost (car violation))
                      (when-let* ((lost (seq-filter #'pdf-corpus-script--body-line-p
                                                    (cdr violation))))
                        (cons 'lost lost))
                    violation))
                violations)))

(defun pdf-corpus-book-report (file)
  "Render the whole of FILE and account for what came out, as a plist.
The buffer it measures is the one `pdf-view-as-text' builds - every
page, headings interleaved, form feeds between them - because a
heading owns text across a page boundary and a page rendered on its
own cannot show that."
  (let* ((total (pdf-info-number-of-pages file))
         (start (current-time))
         (pages (pdf-corpus-script-pages file 1 total))
         (outline (pdf-corpus-script-outline file))
         (entries (pdf-corpus--outline-entries outline))
         (sources (mapcar (lambda (page)
                            (mapcar #'pdf-text-line-text (cdr page)))
                          pages))
         (reflowed (pdf-text-render-lines
                    (mapcar #'cdr pages)
                    (pdf-text-page-headings entries 1 (length pages))))
         (headed (if outline
                     (pdf-text--interleave-outline reflowed entries)
                   (pdf-text--synthesize-headings reflowed)))
         (buffer (string-join headed "\n\f\n"))
         (scanned (pdf-text--scanned-p (mapcar (lambda (lines)
                                                 (string-join lines "\n"))
                                               sources)))
         (lost 0) (added 0) (mid 0) (markers 0))
    (cl-loop for source in sources
             for text in reflowed
             do (let ((diff (pdf-corpus-diff source text)))
                  (cl-incf lost (length (seq-filter #'pdf-corpus-script--body-line-p
                                                    (plist-get diff :lost))))
                  (cl-incf added (length (plist-get diff :added)))
                  (cl-incf mid (length (pdf-corpus-mid-sentence-breaks text)))
                  (cl-incf markers (length (pdf-corpus-page-marker-lines text)))))
    (list :file file
          :pages total
          :scanned scanned
          :seconds (float-time (time-since start))
          :source-chars (apply #'+ (mapcar (lambda (lines)
                                             (length (pdf-corpus-stream
                                                      (string-join lines ""))))
                                           sources))
          :render-chars (length (pdf-corpus-stream (string-join reflowed "")))
          :headings (length (pdf-corpus-heading-bodies buffer))
          :empty (length (pdf-corpus-empty-headings buffer))
          :lost lost :added added :mid mid :markers markers)))

(defun pdf-corpus-audit (&optional fragment limit)
  "Render every book under `pdf-corpus-books-directory' and account for it.
FRAGMENT narrows the list to paths matching it, LIMIT to the first N.
One line per book: how much of the source reached the reader, how many
headings fold to nothing, how much text no running head explains.  A
book with no text layer is named and skipped, since a scan has nothing
to convert."
  (pdf-corpus-script-connect)
  (let* ((files (seq-filter
                 (lambda (file)
                   (or (null fragment) (string-empty-p fragment)
                       (string-match-p (regexp-quote (downcase fragment))
                                       (downcase file))))
                 (sort (directory-files-recursively pdf-corpus-books-directory
                                                    "\\.pdf\\'")
                       #'string<)))
         (files (if limit (seq-take files (if (stringp limit)
                                              (string-to-number limit)
                                            limit))
                  files))
         reports)
    (princ (format "%-44s %5s %7s %6s %6s %5s %5s %5s\n"
                   "book" "pages" "kept%" "heads" "empty" "lost" "mid" "mark"))
    (dolist (file files)
      (condition-case error
          (let* ((report (pdf-corpus-book-report file))
                 (source (plist-get report :source-chars)))
            (push report reports)
            (princ (format "%-44s %5d %6.2f%% %6d %6d %5d %5d %5d%s\n"
                           (truncate-string-to-width (file-name-base file) 44)
                           (plist-get report :pages)
                           (if (< 0 source)
                               (* 100.0 (/ (float (plist-get report :render-chars))
                                           source))
                             0.0)
                           (plist-get report :headings)
                           (plist-get report :empty)
                           (plist-get report :lost)
                           (plist-get report :mid)
                           (plist-get report :markers)
                           (if (plist-get report :scanned) "  SCAN" ""))))
        ;; a book epdfinfo cannot read must not take the run down with
        ;; it: name it, drop the server, and the next call starts a new one
        (error (princ (format "%-44s  FAILED: %s\n"
                              (truncate-string-to-width (file-name-base file) 44)
                              (error-message-string error)))
               (ignore-errors (pdf-info-quit))))
      ;; close the document, keep the server: poppler would otherwise hold
      ;; every book of the library open at once
      (ignore-errors (pdf-info-close file)))
    (let ((usable (seq-remove (lambda (r) (plist-get r :scanned)) reports)))
      (princ (format "\n%d books, %d with a text layer, %d headings fold to nothing, %d pages-worth of prose lost\n"
                     (length reports) (length usable)
                     (apply #'+ 0 (mapcar (lambda (r) (plist-get r :empty)) usable))
                     (apply #'+ 0 (mapcar (lambda (r) (plist-get r :lost)) usable)))))))

(defun pdf-corpus-sweep (book &optional first last)
  "Render BOOK and rank its pages by the invariants they break.
FIRST and LAST limit the range; the whole book by default."
  (pdf-corpus-script-connect)
  (let* ((file (pdf-corpus-script-book book))
         (total (pdf-info-number-of-pages file))
         (first (max 1 (if (stringp first) (string-to-number first) (or first 1))))
         (last (min total (if (stringp last) (string-to-number last) (or last total))))
         (start (current-time))
         (pages (pdf-corpus-script-pages file first last))
         (outline (pdf-corpus-script-outline file))
         (entries (pdf-corpus--outline-entries outline))
         (sources (mapcar (lambda (page)
                            (mapcar #'pdf-text-line-text (cdr page)))
                          pages))
         (reflowed (pdf-text-render-lines
                    (mapcar #'cdr pages)
                    (pdf-text-page-headings entries first (length pages))))
         (padded (append (make-list (1- first) "") reflowed))
         (headed (nthcdr (1- first)
                         (if outline
                             (pdf-text--interleave-outline padded entries)
                           (pdf-text--synthesize-headings padded))))
         (elapsed (float-time (time-since start)))
         (totals (make-hash-table))
         ranked)
    (cl-loop for page from first
             for source in sources
             for text in reflowed
             for with-headings in headed
             do (when-let* ((violations
                             (pdf-corpus-script--sweep-violations
                              (pdf-corpus-violations
                               :source source
                               :reflowed text
                               :headed with-headings
                               :titles (pdf-corpus-titles outline page)))))
                  (dolist (violation violations)
                    (cl-incf (gethash (car violation) totals 0)))
                  (push (cons page violations) ranked)))
    (setq ranked (sort (nreverse ranked)
                       (lambda (a b) (< (length (cdr b)) (length (cdr a))))))
    (princ (format "%s: pages %d-%d rendered in %.1fs, %d of them break something\n"
                   (file-name-base file) first last elapsed (length ranked)))
    (maphash (lambda (kind n) (princ (format "  %s: %d page(s)\n" kind n))) totals)
    (dolist (entry (seq-take ranked 25))
      (princ (format "\npage %d (%d)\n" (car entry) (length (cdr entry))))
      (dolist (violation (cdr entry))
        (princ (format "  %s\n" (pdf-corpus-describe-violation violation)))))
    (when (< 25 (length ranked))
      (princ (format "\n... and %d more pages\n" (- (length ranked) 25))))))
