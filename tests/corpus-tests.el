;;; tests/pdf/corpus-tests.el --- the pdf-text corpus, run as specs -*- lexical-binding: t; -*-

;; Renders every case under tests/pdf/corpus/ from its stored line
;; records and holds it to its golden and to the invariants.  No PDF,
;; no epdfinfo, so a real book's page is a regression test like any
;; other spec.

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/pdf/autoload/pdf-text.el")
(load-module-file "tests/pdf/corpus.el")

;;; Fixtures

(defun pdf-corpus-tests--lines (&rest texts)
  "Line records for TEXTS, geometry enough to reflow them as one column."
  (let ((base 0.10))
    (mapcar (lambda (text)
              (setq base (+ base 0.02))
              (pdf-text-line-create
               :text text :x0 0.10 :x1 0.90 :base base
               :top (- base 0.015) :bot base :height 0.015 :space 0.005
               :cv 0.3 :first-width 0.03))
            texts)))

(defun pdf-corpus-tests--difference (expected actual)
  "The first line EXPECTED and ACTUAL disagree on, as (N EXPECTED ACTUAL).
Nil when they agree.  A whole page printed twice buries the one line
that moved; this reports the line and its number."
  (let ((want (split-string (or expected "") "\n"))
        (got (split-string (or actual "") "\n"))
        (n 0)
        difference)
    (while (and (null difference) (or want got))
      (let ((a (pop want)) (b (pop got)))
        (cl-incf n)
        (unless (equal a b) (setq difference (list n a b)))))
    difference))

;;; The capture format

(describe "the corpus capture format"
  (it "round-trips a line record through the file it is written to"
    (let* ((line (car (pdf-corpus-tests--lines "a captured line")))
           (printed (pdf-corpus-print-lines
                     '(4 . 4) '((1 4 "Chapter")) '("well-known")
                     (list (cons 4 (list line)))))
           (read-back (with-temp-buffer
                        (insert printed)
                        (goto-char (point-min))
                        (read (current-buffer))))
           (record (car (cdr (car (plist-get read-back :pages))))))
      (expect (plist-get read-back :window) :to-equal '(4 . 4))
      (expect (plist-get read-back :outline) :to-equal '((1 4 "Chapter")))
      (expect (plist-get read-back :vocabulary) :to-equal '("well-known"))
      (expect (pdf-text-line-text (pdf-corpus-line record))
              :to-equal "a captured line")
      (expect (pdf-text-line-x1 (pdf-corpus-line record)) :to-be-close-to 0.90 4)
      (expect (pdf-text-line-base (pdf-corpus-line record)) :to-be-close-to 0.12 4)))

  (it "writes a measurement no glyph established as nil"
    (let ((blank (pdf-text-line-create :text "")))
      (expect (pdf-corpus--print-record blank)
              :to-equal "  (\"\" nil nil nil nil nil nil nil nil nil)")))

  (it "builds a fresh record every time, so one render cannot tag the next"
    (let* ((record (pdf-corpus-record (car (pdf-corpus-tests--lines "text"))))
           (first (pdf-corpus-line record)))
      (setf (pdf-text-line-kind first) 'mono)
      (expect (pdf-text-line-kind (pdf-corpus-line record)) :to-be nil))))

;;; Invariants

(describe "pdf-corpus-diff"
  (it "sees no change in a reflow that only re-wraps"
    (let ((diff (pdf-corpus-diff '("one two" "three four") "one two three four")))
      (expect (plist-get diff :lost) :to-be nil)
      (expect (plist-get diff :added) :to-be nil)))

  (it "sees no change in de-hyphenation or an inserted list dash"
    (expect (plist-get (pdf-corpus-diff '("• informa-" "tion") "- information")
                       :lost)
            :to-be nil))

  (it "names the dropped line, not the letters it was made of"
    ;; a running head shares letters with the prose around it, so a
    ;; character-by-character walk aligns it wrong and reports gibberish
    (let ((diff (pdf-corpus-diff '("INTRODUCTION | 7" "the body") "the body")))
      (expect (plist-get diff :lost) :to-equal '("INTRODUCTION | 7"))
      (expect (plist-get diff :added) :to-be nil)))

  (it "reads a line that survives in half as lost, with the half added"
    (let ((diff (pdf-corpus-diff '("a sentence that gets cut") "a sentence")))
      (expect (plist-get diff :lost) :to-equal '("a sentence that gets cut"))
      (expect (plist-get diff :added) :to-equal '("asentence"))))

  (it "keeps one dropped line from cascading over the ones after it"
    (let ((diff (pdf-corpus-diff '("PREFACE | xix" "first line" "second line")
                                 "first line second line")))
      (expect (plist-get diff :lost) :to-equal '("PREFACE | xix"))
      (expect (plist-get diff :added) :to-be nil)))

  (it "does not chase a dropped line to a later echo of its own words"
    ;; a section title also runs in the page's head, and the render lost
    ;; the heading itself; matching it against the running head would
    ;; strand every line of the section under it
    (let* ((body (make-string (* 2 pdf-corpus-insert-tolerance) ?a))
           (diff (pdf-corpus-diff (list "Supervised Segmentation" body
                                        "Chapter 3: Supervised Segmentation")
                                  (concat body "\nChapter 3: Supervised Segmentation"))))
      (expect (plist-get diff :lost) :to-equal '("Supervised Segmentation"))
      (expect (plist-get diff :added) :to-be nil)))

  (it "reports text the render invented"
    (expect (plist-get (pdf-corpus-diff '("body") "body extra") :added)
            :to-equal '("extra"))))

(describe "pdf-corpus-mid-sentence-breaks"
  (it "reports a paragraph broken between two lowercase words"
    (expect (pdf-corpus-mid-sentence-breaks "the sentence runs on\nand on here")
            :to-equal '(("the sentence runs on" . "and on here"))))

  (it "leaves a sentence that ends and a line that starts a new one"
    (expect (pdf-corpus-mid-sentence-breaks "The sentence ends.\nAnother begins")
            :to-be nil))

  (it "does not read a capital as lowercase in batch"
    ;; case-fold-search defaults to t there, where [[:lower:]] matches A
    (expect (pdf-corpus-mid-sentence-breaks "an opening clause,\nAnd a capital")
            :to-be nil)))

(describe "pdf-corpus-page-marker-lines"
  (it "reports a folio the running-head removal left behind"
    (expect (pdf-corpus-page-marker-lines "body text\n17\nmore body")
            :to-equal '("17")))

  (it "leaves an enumerator alone"
    (expect (pdf-corpus-page-marker-lines "1. first item") :to-be nil)))

(describe "pdf-corpus-outline-placements"
  (it "accepts a title the page carries once"
    (expect (pdf-corpus-outline-placements
             '("Sections and Notation") "** Sections and Notation\n\nthe body")
            :to-be nil))

  (it "reports a heading prepended above the line it names"
    (expect (pdf-corpus-outline-placements
             '("Sections and Notation")
             "** Sections and Notation\n\nprose\n\nSections and Notation\n\nmore")
            :to-equal '(("Sections and Notation" . 2))))

  (it "reports a title the page never says"
    (expect (pdf-corpus-outline-placements '("Figure 1-1") "prose only")
            :to-equal '(("Figure 1-1" . 0))))

  (it "matches through case and typography"
    (expect (pdf-corpus-outline-placements '("Sections and Notation")
                                           "** SECTIONS AND NOTATION")
            :to-be nil)))

(describe "pdf-corpus-empty-headings"
  (it "reports a heading with nothing between it and the next"
    ;; the Round 4 report, as the reader met it: the section folds shut
    ;; on nothing and the one after it owns the whole page
    (expect (pdf-corpus-empty-headings
             "** Other Skills and Concepts\n** Sections and Notation\nthe body")
            :to-equal '("Other Skills and Concepts")))

  (it "leaves a section that opens with its own first subsection"
    ;; the book sets the subsection title straight under the section
    ;; title; folded, that section shows the child, not nothing
    (expect (pdf-corpus-empty-headings
             "** Some Important Technical Details\n*** Heterogeneous Attributes\nbody")
            :to-be nil))

  (it "exempts the last heading, whose section runs on past the page"
    (expect (pdf-corpus-empty-headings "** Sections and Notation")
            :to-be nil))

  (it "counts the text a heading owns, not the lines it spans"
    (expect (pdf-corpus-empty-headings "** One\n\n\n** Two\nbody")
            :to-equal '("One"))))

(describe "pdf-corpus-violations"
  (it "reports nothing about a clean page"
    (expect (pdf-corpus-violations
             :source '("The sentence ends." "A second one.")
             :reflowed "The sentence ends. A second one.")
            :to-be nil))

  (it "licenses the running head a case declares, and no other loss"
    (expect (pdf-corpus-violations
             :source '("PREFACE | xix" "the body")
             :reflowed "the body"
             :drops '("PREFACE | xix"))
            :to-be nil)
    (expect (mapcar #'car (pdf-corpus-violations
                           :source '("PREFACE | xix" "a footnote line" "the body")
                           :reflowed "the body"
                           :drops '("PREFACE | xix")))
            :to-equal '(lost)))

  (it "spends a mid-sentence budget before it complains"
    (expect (pdf-corpus-violations
             :source '("display maths follows," "and the sentence resumes")
             :reflowed "display maths follows,\nand the sentence resumes"
             :budget '(:mid-sentence 1))
            :to-be nil)
    (expect (mapcar #'car (pdf-corpus-violations
                           :source '("display maths follows," "and the sentence resumes")
                           :reflowed "display maths follows,\nand the sentence resumes"))
            :to-equal '(mid-sentence)))

  (it "judges heading placement on the reader's text, not the reflow"
    (expect (mapcar #'car (pdf-corpus-violations
                           :source '("Sections and Notation" "The body follows.")
                           :reflowed "Sections and Notation\n\nThe body follows."
                           :headed (concat "** Sections and Notation\n\n"
                                           "Sections and Notation\n\nThe body follows.")
                           :titles '("Sections and Notation")))
            :to-equal '(outline))))

;;; The cases themselves

(describe "pdf-corpus-render"
  (it "keeps a window's pages at the numbers they have in the book"
    ;; the outline is keyed by the book's page numbers, so a window that
    ;; renders as pages 1 and 2 would take another chapter's headings
    (let* ((case (list :window '(4 . 5)
                       :outline '((1 4 "Chapter One") (1 5 "Chapter Two"))
                       :pages (list (cons 4 (pdf-corpus-tests--lines "page four."))
                                    (cons 5 (pdf-corpus-tests--lines "page five.")))))
           (rendered (pdf-corpus-render case)))
      (expect (mapcar #'car (plist-get rendered :reflowed)) :to-equal '(4 5))
      (expect (alist-get 4 (plist-get rendered :reflowed)) :to-equal "page four.")
      (expect (alist-get 4 (plist-get rendered :headed))
              :to-equal "* Chapter One\npage four.")
      (expect (alist-get 5 (plist-get rendered :headed))
              :to-equal "* Chapter Two\npage five."))))

(defun pdf-corpus-tests--kinds (violations)
  "The kinds VIOLATIONS are of, once each, in a fixed order."
  (sort (seq-uniq (mapcar #'car violations))
        (lambda (a b) (string< (symbol-name a) (symbol-name b)))))

(dolist (slug (pdf-corpus-slugs))
  (let* ((case (pdf-corpus-read slug))
         (meta (plist-get case :meta))
         (page (pdf-corpus-subject case))
         (tolerated (pdf-corpus-tests--kinds
                     (mapcar #'list (plist-get meta :tolerates))))
         (rendered nil)
         (render (lambda ()
                   (or rendered (setq rendered (pdf-corpus-render case)))))
         (subject (lambda ()
                    (alist-get page (plist-get (funcall render) :headed)))))
    (describe (format "corpus case %s (%s p%d)"
                      slug (or (plist-get meta :book) "?") (or page 0))
      (it "renders its golden"
        (expect (pdf-corpus-tests--difference
                 (plist-get case :golden) (funcall subject))
                :to-be nil))

      (it (if tolerated
              (format "breaks nothing but the %s it declares"
                      (string-join (mapcar #'symbol-name tolerated) " and "))
            "breaks no invariant")
        ;; an exact set, not a floor: the day a declared defect is fixed
        ;; this fails and says so, instead of a stale licence outliving it
        ;; the verdict is computed outside the `expect' form on purpose:
        ;; buttercup formats the expression's own source into the failure
        ;; message, and a % in there is read as a format directive
        (let* ((violations (pdf-corpus-case-violations case (funcall render)))
               (kinds (pdf-corpus-tests--kinds violations))
               (verdict (cond
                         ((equal kinds tolerated) 'as-declared)
                         (violations (mapcar #'pdf-corpus-describe-violation violations))
                         (t (list "declared defects are gone: drop :tolerates from"
                                  "case.eld and run bb corpus-accept")))))
          (expect verdict :to-equal 'as-declared))))))
