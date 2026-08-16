;;; tests/pdf/pdf-text-tests.el --- pdf/autoload/pdf-text.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/pdf/autoload/pdf-text.el")

(describe "pdf-text-unfill"
  (it "joins hard-wrapped lines into one paragraph line"
    (expect (pdf-text-unfill "Lorem ipsum dolor\nsit amet, consectetur\nadipiscing elit")
            :to-equal "Lorem ipsum dolor sit amet, consectetur adipiscing elit"))

  (it "keeps the first line's paragraph indent"
    (expect (pdf-text-unfill "  Lorem ipsum\ndolor sit")
            :to-equal "  Lorem ipsum dolor sit"))

  (it "preserves blank lines between paragraphs"
    (expect (pdf-text-unfill "one two\nthree\n\nfour five\nsix")
            :to-equal "one two three\n\nfour five six"))

  (it "preserves runs of blank lines"
    (expect (pdf-text-unfill "one\n\n\ntwo")
            :to-equal "one\n\n\ntwo"))

  (it "de-hyphenates a wrap hyphen before a lowercase continuation"
    (expect (pdf-text-unfill "modern informa-\ntion retrieval")
            :to-equal "modern information retrieval"))

  (it "keeps a compound's hyphen, joining without a space"
    (expect (pdf-text-unfill "the Navier-\nStokes equations")
            :to-equal "the Navier-Stokes equations"))

  (it "keeps a numeric range's hyphen, joining without a space"
    (expect (pdf-text-unfill "pages 3-\n10 cover it")
            :to-equal "pages 3-10 cover it"))

  (it "space-joins after a dangling hyphen"
    (expect (pdf-text-unfill "weights are x -\ny at most")
            :to-equal "weights are x - y at most"))

  (it "passes deeply indented lines through unjoined"
    (expect (pdf-text-unfill "Intro paragraph\n    (defun foo ()\n      (bar baz))\nclosing words")
            :to-equal "Intro paragraph\n    (defun foo ()\n      (bar baz))\nclosing words"))

  (it "passes lines with interior space runs through unjoined"
    (expect (pdf-text-unfill "Name    Type    Default\nfill    bool    nil")
            :to-equal "Name    Type    Default\nfill    bool    nil"))

  (it "treats a tab-led line as preformatted"
    (expect (pdf-text-unfill "para\n\tcode line\nmore para")
            :to-equal "para\n\tcode line\nmore para"))

  (it "still joins lines with double spaces after sentence ends"
    (expect (pdf-text-unfill "It ends here.  And\nresumes after")
            :to-equal "It ends here.  And resumes after")))

(describe "pdf-text-remove-recurring-lines"
  (it "strips a running header recurring at page tops"
    (expect (pdf-text-remove-recurring-lines
             '("INTRO | 1\nbody one" "INTRO | 2\nbody two"
               "INTRO | 3\nbody three" "INTRO | 4\nbody four"))
            :to-equal '("body one" "body two" "body three" "body four")))

  (it "strips alternating odd/even header forms"
    (expect (pdf-text-remove-recurring-lines
             '("HEAD | 1\na" "2 | HEAD\nb" "HEAD | 3\nc"
               "4 | HEAD\nd" "HEAD | 5\ne" "6 | HEAD\nf"))
            :to-equal '("a" "b" "c" "d" "e" "f")))

  (it "strips page-number footers"
    (expect (pdf-text-remove-recurring-lines
             '("body one\n7" "body two\n8" "body three\n9"))
            :to-equal '("body one" "body two" "body three")))

  (it "leaves non-recurring edge lines alone"
    (expect (pdf-text-remove-recurring-lines
             '("Chapter One\nfirst body" "Chapter Two\nsecond body"
               "Chapter Three\nthird body"))
            :to-equal '("Chapter One\nfirst body" "Chapter Two\nsecond body"
                        "Chapter Three\nthird body")))

  (it "strips qualified header forms mid-page too (2-up spreads embed them)"
    (expect (pdf-text-remove-recurring-lines
             '("HEAD | 1\nbody\nHEAD | 9\nmore" "HEAD | 2\nb" "HEAD | 3\nc"))
            :to-equal '("body\nmore" "b" "c"))))

(describe "pdf-text--collapse-doubled"
  (it "collapses a line painted twice"
    (expect (pdf-text--collapse-doubled "PATTERNS OF CONFLICT PATTERNS OF CONFLICT")
            :to-equal "PATTERNS OF CONFLICT"))

  (it "collapses a doubled single word"
    (expect (pdf-text--collapse-doubled "ABSTRACT ABSTRACT")
            :to-equal "ABSTRACT"))

  (it "leaves near-doubles alone"
    (expect (pdf-text--collapse-doubled "ABSTRACT ABSTRACTS")
            :to-equal "ABSTRACT ABSTRACTS"))

  (it "leaves ordinary prose alone"
    (expect (pdf-text--collapse-doubled "the quick brown fox")
            :to-equal "the quick brown fox")))

(describe "pdf-text--dedup-adjacent"
  (it "collapses the same title on adjacent lines"
    (expect (pdf-text--dedup-adjacent
             '("PATTERNS OF CONFLICT" "PATTERNS OF CONFLICT" "body"))
            :to-equal '("PATTERNS OF CONFLICT" "body")))

  (it "keeps identical lines separated by a blank"
    (expect (pdf-text--dedup-adjacent '("refrain" "" "refrain"))
            :to-equal '("refrain" "" "refrain")))

  (it "keeps blank runs"
    (expect (pdf-text--dedup-adjacent '("a" "" "" "b"))
            :to-equal '("a" "" "" "b"))))

(describe "pdf-text-render-pages"
  (it "strips headers, joins paragraphs, collapses doubled titles"
    (expect (pdf-text-render-pages
             '("14 | ABSTRACT\nABSTRACT ABSTRACT\n\nbody one\ncontinues"
               "ABSTRACT | 15\nbody two"
               "16 | ABSTRACT\nbody three"
               "ABSTRACT | 17\nbody four"
               "18 | ABSTRACT\nbody five"
               "ABSTRACT | 19\nbody six"))
            :to-equal '("ABSTRACT\n\nbody one continues" "body two" "body three"
                        "body four" "body five" "body six")))

  (it "collapses a shadow paint split across raw lines"
    (expect (pdf-text-render-pages
             '("PATTERNS OF CONFLICT\nPATTERNS OF\nCONFLICT\n\nbody"))
            :to-equal '("PATTERNS OF CONFLICT\n\nbody"))))

(describe "pdf-text--insert-pages"
  (it "separates pages with form-feed lines"
    (with-temp-buffer
      (pdf-text--insert-pages '("one" "two" "three"))
      (expect (buffer-string) :to-equal "one\n\f\ntwo\n\f\nthree\n")))

  (it "records each page's start position"
    (with-temp-buffer
      (pdf-text--insert-pages '("one" "two" "three"))
      (expect (append pdf-text--page-starts nil) :to-equal '(1 7 13))
      (expect (buffer-substring 7 10) :to-equal "two")))

  (it "trims page padding before insertion"
    (with-temp-buffer
      (pdf-text--insert-pages '("\n\nfoo  \n"))
      (expect (buffer-string) :to-equal "foo\n"))))

(describe "pdf-text--page-start"
  (it "clamps out-of-range pages"
    (with-temp-buffer
      (pdf-text--insert-pages '("one" "two"))
      (expect (pdf-text--page-start 0) :to-equal (pdf-text--page-start 1))
      (expect (pdf-text--page-start 99) :to-equal (pdf-text--page-start 2)))))

(describe "pdf-text-page-at-point"
  (it "counts form feeds above point"
    (with-temp-buffer
      (pdf-text--insert-pages '("alpha" "beta" "gamma"))
      (goto-char (point-min))
      (expect (pdf-text-page-at-point) :to-equal 1)
      (goto-char (point-max))
      (expect (pdf-text-page-at-point) :to-equal 3)))

  (it "round-trips with the page map"
    (with-temp-buffer
      (pdf-text--insert-pages '("alpha" "beta" "gamma" "delta"))
      (dolist (page '(1 2 3 4))
        (goto-char (pdf-text--page-start page))
        (expect (pdf-text-page-at-point) :to-equal page)))))
