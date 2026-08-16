;;; tests/pdf/pdf-text-tests.el --- pdf/autoload/pdf-text.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/pdf/autoload/pdf-text.el")

(require 'org)
(require 'org-element)

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

(describe "pdf-text-join-small-caps"
  (it "closes gaps in a multi-word small-caps line"
    (expect (pdf-text-join-small-caps "H OW TO D ESIGN P ROGRAMS")
            :to-equal "HOW TO DESIGN PROGRAMS"))

  (it "joins a two-pair line"
    (expect (pdf-text-join-small-caps "S ECOND E DITION")
            :to-equal "SECOND EDITION"))

  (it "joins chained single initials"
    (expect (pdf-text-join-small-caps
             "A N I NTRODUCTION TO P ROGRAMMING AND C OMPUTING")
            :to-equal "AN INTRODUCTION TO PROGRAMMING AND COMPUTING"))

  (it "joins a lone-pair heading line"
    (expect (pdf-text-join-small-caps "P REFACE") :to-equal "PREFACE"))

  (it "leaves a lone A or I pair alone (real one-letter words)"
    (expect (pdf-text-join-small-caps "A DISCOURSE") :to-equal "A DISCOURSE")
    (expect (pdf-text-join-small-caps "I AGREE") :to-equal "I AGREE"))

  (it "leaves a single pair inside a longer caps line alone"
    (expect (pdf-text-join-small-caps "U S NAVY") :to-equal "U S NAVY"))

  (it "leaves lines containing lowercase alone"
    (expect (pdf-text-join-small-caps "see EXHIBIT A NOW and SECTION B LATER")
            :to-equal "see EXHIBIT A NOW and SECTION B LATER"))

  (it "leaves ordinary prose alone"
    (expect (pdf-text-join-small-caps "the quick brown fox")
            :to-equal "the quick brown fox")))

(describe "pdf-text-render-pages"
  (it "closes small-caps gaps in the pipeline"
    (expect (pdf-text-render-pages '("H OW TO D ESIGN P ROGRAMS\nS ECOND E DITION"))
            :to-equal '("HOW TO DESIGN PROGRAMS SECOND EDITION")))

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

(describe "pdf-text--escape-org-lines"
  (it "neutralizes headline-looking bullet lines"
    (expect (pdf-text--escape-org-lines "* item\n** sub")
            :to-equal "\u200B* item\n\u200B** sub"))

  (it "neutralizes keyword and block lines, indented too"
    (expect (pdf-text--escape-org-lines "#+TITLE: x\n  #+end_src")
            :to-equal "\u200B#+TITLE: x\n\u200B  #+end_src"))

  (it "neutralizes drawer and property lines"
    (expect (pdf-text--escape-org-lines ":PROPERTIES:\n  :END:")
            :to-equal "\u200B:PROPERTIES:\n\u200B  :END:"))

  (it "leaves prose, inline stars, and bare star runs alone"
    (expect (pdf-text--escape-org-lines
             "a * b\n*emphasis* text\n***\nplain :not a drawer: here")
            :to-equal "a * b\n*emphasis* text\n***\nplain :not a drawer: here"))

  (it "yields no org headlines once inserted into an org buffer"
    (with-temp-buffer
      (insert (pdf-text--escape-org-lines "* item\n** sub\nbody"))
      (org-mode)
      (font-lock-ensure)
      (expect (org-element-map (org-element-parse-buffer 'headline)
                  'headline #'identity)
              :to-equal nil)))

  (it "keeps the escaped line searchable as typed"
    (with-temp-buffer
      (insert (pdf-text--escape-org-lines "* item one"))
      (goto-char (point-min))
      (expect (search-forward "* item one" nil t) :to-be-truthy))))

(describe "pdf-text--interleave-outline"
  (it "prepends a heading of the entry's depth at its page start"
    (expect (pdf-text--interleave-outline
             '("front" "chapter body")
             '(((depth . 1) (type . goto-dest) (title . "One") (page . 2))))
            :to-equal '("front" "* One\nchapter body")))

  (it "stacks several entries landing on one page in outline order"
    (expect (pdf-text--interleave-outline
             '("body")
             '(((depth . 1) (title . "Ch") (page . 1))
               ((depth . 2) (title . "Sec") (page . 1))))
            :to-equal '("* Ch\n** Sec\nbody")))

  (it "drops entries without a usable page or title, trims the rest"
    (expect (pdf-text--interleave-outline
             '("a" "b")
             '(((depth . 1) (type . uri) (title . "Site") (uri . "x"))
               ((depth . 1) (title . "Broken") (page . 0))
               ((depth . 1) (title . "  ") (page . 1))
               ((depth . 3) (title . " Deep ") (page . 2))))
            :to-equal '("a" "*** Deep\nb")))

  (it "returns pages unchanged without an outline"
    (expect (pdf-text--interleave-outline '("a" "b") nil)
            :to-equal '("a" "b"))))

(describe "pdf-text org structure"
  (it "builds headings matching the outline, escaped bullets staying text"
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (pdf-text-mode)
        (pdf-text--insert-pages
         (pdf-text--interleave-outline
          (pdf-text-render-pages
           '("Front matter" "Alpha starts\n\n* bullet stays text" "Beta starts"))
          '(((depth . 1) (title . "Alpha") (page . 2))
            ((depth . 2) (title . "Detail") (page . 2))
            ((depth . 1) (title . "Beta") (page . 3))))))
      (font-lock-ensure)
      (expect (org-element-map (org-element-parse-buffer 'headline) 'headline
                (lambda (h) (list (org-element-property :level h)
                                  (org-element-property :raw-value h))))
              :to-equal '((1 "Alpha") (2 "Detail") (1 "Beta")))))

  (it "keeps the page map round-tripping with headings interleaved"
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (pdf-text-mode)
        (pdf-text--insert-pages
         (pdf-text--interleave-outline
          '("front" "alpha body" "beta body")
          '(((depth . 1) (title . "Alpha") (page . 2))
            ((depth . 1) (title . "Beta") (page . 3))))))
      (dolist (page '(1 2 3))
        (goto-char (pdf-text--page-start page))
        (expect (pdf-text-page-at-point) :to-equal page))))

  (it "overview-folds chapter bodies, leaving front matter visible"
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (pdf-text-mode)
        (pdf-text--insert-pages
         (pdf-text--interleave-outline
          '("front matter" "alpha body" "beta body")
          '(((depth . 1) (title . "Alpha") (page . 2))
            ((depth . 1) (title . "Beta") (page . 3)))))
        (org-cycle-overview))
      (goto-char (point-min))
      (search-forward "alpha body")
      (expect (invisible-p (1- (point))) :to-be-truthy)
      (goto-char (point-min))
      (search-forward "front matter")
      (expect (invisible-p (1- (point))) :to-equal nil))))

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
