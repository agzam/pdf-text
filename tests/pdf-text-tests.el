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
            :to-equal "It ends here.  And resumes after"))

  (it "keeps TOC entry lines separate, joining only wrapped continuations"
    (expect (pdf-text-unfill
             (concat "contents\n"
                     "PRAISE FOR A MIND FOR NUMBERS\n"
                     "TITLE PAGE\n"
                     "COPYRIGHT\n"
                     "FOREWORD by Terrence J. Sejnowski, Francis Crick Professor, Salk Institute for\n"
                     "Biological Studies\n"
                     "NOTE TO THE READER\n"
                     "1 Open the Door"))
            :to-equal
            (concat "contents\n"
                    "PRAISE FOR A MIND FOR NUMBERS\n"
                    "TITLE PAGE\n"
                    "COPYRIGHT\n"
                    "FOREWORD by Terrence J. Sejnowski, Francis Crick Professor, Salk Institute for Biological Studies\n"
                    "NOTE TO THE READER\n"
                    "1 Open the Door")))

  (it "does not join across a short line: it ended its paragraph"
    (expect (pdf-text-unfill
             (concat "The final short line.\n"
                     "A new paragraph begins here and runs to the full width of the page\n"
                     "continuing on"))
            :to-equal
            (concat "The final short line.\n"
                    "A new paragraph begins here and runs to the full width of the page continuing on")))

  (it "starts a new paragraph at a first-line indent"
    (expect (pdf-text-unfill
             (concat "This paragraph runs to a full width line here\n"
                     "   The next paragraph opens indented and goes\n"
                     "on further"))
            :to-equal
            (concat "This paragraph runs to a full width line here\n"
                    "   The next paragraph opens indented and goes on further")))

  (it "starts fresh at a numbered entry even after a full line"
    (expect (pdf-text-unfill
             (concat "Why Trying Too Hard Can Sometimes Be Part of the Problem\n"
                     "3 Learning Is Creating:"))
            :to-equal
            (concat "Why Trying Too Hard Can Sometimes Be Part of the Problem\n"
                    "3 Learning Is Creating:"))))

(defun pdf-text-tests--geo (&rest pairs)
  "Geometry table from PAIRS of (LINE-TEXT X0 . X1)."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (p pairs table)
      (puthash (car p) (cons (cadr p) (cddr p)) table))))

(describe "pdf-text-unfill with geometry"
  (it "joins on glyph fullness though footnote chars inflate the page width"
    ;; the footnote line has the most characters, so the char threshold
    ;; alone would call the body lines short and break mid-paragraph
    (expect (pdf-text-unfill
             (concat "Body line one continues\n"
                     "body ends now.\n"
                     "A footnote in tiny type that runs much longer in characters than the body")
             (pdf-text-tests--geo
              '("Body line one continues" 0.1 . 0.9)
              '("body ends now." 0.1 . 0.5)
              '("A footnote in tiny type that runs much longer in characters than the body" 0.1 . 0.9))
             (cons 0.9 0.025))
            :to-equal
            (concat "Body line one continues body ends now.\n"
                    "A footnote in tiny type that runs much longer in characters than the body")))

  (it "breaks at a geometric indent and renders it as a two-space prefix"
    (expect (pdf-text-unfill
             (concat "First paragraph line runs the full width\n"
                     "Second paragraph starts here\n"
                     "and continues at the margin")
             (pdf-text-tests--geo
              '("First paragraph line runs the full width" 0.1 . 0.9)
              '("Second paragraph starts here" 0.13 . 0.9)
              '("and continues at the margin" 0.1 . 0.9))
             (cons 0.9 0.025))
            :to-equal
            (concat "First paragraph line runs the full width\n"
                    "  Second paragraph starts here and continues at the margin")))

  (it "rejoins a drop cap and its inset lines, resuming at the margin"
    (expect (pdf-text-unfill
             (concat "T\n"
                     "his inset line continues on\n"
                     "still inset here fully wide\n"
                     "back at margin now and wide\n"
                     "one more margin line here padding")
             (pdf-text-tests--geo
              '("T" 0.1 . 0.13)
              '("his inset line continues on" 0.15 . 0.9)
              '("still inset here fully wide" 0.15 . 0.9)
              '("back at margin now and wide" 0.1 . 0.9)
              '("one more margin line here padding" 0.1 . 0.9))
             (cons 0.9 0.025))
            :to-equal
            (concat "This inset line continues on still inset here fully wide"
                    " back at margin now and wide one more margin line here padding")))

  (it "joins a hanging continuation under a wrapped inset list item"
    (expect (pdf-text-unfill
             (concat "Prose paragraph before the list runs wide\n"
                     "1. An item line that wraps fully wide\n"
                     "continuation hangs deeper\n"
                     "Prose after the list also runs wide")
             (pdf-text-tests--geo
              '("Prose paragraph before the list runs wide" 0.1 . 0.9)
              '("1. An item line that wraps fully wide" 0.13 . 0.9)
              '("continuation hangs deeper" 0.145 . 0.6)
              '("Prose after the list also runs wide" 0.1 . 0.9))
             (cons 0.9 0.025))
            :to-equal
            (concat "Prose paragraph before the list runs wide\n"
                    "  1. An item line that wraps fully wide continuation hangs deeper\n"
                    "Prose after the list also runs wide")))

  (it "ends an inset block at its dedent instead of joining the prose"
    (expect (pdf-text-unfill
             (concat "Prose opening line also fully wide.\n"
                     "Prose intro line ending here wide\n"
                     "listing one inset\n"
                     "listing two inset runs fully wide\n"
                     "Prose resumes at margin after")
             (pdf-text-tests--geo
              '("Prose opening line also fully wide." 0.1 . 0.9)
              '("Prose intro line ending here wide" 0.1 . 0.9)
              '("listing one inset" 0.14 . 0.5)
              '("listing two inset runs fully wide" 0.14 . 0.9)
              '("Prose resumes at margin after" 0.1 . 0.9))
             (cons 0.9 0.025))
            :to-equal
            (concat "Prose opening line also fully wide. Prose intro line ending here wide\n"
                    "  listing one inset\n"
                    "listing two inset runs fully wide\n"
                    "Prose resumes at margin after"))))

(describe "pdf-text--join-lines"
  (it "rejoins a drop cap without a space"
    (expect (pdf-text--join-lines "T" "his book") :to-equal "This book")))

(describe "pdf-text--modal-edge"
  (it "yields the mode, immune to outliers on either side"
    (let ((geos '((0.1 . 0.9) (0.1 . 0.9) (0.1 . 0.9) (0.05 . 0.93))))
      (expect (pdf-text--modal-edge geos #'car) :to-be-close-to 0.1 2)
      (expect (pdf-text--modal-edge geos #'cdr) :to-be-close-to 0.9 2))))

(describe "pdf-text--doc-right-edge"
  (it "reads tightly concentrated margins as justified: small slack"
    (let ((tables (list (pdf-text-tests--geo
                         '("a line" 0.1 . 0.9) '("b line" 0.1 . 0.901)
                         '("c line" 0.1 . 0.9) '("d line" 0.1 . 0.5)))))
      (expect (cdr (pdf-text--doc-right-edge tables))
              :to-equal pdf-text-full-slack)))

  (it "reads smeared margins as ragged: wide slack"
    (let ((tables (list (pdf-text-tests--geo
                         '("a line" 0.1 . 0.9) '("b line" 0.1 . 0.86)
                         '("c line" 0.1 . 0.82) '("d line" 0.1 . 0.79)))))
      (expect (cdr (pdf-text--doc-right-edge tables))
              :to-equal pdf-text-full-slack-ragged))))

(describe "pdf-text--page-geometry"
  (it "maps each matching line to its first and last glyph edges"
    (let ((table (pdf-text--page-geometry
                  "ab\ncd"
                  '((?a (0.10 0.1 0.11 0.12)) (?b (0.11 0.1 0.12 0.12))
                    (?\n (0.12 0.1 0.12 0.12))
                    (?c (0.20 0.2 0.21 0.22)) (?d (0.21 0.2 0.22 0.22))))))
      (expect (gethash "ab" table) :to-equal '(0.10 . 0.12))
      (expect (gethash "cd" table) :to-equal '(0.20 . 0.22))))

  (it "skips lines whose layout text disagrees, failing open"
    (let ((table (pdf-text--page-geometry
                  "ab\nXY"
                  '((?a (0.10 0.1 0.11 0.12)) (?b (0.11 0.1 0.12 0.12))
                    (?\n (0.12 0.1 0.12 0.12))
                    (?c (0.20 0.2 0.21 0.22)) (?d (0.21 0.2 0.22 0.22))))))
      (expect (gethash "ab" table) :to-be-truthy)
      (expect (gethash "XY" table) :to-be nil)
      (expect (gethash "cd" table) :to-be nil))))

(describe "pdf-text--layout-lines"
  (it "splits the glyph stream at newline glyphs"
    (expect (mapcar (lambda (l) (apply #'string (mapcar #'car l)))
                    (pdf-text--layout-lines
                     '((?a (0.1 0.1 0.2 0.2)) (?\n (0.2 0.1 0.2 0.2))
                       (?b (0.1 0.3 0.2 0.4)) (?c (0.2 0.3 0.3 0.4)))))
            :to-equal '("a" "bc"))))

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

(describe "pdf-text--drop-split-echoes"
  (it "drops a following run that re-spells the previous line"
    (expect (pdf-text--drop-split-echoes
             '("PATTERNS OF CONFLICT" "PATTERNS OF" "CONFLICT" "body"))
            :to-equal '("PATTERNS OF CONFLICT" "body")))

  (it "keeps partial overlaps that never equal the line"
    (expect (pdf-text--drop-split-echoes
             '("PATTERNS OF CONFLICT" "PATTERNS OF" "WAR"))
            :to-equal '("PATTERNS OF CONFLICT" "PATTERNS OF" "WAR")))

  (it "stops the candidate run at a blank line"
    (expect (pdf-text--drop-split-echoes '("refrain" "" "refrain"))
            :to-equal '("refrain" "" "refrain"))))

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
             '("14 | ABSTRACT\nABSTRACT ABSTRACT\n\nbody one runs to a full width line\ncontinues"
               "ABSTRACT | 15\nbody two"
               "16 | ABSTRACT\nbody three"
               "ABSTRACT | 17\nbody four"
               "18 | ABSTRACT\nbody five"
               "ABSTRACT | 19\nbody six"))
            :to-equal '("ABSTRACT\n\nbody one runs to a full width line continues"
                        "body two" "body three"
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
             '(((depth . 1) (type . goto-dest) (title . "One") (page . 1))
               ((depth . 1) (type . goto-dest) (title . "Two") (page . 2))))
            :to-equal '("* One\nfront" "* Two\nchapter body")))

  (it "stacks several entries landing on one page in outline order"
    (expect (pdf-text--interleave-outline
             '("body" "later")
             '(((depth . 1) (title . "Ch") (page . 1))
               ((depth . 2) (title . "Sec") (page . 1))
               ((depth . 1) (title . "Next") (page . 2))))
            :to-equal '("* Ch\n** Sec\nbody" "* Next\nlater")))

  (it "drops entries without a usable page or title, trims the rest"
    (expect (pdf-text--interleave-outline
             '("a" "b")
             '(((depth . 1) (type . uri) (title . "Site") (uri . "x"))
               ((depth . 1) (title . "Broken") (page . 0))
               ((depth . 1) (title . "  ") (page . 1))
               ((depth . 3) (title . " Deep ") (page . 1))
               ((depth . 3) (title . " Deeper ") (page . 2))))
            :to-equal '("*** Deep\na" "*** Deeper\nb")))

  (it "renders a lone top-level entry as #+TITLE and promotes the rest"
    (expect (pdf-text--interleave-outline
             '("front" "ch one" "sec body")
             '(((depth . 1) (title . "The Book") (page . 1))
               ((depth . 2) (title . "One") (page . 2))
               ((depth . 3) (title . "Detail") (page . 3))))
            :to-equal '("#+TITLE: The Book\nfront" "* One\nch one" "** Detail\nsec body")))

  (it "renders a one-entry outline as #+TITLE alone"
    (expect (pdf-text--interleave-outline
             '("only page")
             '(((depth . 1) (title . "Pamphlet") (page . 1))))
            :to-equal '("#+TITLE: Pamphlet\nonly page")))

  (it "returns pages unchanged without an outline"
    (expect (pdf-text--interleave-outline '("a" "b") nil)
            :to-equal '("a" "b"))))

(describe "pdf-text--synthesize-headings"
  (it "promotes short numbered section lines, dot count as level"
    (expect (pdf-text--synthesize-headings
             '("2.2 Arguments\n2.2.1 Informal arguments\nA notion that is central to our work is that of argument here"))
            :to-equal
            '("** 2.2 Arguments\n*** 2.2.1 Informal arguments\nA notion that is central to our work is that of argument here")))

  (it "accepts a trailing dot on the section number"
    (expect (pdf-text--synthesize-headings
             '("1.2. Background material\nThis page has a long full width prose line to set the page width"))
            :to-equal
            '("** 1.2. Background material\nThis page has a long full width prose line to set the page width")))

  (it "leaves TOC entries ending in page numbers alone"
    (expect (pdf-text--synthesize-headings
             '("2.2 Arguments 20\nAnother line long enough to set the width for the page here"))
            :to-equal
            '("2.2 Arguments 20\nAnother line long enough to set the width for the page here")))

  (it "leaves long numbered lines alone"
    (expect (pdf-text--synthesize-headings
             '("2001 A Space Odyssey came out and this line runs the full width"))
            :to-equal
            '("2001 A Space Odyssey came out and this line runs the full width")))

  (it "leaves lowercase continuations alone"
    (expect (pdf-text--synthesize-headings
             '("3 apples fell\nA long full width line sets the page wrap column for this page"))
            :to-equal
            '("3 apples fell\nA long full width line sets the page wrap column for this page"))))

(describe "pdf-text--scanned-p"
  (it "flags an all-blank document"
    (expect (pdf-text--scanned-p '("" "  " "\n")) :to-be-truthy))

  (it "flags a book with only a stray text page or two"
    (expect (pdf-text--scanned-p
             (append '("some text" "more text") (make-list 249 "")))
            :to-be-truthy))

  (it "passes ordinary documents"
    (expect (pdf-text--scanned-p '("text" "text" "" "text")) :to-equal nil))

  (it "passes a single textual page"
    (expect (pdf-text--scanned-p '("just one page")) :to-equal nil)))

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

  (it "parses synthesized numbered headings as real headings"
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (pdf-text-mode)
        (pdf-text--insert-pages
         (pdf-text--synthesize-headings
          (pdf-text-render-pages
           '("2.1 Section One\nBody prose runs to the full width of the page right here\nmore")))))
      (font-lock-ensure)
      (expect (org-element-map (org-element-parse-buffer 'headline) 'headline
                (lambda (h) (list (org-element-property :level h)
                                  (org-element-property :raw-value h))))
              :to-equal '((2 "2.1 Section One")))))

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

(describe "pdf-text--page-end"
  (it "spans exactly each page's text against its start"
    (with-temp-buffer
      (pdf-text--insert-pages '("one" "two" "three"))
      (expect (buffer-substring (pdf-text--page-start 1) (pdf-text--page-end 1))
              :to-equal "one")
      (expect (buffer-substring (pdf-text--page-start 2) (pdf-text--page-end 2))
              :to-equal "two")))

  (it "runs the last page to the end of the buffer"
    (with-temp-buffer
      (pdf-text--insert-pages '("one" "two"))
      (expect (pdf-text--page-end 2) :to-equal (point-max))))

  (it "clamps out-of-range pages"
    (with-temp-buffer
      (pdf-text--insert-pages '("one" "two"))
      (expect (pdf-text--page-end 0) :to-equal (pdf-text--page-end 1))
      (expect (pdf-text--page-end 99) :to-equal (pdf-text--page-end 2)))))

(describe "pdf-text--page-position"
  (it "lands on the page start at fraction zero"
    (with-temp-buffer
      (pdf-text--insert-pages '("0123456789" "next"))
      (expect (pdf-text--page-position 1 0.0)
              :to-equal (pdf-text--page-start 1))))

  (it "interpolates by characters within the page"
    (with-temp-buffer
      (pdf-text--insert-pages '("0123456789" "next"))
      (expect (pdf-text--page-position 1 0.5)
              :to-equal (+ (pdf-text--page-start 1) 5))))

  (it "clamps the fraction into the page"
    (with-temp-buffer
      (pdf-text--insert-pages '("0123456789" "next"))
      (expect (pdf-text--page-position 1 2.0)
              :to-equal (pdf-text--page-end 1))
      (expect (pdf-text--page-position 1 -1.0)
              :to-equal (pdf-text--page-start 1)))))

(describe "pdf-text--file-stamp"
  (it "is stable while the file is untouched, moves with mtime"
    (let ((f (make-temp-file "pdf-text-stamp")))
      (unwind-protect
          (let ((before (pdf-text--file-stamp f)))
            (expect before :to-equal (pdf-text--file-stamp f))
            (set-file-times f (encode-time '(0 0 0 1 1 2000)))
            (expect (pdf-text--file-stamp f) :not :to-equal before))
        (delete-file f))))

  (it "moves with the render version, staling pre-change companions"
    (let ((f (make-temp-file "pdf-text-stamp")))
      (unwind-protect
          (let ((before (pdf-text--file-stamp f)))
            (expect (let ((pdf-text-render-version (1+ pdf-text-render-version)))
                      (pdf-text--file-stamp f))
                    :not :to-equal before))
        (delete-file f))))

  (it "is nil without a file and for a missing file"
    (expect (pdf-text--file-stamp nil) :to-be nil)
    (expect (pdf-text--file-stamp "/nonexistent/no.pdf") :to-be nil)))

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
