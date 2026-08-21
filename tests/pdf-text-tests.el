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

;;; Fixtures

(cl-defun pdf-text-tests--line (text &key (x0 0.10) (x1 0.90) (base 0.10)
                                     (height 0.015) (space 0.005) (cv 0.3)
                                     first-width
                                     &allow-other-keys)
  "A line record for TEXT with page-relative geometry.
The defaults describe a body line filling a column that runs 0.10 to
0.90; a spec overrides only what its case is about."
  (pdf-text-line-create
   :text text :x0 x0 :x1 x1 :base base :top (- base height) :bot base
   :height height :space space :cv cv
   :first-width (or first-width
                    ;; a character measures about 0.01 of the page
                    (* 0.01 (length (car (split-string (string-trim text))))))))

(defun pdf-text-tests--page (specs)
  "Line records for one page from SPECS, each (TEXT . PROPS).
PROPS take the `pdf-text-tests--line' keywords plus :gap, the baseline
step down from the line above in leadings (1 by default)."
  (let ((base 0.10) (leading 0.02) started)
    (mapcar (lambda (spec)
              (let ((props (cdr spec)))
                (setq base (cond ((plist-get props :base))
                                 (started (+ base (* (or (plist-get props :gap) 1)
                                                     leading)))
                                 (t base))
                      started t)
                (apply #'pdf-text-tests--line (car spec) :base base props)))
            specs)))

(defun pdf-text-tests--render (specs)
  "SPECS rendered as one page, the way `pdf-text-render-pages' renders it."
  (let* ((lines (pdf-text-tests--page specs))
         (profile (pdf-text--profile (list lines)))
         (page (pdf-text--page-profile lines profile)))
    (pdf-text--render-blocks (pdf-text--blocks lines page) page
                             (pdf-text--hyphenated-words (list lines)))))

(defun pdf-text-tests--glyphs (text &optional x0 base width height)
  "Charlayout glyphs for TEXT laid out from X0 at BASE, WIDTH per glyph.
Glyph widths alternate around WIDTH unless WIDTH is given, so a fixture
line reads as proportional type; pass an explicit WIDTH for a
monospaced one."
  (let* ((x (or x0 0.10))
         (base (or base 0.12))
         (height (or height 0.015))
         (n -1))
    (mapcar (lambda (char)
              (let ((advance (or width (if (cl-oddp (cl-incf n)) 0.006 0.014))))
                (prog1 (list char (list x (- base height) (+ x advance) base))
                  (setq x (+ x advance)))))
            (append text nil))))

;;; Line records and profile

(describe "pdf-text--glyph-line"
  (it "measures the line's edges, baseline, and first word"
    (let ((line (pdf-text--glyph-line (pdf-text-tests--glyphs "ab cd" nil nil 0.01))))
      (expect (pdf-text-line-text line) :to-equal "ab cd")
      (expect (pdf-text-line-x0 line) :to-be-close-to 0.10 3)
      (expect (pdf-text-line-x1 line) :to-be-close-to 0.15 3)
      (expect (pdf-text-line-base line) :to-be-close-to 0.12 3)
      (expect (pdf-text-line-height line) :to-be-close-to 0.015 3)
      (expect (pdf-text-line-first-width line) :to-be-close-to 0.02 3)))

  (it "reports even advances as a monospaced line"
    (expect (pdf-text-line-cv (pdf-text--glyph-line
                              (pdf-text-tests--glyphs "let x = 1;" nil nil 0.01)))
            :to-be-close-to 0.0 3))

  (it "reports uneven advances as proportional type"
    (expect (pdf-text-line-cv (pdf-text--glyph-line
                              (pdf-text-tests--glyphs "ordinary prose here")))
            :to-be-greater-than pdf-text-monospace-variation))

  (it "keeps a blank line, geometry and all, out of the measurements"
    (let ((line (pdf-text--glyph-line nil)))
      (expect (pdf-text-line-text line) :to-equal "")
      (expect (pdf-text-line-x0 line) :to-be nil))))

(describe "pdf-text--page-lines"
  (it "takes text and geometry from the same glyph stream"
    (let ((lines (pdf-text--page-lines
                  "ignored"
                  (append (pdf-text-tests--glyphs "ab" nil nil 0.01)
                          (list (list ?\n '(0.12 0.10 0.12 0.12)))
                          (pdf-text-tests--glyphs "cd" 0.10 0.14 0.01)))))
      (expect (mapcar #'pdf-text-line-text lines) :to-equal '("ab" "cd"))
      (expect (pdf-text-line-base (nth 1 lines)) :to-be-close-to 0.14 3)))

  (it "falls back to the plain text lines without a layout"
    (let ((lines (pdf-text--page-lines "one\ntwo")))
      (expect (mapcar #'pdf-text-line-text lines) :to-equal '("one" "two"))
      (expect (pdf-text-line-x0 (car lines)) :to-be nil))))

(describe "pdf-text--profile"
  (it "reports the modal body geometry, not the extremes"
    (let* ((lines (pdf-text-tests--page
                   '(("heading" :x0 0.30 :x1 0.60 :height 0.03)
                     ("body one") ("body two") ("body three")
                     ("page number" :x0 0.85 :x1 0.88 :height 0.008))))
           (profile (pdf-text--profile (list lines))))
      (expect (plist-get profile :height) :to-be-close-to 0.015 3)
      (expect (plist-get profile :leading) :to-be-close-to 0.02 3)
      (expect (plist-get profile :left) :to-be-close-to 0.10 2)
      (expect (plist-get profile :right) :to-be-close-to 0.90 2)))

  (it "is all nil for pages that carry no geometry"
    (let ((profile (pdf-text--profile (list (pdf-text--page-lines "a\nb")))))
      (expect (plist-get profile :height) :to-be nil)
      (expect (plist-get profile :right) :to-be nil))))

(describe "pdf-text--page-profile"
  (it "takes the column edges from the page, as mirrored margins need"
    (let* ((doc '(:height 0.015 :leading 0.02 :left 0.10 :right 0.80 :space 0.005))
           (lines (pdf-text-tests--page
                   (make-list 9 '("body line here" :x0 0.20 :x1 0.90))))
           (page (pdf-text--page-profile lines doc)))
      (expect (plist-get page :left) :to-be-close-to 0.20 2)
      (expect (plist-get page :right) :to-be-close-to 0.90 2)))

  (it "keeps the document's edges on a page too thin to judge"
    (let* ((doc '(:height 0.015 :leading 0.02 :left 0.10 :right 0.80 :space 0.005))
           (lines (pdf-text-tests--page '(("lonely" :x0 0.20 :x1 0.90)))))
      (expect (pdf-text--page-profile lines doc) :to-equal doc)))

  (it "ignores lines set at another size, which have margins of their own"
    (let* ((doc '(:height 0.015 :leading 0.02 :left 0.10 :right 0.80 :space 0.005))
           (lines (pdf-text-tests--page
                   (append (make-list 9 '("body line here" :x0 0.20 :x1 0.90))
                           (make-list 5 '("note" :x0 0.05 :x1 0.15 :height 0.008)))))
           (page (pdf-text--page-profile lines doc)))
      (expect (plist-get page :left) :to-be-close-to 0.20 2))))

;;; Classification

(describe "pdf-text--mark-monospace"
  (it "tags a listing line by its even advances"
    (let ((lines (pdf-text-tests--page '(("let x = 1;" :cv 0.0)))))
      (pdf-text--mark-monospace lines)
      (expect (pdf-text-line-kind (car lines)) :to-equal 'mono)))

  (it "leaves prose alone"
    (let ((lines (pdf-text-tests--page '(("ordinary prose line" :cv 0.3)))))
      (pdf-text--mark-monospace lines)
      (expect (pdf-text-line-kind (car lines)) :to-be nil)))

  (it "does not read tabular figures as a listing"
    (let ((lines (pdf-text-tests--page '(("488" :cv 0.0)))))
      (pdf-text--mark-monospace lines)
      (expect (pdf-text-line-kind (car lines)) :to-be nil)))

  (it "takes a lone brace into the listing above it"
    (let ((lines (pdf-text-tests--page
                  '(("let x = 1;" :cv 0.0 :height 0.010)
                    ("}" :cv nil :height 0.010)))))
      (pdf-text--mark-monospace lines)
      (expect (pdf-text-line-kind (nth 1 lines)) :to-equal 'mono)))

  (it "leaves a short prose line out of a listing of another size"
    (let ((lines (pdf-text-tests--page
                  '(("let x = 1;" :cv 0.0 :height 0.010)
                    ("Paris." :cv nil :height 0.015)))))
      (pdf-text--mark-monospace lines)
      (expect (pdf-text-line-kind (nth 1 lines)) :to-be nil))))

(describe "pdf-text--mark-alignment"
  (it "tags a flush-right run whose left edge moves"
    (let* ((lines (pdf-text-tests--page
                   '(("We drew inspiration" :x0 0.08 :x1 0.21)
                     ("from Michael" :x0 0.13 :x1 0.21)
                     ("Jackson's method for" :x0 0.08 :x1 0.21))))
           (profile '(:height 0.015 :leading 0.02 :left 0.10 :right 0.90
                      :space 0.005)))
      (pdf-text--mark-alignment lines profile)
      (expect (mapcar #'pdf-text-line-align lines)
              :to-equal '(right right right))))

  (it "tags a centred run"
    (let* ((lines (pdf-text-tests--page
                   '(("program design deserves the same" :x0 0.30 :x1 0.70)
                     ("role as language skills" :x0 0.34 :x1 0.66))))
           (profile '(:height 0.015 :leading 0.02 :left 0.10 :right 0.90
                      :space 0.005)))
      (pdf-text--mark-alignment lines profile)
      (expect (mapcar #'pdf-text-line-align lines) :to-equal '(center center))))

  (it "leaves justified prose alone, first-line indent and all"
    (let* ((lines (pdf-text-tests--page
                   '(("An indented paragraph opening" :x0 0.13 :x1 0.90)
                     ("continues at the column margin" :x0 0.10 :x1 0.90))))
           (profile '(:height 0.015 :leading 0.02 :left 0.10 :right 0.90
                      :space 0.005)))
      (pdf-text--mark-alignment lines profile)
      (expect (mapcar #'pdf-text-line-align lines) :to-equal '(nil nil)))))

(describe "pdf-text--list-marker"
  (it "reads a bullet glyph anywhere"
    (expect (pdf-text--list-marker "• Business people who will be") :to-equal "•"))

  (it "reads an enumerator"
    (expect (pdf-text--list-marker "1. An item") :to-equal "1.")
    (expect (pdf-text--list-marker "2) Another item") :to-equal "2)")
    (expect (pdf-text--list-marker "(3) A third item") :to-equal "(3)")
    (expect (pdf-text--list-marker "iv. A fourth item") :to-equal "iv."))

  (it "reads a dash only where the line is indented"
    (expect (pdf-text--list-marker "- an item" t) :to-equal "-")
    (expect (pdf-text--list-marker "- an item") :to-be nil))

  (it "leaves prose and footnote markers alone"
    (expect (pdf-text--list-marker "*A word on scientific notation:" t) :to-be nil)
    (expect (pdf-text--list-marker "ordinary prose here" t) :to-be nil)
    (expect (pdf-text--list-marker "1961 was the year" t) :to-be nil)))

;;; Reflow

(describe "pdf-text reflow"
  (it "joins wrapped lines into one paragraph"
    (expect (pdf-text-tests--render
             '(("Lorem ipsum dolor sit amet")
               ("consectetur adipiscing elit")
               ("sed do eiusmod tempor" :x1 0.40)))
            :to-equal
            "Lorem ipsum dolor sit amet consectetur adipiscing elit sed do eiusmod tempor"))

  (it "separates paragraphs the page set apart with a blank line"
    (expect (pdf-text-tests--render
             '(("First paragraph line one")
               ("first paragraph ends here" :x1 0.40)
               ("Second paragraph opens" :gap 1.5)
               ("and runs on to its end" :x1 0.40)))
            :to-equal
            (concat "First paragraph line one first paragraph ends here\n\n"
                    "Second paragraph opens and runs on to its end")))

  (it "separates paragraphs marked only by a first-line indent"
    (expect (pdf-text-tests--render
             '(("First paragraph line one")
               ("first paragraph runs to the margin")
               ("Second paragraph opens indented" :x0 0.13)
               ("and returns to the margin" :x1 0.40)))
            :to-equal
            (concat "First paragraph line one first paragraph runs to the margin\n\n"
                    "Second paragraph opens indented and returns to the margin")))

  (it "breaks where the next line's first word would have fit"
    (expect (pdf-text-tests--render
             '(("A body paragraph filling the column")
               ("and wrapping to a second full line")
               ("and to a third full line as well")
               ("and to a fourth full line as well")
               ("that ends here." :x1 0.30)
               ("A short entry" :x1 0.40)
               ("Another short entry" :x1 0.45)))
            :to-equal
            (concat "A body paragraph filling the column and wrapping to a"
                    " second full line and to a third full line as well and to"
                    " a fourth full line as well that ends here.\n"
                    "A short entry\nAnother short entry")))

  (it "keeps a wrapped line joined when the next word would not have fit"
    (expect (pdf-text-tests--render
             '(("A line stopping just short" :x1 0.86)
               ("internationalisation follows" :first-width 0.09 :x1 0.40)))
            :to-equal "A line stopping just short internationalisation follows"))

  (it "keeps entries of one measure tight, with no blank line between"
    (expect (pdf-text-tests--render
             '(("A body paragraph filling the column")
               ("and wrapping to a second full line")
               ("and to a third full line as well")
               ("and to a fourth full line as well")
               ("that ends here." :x1 0.30)
               ("ACKNOWLEDGMENTS" :x1 0.40)
               ("ENDNOTES" :x1 0.30)
               ("REFERENCES" :x1 0.32)))
            :to-equal
            (concat "A body paragraph filling the column and wrapping to a"
                    " second full line and to a third full line as well and to"
                    " a fourth full line as well that ends here.\n"
                    "ACKNOWLEDGMENTS\nENDNOTES\nREFERENCES")))

  (it "rejoins a drop cap and carries the paragraph past its inset lines"
    (expect (pdf-text-tests--render
             '(("Y" :x0 0.10 :x1 0.13 :height 0.05)
               ("our brain has amazing abilities" :x0 0.16)
               ("but it did not come with one" :x0 0.16)
               ("a novice or an expert will find" :x0 0.10)
               ("great new ways to improve your" :x0 0.10)
               ("skills and your techniques for" :x0 0.10)
               ("learning, especially the ones" :x0 0.10)
               ("related to math and science." :x0 0.10 :x1 0.40)))
            :to-equal
            (concat "Your brain has amazing abilities but it did not come with one"
                    " a novice or an expert will find great new ways to improve your"
                    " skills and your techniques for learning, especially the ones"
                    " related to math and science.")))

  (it "joins a flush-right margin note into its own paragraph"
    ;; the note is a column of its own, off to the left of the body's
    (expect (pdf-text-tests--render
             '(("A body paragraph filling the column" :x0 0.30)
               ("and wrapping to a second full line" :x0 0.30)
               ("and to a third full line as well" :x0 0.30)
               ("and to a fourth full line as well" :x0 0.30)
               ("and ending here." :x0 0.30 :x1 0.50)
               ("We drew inspiration" :x0 0.08 :x1 0.21 :first-width 0.02 :gap 3)
               ("from Michael Jackson's" :x0 0.06 :x1 0.21 :first-width 0.04)
               ("method for COBOL." :x0 0.07 :x1 0.21 :first-width 0.06)))
            :to-equal
            (concat "A body paragraph filling the column and wrapping to a second"
                    " full line and to a third full line as well and to a fourth"
                    " full line as well and ending here.\n\n"
                    "We drew inspiration from Michael Jackson's method for COBOL.")))

  (it "keeps a line of aligned columns verbatim"
    (expect (pdf-text-tests--render
             '(("Name    Type    Default")
               ("fill    bool    nil")))
            :to-equal "Name    Type    Default\nfill    bool    nil")))

(describe "pdf-text lists"
  (it "renders bullet items as org list items, continuations joined"
    (expect (pdf-text-tests--render
             '(("Data Science for Business is intended for several sorts of readers:"
                :x1 0.67)
               ("• Business people who will be working with data scientists"
                :x0 0.12 :gap 1.7 :first-width 0.01)
               ("oriented projects, or investing in ventures," :x0 0.14 :x1 0.63)
               ("• Developers who will be implementing solutions, and"
                :x0 0.12 :x1 0.72 :gap 1.3 :first-width 0.01)
               ("• Aspiring data scientists." :x0 0.12 :x1 0.38 :gap 1.3
                :first-width 0.01)
               ("This is not a book about algorithms, nor is it" :gap 1.7)
               ("a replacement for a book about them, since the concepts")
               ("underlie the techniques rather than the other way round")
               ("and that is what the exposition follows here." :x1 0.40)))
            :to-equal
            (concat "Data Science for Business is intended for several sorts of readers:\n\n"
                    "- Business people who will be working with data scientists"
                    " oriented projects, or investing in ventures,\n\n"
                    "- Developers who will be implementing solutions, and\n\n"
                    "- Aspiring data scientists.\n\n"
                    "This is not a book about algorithms, nor is it"
                    " a replacement for a book about them, since the concepts"
                    " underlie the techniques rather than the other way round"
                    " and that is what the exposition follows here.")))

  (it "keeps an enumerator as the document wrote it"
    (expect (pdf-text-tests--render
             '(("1. The set of natural numbers" :x0 0.12 :x1 0.50)
               ("2. The set of integers" :x0 0.12 :x1 0.45)))
            :to-equal "1. The set of natural numbers\n2. The set of integers"))

  (it "nests a deeper marker under its parent"
    (expect (pdf-text-tests--render
             '(("• First item at the outer level" :x0 0.12 :x1 0.50
                :first-width 0.01)
               ("• A nested item further in" :x0 0.16 :x1 0.48 :first-width 0.01)
               ("• Back at the outer level again" :x0 0.12 :x1 0.52
                :first-width 0.01)))
            :to-equal
            (concat "- First item at the outer level\n"
                    "  - A nested item further in\n"
                    "- Back at the outer level again")))

  (it "takes an item's continuation from its second line, hanging or not"
    (expect (pdf-text-tests--render
             '(("(2) is countable because there is an injection" :x0 0.13)
               ("injective function." :x0 0.10 :x1 0.33)))
            :to-equal
            "(2) is countable because there is an injection injective function.")))

(describe "pdf-text listings"
  (it "keeps monospaced lines verbatim, indentation and all"
    (expect (pdf-text-tests--render
             '(("we can run the tests as follows:" :x1 0.40)
               ("$ cargo test" :x0 0.18 :x1 0.28 :cv 0.0 :height 0.010
                :space 0.008 :gap 1.6)
               ("fn main() {" :x0 0.18 :x1 0.27 :cv 0.0 :height 0.010
                :space 0.008)
               ("let x = 1;" :x0 0.21 :x1 0.30 :cv 0.0 :height 0.010
                :space 0.008)
               ("}" :x0 0.18 :x1 0.19 :cv nil :height 0.010)
               ("We can have test functions scattered" :gap 1.6)
               ("throughout our source tree." :x1 0.40)))
            :to-equal
            (concat "we can run the tests as follows:\n\n"
                    "  $ cargo test\n"
                    "  fn main() {\n"
                    "      let x = 1;\n"
                    "  }\n\n"
                    "We can have test functions scattered"
                    " throughout our source tree."))))

(describe "pdf-text--join-lines"
  (it "drops a wrap hyphen before a lowercase continuation"
    (expect (pdf-text--join-lines "modern informa-" "tion retrieval")
            :to-equal "modern information retrieval"))

  (it "drops a typographic or soft wrap hyphen too"
    (expect (pdf-text--join-lines "detailed algo\u2010" "rithmic steps")
            :to-equal "detailed algorithmic steps")
    (expect (pdf-text--join-lines "signifi\u00AD" "cant understanding")
            :to-equal "significant understanding"))

  (it "keeps the hyphen of a compound the document writes hyphenated"
    (let ((vocabulary (make-hash-table :test #'equal)))
      (puthash "well-known" t vocabulary)
      (expect (pdf-text--join-lines "many well-" "known algorithms" vocabulary)
              :to-equal "many well-known algorithms")))

  (it "keeps a compound broken at its own hyphen"
    (expect (pdf-text--join-lines "the Navier-" "Stokes equations")
            :to-equal "the Navier-Stokes equations")
    (expect (pdf-text--join-lines "pages 3-" "10 cover it")
            :to-equal "pages 3-10 cover it"))

  (it "closes up an en or em dash without eating it"
    (expect (pdf-text--join-lines "data science\u2013" "oriented projects")
            :to-equal "data science\u2013oriented projects")
    (expect (pdf-text--join-lines "the Big Bang\u2014" "forms too alien")
            :to-equal "the Big Bang\u2014forms too alien"))

  (it "space-joins after a dangling hyphen"
    (expect (pdf-text--join-lines "weights are x -" "y at most")
            :to-equal "weights are x - y at most"))

  (it "rejoins a drop cap without a space"
    (expect (pdf-text--join-lines "T" "his book") :to-equal "This book")))

(describe "pdf-text--hyphenated-words"
  (it "collects the document's own hyphenated words, stripped and downcased"
    (let ((table (pdf-text--hyphenated-words
                  (list (pdf-text-tests--page
                         '(("many well-known (data-driven) results")
                           ("no hyphen here")))))))
      (expect (gethash "well-known" table) :to-be-truthy)
      (expect (gethash "data-driven" table) :to-be-truthy)
      (expect (gethash "hyphen" table) :to-be nil))))

;;; Statistics

(describe "pdf-text--mode-value"
  (it "yields the mode, immune to outliers on either side"
    (expect (pdf-text--mode-value '(0.1 0.1 0.1 0.05 0.93) 0.005)
            :to-be-close-to 0.1 2))

  (it "buckets near-equal measurements together"
    (expect (pdf-text--mode-value '(0.0191 0.0199 0.0190 0.0350) 0.002)
            :to-be-close-to 0.02 2)))

(describe "pdf-text--quantile"
  (it "orders the values and picks at the fraction"
    (expect (pdf-text--quantile '(3 1 2 5 4) 0.5) :to-equal 3)
    (expect (pdf-text--quantile '(3 1 2 5 4) 0.0) :to-equal 1)
    (expect (pdf-text--quantile '(3 1 2 5 4) 1.0) :to-equal 5))

  (it "is nil for no values"
    (expect (pdf-text--quantile nil 0.5) :to-be nil)))

(describe "pdf-text--variation"
  (it "is zero for even spacing and grows with the spread"
    (expect (pdf-text--variation '(0.01 0.01 0.01)) :to-be-close-to 0.0 3)
    (expect (pdf-text--variation '(0.005 0.01 0.02)) :to-be-close-to 0.53 1))

  (it "is nil below two values"
    (expect (pdf-text--variation '(0.01)) :to-be nil)))

;;; Cleanups

(describe "pdf-text-remove-marginal-lines"
  (it "strips a running head and folio detached in the margin band"
    (let* ((pages (mapcar
                   (lambda (n)
                     (pdf-text-tests--page
                      `((,(format "INTRO | %d" n) :x0 0.10 :x1 0.30 :base 0.06)
                        ("body line one filling the column" :base 0.20)
                        ("body line two filling the column")
                        ("body line three filling the column")
                        ("body line four ends here" :x1 0.40))))
                   '(1 2 3)))
           (profile (pdf-text--profile pages))
           (profiles (mapcar (lambda (lines) (pdf-text--page-profile lines profile))
                             pages)))
      (expect (mapcar (lambda (lines) (mapcar #'pdf-text-line-text lines))
                      (pdf-text-remove-marginal-lines pages profiles))
              :to-equal (make-list 3 '("body line one filling the column"
                                       "body line two filling the column"
                                       "body line three filling the column"
                                       "body line four ends here")))))

  (it "strips a running head sharing the folio's baseline"
    (let* ((pages (mapcar
                   (lambda (n)
                     (pdf-text-tests--page
                      `(("body line one filling the column")
                        ("body line two filling the column")
                        ("body line three filling the column")
                        ("body line four ends here" :x1 0.40)
                        (,(number-to-string n) :x0 0.10 :x1 0.12 :base 0.93)
                        ("A Tour of Rust" :x0 0.20 :x1 0.34 :base 0.93))))
                   '(11 12 13)))
           (profile (pdf-text--profile pages))
           (profiles (mapcar (lambda (lines) (pdf-text--page-profile lines profile))
                             pages)))
      (expect (mapcar (lambda (lines) (mapcar #'pdf-text-line-text lines))
                      (pdf-text-remove-marginal-lines pages profiles))
              :to-equal (make-list 3 '("body line one filling the column"
                                       "body line two filling the column"
                                       "body line three filling the column"
                                       "body line four ends here")))))

  (it "keeps a footnote block, which is neither narrow nor detached"
    (let* ((pages (mapcar
                   (lambda (n)
                     (pdf-text-tests--page
                      `(("body line one filling the column")
                        ("body line two filling the column")
                        ("body line three filling the column")
                        ("body line four ends here" :x1 0.40)
                        ("* A footnote that runs the full measure" :gap 3
                         :height 0.010)
                        (,(format "a second footnote line here %d" n)
                         :height 0.010))))
                   '(1 2 3)))
           (profile (pdf-text--profile pages))
           (profiles (mapcar (lambda (lines) (pdf-text--page-profile lines profile))
                             pages)))
      (expect (length (car (pdf-text-remove-marginal-lines pages profiles)))
              :to-equal 6))))

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
    (expect (mapcar #'pdf-text-line-text
                    (pdf-text--dedup-adjacent
                     (pdf-text-tests--page
                      '(("PATTERNS OF CONFLICT") ("PATTERNS OF CONFLICT") ("body")))))
            :to-equal '("PATTERNS OF CONFLICT" "body")))

  (it "keeps identical lines separated by a blank"
    (expect (mapcar #'pdf-text-line-text
                    (pdf-text--dedup-adjacent
                     (pdf-text-tests--page '(("refrain") ("") ("refrain")))))
            :to-equal '("refrain" "" "refrain"))))

(describe "pdf-text--drop-split-echoes"
  (it "drops a following run that re-spells the previous line"
    (expect (mapcar #'pdf-text-line-text
                    (pdf-text--drop-split-echoes
                     (pdf-text-tests--page
                      '(("PATTERNS OF CONFLICT") ("PATTERNS OF") ("CONFLICT")
                        ("body")))))
            :to-equal '("PATTERNS OF CONFLICT" "body")))

  (it "keeps partial overlaps that never equal the line"
    (expect (mapcar #'pdf-text-line-text
                    (pdf-text--drop-split-echoes
                     (pdf-text-tests--page
                      '(("PATTERNS OF CONFLICT") ("PATTERNS OF") ("WAR")))))
            :to-equal '("PATTERNS OF CONFLICT" "PATTERNS OF" "WAR"))))

(describe "pdf-text-join-small-caps"
  (it "closes gaps in a multi-word small-caps line"
    (expect (pdf-text-join-small-caps "H OW TO D ESIGN P ROGRAMS")
            :to-equal "HOW TO DESIGN PROGRAMS"))

  (it "joins a two-pair line"
    (expect (pdf-text-join-small-caps "S ECOND E DITION")
            :to-equal "SECOND EDITION"))

  (it "joins a lone-pair heading line"
    (expect (pdf-text-join-small-caps "P REFACE") :to-equal "PREFACE"))

  (it "leaves a lone A or I pair alone (real one-letter words)"
    (expect (pdf-text-join-small-caps "A DISCOURSE") :to-equal "A DISCOURSE")
    (expect (pdf-text-join-small-caps "I AGREE") :to-equal "I AGREE"))

  (it "leaves a single pair inside a longer caps line alone"
    (expect (pdf-text-join-small-caps "U S NAVY") :to-equal "U S NAVY"))

  (it "leaves lines containing lowercase alone"
    (expect (pdf-text-join-small-caps "see EXHIBIT A NOW and SECTION B LATER")
            :to-equal "see EXHIBIT A NOW and SECTION B LATER")))

;;; The pipeline end to end

(describe "pdf-text-render-pages"
  (it "reflows pages from their glyph layout"
    (let* ((page (append
                  (pdf-text-tests--glyphs "First paragraph line one" 0.10 0.12)
                  (list (list ?\n '(0.34 0.10 0.34 0.12)))
                  (pdf-text-tests--glyphs "ends here." 0.10 0.14)
                  (list (list ?\n '(0.20 0.12 0.20 0.14)))
                  (pdf-text-tests--glyphs "Second paragraph opens" 0.13 0.16)
                  (list (list ?\n '(0.35 0.14 0.35 0.16)))
                  (pdf-text-tests--glyphs "and ends." 0.10 0.18))))
      (expect (pdf-text-render-pages '("ignored") (list page))
              :to-equal
              '("First paragraph line one ends here.\n\nSecond paragraph opens and ends."))))

  (it "closes small-caps gaps in the pipeline"
    (expect (pdf-text-render-pages '("H OW TO D ESIGN P ROGRAMS\nS ECOND E DITION"))
            :to-equal '("HOW TO DESIGN PROGRAMS SECOND EDITION")))

  (it "strips recurring headers and joins paragraphs without a layout"
    (expect (pdf-text-render-pages
             '("ABSTRACT | 14\nbody one runs to a full width line\ncontinues"
               "ABSTRACT | 15\nbody two"
               "ABSTRACT | 16\nbody three"
               "ABSTRACT | 17\nbody four"))
            :to-equal '("body one runs to a full width line continues"
                        "body two" "body three" "body four")))

  (it "collapses a shadow paint split across raw lines"
    (expect (pdf-text-render-pages
             '("PATTERNS OF CONFLICT\nPATTERNS OF\nCONFLICT\n\nbody"))
            :to-equal '("PATTERNS OF CONFLICT\n\nbody")))

  (it "escapes extracted lines org would read as structure"
    (expect (pdf-text-render-pages '("* bullet line" "#+keyword line"))
            :to-equal '("\u200B* bullet line" "\u200B#+keyword line"))))

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

;;; The companion buffer

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
