;;; tests/pdf-text-tests.el --- pdf-text.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(require 'pdf-text)

(require 'org)
(require 'org-element)

;;; Fixtures

(cl-defun pdf-text-tests--line (text &key (x0 0.10) (x1 0.90) (base 0.10)
                                     (height 0.015) (space 0.005) (cv 0.3)
                                     first-width font bold lead-font lead-bold
                                     &allow-other-keys)
  "A line record for TEXT with page-relative geometry.
The defaults describe a body line filling a column that runs 0.10 to
0.90; a spec overrides only what its case is about."
  (pdf-text-line-create
   :text text :x0 x0 :x1 x1 :base base :top (- base height) :bot base
   :height height :space space :cv cv
   :font font :bold bold :lead-font lead-font :lead-bold lead-bold
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

(defun pdf-text-tests--render (specs &optional headings)
  "SPECS rendered as one page, the way `pdf-text-render-pages' renders it.
HEADINGS are the org heading lines the outline puts on that page."
  (let* ((lines (pdf-text-tests--page specs))
         (profile (pdf-text--profile (list lines)))
         (page (pdf-text--page-profile lines profile)))
    (pdf-text--render-blocks (pdf-text--blocks lines page) page
                             (pdf-text--hyphenated-words (list lines))
                             headings)))

(cl-defun pdf-text-tests--run (off len &key (x0 0.10) (x1 0.20) (oy 0.3400)
                                       (qh 0.0134) (size 0.016) bold italic
                                       (font "Body"))
  "A walker font run over OFF..OFF+LEN of its line's text."
  (list off len x0 x1 oy qh size bold italic font))

(cl-defun pdf-text-tests--form (text runs &key (page 1) (x0 0.10) (top 0.3266)
                                     (x1 0.90) (bot 0.3400) (height 0.0134)
                                     (space 0.005) (cv 0.3) (fw 0.02)
                                     (synth 0))
  "A walker line form carrying TEXT and its font RUNS."
  (list page x0 top x1 bot height space cv fw synth runs text))

;;; Charlayout off the wire

;;; Line records off the walker

(describe "pdf-text--mupdf-record"
  (it "carries the walker's measurements into the record"
    (let ((line (pdf-text--mupdf-record
                 (pdf-text-tests--form
                  "ab cd" (list (pdf-text-tests--run 0 5 :x0 0.10 :x1 0.15
                                                     :bold t :font "Mono"))
                  :x0 0.10 :x1 0.15 :top 0.3266 :bot 0.3400 :height 0.015
                  :space 0.004 :cv 0.02 :fw 0.02 :synth 3))))
      (expect (pdf-text-line-text line) :to-equal "ab cd")
      (expect (pdf-text-line-x0 line) :to-be-close-to 0.10 3)
      (expect (pdf-text-line-x1 line) :to-be-close-to 0.15 3)
      (expect (pdf-text-line-base line) :to-be-close-to 0.34 3)
      (expect (pdf-text-line-height line) :to-be-close-to 0.015 3)
      (expect (pdf-text-line-space line) :to-be-close-to 0.004 3)
      (expect (pdf-text-line-cv line) :to-be-close-to 0.02 3)
      (expect (pdf-text-line-first-width line) :to-be-close-to 0.02 3)
      (expect (pdf-text-line-font line) :to-equal "Mono")
      (expect (pdf-text-line-bold line) :to-be t)
      (expect (pdf-text-line-italic line) :to-be nil)
      (expect (pdf-text-line-synth line) :to-be 3)))

  (it "names the line's font after the run with the most ink"
    (let ((line (pdf-text--mupdf-record
                 (pdf-text-tests--form
                  "word one" (list (pdf-text-tests--run 0 5 :x0 0.10 :x1 0.20
                                                        :font "Body")
                                   (pdf-text-tests--run 5 3 :x0 0.21 :x1 0.24
                                                        :italic t
                                                        :font "Italic"))))))
      (expect (pdf-text-line-font line) :to-equal "Body")
      (expect (pdf-text-line-italic line) :to-be nil)))

  (it "keeps the opening run's face beside the dominant one"
    ;; applicative p3, "2 The Applicative class": the sans identifier
    ;; out-inks the bold around it and takes the dominant slot, but the
    ;; line opens bold, and that is what heading rules key on
    (let ((line (pdf-text--mupdf-record
                 (pdf-text-tests--form
                  "2 The Applicative class"
                  (list (pdf-text-tests--run 0 6 :x0 0.4072 :x1 0.4594
                                             :bold t :font "CMBX10")
                        (pdf-text-tests--run 6 12 :x0 0.4658 :x1 0.5429
                                             :font "CMSS10")
                        (pdf-text-tests--run 18 5 :x0 0.5494 :x1 0.5878
                                             :bold t :font "CMBX10"))))))
      (expect (pdf-text-line-font line) :to-equal "CMSS10")
      (expect (pdf-text-line-bold line) :to-be nil)
      (expect (pdf-text-line-lead-font line) :to-equal "CMBX10")
      (expect (pdf-text-line-lead-bold line) :to-be t)))

  (it "keeps a line with no ink as text alone, geometry and all"
    (let ((line (pdf-text--mupdf-record
                 (pdf-text-tests--form "\t" (list (list 0 1 nil nil nil nil
                                                        0.016 nil nil "Body"))
                                       :x0 nil :top nil :x1 nil :bot nil
                                       :height nil :space nil :cv nil
                                       :fw nil))))
      (expect (pdf-text-line-text line) :to-equal "\t")
      (expect (pdf-text-line-x0 line) :to-be nil)))

  (it "trims the trailing space the walker preserves, before the strip"
    ;; a trailing space would hide a line-final wrap hyphen from the join
    (expect (pdf-text-line-text
             (pdf-text--mupdf-record
              (pdf-text-tests--form
               "informa\u00AD " (list (pdf-text-tests--run 0 9)))))
            :to-equal "informa\u00AD"))

  (it "collapses the page's own space runs to the shape the rules know"
    ;; double-spaced sentences and wide-set section numbers arrive as
    ;; real space runs; the preformatted test would read them as
    ;; tabular alignment and break the paragraph
    (expect (pdf-text-line-text
             (pdf-text--mupdf-record
              (pdf-text-tests--form
               "all.  Tune the set" (list (pdf-text-tests--run 0 18)))))
            :to-equal "all. Tune the set")
    (expect (pdf-text-line-text
             (pdf-text--mupdf-record
              (pdf-text-tests--form
               "3   Distance Measure" (list (pdf-text-tests--run 0 20)))))
            :to-equal "3 Distance Measure"))

  (it "strips the leading space run, whose indent the geometry carries"
    ;; a paragraph's typographic first-line indent arrives as leading
    ;; spaces; as text they read as preformatted indentation, while x0
    ;; already says everything the indent rules ask
    (expect (pdf-text-line-text
             (pdf-text--mupdf-record
              (pdf-text-tests--form
               "    Soon afterward" (list (pdf-text-tests--run 0 18)))))
            :to-equal "Soon afterward"))

  (it "strips what the page never prints"
    (expect (pdf-text-line-text
             (pdf-text--mupdf-record
              (pdf-text-tests--form
               "l\u001F11 rule" (list (pdf-text-tests--run 0 8)))))
            :to-equal "l11 rule")))

(describe "pdf-text--float-drop-caps"
  :var ((profile '(:height 0.0165 :leading 0.019)))

  (it "floats a cap served before the running head down to its body line"
    (let* ((cap (pdf-text-tests--line "T" :x0 0.117 :x1 0.165 :base 0.3044
                                      :height 0.0563))
           (head (pdf-text-tests--line "preface" :x0 0.42 :x1 0.58
                                       :base 0.1922 :height 0.0341))
           (body (pdf-text-tests--line "his book can make" :base 0.2739))
           (floated (pdf-text--float-drop-caps (list cap head body) profile)))
      (expect (mapcar #'pdf-text-line-text floated)
              :to-equal '("preface" "T" "his book can make"))))

  (it "leaves a cap already in reading order alone"
    (let* ((head (pdf-text-tests--line "preface" :base 0.1922 :height 0.0341))
           (cap (pdf-text-tests--line "T" :base 0.3044 :height 0.0563))
           (body (pdf-text-tests--line "his book can make" :base 0.2739)))
      (expect (mapcar #'pdf-text-line-text
                      (pdf-text--float-drop-caps (list head cap body) profile))
              :to-equal '("preface" "T" "his book can make"))))

  (it "leaves a cap alone when no line in reach opens under it"
    (let* ((cap (pdf-text-tests--line "T" :base 0.3044 :height 0.0563))
           (head (pdf-text-tests--line "preface" :base 0.1922 :height 0.0341)))
      (expect (mapcar #'pdf-text-line-text
                      (pdf-text--float-drop-caps (list cap head) profile))
              :to-equal '("T" "preface")))))

(describe "pdf-text--defer-margin-notes"
  :var ((profile '(:height 0.0136 :leading 0.017 :left 0.095 :right 0.795
                   :space 0.0047)))

  (it "serves an outer-margin term label after the page's flow"
    (let ((lines (list (pdf-text-tests--line
                        "Two aspects of HMMs are relevant."
                        :x0 0.095 :x1 0.795 :base 0.66 :height 0.0136)
                       (pdf-text-tests--line
                        "Hidden Markov models"
                        :x0 0.809 :x1 0.915 :base 0.677 :height 0.0098)
                       (pdf-text-tests--line
                        "First, they are based on a rigorous theory."
                        :x0 0.095 :x1 0.795 :base 0.68 :height 0.0136))))
      (expect (mapcar #'pdf-text-line-text
                      (pdf-text--defer-margin-notes lines profile))
              :to-equal '("Two aspects of HMMs are relevant."
                          "First, they are based on a rigorous theory."
                          "Hidden Markov models"))))

  (it "leaves a lane's claimed record and a wide record in place"
    (let ((claimed (pdf-text-tests--line "right lane cell"
                                         :x0 0.82 :x1 0.93 :base 0.30))
          (wide (pdf-text-tests--line "a spanning row of its own"
                                      :x0 0.80 :x1 0.93 :base 0.32)))
      (setf (pdf-text-line-claimed claimed) t)
      (setf (pdf-text-line-x1 wide) 1.06)
      (expect (mapcar #'pdf-text-line-text
                      (pdf-text--defer-margin-notes
                       (list claimed wide
                             (pdf-text-tests--line "body text line"
                                                   :x0 0.095 :x1 0.795
                                                   :base 0.34 :height 0.0136))
                       profile))
              :to-equal '("right lane cell" "a spanning row of its own"
                          "body text line"))))

  ;; fastloose p1: a two-column paper's modal column is the left body
  ;; column, and the title page's right-side author column sits past
  ;; it - inside the page's own text area, whose far strong edge the
  ;; right body column establishes.  Only what stands past ALL the
  ;; columns is a margin note.
  (it "keeps a right author column inside the two-column text area"
    (let ((two-col '(:height 0.0100 :leading 0.0126 :left 0.0882
                     :right 0.4789 :text-left 0.0882 :text-right 0.9101
                     :space 0.0052)))
      (expect (mapcar #'pdf-text-line-text
                      (pdf-text--defer-margin-notes
                       (list (pdf-text-tests--line
                              "Nils Anders Danielsson"
                              :x0 0.1465 :x1 0.3269 :base 0.1628
                              :height 0.0122)
                             (pdf-text-tests--line
                              "Jeremy Gibbons"
                              :x0 0.6304 :x1 0.7572 :base 0.1628
                              :height 0.0122)
                             (pdf-text-tests--line
                              "Functional programmers often reason about"
                              :x0 0.0882 :x1 0.4789 :base 0.3035
                              :height 0.0100)
                             (pdf-text-tests--line
                              "The moral of the story runs the right column"
                              :x0 0.5211 :x1 0.9101 :base 0.3035
                              :height 0.0100))
                       two-col))
              :to-equal '("Nils Anders Danielsson"
                          "Jeremy Gibbons"
                          "Functional programmers often reason about"
                          "The moral of the story runs the right column")))))

(describe "pdf-text--mark-entry-runs"
  (it "tags a contents run to render one entry per line"
    (let ((lines (list (pdf-text-tests--line "Generalization 111")
                       (pdf-text-tests--line "Overfitting 113")
                       (pdf-text-tests--line "Overfitting Examined 113"))))
      (expect (mapcar #'pdf-text-line-kind
                      (pdf-text--mark-entry-runs lines))
              :to-equal '(entry entry entry))))

  (it "renders a contents run as org list items"
    (let* ((lines (pdf-text-tests--page
                   '(("Generalization 111")
                     ("Overfitting 113")
                     ("Overfitting Examined 113"))))
           (profile (pdf-text--profile (list lines))))
      (dolist (l lines) (setf (pdf-text-line-kind l) 'entry))
      (let ((page (pdf-text--page-profile lines profile)))
        (expect (pdf-text--render-blocks
                 (pdf-text--blocks lines page) page
                 (pdf-text--hyphenated-words (list lines)))
                :to-equal
                (concat "- Generalization 111\n"
                        "- Overfitting 113\n"
                        "- Overfitting Examined 113")))))

  (it "leaves fewer folio-closed neighbours than a run as the prose they are"
    (let ((lines (list (pdf-text-tests--line "the events of 1988")
                       (pdf-text-tests--line "shaped the field for 30")
                       (pdf-text-tests--line "years to come."))))
      (expect (mapcar #'pdf-text-line-kind
                      (pdf-text--mark-entry-runs lines))
              :to-equal '(nil nil nil)))))

(describe "pdf-text--strip-leaders"
  ;; DSB p11: the chapter entry carries its spaced-dot fill and folio
  ;; in one record - "5. Overfitting and Its Avoidance. . . [50+] . 111"
  (it "strips an inline leader fill and keeps the entry-folio pairing"
    (let ((out (pdf-text--strip-leaders
                (list (pdf-text-tests--line
                       "5. Overfitting and Its Avoidance. . . . . . . . . . 111"
                       :x0 0.1447 :x1 0.8571 :base 0.1370 :height 0.0217)))))
      (expect (mapcar #'pdf-text-line-text out)
              :to-equal '("5. Overfitting and Its Avoidance 111"))
      (expect (pdf-text-line-kind (car out)) :to-be 'entry)))

  ;; fpio p2: the fill is its own record on the entry's baseline, the
  ;; glyphs cmmi periods dvips named ":" - detection is by repetition,
  ;; not by the glyph's identity
  (it "drops a leader-run record and pairs the entry with its folio"
    (let ((out (pdf-text--strip-leaders
                (list (pdf-text-tests--line
                       "1.2 A brief history of functional I/O"
                       :x0 0.1657 :x1 0.4604 :base 0.4620 :height 0.0138)
                      (pdf-text-tests--line
                       ": : : : : : : : : : : : : : : : : : : : : : : :"
                       :x0 0.4761 :x1 0.8000 :base 0.4620 :height 0.0138)
                      (pdf-text-tests--line
                       "2" :x0 0.8249 :x1 0.8337 :base 0.4620
                       :height 0.0138)))))
      (expect (mapcar #'pdf-text-line-text out)
              :to-equal '("1.2 A brief history of functional I/O 2"))
      (expect (pdf-text-line-kind (car out)) :to-be 'entry)
      (expect (pdf-text-line-x1 (car out)) :to-equal 0.8337)))

  ;; fpio p2 line 10: the word join welds the fill onto the entry's own
  ;; record when the gap is narrow, and the folio still stands apart
  (it "strips a trailing run welded into the entry's record"
    (expect (mapcar #'pdf-text-line-text
                    (pdf-text--strip-leaders
                     (list (pdf-text-tests--line
                            "1.1 Functional programming : : : : : : : : : :"
                            :x0 0.1657 :x1 0.8000 :base 0.4390
                            :height 0.0138)
                           (pdf-text-tests--line
                            "1" :x0 0.8249 :x1 0.8337 :base 0.4390
                            :height 0.0138))))
            :to-equal '("1.1 Functional programming 1")))

  (it "reads any repeated glyph as a fill, middots included"
    (expect (mapcar #'pdf-text-line-text
                    (pdf-text--strip-leaders
                     (list (pdf-text-tests--line
                            "Overfitting Examined"
                            :x0 0.10 :x1 0.40 :base 0.30)
                           (pdf-text-tests--line
                            "· · · · · · · · ·"
                            :x0 0.42 :x1 0.78 :base 0.30)
                           (pdf-text-tests--line
                            "113" :x0 0.82 :x1 0.85 :base 0.30))))
            :to-equal '("Overfitting Examined 113")))

  (it "gathers a fill served one glyph per record"
    (expect (mapcar #'pdf-text-line-text
                    (pdf-text--strip-leaders
                     (list (pdf-text-tests--line
                            "Learning Curves" :x0 0.10 :x1 0.35 :base 0.30)
                           (pdf-text-tests--line "." :x0 0.40 :x1 0.41
                                                 :base 0.30)
                           (pdf-text-tests--line "." :x0 0.44 :x1 0.45
                                                 :base 0.30)
                           (pdf-text-tests--line "." :x0 0.48 :x1 0.49
                                                 :base 0.30)
                           (pdf-text-tests--line "." :x0 0.52 :x1 0.53
                                                 :base 0.30)
                           (pdf-text-tests--line "." :x0 0.56 :x1 0.57
                                                 :base 0.30)
                           (pdf-text-tests--line "130" :x0 0.82 :x1 0.85
                                                 :base 0.30))))
            :to-equal '("Learning Curves 130")))

  ;; fpio p2 lines 54-57: a math-font fragment stands between the
  ;; entry's words and its fill, and it belongs inside the entry
  (it "keeps a math fragment the page sets inside its entry"
    (expect (mapcar #'pdf-text-line-text
                    (pdf-text--strip-leaders
                     (list (pdf-text-tests--line
                            "3.3 Operational semantics of"
                            :x0 0.1657 :x1 0.4006 :base 0.8813
                            :height 0.0138)
                           (pdf-text-tests--line
                            "M" :x0 0.4065 :x1 0.4278 :base 0.8813
                            :height 0.0138)
                           (pdf-text-tests--line
                            ": : : : : : : : : : : : : : : : : : : : : : : : : : :"
                            :x0 0.4345 :x1 0.8000 :base 0.8813
                            :height 0.0138)
                           (pdf-text-tests--line
                            "30" :x0 0.8159 :x1 0.8335 :base 0.8813
                            :height 0.0138))))
            :to-equal '("3.3 Operational semantics of M 30")))

  ;; fpio p2: "Summary vii" and "2 A calculus of recursive types 15"
  ;; carry no fill of their own, and pair because the page's fills say
  ;; the page is a contents page
  (it "pairs leaderless entries with their folios on a contents page"
    (let ((out (pdf-text--strip-leaders
                (list (pdf-text-tests--line
                       "1.5 Synopsis : : : : : : : : : : : : :"
                       :x0 0.1657 :x1 0.8000 :base 0.30 :height 0.0138)
                      (pdf-text-tests--line "8" :x0 0.8249 :x1 0.8337
                                            :base 0.30 :height 0.0138)
                      (pdf-text-tests--line
                       "1.6 Results : : : : : : : : : : : : : :"
                       :x0 0.1657 :x1 0.8000 :base 0.33 :height 0.0138)
                      (pdf-text-tests--line "10" :x0 0.8159 :x1 0.8335
                                            :base 0.33 :height 0.0138)
                      (pdf-text-tests--line
                       "1.7 How to read it : : : : : : : : : :"
                       :x0 0.1657 :x1 0.8000 :base 0.36 :height 0.0138)
                      (pdf-text-tests--line "10" :x0 0.8159 :x1 0.8335
                                            :base 0.36 :height 0.0138)
                      (pdf-text-tests--line "Summary" :x0 0.1390 :x1 0.2255
                                            :base 0.40 :height 0.0138)
                      (pdf-text-tests--line "vii" :x0 0.8116 :x1 0.8337
                                            :base 0.40 :height 0.0138)
                      (pdf-text-tests--line "2" :x0 0.1390 :x1 0.1492
                                            :base 0.44 :height 0.0138)
                      (pdf-text-tests--line "A calculus of recursive types"
                                            :x0 0.1657 :x1 0.4233
                                            :base 0.44 :height 0.0138)
                      (pdf-text-tests--line "15" :x0 0.8131 :x1 0.8335
                                            :base 0.44 :height 0.0138)))))
      (expect (mapcar #'pdf-text-line-text out)
              :to-equal '("1.5 Synopsis 8"
                          "1.6 Results 10"
                          "1.7 How to read it 10"
                          "Summary vii"
                          "2 A calculus of recursive types 15"))
      (expect (mapcar #'pdf-text-line-kind out)
              :to-equal '(entry entry entry entry entry))))

  (it "leaves a text-folio baseline apart where no fill says contents"
    (let ((out (pdf-text--strip-leaders
                (list (pdf-text-tests--line "1. Birth of a habit"
                                            :x0 0.10 :x1 0.70 :base 0.30)
                      (pdf-text-tests--line "3" :x0 0.72 :x1 0.73
                                            :base 0.30)))))
      (expect (mapcar #'pdf-text-line-text out)
              :to-equal '("1. Birth of a habit" "3"))
      (expect (mapcar #'pdf-text-line-kind out)
              :to-equal '(nil nil))))

  (it "leaves the margin band's furniture out of the pairing"
    (expect (mapcar #'pdf-text-line-text
                    (pdf-text--strip-leaders
                     (list (pdf-text-tests--line
                            "2.5 Two examples : : : : : : : : : : :"
                            :x0 0.1657 :x1 0.8000 :base 0.30 :height 0.0138)
                           (pdf-text-tests--line "20" :x0 0.8159 :x1 0.8335
                                                 :base 0.30 :height 0.0138)
                           (pdf-text-tests--line
                            "2.6 Strong normalisation : : : : : : :"
                            :x0 0.1657 :x1 0.8000 :base 0.33 :height 0.0138)
                           (pdf-text-tests--line "21" :x0 0.8159 :x1 0.8335
                                                 :base 0.33 :height 0.0138)
                           (pdf-text-tests--line
                            "3.1 Syntax : : : : : : : : : : : : : :"
                            :x0 0.1657 :x1 0.8000 :base 0.36 :height 0.0138)
                           (pdf-text-tests--line "28" :x0 0.8159 :x1 0.8335
                                                 :base 0.36 :height 0.0138)
                           (pdf-text-tests--line "Table of Contents"
                                                 :x0 0.7088 :x1 0.8110
                                                 :base 0.9388 :height 0.0163)
                           (pdf-text-tests--line "v" :x0 0.8505 :x1 0.8571
                                                 :base 0.9388
                                                 :height 0.0163))))
            :to-equal '("2.5 Two examples 20"
                        "2.6 Strong normalisation 21"
                        "3.1 Syntax 28"
                        "Table of Contents" "v")))

  (it "leaves prose ending in repeated punctuation alone"
    (let ((out (pdf-text--strip-leaders
                (list (pdf-text-tests--line "He shouted Wow!!!!"
                                            :x0 0.10 :x1 0.50 :base 0.30)))))
      (expect (mapcar #'pdf-text-line-text out)
              :to-equal '("He shouted Wow!!!!"))
      (expect (pdf-text-line-kind (car out)) :to-be nil))
    (expect (mapcar #'pdf-text-line-text
                    (pdf-text--strip-leaders
                     (list (pdf-text-tests--line "What?!?!?! 42"
                                                 :x0 0.10 :x1 0.50
                                                 :base 0.30))))
            :to-equal '("What?!?!?! 42")))

  (it "leaves an ellipsis short of the fill minimum alone"
    (expect (mapcar #'pdf-text-line-text
                    (pdf-text--strip-leaders
                     (list (pdf-text-tests--line "He waited . . . 1984"
                                                 :x0 0.10 :x1 0.50
                                                 :base 0.30))))
            :to-equal '("He waited . . . 1984")))

  (it "keeps a scene-break run that stands alone on its baseline"
    (let ((out (pdf-text--strip-leaders
                (list (pdf-text-tests--line ". . . . . . ."
                                            :x0 0.40 :x1 0.60
                                            :base 0.50)))))
      (expect (mapcar #'pdf-text-line-text out)
              :to-equal '(". . . . . . ."))))

  (it "strips an inline fill from a line with no geometry"
    ;; the gettext fallback serves text-only records; the folio names
    ;; the entry even there
    (expect (mapcar #'pdf-text-line-text
                    (pdf-text--strip-leaders
                     (pdf-text--page-lines
                      "1.2 A brief history : : : : : : 2")))
            :to-equal '("1.2 A brief history 2"))))

(describe "pdf-text--join-split-lines"
  :var ((profile '(:height 0.0136 :leading 0.017 :space 0.0047)))

  (cl-flet ((faced (text &rest props)
              (let ((line (apply #'pdf-text-tests--line text props)))
                (setf (pdf-text-line-font line) "CMR10")
                line)))

    (it "rejoins one typeset line the extractor served as two"
      (expect (mapcar #'pdf-text-line-text
                      (pdf-text--join-split-lines
                       (list (faced "an element from the input only"
                                    :x0 0.0973 :x1 0.4233 :base 0.7822
                                    :height 0.0136)
                             (faced "causes an incremental change"
                                    :x0 0.4420 :x1 0.9014 :base 0.7822
                                    :height 0.0136))
                       profile))
              :to-equal
              '("an element from the input only causes an incremental change")))

    ;; fpio p1: a Type 3 DVI document serves every kern chunk as its
    ;; own record - "Univ" ends x1 0.5765, "ersit" opens x0 0.5759 -
    ;; and the chunks are one word, rejoined with no space
    (it "rejoins kern chunks whose gaps sit at or under zero into one word"
      (expect (mapcar #'pdf-text-line-text
                      (pdf-text--join-split-lines
                       (list (faced "Univ" :x0 0.5269 :x1 0.5765
                                    :base 0.8210 :height 0.0168)
                             (faced "ersit" :x0 0.5759 :x1 0.6204
                                    :base 0.8210 :height 0.0168)
                             (faced "y" :x0 0.6198 :x1 0.6322
                                    :base 0.8210 :height 0.0168))
                       profile))
              :to-equal '("University")))

    (it "rejoins an overlapping swash pair, the overlap running deep"
      ;; fpio's italic title "F" overlaps "unctional" by 0.0039 -
      ;; most of a modal space - and the word is still one word
      (expect (mapcar #'pdf-text-line-text
                      (pdf-text--join-split-lines
                       (list (faced "F" :x0 0.2322 :x1 0.2614
                                    :base 0.1241 :height 0.0314)
                             (faced "unctional" :x0 0.2575 :x1 0.4457
                                    :base 0.1241 :height 0.0314))
                       profile))
              :to-equal '("Functional")))

    (it "rejoins a hair-gap chunk on the lowercase confirming signal"
      ;; fpio p30: "resp" to "ectively" gaps +0.0005 - a tenth of a
      ;; modal space, far under any word gap the page sets
      (expect (mapcar #'pdf-text-line-text
                      (pdf-text--join-split-lines
                       (list (faced "resp" :x0 0.2204 :x1 0.2524
                                    :base 0.1290 :height 0.0138)
                             (faced "ectively" :x0 0.2529 :x1 0.3100
                                    :base 0.1290 :height 0.0138))
                       profile))
              :to-equal '("respectively")))

    (it "rejoins overlapping capitals, the overlap alone confirming"
      ;; CMU-CS-95-113: "Pittsburgh, P" then "A" opening 0.0016 inside
      ;; the P's ink - no lowercase to confirm, but ink does not overlap
      ;; across a word boundary
      (expect (mapcar #'pdf-text-line-text
                      (pdf-text--join-split-lines
                       (list (faced "P" :x0 0.5376 :x1 0.5506
                                    :base 0.5109 :height 0.0152)
                             (faced "A" :x0 0.5490 :x1 0.5635
                                    :base 0.5109 :height 0.0152))
                       profile))
              :to-equal '("PA")))

    (it "keeps a hair-gap capital opening apart, unconfirmed"
      ;; a positive gap needs the lowercase signal: a shattered word
      ;; never continues into a capital, but a tight table cell can
      (expect (mapcar #'pdf-text-line-text
                      (pdf-text--join-split-lines
                       (list (faced "cell" :x0 0.10 :x1 0.30 :base 0.30)
                             (faced "Next" :x0 0.3005 :x1 0.35 :base 0.30))
                       profile))
              :to-equal '("cell" "Next")))

    (it "chains kern rejoins into the clause join's word space"
      ;; fpio p1: "to" | "w" | "ards" | "the" - two kern seams heal
      ;; wordwise, then the word gap takes the clause join's space
      (expect (mapcar #'pdf-text-line-text
                      (pdf-text--join-split-lines
                       (list (faced "to" :x0 0.2637 :x1 0.2845
                                    :base 0.8435 :height 0.0168)
                             (faced "w" :x0 0.2839 :x1 0.3008
                                    :base 0.8435 :height 0.0168)
                             (faced "ards" :x0 0.3002 :x1 0.3433
                                    :base 0.8435 :height 0.0168)
                             (faced "the" :x0 0.3512 :x1 0.3835
                                    :base 0.8435 :height 0.0168))
                       profile))
              :to-equal '("towards the")))

    (it "keeps records of different faces off the kern rejoin"
      ;; fpio p30's math: symbol chunks abut letter chunks across font
      ;; changes, and only same-font neighbours can be one word
      (let ((a (faced "ab" :x0 0.10 :x1 0.13 :base 0.30))
            (b (faced "cd" :x0 0.1295 :x1 0.16 :base 0.30)))
        (setf (pdf-text-line-font b) "T9")
        (expect (length (pdf-text--join-split-lines (list a b) profile))
                :to-be 2)))

    (it "keeps a wholly overlapped record out of the rejoin"
      ;; a shadow-painted doubled title overlaps by whole words, not
      ;; by a kern; past a modal space of overlap nothing is one word
      (expect (length (pdf-text--join-split-lines
                       (list (faced "PATTERNS" :x0 0.10 :x1 0.30 :base 0.30)
                             (faced "PATTERNS" :x0 0.101 :x1 0.301
                                    :base 0.30))
                       profile))
              :to-be 2))

    (it "reassembles a shattered document's line across its word gaps"
      ;; fpio p1: "Functional" and "Programming" a word gap apart on
      ;; one baseline, each its own record; under the Type 3 class the
      ;; typeset line comes back, case notwithstanding
      (expect (mapcar #'pdf-text-line-text
                      (pdf-text--join-split-lines
                       (list (faced "Functional" :x0 0.2322 :x1 0.4457
                                    :base 0.1241 :height 0.0314)
                             (faced "Programming" :x0 0.4614 :x1 0.7410
                                    :base 0.1241 :height 0.0314))
                       (append '(:em 0.0001) profile)))
              :to-equal '("Functional Programming")))

    (it "keeps word-gap capitals apart where the ems are healthy"
      (expect (length (pdf-text--join-split-lines
                       (list (faced "Functional" :x0 0.2322 :x1 0.4457
                                    :base 0.1241 :height 0.0314)
                             (faced "Programming" :x0 0.4614 :x1 0.7410
                                    :base 0.1241 :height 0.0314))
                       (append '(:em 0.016) profile)))
              :to-be 2))

    (it "keeps lane-wide neighbours apart in a shattered document too"
      (expect (length (pdf-text--join-split-lines
                       (list (faced "cell" :x0 0.10 :x1 0.30 :base 0.30)
                             (faced "Far" :x0 0.32 :x1 0.40 :base 0.30))
                       (append '(:em 0.0001) profile)))
              :to-be 2))

    (it "keeps a folio and a capitalized cell out of the join"
      (expect (mapcar #'pdf-text-line-text
                      (pdf-text--join-split-lines
                       (list (faced "1. Birth of a habit"
                                    :x0 0.10 :x1 0.70 :base 0.30)
                             (faced "3" :x0 0.72 :x1 0.73 :base 0.30))
                       profile))
              :to-equal '("1. Birth of a habit" "3"))
      (expect (mapcar #'pdf-text-line-text
                      (pdf-text--join-split-lines
                       (list (faced "voiced bilabial fricative"
                                    :x0 0.22 :x1 0.39 :base 0.30)
                             (faced "Air released between the lips"
                                    :x0 0.41 :x1 0.57 :base 0.30))
                       profile))
              :to-equal '("voiced bilabial fricative"
                          "Air released between the lips")))

    (it "keeps paired lane items apart, their gap running wide"
      (expect (mapcar #'pdf-text-line-text
                      (pdf-text--join-split-lines
                       (list (faced "el alerta alert"
                                    :x0 0.109 :x1 0.30 :base 0.30)
                             (faced "la alerta alarm"
                                    :x0 0.523 :x1 0.70 :base 0.30))
                       profile))
              :to-equal '("el alerta alert" "la alerta alarm")))

    (it "keeps records of different faces apart"
      (let ((body (faced "development of Bayesian networks"
                         :x0 0.09 :x1 0.79 :base 0.68))
            (note (faced "margin note" :x0 0.80 :x1 0.91 :base 0.68)))
        (setf (pdf-text-line-font note) "CMSS8")
        (expect (length (pdf-text--join-split-lines (list body note) profile))
                :to-be 2)))

    (it "joins a dotted section number to its title across a face change"
      (let ((number (faced "3.2.2" :x0 0.1812 :x1 0.2205 :base 0.4996))
            (title (faced "Hilbert Systems" :x0 0.2358 :x1 0.3732
                          :base 0.4996)))
        (setf (pdf-text-line-font title) "CMBX12")
        (expect (mapcar #'pdf-text-line-text
                        (pdf-text--join-split-lines (list number title)
                                                    profile))
                :to-equal '("3.2.2 Hilbert Systems"))))

    (it "leaves a bare enumerator apart from its item"
      (expect (mapcar #'pdf-text-line-text
                      (pdf-text--join-split-lines
                       (list (faced "1." :x0 0.2223 :x1 0.2328 :base 0.30)
                             (faced "For each word token" :x0 0.25 :x1 0.84
                                    :base 0.30))
                       profile))
              :to-equal '("1." "For each word token")))

    ;; denotational_design p2: "2." and its title served apart on one
    ;; baseline, one face, 1.23 body heights
    (it "joins a bare display number to its title set in one face over the body"
      (let* ((number (faced "2." :x0 0.0882 :x1 0.1017 :base 0.3212
                            :height 0.017))
             (title (faced "Denotational semantics and data types"
                           :x0 0.1196 :x1 0.4130 :base 0.3212 :height 0.017))
             (joined (progn
                       (setf (pdf-text-line-lead-font number)
                             "KCGTFG+NimbusRomNo9L-Medi")
                       (setf (pdf-text-line-lead-bold number) t)
                       (pdf-text--join-split-lines (list number title)
                                                   profile))))
        (expect (mapcar #'pdf-text-line-text joined)
                :to-equal '("2. Denotational semantics and data types"))
        ;; the merged record opens as its number does
        (expect (pdf-text-line-lead-font (car joined))
                :to-equal "KCGTFG+NimbusRomNo9L-Medi")
        (expect (pdf-text-line-lead-bold (car joined)) :to-be t)))

    (it "keeps a bare display number off a title in another face"
      (let ((number (faced "2." :x0 0.0882 :x1 0.1017 :base 0.3212
                           :height 0.017))
            (title (faced "Denotational semantics and data types"
                          :x0 0.1196 :x1 0.4130 :base 0.3212 :height 0.017)))
        (setf (pdf-text-line-font title) "CMSS10")
        (expect (length (pdf-text--join-split-lines (list number title)
                                                    profile))
                :to-be 2)))))

(describe "pdf-text--run-baseline"
  (it "reads a one-run line's descent line as its baseline"
    (expect (car (pdf-text--run-baseline
                  (list (pdf-text-tests--run 0 4 :oy 0.3400 :qh 0.0134))))
            :to-be-close-to 0.3400 4))

  (it "re-bases a fragment set mostly in subscript on its full-size run"
    (let ((ref (pdf-text--run-baseline
                (list (pdf-text-tests--run 0 4 :x0 0.10 :x1 0.123
                                           :oy 0.3424 :qh 0.0089)
                      (pdf-text-tests--run 4 2 :x0 0.125 :x1 0.1424
                                           :oy 0.3394 :qh 0.0134)))))
      (expect (car ref) :to-be-close-to 0.3394 4)
      (expect (cdr ref) :to-be-close-to 0.0134 4)))

  (it "keeps the baseline on the letters when a symbol font hangs below"
    (expect (car (pdf-text--run-baseline
                  (list (pdf-text-tests--run 0 2 :x0 0.10 :x1 0.116
                                             :oy 0.3400 :qh 0.0134)
                        (pdf-text-tests--run 2 1 :x0 0.117 :x1 0.129
                                             :oy 0.3424 :qh 0.0196))))
            :to-be-close-to 0.3400 4))

  (it "reads no baseline off wordless runs"
    (expect (pdf-text--run-baseline (list (list 0 1 nil nil nil nil
                                                0.016 nil nil "Body")))
            :to-be nil)))

(describe "pdf-text--run-markup script detection"
  (cl-flet ((markup (text runs)
              (pdf-text-line-text
               (pdf-text--mupdf-record (pdf-text-tests--form text runs)))))

    (it "wraps a raised smaller run as a superscript, exponent sign included"
      (expect (markup "10-43s"
                      (list (pdf-text-tests--run 0 2 :x0 0.10 :x1 0.12)
                            (pdf-text-tests--run 2 3 :x0 0.12 :x1 0.138
                                                 :oy 0.3336 :qh 0.0089)
                            (pdf-text-tests--run 5 1 :x0 0.14 :x1 0.15)))
              :to-equal "10^{-43}s"))

    (it "wraps a far-raised footnote asterisk, symbols alone sufficing"
      (expect (markup "d.*"
                      (list (pdf-text-tests--run 0 2 :x0 0.10 :x1 0.115)
                            (pdf-text-tests--run 2 1 :x0 0.116 :x1 0.122
                                                 :oy 0.3336 :qh 0.0089)))
              :to-equal "d.^{*}"))

    (it "wraps a dropped smaller run as a subscript"
      (expect (markup "Fjk"
                      (list (pdf-text-tests--run 0 1 :x0 0.10 :x1 0.112)
                            (pdf-text-tests--run 1 2 :x0 0.113 :x1 0.125
                                                 :oy 0.3413 :qh 0.0089)))
              :to-equal "F_{jk}"))

    (it "wraps the subscript majority when the full-size run re-bases it"
      (expect (markup "k=1 F."
                      (list (pdf-text-tests--run 0 4 :x0 0.10 :x1 0.123
                                                 :oy 0.3424 :qh 0.0089)
                            (pdf-text-tests--run 4 2 :x0 0.125 :x1 0.1424
                                                 :oy 0.3394 :qh 0.0134)))
              :to-equal "_{k=1} F."))

    (it "leaves an inline code font alone, its raise being under a script's"
      (expect (markup "la code"
                      (list (pdf-text-tests--run 0 3 :x0 0.10 :x1 0.121
                                                 :oy 0.5000 :qh 0.0214)
                            (pdf-text-tests--run 3 4 :x0 0.121 :x1 0.153
                                                 :oy 0.4968 :qh 0.0181)))
              :to-equal "la code"))

    (it "leaves a slightly raised operator alone, symbols needing a symbol's offset"
      (expect (markup "x ⋯"
                      (list (pdf-text-tests--run 0 2 :x0 0.10 :x1 0.108)
                            (pdf-text-tests--run 2 1 :x0 0.113 :x1 0.125
                                                 :oy 0.3414 :qh 0.0094)))
              :to-equal "x ⋯"))

    (it "reads a run past a script's reach as another line, not a script"
      (expect (markup "This"
                      (list (pdf-text-tests--run 0 1 :x0 0.10 :x1 0.13
                                                 :oy 0.4200 :qh 0.0500)
                            (pdf-text-tests--run 1 3 :x0 0.131 :x1 0.155)))
              :to-equal "This"))

    (it "absorbs the hair space a font switch leaves before a script"
      (expect (markup "in α 2"
                      (list (pdf-text-tests--run 0 5 :x0 0.10 :x1 0.1295)
                            (pdf-text-tests--run 5 1 :x0 0.1310 :x1 0.137
                                                 :oy 0.3424 :qh 0.0089)))
              :to-equal "in α_{2}"))

    (it "keeps a word gap the page set before a script"
      (expect (markup "in α 2"
                      (list (pdf-text-tests--run 0 5 :x0 0.10 :x1 0.1295)
                            (pdf-text-tests--run 5 1 :x0 0.1345 :x1 0.140
                                                 :oy 0.3424 :qh 0.0089)))
              :to-equal "in α _{2}"))

    ;; logic p261: TeX writes no space character between an operator
    ;; glyph and its operand - the 0.29 em gap is the space
    (it "restores the space a font switch's word gap sets"
      (expect (markup "∗y"
                      (list (pdf-text-tests--run 0 1 :x0 0.10 :x1 0.1098
                                                 :font "CMSY10")
                            (pdf-text-tests--run 1 1 :x0 0.1141 :x1 0.1235
                                                 :font "CMMI12")))
              :to-equal "∗ y"))

    (it "leaves a kern-tight font switch glued"
      (expect (markup "i(x"
                      (list (pdf-text-tests--run 0 1 :x0 0.10 :x1 0.1066
                                                 :font "CMMI12")
                            (pdf-text-tests--run 1 1 :x0 0.1066 :x1 0.1140
                                                 :font "CMR12")
                            (pdf-text-tests--run 2 1 :x0 0.1140 :x1 0.1249
                                                 :font "CMMI12")))
              :to-equal "i(x"))

    (it "does not double a space the page already set"
      (expect (markup "x ∗"
                      (list (pdf-text-tests--run 0 2 :x0 0.10 :x1 0.1078)
                            (pdf-text-tests--run 2 1 :x0 0.1121 :x1 0.1219
                                                 :font "CMSY10")))
              :to-equal "x ∗"))

    (it "never splits a word whose font change gaps into lowercase"
      ;; DSB's formula subscripts: "Ag" and "e" arrive as two runs a
      ;; wide kern apart, and "Ag e" is not a word
      (expect (markup "Age"
                      (list (pdf-text-tests--run 0 2 :x0 0.10 :x1 0.1078
                                                 :font "MinionPro-It")
                            (pdf-text-tests--run 2 1 :x0 0.1121 :x1 0.1219
                                                 :font "MinionPro-Regular")))
              :to-equal "Age"))))

(describe "pdf-text--run-markup under degenerate ems"
  ;; a Type 3 bitmap font's FontMatrix scale reaches MuPDF raw: em
  ;; sizes 0.0001-0.0003 against glyph heights 0.012-0.03, zeroing
  ;; every em-multiplied threshold
  (cl-flet ((markup (text runs)
              (let ((pdf-text--degenerate-ems t))
                (pdf-text-line-text
                 (pdf-text--mupdf-record (pdf-text-tests--form text runs))))))

    (it "stands the glyph height in for a garbage em at the space test"
      ;; a kern gap of 0.0006 clears 0.2 of em 0.0003 twentyfold; it
      ;; must not read as a word space
      (expect (markup "95113"
                      (list (pdf-text-tests--run 0 2 :x0 0.10 :x1 0.14
                                                 :size 0.0003 :font "T33")
                            (pdf-text-tests--run 2 3 :x0 0.1406 :x1 0.18
                                                 :size 0.0003 :font "T31")))
              :to-equal "95113"))

    (it "still grants a word gap its space under the stand-in"
      (expect (markup "VolII"
                      (list (pdf-text-tests--run 0 3 :x0 0.10 :x1 0.13
                                                 :size 0.0003 :font "T33")
                            (pdf-text-tests--run 3 2 :x0 0.135 :x1 0.15
                                                 :size 0.0003 :font "T31")))
              :to-equal "Vol II"))))

(describe "pdf-text--mupdf-record literals"
  (it "breaks a literal script pair so org will not parse it"
    (expect (pdf-text-line-text
             (pdf-text--mupdf-record
              (pdf-text-tests--form "x_{i}"
                                    (list (pdf-text-tests--run 0 5)))))
            :to-equal "x_\u200B{i}"))

  (it "marks nothing on a line set in one size, script or not"
    (expect (pdf-text-line-text
             (pdf-text--mupdf-record
              (pdf-text-tests--form "k=1"
                                    (list (pdf-text-tests--run 0 3 :oy 0.3424
                                                               :qh 0.0089)))))
            :to-equal "k=1")))

(describe "pdf-text--mupdf-parse"
  (it "assembles pages in order, nil where a page emitted nothing"
    (let ((pages (pdf-text--mupdf-parse
                  (concat (format "%S\n" (pdf-text-tests--form
                                          "on three"
                                          (list (pdf-text-tests--run 0 8))
                                          :page 3))
                          (format "%S\n" (pdf-text-tests--form
                                          "on five"
                                          (list (pdf-text-tests--run 0 7))
                                          :page 5)))
                  3 5)))
      (expect (length pages) :to-be 3)
      (expect (pdf-text-line-text (car (nth 0 pages))) :to-equal "on three")
      (expect (nth 1 pages) :to-be nil)
      (expect (pdf-text-line-text (car (nth 2 pages))) :to-equal "on five")))

  (it "drops a form outside the requested range"
    (expect (pdf-text--mupdf-parse
             (format "%S\n" (pdf-text-tests--form
                             "stray" (list (pdf-text-tests--run 0 5))
                             :page 9))
             1 2)
            :to-equal '(nil nil)))

  (it "reads an empty output as empty pages"
    (expect (pdf-text--mupdf-parse "" 1 2) :to-equal '(nil nil)))

  ;; the document-wide Type 3 detection: modal em size orders of
  ;; magnitude under modal glyph height marks the class, and the
  ;; records build with the height standing in for the garbage em
  (it "detects a document of degenerate ems and floors the space test"
    (let ((pages (pdf-text--mupdf-parse
                  (format "%S\n" (pdf-text-tests--form
                                  "95113"
                                  (list (pdf-text-tests--run
                                         0 2 :x0 0.10 :x1 0.14
                                         :size 0.0003 :font "T33")
                                        (pdf-text-tests--run
                                         2 3 :x0 0.1406 :x1 0.18
                                         :size 0.0003 :font "T31"))))
                  1 1)))
      (expect (pdf-text-line-text (car (nth 0 pages))) :to-equal "95113")))

  (it "keeps exact ems over a healthy document"
    ;; logic p261's operator gap: the space TeX never wrote is still
    ;; restored when the ems are real
    (let ((pages (pdf-text--mupdf-parse
                  (format "%S\n" (pdf-text-tests--form
                                  "∗y"
                                  (list (pdf-text-tests--run
                                         0 1 :x0 0.10 :x1 0.1098
                                         :font "CMSY10")
                                        (pdf-text-tests--run
                                         1 1 :x0 0.1141 :x1 0.1235
                                         :font "CMMI12"))))
                  1 1)))
      (expect (pdf-text-line-text (car (nth 0 pages))) :to-equal "∗ y"))))

(describe "pdf-text--mupdf-output pool"
  (it "fans a large range over workers and reassembles every page"
    (let* ((stub (make-temp-file "pdf-text-stub" nil ".sh"))
           (pdf-text-mupdf-program stub)
           (pdf-text-mupdf-pool-min 2)
           (pdf-text-mupdf-workers 2))
      (with-temp-file stub
        (insert "#!/bin/sh\n"
                "for p in $(seq \"$4\" \"$5\"); do\n"
                "printf '(%s 0.1 0.3 0.9 0.34 0.0134 0.005 0.3 0.02 0"
                " ((0 6 0.1 0.2 0.34 0.0134 0.016 nil nil \"F\"))"
                " \"page-%s\")\\n' \"$p\" \"$p\"\n"
                "done\n"))
      (set-file-modes stub #o755)
      (unwind-protect
          (let ((pages (pdf-text--mupdf-pages "/tmp/fake.pdf" 1 4)))
            (expect (length pages) :to-be 4)
            (expect (mapcar (lambda (lines)
                              (pdf-text-line-text (car lines)))
                            pages)
                    :to-equal '("page-1" "page-2" "page-3" "page-4")))
        (delete-file stub)))))

(describe "pdf-text--walker-file"
  (it "writes the walker once and reuses the file"
    (let ((pdf-text--walker-cache nil))
      (let ((first (pdf-text--walker-file)))
        (unwind-protect
            (progn
              (expect (pdf-text--walker-file) :to-equal first)
              (expect (with-temp-buffer
                        (insert-file-contents first)
                        (buffer-string))
                      :to-equal pdf-text--walker-source))
          (delete-file first))))))

(describe "pdf-text--page-lines"
  (it "reads plain text lines with no geometry"
    (let ((lines (pdf-text--page-lines "one\ntwo")))
      (expect (mapcar #'pdf-text-line-text lines) :to-equal '("one" "two"))
      (expect (pdf-text-line-x0 (car lines)) :to-be nil)))

  (it "strips what the page never prints"
    ;; the Spanish grammar leaves the discretionary hyphen at the head
    ;; of the continuation line and a BELL inside the heading's text
    (let ((lines (pdf-text--page-lines
                  "1.2.9\aGender\n\u00ADapplied to hu\u00ADmans\ninforma\u00AD")))
      (expect (mapcar #'pdf-text-line-text lines)
              :to-equal '("1.2.9Gender" "applied to humans" "informa\u00AD")))))

(describe "pdf-text--strip-unprinted"
  (it "removes a soft hyphen anywhere but the line's end"
    (expect (pdf-text--strip-unprinted "\u00ADapplied to hu\u00ADmans")
            :to-equal "applied to humans"))

  (it "keeps the line-final soft hyphen the wrap join reads"
    (expect (pdf-text--strip-unprinted "informa\u00AD")
            :to-equal "informa\u00AD"))

  (it "removes control garbage, tab excepted"
    (expect (pdf-text--strip-unprinted "a\ab\u001Fc\td")
            :to-equal "abc\td"))

  (it "reads every typographic space as the plain space it prints as"
    ;; en spaces in a heading, em-space runs padding a table row: the
    ;; page shows blanks, Emacs highlights the characters
    (expect (pdf-text--strip-unprinted
             "1.2.9\u2002Gender\u00A0of\u2003nouns\u2009here\u202Fnow")
            :to-equal "1.2.9 Gender of nouns here now"))

  (it "leaves the zero-width space, which the escapes own"
    (expect (pdf-text--strip-unprinted "x_\u200B{i}")
            :to-equal "x_\u200B{i}")))

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
      (expect (plist-get profile :right) :to-be nil)))

  (it "reads the modal body span past the furniture"
    ;; a narrow centred head over a column of body lines, three pages:
    ;; the span starts at the first column-anchored line, not the head
    (let* ((page (pdf-text-tests--page
                  '(("Running Head" :x0 0.35 :x1 0.62 :base 0.05)
                    ("body prose filling the column" :base 0.15)
                    ("body prose filling the column")
                    ("body prose filling the column")
                    ("body prose filling the column"))))
           (profile (pdf-text--profile (list page page page))))
      (expect (plist-get profile :text-top) :to-be-close-to 0.15 3)
      (expect (plist-get profile :text-bottom) :to-be-close-to 0.21 3)))

  (it "carries no span without geometry to measure one from"
    (let ((profile (pdf-text--profile (list (pdf-text--page-lines "a\nb")))))
      (expect (plist-get profile :text-top) :to-be nil)
      (expect (plist-get profile :text-bottom) :to-be nil))))

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

;;; Reading-order repair

(describe "pdf-text--merge-script-fragments"
  (let ((profile '(:height 0.015 :leading 0.02 :left 0.10 :right 0.90
                   :space 0.005)))
    (it "rejoins a script fragment to its typeset line"
      (let* ((lines (pdf-text-tests--page
                     '(("that is, A |= ∧m" :x0 0.10 :x1 0.45 :base 0.40)
                       ("k=1 Fjk ." :x0 0.44 :x1 0.50 :base 0.403))))
             (merged (pdf-text--merge-script-fragments lines profile)))
        (expect (length merged) :to-be 1)
        (expect (pdf-text-line-text (car merged))
                :to-equal "that is, A |= ∧m k=1 Fjk .")
        (expect (pdf-text-line-x1 (car merged)) :to-be-close-to 0.50 2)
        ;; the host's baseline speaks for the merged line
        (expect (pdf-text-line-base (car merged)) :to-be-close-to 0.40 4)))

    (it "keeps a page number away from the entry it shares a baseline with"
      (let* ((lines (pdf-text-tests--page
                     '(("2.1 Syntax" :x0 0.10 :x1 0.40 :base 0.40)
                       ("33" :x0 0.85 :x1 0.90 :base 0.40))))
             (merged (pdf-text--merge-script-fragments lines profile)))
        (expect (length merged) :to-be 2)))

    (it "keeps real next lines apart"
      (let* ((lines (pdf-text-tests--page
                     '(("first line of the paragraph runs on" :x1 0.90 :base 0.40)
                       ("second line continues below it" :x1 0.88 :base 0.42))))
             (merged (pdf-text--merge-script-fragments lines profile)))
        (expect (length merged) :to-be 2)))

    (it "leaves the running head and its folio to the marginal rules"
      (let* ((lines (pdf-text-tests--page
                     '(("Preface" :x0 0.40 :x1 0.55 :base 0.06)
                       ("xiv" :x0 0.56 :x1 0.60 :base 0.06))))
             (merged (pdf-text--merge-script-fragments lines profile)))
        (expect (length merged) :to-be 2)))

    (it "wraps a dropped smaller fragment as the subscript it is"
      (let* ((lines (pdf-text-tests--page
                     '(("sound, ∧" :x0 0.10 :x1 0.45 :base 0.40)
                       ("k=1" :x0 0.45 :x1 0.50 :base 0.403 :height 0.0089))))
             (merged (pdf-text--merge-script-fragments lines profile)))
        (expect (pdf-text-line-text (car merged)) :to-equal "sound, ∧_{k=1}")))

    (it "wraps a raised smaller fragment as the superscript it is"
      (let* ((lines (pdf-text-tests--page
                     '(("the value x" :x0 0.10 :x1 0.30 :base 0.40)
                       ("m" :x0 0.30 :x1 0.32 :base 0.3936 :height 0.0089))))
             (merged (pdf-text--merge-script-fragments lines profile)))
        (expect (pdf-text-line-text (car merged)) :to-equal "the value x^{m}")))

    (it "prefixes a raised leading fragment, a footnote marker before its note"
      ;; the alignment paper raises each note's marker as a record of
      ;; its own, served before the note's first line
      (let* ((lines (pdf-text-tests--page
                     '(("*" :x0 0.098 :x1 0.105 :base 0.7957 :height 0.0083)
                       ("Work done under partial support" :x0 0.1179 :x1 0.90
                        :base 0.8027 :height 0.0125))))
             (merged (pdf-text--merge-script-fragments lines profile)))
        (expect (length merged) :to-be 1)
        (expect (pdf-text-line-text (car merged))
                :to-equal "^{*}Work done under partial support")))

    (it "joins a same-baseline leading fragment flat, a bullet not a script"
      (let* ((lines (pdf-text-tests--page
                     '(("•" :x0 0.098 :x1 0.105 :base 0.40 :height 0.0089)
                       ("item text follows the bullet" :x0 0.1179 :x1 0.60
                        :base 0.40))))
             (merged (pdf-text--merge-script-fragments lines profile)))
        (expect (pdf-text-line-text (car merged))
                :to-equal "• item text follows the bullet")))

    (it "keeps a wordy leading fragment as the plain text it is"
      (let* ((lines (pdf-text-tests--page
                     '(("the" :x0 0.10 :x1 0.13 :base 0.3936 :height 0.0089)
                       ("value continues along the line" :x0 0.131 :x1 0.60
                        :base 0.40))))
             (merged (pdf-text--merge-script-fragments lines profile)))
        (expect (pdf-text-line-text (car merged))
                :to-equal "the value continues along the line")))

    (it "keeps a raised alpha fragment flat, a math limit not a marker"
      ;; logic's display math serves the ∧'s upper limit before its
      ;; line; gluing it on as ^{m} would read as scripting the line's
      ;; first word (the corpus caught exactly this)
      (let* ((lines (pdf-text-tests--page
                     '(("m" :x0 0.30 :x1 0.32 :base 0.3936 :height 0.0089)
                       ("theorem continues the sentence" :x0 0.10 :x1 0.60
                        :base 0.40))))
             (merged (pdf-text--merge-script-fragments lines profile)))
        (expect (pdf-text-line-text (car merged))
                :to-equal "m theorem continues the sentence")))

    (it "stacks both limits of an operator, superscript then subscript"
      (let* ((lines (pdf-text-tests--page
                     '(("that is, A |= ∧" :x0 0.10 :x1 0.45 :base 0.40)
                       ("m" :x0 0.45 :x1 0.47 :base 0.3936 :height 0.0089)
                       ("k=1" :x0 0.45 :x1 0.50 :base 0.403 :height 0.0089))))
             (merged (pdf-text--merge-script-fragments lines profile)))
        (expect (length merged) :to-be 1)
        (expect (pdf-text-line-text (car merged))
                :to-equal "that is, A |= ∧^{m}_{k=1}")))

    (it "attaches a fragment the glyph pass dissected without a space"
      (let* ((lines (pdf-text-tests--page
                     '(("A |= ∧^{m}" :x0 0.10 :x1 0.45 :base 0.40)
                       ("_{k=1} F_{jk} ." :x0 0.44 :x1 0.50 :base 0.4001
                        :height 0.0134))))
             (merged (pdf-text--merge-script-fragments lines profile)))
        (expect (pdf-text-line-text (car merged))
                :to-equal "A |= ∧^{m}_{k=1} F_{jk} .")))

    (it "keeps a long offset fragment as text, limits being a few glyphs"
      (let* ((lines (pdf-text-tests--page
                     '(("⟨ (11) i(x ∗ y) ⇒" :x0 0.10 :x1 0.45 :base 0.40)
                       ("(4) i(y) ∗ i(x)" :x0 0.45 :x1 0.70 :base 0.403
                        :height 0.0089))))
             (merged (pdf-text--merge-script-fragments lines profile)))
        (expect (pdf-text-line-text (car merged))
                :to-equal "⟨ (11) i(x ∗ y) ⇒ (4) i(y) ∗ i(x)")))))

(describe "pdf-text--reassemble-zones"
  (let ((profile '(:height 0.015 :leading 0.02 :left 0.10 :right 0.90
                   :space 0.005)))
    (it "puts a scattered aligned array back into rows"
      (let* ((lines (pdf-text-tests--page
                     '(("the final set of rewrite rules are" :x0 0.10 :x1 0.55
                        :base 0.30)
                       ("1" :x0 0.30 :x1 0.31 :base 0.34)
                       ("e∗x" :x0 0.35 :x1 0.40 :base 0.34)
                       ("2" :x0 0.30 :x1 0.31 :base 0.36)
                       ("i(x) ∗ x" :x0 0.35 :x1 0.42 :base 0.36)
                       ("→" :x0 0.50 :x1 0.52 :base 0.34)
                       ("→" :x0 0.50 :x1 0.52 :base 0.36)
                       ("x" :x0 0.60 :x1 0.61 :base 0.34)
                       ("e" :x0 0.60 :x1 0.61 :base 0.36)
                       ("All the critical pairs can be reduced." :x0 0.10
                        :x1 0.88 :base 0.40))))
             (rows (pdf-text--reassemble-zones lines profile)))
        (expect (length rows) :to-be 4)
        (expect (pdf-text-line-text (nth 1 rows)) :to-equal "1 e∗x → x")
        (expect (pdf-text-line-text (nth 2 rows)) :to-equal "2 i(x) ∗ x → e")))

    (it "leaves a two-column page as it came"
      (let* ((specs (append
                     (cl-loop for n from 0 below 6
                              collect (list (format "left column line %d" n)
                                            :x0 0.10 :x1 0.45
                                            :base (+ 0.10 (* n 0.02))))
                     (cl-loop for n from 0 below 6
                              collect (list (format "right column line %d" n)
                                            :x0 0.55 :x1 0.90
                                            :base (+ 0.10 (* n 0.02))))))
             (lines (pdf-text-tests--page specs))
             (out (pdf-text--reassemble-zones lines profile)))
        (expect (mapcar #'pdf-text-line-text out)
                :to-equal (mapcar (lambda (spec) (car spec)) specs))))

    (it "does not fold the folio's jump back up the page into a zone"
      (let* ((specs '(("a first full line of body prose here" :base 0.60)
                      ("a second full line of body prose too" :base 0.62)
                      ("a third full line closes the page" :base 0.64)
                      ("255" :x0 0.85 :x1 0.90 :base 0.05)))
             (lines (pdf-text-tests--page specs))
             (out (pdf-text--reassemble-zones lines profile)))
        (expect (mapcar #'pdf-text-line-text out)
                :to-equal (mapcar #'car specs))))

    (it "does not read a drop cap's jump back as a zone"
      ;; the initial spans its paragraph's first lines, so its baseline
      ;; sits at the last of them while poppler emits it first: the
      ;; step back to the paragraph's first line is typesetting, not a
      ;; column run.  Without the guard the cap lands beside the second
      ;; line - "T understand learning" - and the first dangles bare
      (let* ((specs '(("T" :x0 0.10 :x1 0.14 :base 0.315 :height 0.05)
                      ("his book can make a profound difference"
                       :x0 0.145 :x1 0.75 :base 0.30)
                      ("understand learning. You will learn"
                       :x0 0.145 :x1 0.86 :base 0.32)
                      ("efficient techniques researchers know"
                       :x0 0.10 :x1 0.87 :base 0.34)))
             (lines (pdf-text-tests--page specs))
             (out (pdf-text--reassemble-zones lines profile)))
        (expect (mapcar #'pdf-text-line-text out)
                :to-equal (mapcar #'car specs))))))

(describe "pdf-text-reading-order"
  (it "merges and reorders, never drops a glyph"
    (let* ((pages (list (pdf-text-tests--page
                         '(("a first line of prose pins the leading" :base 0.26)
                           ("a second line of prose pins it too" :base 0.28)
                           ("prose before the display runs full" :base 0.30)
                           ("1" :x0 0.30 :x1 0.31 :base 0.34)
                           ("e∗x" :x0 0.35 :x1 0.40 :base 0.34)
                           ("2" :x0 0.30 :x1 0.31 :base 0.36)
                           ("i(x) ∗ x" :x0 0.35 :x1 0.42 :base 0.36)
                           ("→" :x0 0.50 :x1 0.52 :base 0.34)
                           ("→" :x0 0.50 :x1 0.52 :base 0.36)
                           ("prose after, A |= ∧m" :x0 0.10 :x1 0.45 :base 0.40)
                           ("k=1 ." :x0 0.44 :x1 0.50 :base 0.403)))))
           (glyphs (lambda (lines)
                     (sort (string-to-list
                            (replace-regexp-in-string
                             "[^[:alnum:]]" ""
                             (mapconcat #'pdf-text-line-text lines "")))
                           #'<)))
           (before (funcall glyphs (car pages)))
           (after (funcall glyphs (car (pdf-text-reading-order pages)))))
      (expect after :to-equal before)))

  (it "passes a page without geometry through untouched"
    (let ((pages (list (mapcar (lambda (text) (pdf-text-line-create :text text))
                               '("plain first line" "plain second line")))))
      (expect (mapcar #'pdf-text-line-text (car (pdf-text-reading-order pages)))
              :to-equal '("plain first line" "plain second line")))))

;;; Multicolumn lanes

(defun pdf-text-tests--lanes (specs)
  "SPECS as one page run through `pdf-text--mark-lanes'."
  (let* ((lines (pdf-text-tests--page specs))
         (profile (pdf-text--profile (list lines))))
    (pdf-text--mark-lanes lines profile)))

(defconst pdf-text-tests--lane-prose
  '(("a first line of prose pins the leading and column" :base 0.12)
    ("a second line of prose pins them too" :base 0.14))
  "Full-measure lines that give a lane fixture its body profile.")

(describe "pdf-text--mark-lanes"
  (it "reads row-served lanes as an org table"
    (let ((out (pdf-text-tests--lanes
                (append pdf-text-tests--lane-prose
                        '(("Name" :x0 0.10 :x1 0.16 :base 0.18)
                          ("Size" :x0 0.35 :x1 0.41 :gap 0)
                          ("Note" :x0 0.60 :x1 0.66 :gap 0)
                          ("alpha" :x0 0.10 :x1 0.20)
                          ("small" :x0 0.35 :x1 0.45 :gap 0)
                          ("first kind" :x0 0.60 :x1 0.75 :gap 0)
                          ("beta" :x0 0.10 :x1 0.18)
                          ("large" :x0 0.35 :x1 0.46 :gap 0)
                          ("second" :x0 0.60 :x1 0.70 :gap 0)
                          ("prose resumes at the full measure after" :base 0.26))))))
      (expect (mapcar #'pdf-text-line-text out)
              :to-equal '("a first line of prose pins the leading and column"
                          "a second line of prose pins them too"
                          "| Name  | Size  | Note       |"
                          "| alpha | small | first kind |"
                          "| beta  | large | second     |"
                          "prose resumes at the full measure after"))
      (expect (pdf-text-line-kind (nth 2 out)) :to-equal 'row)))

  (it "joins a wrapped cell into its row"
    (let ((out (pdf-text-tests--lanes
                (append pdf-text-tests--lane-prose
                        '(("alpha" :x0 0.10 :x1 0.20 :base 0.18)
                          ("small" :x0 0.35 :x1 0.45 :gap 0)
                          ("first kind" :x0 0.60 :x1 0.75 :gap 0)
                          ("of note" :x0 0.62 :x1 0.72)
                          ("beta" :x0 0.10 :x1 0.18)
                          ("large" :x0 0.35 :x1 0.46 :gap 0)
                          ("second" :x0 0.60 :x1 0.70 :gap 0)
                          ("gamma" :x0 0.10 :x1 0.21)
                          ("mid" :x0 0.35 :x1 0.42 :gap 0)
                          ("third" :x0 0.60 :x1 0.68 :gap 0))))))
      (expect (mapcar #'pdf-text-line-text out)
              :to-contain "| alpha | small | first kind of note |")))

  (it "routes a tab-prefixed continuation by its right edge"
    ;; leading tab glyphs are excluded from ink, so the record's x0
    ;; lies left of its lane; the ink's end names the lane
    (let ((out (pdf-text-tests--lanes
                (append pdf-text-tests--lane-prose
                        '(("alpha" :x0 0.10 :x1 0.20 :base 0.18)
                          ("small" :x0 0.35 :x1 0.45 :gap 0)
                          ("first kind" :x0 0.60 :x1 0.75 :gap 0)
                          ("\t\tof note" :x0 0.40 :x1 0.72)
                          ("beta" :x0 0.10 :x1 0.18)
                          ("large" :x0 0.35 :x1 0.46 :gap 0)
                          ("second" :x0 0.60 :x1 0.70 :gap 0)
                          ("gamma" :x0 0.10 :x1 0.21)
                          ("mid" :x0 0.35 :x1 0.42 :gap 0)
                          ("third" :x0 0.60 :x1 0.68 :gap 0))))))
      (expect (mapcar #'pdf-text-line-text out)
              :to-contain "| alpha | small | first kind of note |")))

  (it "splits a tab-joined record into its row's cells"
    (let ((out (pdf-text-tests--lanes
                (append pdf-text-tests--lane-prose
                        '(("alpha" :x0 0.10 :x1 0.20 :base 0.18)
                          ("small" :x0 0.35 :x1 0.45 :gap 0)
                          ("first" :x0 0.60 :x1 0.70 :gap 0)
                          ("beta\tlarge" :x0 0.10 :x1 0.46)
                          ("second" :x0 0.60 :x1 0.70 :gap 0)
                          ("gamma" :x0 0.10 :x1 0.21)
                          ("mid" :x0 0.35 :x1 0.42 :gap 0)
                          ("third" :x0 0.60 :x1 0.68 :gap 0)
                          ("delta" :x0 0.10 :x1 0.20)
                          ("wide" :x0 0.35 :x1 0.44 :gap 0)
                          ("fourth" :x0 0.60 :x1 0.70 :gap 0))))))
      (expect (mapcar #'pdf-text-line-text out)
              :to-contain "| beta  | large | second |")))

  (it "sets a literal bar inside a cell as a broken bar"
    (let ((out (pdf-text-tests--lanes
                (append pdf-text-tests--lane-prose
                        '(("al|pha" :x0 0.10 :x1 0.20 :base 0.18)
                          ("small" :x0 0.35 :x1 0.45 :gap 0)
                          ("beta" :x0 0.10 :x1 0.18)
                          ("large" :x0 0.35 :x1 0.46 :gap 0)
                          ("gamma" :x0 0.10 :x1 0.21)
                          ("mid" :x0 0.35 :x1 0.42 :gap 0))))))
      (expect (mapcar #'pdf-text-line-text out)
              :to-contain "| al¦pha | small |")))

  (it "pairs a numeric lane with its entries served lane by lane"
    (let ((out (pdf-text-tests--lanes
                (append pdf-text-tests--lane-prose
                        '(("One thing" :x0 0.10 :x1 0.28 :base 0.18)
                          ("Another entry" :x0 0.10 :x1 0.33)
                          ("Third" :x0 0.10 :x1 0.22)
                          ("11" :x0 0.85 :x1 0.88 :base 0.18)
                          ("12" :x0 0.85 :x1 0.88)
                          ("13" :x0 0.85 :x1 0.88))))))
      (expect (mapcar #'pdf-text-line-text out)
              :to-equal '("a first line of prose pins the leading and column"
                          "a second line of prose pins them too"
                          "| One thing     | 11 |"
                          "| Another entry | 12 |"
                          "| Third         | 13 |"))))

  (it "adopts a stray pair against the block's gutters"
    (let ((out (pdf-text-tests--lanes
                (append pdf-text-tests--lane-prose
                        '(("Stray" :x0 0.10 :x1 0.24 :base 0.18)
                          ("9" :x0 0.85 :x1 0.88 :gap 0)
                          ("a chapter line running the full measure" :base 0.22)
                          ("One thing" :x0 0.10 :x1 0.28 :base 0.26)
                          ("Another entry" :x0 0.10 :x1 0.33)
                          ("Third" :x0 0.10 :x1 0.22)
                          ("11" :x0 0.85 :x1 0.88 :base 0.26)
                          ("12" :x0 0.85 :x1 0.88)
                          ("13" :x0 0.85 :x1 0.88))))))
      (expect (mapcar #'pdf-text-line-text out)
              :to-contain "| Stray | 9 |")))

  (it "reads column-served ragged pairs as rows"
    (let ((out (pdf-text-tests--lanes
                (append pdf-text-tests--lane-prose
                        '(("Me caí por unas escaleras" :x0 0.10 :x1 0.30 :base 0.18)
                          ("Voy a tomarme unas vacaciones" :x0 0.10 :x1 0.35)
                          ("unos pantalones" :x0 0.10 :x1 0.28)
                          ("Llevaba unas botas" :x0 0.10 :x1 0.33)
                          ("I fell down some stairs" :x0 0.50 :x1 0.70 :base 0.18)
                          ("I will have a holiday" :x0 0.50 :x1 0.78)
                          ("a pair of trousers" :x0 0.50 :x1 0.66)
                          ("She wore blue boots" :x0 0.50 :x1 0.74))))))
      (expect (mapcar #'pdf-text-line-text out)
              :to-contain
              "| Me caí por unas escaleras     | I fell down some stairs |")))

  (it "reads an enumerated two-up list as flows in page order"
    ;; the stream serves the right lane first; the reader still gets
    ;; 1 through 8
    (let ((out (pdf-text-tests--lanes
                (append pdf-text-tests--lane-prose
                        '(("5. epsilon" :x0 0.50 :x1 0.66 :base 0.18)
                          ("6. zeta" :x0 0.50 :x1 0.62)
                          ("7. eta" :x0 0.50 :x1 0.60)
                          ("8. theta" :x0 0.50 :x1 0.64)
                          ("1. alfa" :x0 0.10 :x1 0.25 :base 0.18)
                          ("2. beta" :x0 0.10 :x1 0.26)
                          ("3. gamma" :x0 0.10 :x1 0.28)
                          ("4. delta" :x0 0.10 :x1 0.27))))))
      (expect (mapcar #'pdf-text-line-text out)
              :to-equal '("a first line of prose pins the leading and column"
                          "a second line of prose pins them too"
                          "1. alfa" "2. beta" "3. gamma" "4. delta"
                          "5. epsilon" "6. zeta" "7. eta" "8. theta"))
      (expect (pdf-text-line-kind (nth 2 out)) :to-equal 'fixed)
      (expect (pdf-text-line-claimed (nth 2 out)) :to-be-truthy)))

  (it "leaves flush facing columns reflowing"
    (let* ((specs (append pdf-text-tests--lane-prose
                          '(("first column prose runs on" :x0 0.10 :x1 0.48 :base 0.18)
                            ("and keeps running to its" :x0 0.10 :x1 0.48)
                            ("edge every line justified" :x0 0.10 :x1 0.48)
                            ("flush against the middle" :x0 0.10 :x1 0.48)
                            ("second column prose runs" :x0 0.52 :x1 0.90 :base 0.18)
                            ("just as flush against its" :x0 0.52 :x1 0.90)
                            ("own right edge line after" :x0 0.52 :x1 0.90)
                            ("line to the page bottom" :x0 0.52 :x1 0.90))))
           (out (pdf-text-tests--lanes specs)))
      (expect (mapcar #'pdf-text-line-text out)
              :to-equal (mapcar #'car specs))
      (expect (pdf-text-line-kind (nth 2 out)) :to-be nil)
      (expect (pdf-text-line-claimed (nth 2 out)) :to-be-truthy)))

  (it "leaves a margin note beside the body alone"
    (let* ((specs (append pdf-text-tests--lane-prose
                          '(("body prose sharing its baseline with a note" :x0 0.10 :x1 0.85 :base 0.18)
                            ("a note" :x0 0.92 :x1 0.99 :gap 0)
                            ("more body prose at the full measure here" :x0 0.10 :x1 0.85)
                            ("beside it" :x0 0.92 :x1 0.99 :gap 0)
                            ("and a third body line to make three rows" :x0 0.10 :x1 0.85)
                            ("hangs on" :x0 0.92 :x1 0.99 :gap 0))))
           (out (pdf-text-tests--lanes specs)))
      (expect (mapcar #'pdf-text-line-text out)
              :to-equal (mapcar #'car specs))
      (expect (cl-notany #'pdf-text-line-claimed out) :to-be-truthy)))

  (it "leaves an aligned array to the zone repair"
    (let* ((specs (append pdf-text-tests--lane-prose
                          '(("l1" :x0 0.30 :x1 0.34 :base 0.18)
                            ("→" :x0 0.45 :x1 0.47 :gap 0)
                            ("x1" :x0 0.55 :x1 0.58 :gap 0)
                            ("l2" :x0 0.30 :x1 0.34)
                            ("→" :x0 0.45 :x1 0.47 :gap 0)
                            ("x2" :x0 0.55 :x1 0.58 :gap 0)
                            ("l3" :x0 0.30 :x1 0.34)
                            ("→" :x0 0.45 :x1 0.47 :gap 0)
                            ("x3" :x0 0.55 :x1 0.58 :gap 0))))
           (out (pdf-text-tests--lanes specs)))
      (expect (mapcar #'pdf-text-line-text out)
              :to-equal (mapcar #'car specs))))

  (it "leaves a listing's aligned columns alone"
    (let* ((specs (append pdf-text-tests--lane-prose
                          '(("let value = 10;" :x0 0.10 :x1 0.30 :cv 0.0 :base 0.18)
                            ("// first note" :x0 0.60 :x1 0.75 :cv 0.0 :gap 0)
                            ("let other = 20;" :x0 0.10 :x1 0.30 :cv 0.0)
                            ("// second one" :x0 0.60 :x1 0.75 :cv 0.0 :gap 0)
                            ("let third = 30;" :x0 0.10 :x1 0.30 :cv 0.0)
                            ("// third note" :x0 0.60 :x1 0.75 :cv 0.0 :gap 0))))
           (out (pdf-text-tests--lanes specs)))
      (expect (mapcar #'pdf-text-line-text out)
              :to-equal (mapcar #'car specs))))

  (it "takes three aligned rows and not two"
    (let* ((specs (append pdf-text-tests--lane-prose
                          '(("alpha" :x0 0.10 :x1 0.20 :base 0.18)
                            ("small" :x0 0.35 :x1 0.45 :gap 0)
                            ("beta" :x0 0.10 :x1 0.18)
                            ("large" :x0 0.35 :x1 0.46 :gap 0)
                            ("prose resumes at the full measure here" :base 0.24))))
           (out (pdf-text-tests--lanes specs)))
      (expect (mapcar #'pdf-text-line-text out)
              :to-equal (mapcar #'car specs))))

  (it "does not reorder three rows of accidental flows"
    ;; served lane by lane, three lanes, nothing pairing them: a flows
    ;; candidate, but three rows are not evidence enough to move records
    (let* ((specs (append pdf-text-tests--lane-prose
                          '(("uno label" :x0 0.10 :x1 0.22 :base 0.18)
                            ("under one" :x0 0.10 :x1 0.21)
                            ("third one" :x0 0.10 :x1 0.20)
                            ("dos label" :x0 0.40 :x1 0.52 :base 0.18)
                            ("under two" :x0 0.40 :x1 0.51)
                            ("third two" :x0 0.40 :x1 0.51)
                            ("tres label" :x0 0.70 :x1 0.83 :base 0.18)
                            ("under three" :x0 0.70 :x1 0.84)
                            ("third three" :x0 0.70 :x1 0.84))))
           (out (pdf-text-tests--lanes specs)))
      (expect (mapcar #'pdf-text-line-text out)
              :to-equal (mapcar #'car specs))))

  (it "reorders facing columns when the profile locked onto one of them"
    ;; a two-column paper has no full-measure prose to pin a wide
    ;; column: the modal edges ARE one column's, and the facing
    ;; column's cells must not read as margin notes - the guard
    ;; measures the text area, whose strong edges span both columns
    (let* ((specs '(("second column comes first in" :x0 0.52 :x1 0.90 :base 0.12)
                    ("the stream and runs justified" :x0 0.52 :x1 0.90)
                    ("to the shared page bottom by" :x0 0.52 :x1 0.90)
                    ("lines that face their twins" :x0 0.52 :x1 0.90)
                    ("over one more line of prose" :x0 0.52 :x1 0.90)
                    ("first column arrives after but" :x0 0.10 :x1 0.48 :base 0.12)
                    ("reads before its facing twin" :x0 0.10 :x1 0.48)
                    ("every line justified flush to" :x0 0.10 :x1 0.48)
                    ("the middle of the page here" :x0 0.10 :x1 0.48)))
           (out (pdf-text-tests--lanes specs)))
      (expect (mapcar #'pdf-text-line-text out)
              :to-equal '("first column arrives after but"
                          "reads before its facing twin"
                          "every line justified flush to"
                          "the middle of the page here"
                          "second column comes first in"
                          "the stream and runs justified"
                          "to the shared page bottom by"
                          "lines that face their twins"
                          "over one more line of prose"))
      ;; the fold is undone in the geometry too: both lanes land on the
      ;; modal column's frame - the right lane's here, it carries more
      ;; ink - and the second lane's baselines continue the first's
      (expect (pdf-text-line-x0 (nth 0 out)) :to-be-close-to 0.52 2)
      (expect (pdf-text-line-x0 (nth 4 out)) :to-be-close-to 0.52 2)
      (expect (pdf-text-line-base (nth 4 out))
              :to-be-greater-than (pdf-text-line-base (nth 3 out)))))

  (it "reads a smaller foot block after the columns, not inside a lane"
    ;; an author note resumes the left lane after a paragraph gap, set
    ;; under footnote size; swallowed into the lane it would read
    ;; before the facing column whose sentence it interrupts
    (let* ((specs '(("the right column runs beside" :x0 0.52 :x1 0.90 :base 0.12)
                    ("the body and keeps running" :x0 0.52 :x1 0.90)
                    ("past the point where the left" :x0 0.52 :x1 0.90)
                    ("column body already stopped" :x0 0.52 :x1 0.90)
                    ("and on down the shared page" :x0 0.52 :x1 0.90)
                    ("to its own last justified line" :x0 0.52 :x1 0.90)
                    ("closing at the bottom margin" :x0 0.52 :x1 0.90)
                    ("the left column body is set" :x0 0.10 :x1 0.48 :base 0.12)
                    ("in four justified lines that" :x0 0.10 :x1 0.48)
                    ("stop well short of the page" :x0 0.10 :x1 0.48)
                    ("bottom leaving room below" :x0 0.10 :x1 0.48)
                    ("thanks go to a colleague for" :x0 0.10 :x1 0.44 :base 0.22 :height 0.012)
                    ("reading drafts of this kindly" :x0 0.10 :x1 0.42 :height 0.012)))
           (out (pdf-text-tests--lanes specs))
           (note (cl-find "thanks go to a colleague for" out
                          :key #'pdf-text-line-text :test #'equal)))
      (expect (mapcar #'pdf-text-line-text out)
              :to-equal '("the left column body is set"
                          "in four justified lines that"
                          "stop well short of the page"
                          "bottom leaving room below"
                          "the right column runs beside"
                          "the body and keeps running"
                          "past the point where the left"
                          "column body already stopped"
                          "and on down the shared page"
                          "to its own last justified line"
                          "closing at the bottom margin"
                          "thanks go to a colleague for"
                          "reading drafts of this kindly"))
      (expect (pdf-text-line-claimed note) :to-be nil))))

(describe "pdf-text--lane-clean-row-p"
  (it "refuses a row outside the body span, which is page furniture"
    ;; bornstein even pages: folio and head served as a clean two-cell
    ;; row whose claim then bars both from the margin rules
    (let* ((spanned '(:height 0.014 :leading 0.014 :left 0.09 :right 0.86
                      :space 0.005 :text-top 0.10 :text-bottom 0.90))
           (bare '(:height 0.014 :leading 0.014 :left 0.09 :right 0.86
                   :space 0.005))
           (cells (pdf-text-tests--page
                   '(("94" :x0 0.09 :x1 0.11 :base 0.06)
                     ("ROBERT F. BORNSTEIN" :x0 0.30 :x1 0.60 :base 0.06))))
           (row (cons 0.06 (cl-loop for line in cells
                                    for i from 0
                                    collect (cons i line)))))
      (expect (pdf-text--lane-clean-row-p row spanned 0.015) :to-be nil)
      (expect (pdf-text--lane-clean-row-p row bare 0.015) :to-be-truthy))))

(describe "table rows through the pipeline"
  (it "escapes a literal bar-opened record at birth, on both paths"
    (expect (pdf-text--escape-literals "| a literal row |")
            :to-equal "\u200B| a literal row |")
    (expect (pdf-text--escape-literals "  | indented bar")
            :to-equal "  \u200B| indented bar")
    (expect (pdf-text--escape-literals "mid | line") :to-equal "mid | line")
    (let ((lines (pdf-text--page-lines "| x |\nplain line")))
      (expect (pdf-text-line-text (car lines)) :to-equal "\u200B| x |")))

  (it "passes a generated row through the org escape"
    (expect (pdf-text--escape-org-lines "| a | b |") :to-equal "| a | b |"))

  (it "renders a flows list tight, one item per line"
    (let* ((lines (pdf-text-tests--page
                   (append pdf-text-tests--lane-prose
                           '(("1. alfa" :x0 0.10 :x1 0.25 :base 0.18)
                             ("2. beta" :x0 0.10 :x1 0.26)
                             ("3. gamma" :x0 0.10 :x1 0.28)
                             ("4. delta" :x0 0.10 :x1 0.27)
                             ("5. epsilon" :x0 0.50 :x1 0.66 :base 0.18)
                             ("6. zeta" :x0 0.50 :x1 0.62)
                             ("7. eta" :x0 0.50 :x1 0.60)
                             ("8. theta" :x0 0.50 :x1 0.64)))))
           (out (car (pdf-text-render-lines (list lines)))))
      (expect out :to-match "1\\. alfa\n2\\. beta\n3\\. gamma")
      (expect out :to-match "4\\. delta\n5\\. epsilon")))

  (it "joins the sentence a column seam splits"
    ;; flush facing columns are one flow folded to fit the page: the
    ;; left column's hanging sentence continues at the right column's
    ;; head, and the unfolded geometry lets the ordinary join see it
    (let* ((lines (pdf-text-tests--page
                   '(("the right column opens on the" :x0 0.52 :x1 0.90 :base 0.12)
                     ("closing words of the sentence" :x0 0.52 :x1 0.90)
                     ("the left column left hanging" :x0 0.52 :x1 0.90)
                     ("in mid flight without an end" :x0 0.52 :x1 0.90)
                     ("carrying on to a full stop." :x0 0.52 :x1 0.90)
                     ("the left column runs justified" :x0 0.10 :x1 0.48 :base 0.12)
                     ("flush lines that do not close" :x0 0.10 :x1 0.48)
                     ("their sentence and instead go" :x0 0.10 :x1 0.48)
                     ("straight across the seam into" :x0 0.10 :x1 0.48))))
           (out (car (pdf-text-render-lines (list lines)))))
      (expect out :to-match
              "straight across the seam into the right column opens on the")))

  (it "keeps a claimed flows region out of the zone repair"
    (let* ((lines (pdf-text-tests--page
                   (append pdf-text-tests--lane-prose
                           '(("la nao ship" :x0 0.10 :x1 0.24 :base 0.18)
                             ("la foto photo" :x0 0.10 :x1 0.26)
                             ("la disco disco" :x0 0.10 :x1 0.27)
                             ("la mano hand" :x0 0.10 :x1 0.25)
                             ("el sol sun" :x0 0.40 :x1 0.53 :base 0.18)
                             ("el mar sea" :x0 0.40 :x1 0.52)
                             ("el pan bread" :x0 0.40 :x1 0.55)
                             ("el rey king" :x0 0.40 :x1 0.53)
                             ("la luz light" :x0 0.70 :x1 0.84 :base 0.18)
                             ("la paz peace" :x0 0.70 :x1 0.85)
                             ("la sal salt" :x0 0.70 :x1 0.83)
                             ("la red net" :x0 0.70 :x1 0.82)))))
           (out (car (pdf-text-reading-order (list lines)))))
      ;; the zone repair would have re-sorted these into baseline rows -
      ;; "la nao ship el sol sun la luz light" - as it did before the
      ;; lane pass claimed them
      (expect (mapcar #'pdf-text-line-text out)
              :to-equal '("a first line of prose pins the leading and column"
                          "a second line of prose pins them too"
                          "la nao ship" "la foto photo" "la disco disco"
                          "la mano hand"
                          "el sol sun" "el mar sea" "el pan bread" "el rey king"
                          "la luz light" "la paz peace" "la sal salt"
                          "la red net"))))

  (it "conserves every glyph through a table merge"
    (let* ((lines (pdf-text-tests--page
                   (append pdf-text-tests--lane-prose
                           '(("alpha" :x0 0.10 :x1 0.20 :base 0.18)
                             ("small" :x0 0.35 :x1 0.45 :gap 0)
                             ("beta" :x0 0.10 :x1 0.18)
                             ("large" :x0 0.35 :x1 0.46 :gap 0)
                             ("gamma" :x0 0.10 :x1 0.21)
                             ("mid" :x0 0.35 :x1 0.42 :gap 0)))))
           (glyphs (lambda (records)
                     (sort (string-to-list
                            (replace-regexp-in-string
                             "[^[:alnum:]]" ""
                             (mapconcat #'pdf-text-line-text records "")))
                           #'<)))
           (before (funcall glyphs lines))
           (after (funcall glyphs (car (pdf-text-reading-order (list lines))))))
      (expect after :to-equal before))))

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

(describe "pdf-text--mark-monospace font names"
  (it "reads a code font as a listing however short the line"
    (let ((lines (list (pdf-text-tests--line "}" :x0 0.20 :x1 0.22 :cv nil
                                             :font "UbuntuMono-Regular"
                                             :lead-font "UbuntuMono-Regular"))))
      (pdf-text--mark-monospace lines)
      (expect (pdf-text-line-kind (car lines)) :to-be 'mono)))

  (it "keeps prose that a wide code identifier dominates"
    (let ((lines (list (pdf-text-tests--line
                        "call frobnicate_the_widget to spin it"
                        :font "UbuntuMono-Regular"
                        :lead-font "MinionPro-Regular"))))
      (pdf-text--mark-monospace lines)
      (expect (pdf-text-line-kind (car lines)) :to-be nil))))

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

(describe "pdf-text-mathish-text-p"
  (it "reads operator-dense text as mathematics"
    (expect (pdf-text-mathish-text-p "φ ∧ ψ ⊢ ψ ∧ φ") :to-be-truthy)
    (expect (pdf-text-mathish-text-p "H2 : ` (A → (B → C)) → ((A → B) → (A → C)),")
            :to-be-truthy))

  (it "reads single-letter variables beside an operator as mathematics"
    (expect (pdf-text-mathish-text-p "e∗x") :to-be-truthy)
    (expect (pdf-text-mathish-text-p "13 x ∗ (i(x) ∗ y)") :to-be-truthy))

  (it "reads prose as prose"
    (expect (pdf-text-mathish-text-p "The fact that variables are only allowed")
            :to-be nil)
    (expect (pdf-text-mathish-text-p "Mia, Philippe and Sylvie, my children,")
            :to-be nil)
    (expect (pdf-text-mathish-text-p "(a) There is a possible world") :to-be nil)
    (expect (pdf-text-mathish-text-p
             "• pickEquation(E) will pick an equation from E;")
            :to-be nil)))

(describe "pdf-text--mark-math"
  (let ((profile '(:height 0.015 :leading 0.02 :left 0.10 :right 0.90
                   :space 0.005)))
    (it "tags a displayed math line"
      (let ((lines (pdf-text-tests--page
                    '(("H1 : ` (A → (B → A))," :x0 0.31 :x1 0.52)))))
        (pdf-text--mark-math lines profile)
        (expect (pdf-text-line-kind (car lines)) :to-equal 'math)))

    (it "leaves inline math inside a full prose line alone"
      (let ((lines (pdf-text-tests--page
                    '(("for any formulas A and B, A |= B iff |= A → B holds"
                       :x0 0.10 :x1 0.90)))))
        (pdf-text--mark-math lines profile)
        (expect (pdf-text-line-kind (car lines)) :to-be nil)))

    (it "leaves an inset quotation alone"
      (let ((lines (pdf-text-tests--page
                    '(("a passage set in from the margin reads" :x0 0.20 :x1 0.75)
                      ("as a quotation and not as mathematics" :x0 0.20 :x1 0.73)))))
        (pdf-text--mark-math lines profile)
        (expect (mapcar #'pdf-text-line-kind lines) :to-equal '(nil nil))))

    (it "does not touch a line the monospace rule owns"
      (let ((lines (pdf-text-tests--page
                    '(("x = f(y, z);" :x0 0.31 :x1 0.52 :cv 0.0)))))
        (pdf-text--mark-monospace lines)
        (pdf-text--mark-math lines profile)
        (expect (pdf-text-line-kind (car lines)) :to-equal 'mono)))

    (it "spreads to a wordless neighbour of the display"
      (let ((lines (pdf-text-tests--page
                    '(("⟨" :x0 0.55 :x1 0.57)
                      ("(11) i(x ∗ y)" :x0 0.58 :x1 0.69 :gap 0.6)))))
        (pdf-text--mark-math lines profile)
        (expect (mapcar #'pdf-text-line-kind lines) :to-equal '(math math))))

    (it "reads a math-font display as maths whatever its letters say"
      ;; a display whose letters outnumber its operators fails the
      ;; codepoint density; the face knows
      (let ((lines (pdf-text-tests--page
                    '(("eval maps values into meanings"
                       :x0 0.30 :x1 0.60 :font "TTNUXI+CMMI12")))))
        (pdf-text--mark-math lines profile)
        (expect (pdf-text-line-kind (car lines)) :to-equal 'math)))

    (it "keeps a displayed line in the body face prose"
      (let ((lines (pdf-text-tests--page
                    '(("a short displayed line of plain words"
                       :x0 0.30 :x1 0.60 :font "NimbusRomNo9L-Regu")))))
        (pdf-text--mark-math lines profile)
        (expect (pdf-text-line-kind (car lines)) :to-be nil)))))

(describe "pdf-text display maths"
  (it "renders a display block verbatim between its paragraphs"
    (let ((rendered (pdf-text-tests--render
                     '(("The Hilbert system H contains three axiom" :x1 0.90)
                       ("rules plus MP, the rule of modus ponens:" :x1 0.63)
                       ("H1 : ` (A → (B → A))," :x0 0.31 :x1 0.52 :gap 1.6)
                       ("H2 : ` (A → (B → C)) → ((A → B) → (A → C)),"
                        :x0 0.31 :x1 0.75)
                       ("MP : A, A → B ` B." :x0 0.31 :x1 0.49)
                       ("The axiom rule H3 is not needed if the arrow"
                        :x1 0.90 :gap 1.6)
                       ("is the only operator in all the formulas." :x1 0.60)))))
      (expect rendered :to-equal
              (concat "The Hilbert system H contains three axiom"
                      " rules plus MP, the rule of modus ponens:"
                      "\n\n"
                      "  H1 : ` (A → (B → A)),\n"
                      "  H2 : ` (A → (B → C)) → ((A → B) → (A → C)),\n"
                      "  MP : A, A → B ` B."
                      "\n\n"
                      "The axiom rule H3 is not needed if the arrow"
                      " is the only operator in all the formulas."))))

  (it "keeps an enumerated displayed equation out of the lists"
    (let ((rendered (pdf-text-tests--render
                     '(("and generates the twelfth rewrite rule:" :x1 0.55)
                       ("12 i(x ∗ y) → i(y) ∗ i(x)" :x0 0.41 :x1 0.64 :gap 1.6)
                       ("The final critical pair comes from the rules"
                        :x1 0.90 :gap 1.6)))))
      (expect rendered :to-match "^  12 i(x")
      (expect rendered :not :to-match "^- "))))

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

(describe "pdf-text soft hyphens the page printed"
  (it "renders a kept soft-hyphen wrap as a plain hyphen"
    ;; the keep branch concatenates the wrap char as the page wrote
    ;; it; a soft hyphen would reach the reader as the odd glyph the
    ;; Spanish grammar report showed, so the rendered page trades it
    (expect (pdf-text-tests--render
             '(("the range spans pages 3\u00AD")
               ("10 and no further, it says")
               ("body line three ends here" :x1 0.40)))
            :to-equal "the range spans pages 3-10 and no further, it says body line three ends here"))

  (it "renders a paragraph ending mid-word at the page break with its hyphen"
    (expect (pdf-text-tests--render
             '(("body line one filling the column right here")
               ("and the page ends on a broken prob\u00AD")))
            :to-equal "body line one filling the column right here and the page ends on a broken prob-"))

  (it "reads through a discretionary hyphen the page never printed"
    ;; the report: a heading wrapping over "are / applied" reached the
    ;; reader as "are ­applied" - the text layer opens the continuation
    ;; line with the soft hyphen
    (expect (pdf-text-render-pages '("when they are\n\u00ADapplied to humans"))
            :to-equal '("when they are applied to humans"))))

(describe "pdf-text-extra-vocabulary"
  (it "keeps a compound the pages at hand never spell out"
    ;; a window of a book is not the book: the compound may be
    ;; hyphenated three chapters away from the page that wraps it
    (let ((pdf-text-extra-vocabulary '("well-known")))
      (expect (pdf-text-render-pages '("many well-\nknown algorithms"))
              :to-equal '("many well-known algorithms"))))

  (it "leaves an ordinary split word alone"
    (let ((pdf-text-extra-vocabulary '("well-known")))
      (expect (pdf-text-render-pages '("the informa-\ntion here"))
              :to-equal '("the information here")))))

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

  (it "survives a record with no ink geometry among the page's lines"
    ;; the Manual de la Nueva Gramática lays out tab-only records whose
    ;; every geometry slot is nil; the narrow test read x1 - x0 before
    ;; any guard and crashed the whole book's render
    (let* ((pages (list (list (pdf-text-tests--line "body line one filling the column"
                                                    :base 0.20)
                              (pdf-text-line-create :text "\t\t")
                              (pdf-text-tests--line "body line two filling the column"
                                                    :base 0.22))))
           (profile (pdf-text--profile pages))
           (profiles (mapcar (lambda (lines) (pdf-text--page-profile lines profile))
                             pages)))
      (expect (mapcar #'pdf-text-line-text
                      (car (pdf-text-remove-marginal-lines pages profiles)))
              :to-equal '("body line one filling the column"
                          "\t\t"
                          "body line two filling the column"))))

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

  (it "does not read a recurring lone brace as a running head"
    ;; a listings book ends pages on a closing brace often enough for
    ;; the form to recur as a detached narrow candidate; a running head
    ;; carries words and a folio digits, so a punctuation-only line
    ;; never joins the recurring set - the drop-anywhere rule was
    ;; eating every listing's closing braces across Programming Rust
    (let* ((pages (mapcar
                   (lambda (n)
                     (pdf-text-tests--page
                      `(("body line one filling the column" :base 0.20)
                        ("body line two filling the column")
                        (,(format "let x%d = value;" n) :x0 0.20 :x1 0.60)
                        ("}" :x0 0.20 :x1 0.22 :base 0.93))))
                   '(1 2 3)))
           (profile (pdf-text--profile pages))
           (profiles (mapcar (lambda (lines) (pdf-text--page-profile lines profile))
                             pages)))
      (expect (mapcar (lambda (lines) (mapcar #'pdf-text-line-text lines))
                      (pdf-text-remove-marginal-lines pages profiles))
              :to-equal (mapcar
                         (lambda (n)
                           (list "body line one filling the column"
                                 "body line two filling the column"
                                 (format "let x%d = value;" n)
                                 "}"))
                         '(1 2 3)))))

  (it "strips a paper's head and folio set outside the body span"
    ;; applicative: head and folio at 1.5 leadings over a body that
    ;; starts far down a large page - past the band, under the
    ;; detachment, and both outside the profile's body span
    (let* ((pages (mapcar
                   (lambda (n)
                     (pdf-text-tests--page
                      `(("Conor McBride and Ross Paterson"
                         :x0 0.37 :x1 0.63 :base 0.14)
                        (,(number-to-string n) :x0 0.79 :x1 0.80 :base 0.14)
                        ("body line one filling the column" :base 0.17)
                        ("body line two filling the column")
                        ("body line three filling the column")
                        ("body line four filling the column"))))
                   '(2 4 6)))
           (profile (pdf-text--profile pages))
           (profiles (mapcar (lambda (lines)
                               (pdf-text--page-profile lines profile))
                             pages)))
      (expect (mapcar (lambda (lines) (mapcar #'pdf-text-line-text lines))
                      (pdf-text-remove-marginal-lines pages profiles))
              :to-equal (make-list 3 '("body line one filling the column"
                                       "body line two filling the column"
                                       "body line three filling the column"
                                       "body line four filling the column")))))

  (it "strips a wide head sharing its baseline with the folio, past the span"
    ;; denotational: the head runs as wide as a column - never narrow -
    ;; and hangs with its corner folio tight over the text block
    (let* ((pages (mapcar
                   (lambda (n)
                     (pdf-text-tests--page
                      `(("Denotational design with type class morphisms"
                         :x0 0.52 :x1 0.91 :base 0.06)
                        (,(number-to-string n) :x0 0.088 :x1 0.095 :base 0.06)
                        ("body line one fills its column" :x0 0.09 :x1 0.48
                         :base 0.10)
                        ("body line two fills its column" :x0 0.09 :x1 0.48)
                        ("body line three fills its column" :x0 0.09 :x1 0.48)
                        ("body line four fills its column" :x0 0.09 :x1 0.48))))
                   '(1 2 3)))
           (profile (pdf-text--profile pages))
           (profiles (mapcar (lambda (lines)
                               (pdf-text--page-profile lines profile))
                             pages)))
      (expect (mapcar (lambda (lines) (mapcar #'pdf-text-line-text lines))
                      (pdf-text-remove-marginal-lines pages profiles))
              :to-equal (make-list 3 '("body line one fills its column"
                                       "body line two fills its column"
                                       "body line three fills its column"
                                       "body line four fills its column")))))

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
              :to-equal 6)))

  (it "strips a running head standing under two leadings off a dense page"
    ;; the Spanish grammar sets "4 Gender of nouns" 1.9 leadings above
    ;; the body; the old two-leading detachment never counted it
    (let* ((pages (mapcar
                   (lambda (n)
                     (pdf-text-tests--page
                      `((,(format "%d Gender of nouns" n) :x0 0.10 :x1 0.25
                         :base 0.062)
                        ("body line one filling the column" :base 0.10)
                        ("body line two filling the column")
                        ("body line three ends here" :x1 0.40))))
                   '(4 6 8)))
           (profile (pdf-text--profile pages))
           (profiles (mapcar (lambda (lines) (pdf-text--page-profile lines profile))
                             pages)))
      (expect (mapcar (lambda (lines) (mapcar #'pdf-text-line-text lines))
                      (pdf-text-remove-marginal-lines pages profiles))
              :to-equal (make-list 3 '("body line one filling the column"
                                       "body line two filling the column"
                                       "body line three ends here")))))

  (it "strips a full-measure head with its folio merged in, from the band only"
    ;; the even pages run the section title wide with the page number
    ;; joined on; a table of contents carries the same words tightly
    ;; mid-page and must keep them
    (let* ((head "1.3 Group B: Gender of nouns referring to animals and things")
           (pages (append
                   (mapcar
                    (lambda (n)
                      (pdf-text-tests--page
                       `((,(format "%s %d" head n) :x0 0.10 :x1 0.89 :base 0.062)
                         ("body line one filling the column" :base 0.10)
                         ("body line two filling the column")
                         ("body line three ends here" :x1 0.40))))
                    '(9 11 13))
                   (list (pdf-text-tests--page
                          `(("Contents" :x0 0.10 :x1 0.25 :base 0.20)
                            (,(format "%s %d" head 7) :x0 0.10 :x1 0.89 :gap 3)
                            ("1.4 The next section title here 12" :x0 0.10 :x1 0.60))))))
           (profile (pdf-text--profile pages))
           (profiles (mapcar (lambda (lines) (pdf-text--page-profile lines profile))
                             pages))
           (stripped (mapcar (lambda (lines) (mapcar #'pdf-text-line-text lines))
                             (pdf-text-remove-marginal-lines pages profiles))))
      (expect (seq-take stripped 3)
              :to-equal (make-list 3 '("body line one filling the column"
                                       "body line two filling the column"
                                       "body line three ends here")))
      (expect (nth 3 stripped)
              :to-equal (list "Contents"
                              (format "%s %d" head 7)
                              "1.4 The next section title here 12"))))

  (it "spares the chapter title the band holds in display type"
    ;; the opener's title shares its digit-normalised form with every
    ;; head that carries it; display type is what tells them apart
    (let* ((title-page (pdf-text-tests--page
                        '(("1 Gender of nouns" :x0 0.10 :x1 0.45 :base 0.10
                           :height 0.045)
                          ("body line one filling the column" :base 0.20)
                          ("body line two filling the column")
                          ("body line three ends here" :x1 0.40))))
           (pages (cons title-page
                        (mapcar
                         (lambda (n)
                           (pdf-text-tests--page
                            `((,(format "%d Gender of nouns" n) :x0 0.10 :x1 0.25
                               :base 0.062)
                              ("body line one filling the column" :base 0.10)
                              ("body line two filling the column")
                              ("body line three ends here" :x1 0.40))))
                         '(4 6 8))))
           (profile (pdf-text--profile pages))
           (profiles (mapcar (lambda (lines) (pdf-text--page-profile lines profile))
                             pages))
           (stripped (mapcar (lambda (lines) (mapcar #'pdf-text-line-text lines))
                             (pdf-text-remove-marginal-lines
                              pages profiles
                              '(("* 1 Gender of nouns") nil nil nil)))))
      (expect (car stripped)
              :to-equal '("1 Gender of nouns"
                          "body line one filling the column"
                          "body line two filling the column"
                          "body line three ends here"))
      (expect (cdr stripped)
              :to-equal (make-list 3 '("body line one filling the column"
                                       "body line two filling the column"
                                       "body line three ends here")))))

  (it "spares a short footnote however its form recurs or its folio sits"
    ;; three pages of near-identical one-line footnotes: digit
    ;; normalisation makes them one recurring form, and each shares its
    ;; baseline with the folio beside it - both removal paths must yield
    ;; to a bottom-band line set small and opening on a marker
    (let* ((pages (mapcar
                   (lambda (n)
                     (pdf-text-tests--page
                      `(("body line one filling the column" :base 0.20)
                        ("body line two filling the column")
                        ("body line three ends here" :x1 0.40)
                        (,(format "%d. Ibid., page %d." n n) :base 0.90
                         :height 0.010 :x0 0.10 :x1 0.30)
                        (,(number-to-string (* 11 n)) :base 0.90
                         :x0 0.85 :x1 0.87 :height 0.010))))
                   '(1 2 3)))
           (profile (pdf-text--profile pages))
           (profiles (mapcar (lambda (lines) (pdf-text--page-profile lines profile))
                             pages)))
      (expect (mapcar (lambda (lines) (mapcar #'pdf-text-line-text lines))
                      (pdf-text-remove-marginal-lines pages profiles))
              :to-equal
              (cl-loop for n from 1 to 3
                       collect (list "body line one filling the column"
                                     "body line two filling the column"
                                     "body line three ends here"
                                     (format "%d. Ibid., page %d." n n))))))

  (it "spares the heading line a book also runs in its page head"
    ;; the head repeats the section's title on every page of it, which
    ;; makes the title a recurring form; dropping it wherever it appears
    ;; takes the section's own heading with it
    (let* ((pages (cl-loop
                   for n from 1 to 3
                   collect (pdf-text-tests--page
                            `(("Supervised Segmentation" :x0 0.10 :x1 0.32
                               :base 0.06)
                              (,(number-to-string n) :x0 0.85 :x1 0.87 :base 0.06)
                              ,@(when (eql n 1)
                                  '(("Supervised Segmentation" :base 0.20
                                     :x1 0.32)))
                              ("body line one filling the column" :base 0.30)
                              ("body line two filling the column")
                              ("body line three ends here" :x1 0.40)))))
           (profile (pdf-text--profile pages))
           (profiles (mapcar (lambda (lines) (pdf-text--page-profile lines profile))
                             pages))
           (headings '(("** Supervised Segmentation") nil nil)))
      (expect (mapcar (lambda (lines) (mapcar #'pdf-text-line-text lines))
                      (pdf-text-remove-marginal-lines pages profiles headings))
              :to-equal
              (cons '("Supervised Segmentation"
                      "body line one filling the column"
                      "body line two filling the column"
                      "body line three ends here")
                    (make-list 2 '("body line one filling the column"
                                   "body line two filling the column"
                                   "body line three ends here"))))
      ;; the title is a recurring form either way: at the body size,
      ;; what spares the line is the outline naming it
      (expect (length (car (pdf-text-remove-marginal-lines pages profiles)))
              :to-equal 3)))

  (it "spares a paper's own title from the head that echoes it"
    ;; a paper runs its title as the head of every page after the
    ;; first, and no outline entry names a paper's title: what says
    ;; the page-1 line is the paper's own is the display type it is
    ;; set in, which no running head uses
    (let* ((pages (cl-loop
                   for n from 1 to 4
                   collect (pdf-text-tests--page
                            (if (eql n 1)
                                '(("Alignment of Paragraphs in Bilingual Texts"
                                   :x0 0.20 :x1 0.80 :base 0.11 :height 0.0192)
                                  ("body line one filling the column" :base 0.30)
                                  ("body line two filling the column")
                                  ("body line three ends here" :x1 0.40))
                              `(("Alignment of Paragraphs in Bilingual Texts"
                                 :x0 0.45 :x1 0.82 :base 0.06 :height 0.0125)
                                (,(number-to-string n) :x0 0.10 :x1 0.12 :base 0.06)
                                ("body line one filling the column" :base 0.30)
                                ("body line two filling the column")
                                ("body line three ends here" :x1 0.40))))))
           (profile (pdf-text--profile pages))
           (profiles (mapcar (lambda (lines) (pdf-text--page-profile lines profile))
                             pages)))
      (expect (mapcar (lambda (lines) (mapcar #'pdf-text-line-text lines))
                      (pdf-text-remove-marginal-lines pages profiles))
              :to-equal
              (cons '("Alignment of Paragraphs in Bilingual Texts"
                      "body line one filling the column"
                      "body line two filling the column"
                      "body line three ends here")
                    (make-list 3 '("body line one filling the column"
                                   "body line two filling the column"
                                   "body line three ends here"))))))

  (it "drops a wordless folio however large the page sets it"
    ;; a workbook sets its unit digit and its folio in display type;
    ;; sparing display type spares titles, and a title has words
    (let* ((pages (cl-loop
                   for n from 1 to 3
                   collect (pdf-text-tests--page
                            `((,(format "–%d–" n) :x0 0.10 :x1 0.16 :base 0.94
                               :height 0.024)
                              ("body line one filling the column" :base 0.30)
                              ("body line two filling the column")
                              ("body line three ends here" :x1 0.40)))))
           (profile (pdf-text--profile pages))
           (profiles (mapcar (lambda (lines) (pdf-text--page-profile lines profile))
                             pages)))
      (expect (mapcar (lambda (lines) (mapcar #'pdf-text-line-text lines))
                      (pdf-text-remove-marginal-lines pages profiles))
              :to-equal
              (make-list 3 '("body line one filling the column"
                             "body line two filling the column"
                             "body line three ends here"))))))

(describe "pdf-text-remove-marginal-lines document seeds"
  (it "drops a head the document shows recurring that the window cannot"
    ;; one page is not the book: the same head recurs book-wide, and
    ;; the seeded form is what carries that fact into a window render
    (let* ((pages (list (pdf-text-tests--page
                         '(("INTRO | 9" :x0 0.10 :x1 0.30 :base 0.06)
                           ("body line one filling the column" :base 0.20)
                           ("body line two filling the column")
                           ("body line three filling the column")
                           ("body line four ends here" :x1 0.40)))))
           (profile (pdf-text--profile pages))
           (profiles (mapcar (lambda (lines) (pdf-text--page-profile lines profile))
                             pages))
           (texts (lambda () (mapcar #'pdf-text-line-text
                                     (car (pdf-text-remove-marginal-lines
                                           pages profiles))))))
      (expect (funcall texts)
              :to-contain "INTRO | 9")
      (let ((pdf-text-extra-recurring-forms
             (list (pdf-text--normalize-line "INTRO | 9"))))
        (expect (funcall texts)
                :not :to-contain "INTRO | 9"))))

  (it "drops a folio-merged head once the document establishes the style"
    ;; one candidate in the window, three in the book: the seeded count
    ;; is what lets the band drop fire the way it does book-wide
    (let* ((pages (list (pdf-text-tests--page
                         '(("1.3 Group B nouns referring to lifeless things 9"
                            :x0 0.10 :x1 0.80 :base 0.06)
                           ("body line one filling the column" :base 0.20)
                           ("body line two filling the column")
                           ("body line three filling the column")
                           ("body line four ends here" :x1 0.40)))))
           (profile (pdf-text--profile pages))
           (profiles (mapcar (lambda (lines) (pdf-text--page-profile lines profile))
                             pages))
           (texts (lambda () (mapcar #'pdf-text-line-text
                                     (car (pdf-text-remove-marginal-lines
                                           pages profiles))))))
      (expect (funcall texts)
              :to-contain "1.3 Group B nouns referring to lifeless things 9")
      (let ((pdf-text-extra-folio-merged 3))
        (expect (funcall texts)
                :not :to-contain
                "1.3 Group B nouns referring to lifeless things 9")))))

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
  (it "collapses the same title painted twice on one visual line"
    ;; the shadow paint lands a point lower, a fraction of the leading
    (expect (mapcar #'pdf-text-line-text
                    (pdf-text--dedup-adjacent
                     (pdf-text-tests--page
                      '(("PATTERNS OF CONFLICT")
                        ("PATTERNS OF CONFLICT" :gap 0.1)
                        ("body")))))
            :to-equal '("PATTERNS OF CONFLICT" "body")))

  (it "collapses the echo of a page with no geometry"
    (expect (mapcar #'pdf-text-line-text
                    (pdf-text--dedup-adjacent
                     (mapcar (lambda (text) (pdf-text-line-create :text text))
                             '("PATTERNS OF CONFLICT" "PATTERNS OF CONFLICT"
                               "body"))))
            :to-equal '("PATTERNS OF CONFLICT" "body")))

  (it "keeps an operator column that repeats its glyph on every row"
    (expect (mapcar #'pdf-text-line-text
                    (pdf-text--dedup-adjacent
                     (pdf-text-tests--page '(("→") ("→") ("→")))))
            :to-equal '("→" "→" "→")))

  (it "keeps identical lines separated by a blank"
    (expect (mapcar #'pdf-text-line-text
                    (pdf-text--dedup-adjacent
                     (pdf-text-tests--page '(("refrain") ("") ("refrain")))))
            :to-equal '("refrain" "" "refrain")))

  (it "keeps a doubled glyph served beside its twin"
    ;; CMU-CS-95-113: "degree" arrives as "de" "gr" "e" "e", the two
    ;; e-records side by side on one baseline - twins, not an echo,
    ;; whose ink an echo's span test tells apart
    (expect (mapcar #'pdf-text-line-text
                    (pdf-text--dedup-adjacent
                     (list (pdf-text-tests--line "e" :x0 0.4388 :x1 0.4471
                                                 :base 0.5870 :height 0.0121)
                           (pdf-text-tests--line "e" :x0 0.4463 :x1 0.4545
                                                 :base 0.5870
                                                 :height 0.0121))))
            :to-equal '("e" "e"))))

(describe "pdf-text--drop-split-echoes"
  (it "drops a following run that re-spells the previous line"
    (expect (mapcar #'pdf-text-line-text
                    (pdf-text--drop-split-echoes
                     (pdf-text-tests--page
                      '(("PATTERNS OF CONFLICT") ("PATTERNS OF" :gap 0.1)
                        ("CONFLICT" :gap 0)
                        ("body")))))
            :to-equal '("PATTERNS OF CONFLICT" "body")))

  (it "keeps a repeated value set a whole line step down"
    (expect (mapcar #'pdf-text-line-text
                    (pdf-text--drop-split-echoes
                     (pdf-text-tests--page '(("e") ("e") ("body")))))
            :to-equal '("e" "e" "body")))

  (it "keeps partial overlaps that never equal the line"
    (expect (mapcar #'pdf-text-line-text
                    (pdf-text--drop-split-echoes
                     (pdf-text-tests--page
                      '(("PATTERNS OF CONFLICT") ("PATTERNS OF" :gap 0.1)
                        ("WAR" :gap 0)))))
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
  (it "reflows a page of records carrying geometry"
    (expect (pdf-text-render-lines
             (list (pdf-text-tests--page
                    '(("First paragraph line one")
                      ("ends here." :x1 0.30)
                      ("Second paragraph opens" :x0 0.13 :x1 0.88)
                      ("and ends." :x1 0.29)))))
            :to-equal
            '("First paragraph line one ends here.\n\nSecond paragraph opens and ends.")))

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

(describe "pdf-text-render-lines"
  (it "renders the same text the raw-page entry point does"
    ;; the corpus harness stores line records and enters here, so the two
    ;; entry points have to be the same pipeline rather than two copies
    (let ((raw '("ABSTRACT | 14\nbody one runs to a full width line\ncontinues"
                 "ABSTRACT | 15\nbody two"
                 "ABSTRACT | 16\nbody three"
                 "ABSTRACT | 17\nbody four")))
      (expect (pdf-text-render-lines
               (mapcar (lambda (text) (pdf-text--page-lines text)) raw))
              :to-equal (pdf-text-render-pages raw))))

  (it "reflows records carrying geometry"
    (expect (pdf-text-render-lines
             (list (pdf-text-tests--page
                    '(("First paragraph line one" :x1 0.90)
                      ("ends here." :x1 0.30)
                      ("Second paragraph opens" :x0 0.13 :x1 0.90)
                      ("and ends." :x1 0.28)))))
            :to-equal
            '("First paragraph line one ends here.\n\nSecond paragraph opens and ends."))))

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
      (expect (search-forward "* item one" nil t) :to-be-truthy)))

  (it "leaves generated script markup untouched"
    (expect (pdf-text--escape-org-lines "back to 10^{-43}seconds\nand F_{jk} holds")
            :to-equal "back to 10^{-43}seconds\nand F_{jk} holds")))

(describe "pdf-text script rendering"
  (it "breaks literal pairs and only those"
    (expect (pdf-text--escape-literals "a^{b} c_{d} keeps x_1 and y^2")
            :to-equal "a^\u200B{b} c_\u200B{d} keeps x_1 and y^2"))

  (it "displays generated scripts and nothing literal"
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (pdf-text-mode)
        (insert "back to 10^{-43} seconds\n"
                "literal x_1 stays\n"
                (pdf-text--escape-literals "literal x_{i} stays\n")))
      (font-lock-ensure)
      (expect org-use-sub-superscripts :to-equal '{})
      ;; buffer-local: a user config disabling script display for its
      ;; org files must not reach into a rendered book
      (expect (local-variable-p 'org-pretty-entities-include-sub-superscripts)
              :to-be-truthy)
      (expect org-pretty-entities-include-sub-superscripts :to-be t)
      (goto-char (point-min))
      (search-forward "-43")
      (expect (get-text-property (match-beginning 0) 'display) :to-be-truthy)
      (goto-char (point-min))
      (search-forward "x_1")
      (expect (get-text-property (1- (point)) 'display) :to-be nil)
      (goto-char (point-min))
      (search-forward "{i}")
      (expect (get-text-property (match-beginning 0) 'display) :to-be nil))))

(describe "pdf-text--footnote-open"
  (it "reads a flat symbol marker leading into words"
    (expect (car (pdf-text--footnote-open "*A word on scientific notation"))
            :to-equal "*"))

  (it "reads a flat number closed by its period or parenthesis"
    (expect (car (pdf-text--footnote-open "1. Of course, each author"))
            :to-equal "1")
    (expect (car (pdf-text--footnote-open "12) De Moivre gave the curve"))
            :to-equal "12"))

  (it "reads the superscripted form the body writes"
    (expect (pdf-text--footnote-open "^{*}A word on notation")
            :to-equal '("*" . 4)))

  (it "tells the note's own words from the marker"
    (expect (cdr (pdf-text--footnote-open "1. Of course")) :to-equal 3)
    (expect (cdr (pdf-text--footnote-open "*A word")) :to-equal 1))

  (it "does not read a bare number: a TOC entry or a merged folio line"
    (expect (pdf-text--footnote-open "3 Learning Is Creating") :to-be nil)
    (expect (pdf-text--footnote-open "18 THE BIG BANG") :to-be nil))

  (it "does not read a rule of asterisks or a marker standing alone"
    (expect (pdf-text--footnote-open "*** DRAFT ***") :to-be nil)
    (expect (pdf-text--footnote-open "*") :to-be nil)))

(describe "pdf-text--footnote-marker-re"
  (it "matches the marker's superscript, holding what precedes it"
    (expect "fall of 2005.^{1} The original"
            :to-match (pdf-text--footnote-marker-re "1")))

  (it "does not read an exponent as a numeric marker"
    (expect (string-match-p (pdf-text--footnote-marker-re "3")
                            "powers run up to 10^{3} here")
            :to-be nil))

  (it "matches a symbol marker off any base"
    (expect "of a second.^{*}"
            :to-match (pdf-text--footnote-marker-re "*"))))

(describe "pdf-text--footnote-flat-re"
  (it "matches the word-attached symbol closing a title"
    (expect "Using Bilingual Dictionaries and Dynamic Programming*"
            :to-match (pdf-text--footnote-flat-re "*")))

  (it "does not half-match a generated superscript"
    (expect (string-match-p (pdf-text--footnote-flat-re "*")
                            "a sentence closing on a marker.^{*}")
            :to-be nil))

  (it "does not read a detached star as a citation"
    (expect (string-match-p (pdf-text--footnote-flat-re "*")
                            "a line closing on a dinkus *")
            :to-be nil))

  (it "gives a numeric token no flat form"
    (expect (pdf-text--footnote-flat-re "1") :to-be nil)))

(describe "pdf-text footnotes"
  (it "renders a symbol marker and its block as an org footnote"
    (expect (pdf-text-tests--render
             '(("body line one, closing a sentence on a marker.^{*} then"
                :height 0.015)
               ("body line two filling the column continues the paragraph")
               ("body line three ends here" :x1 0.40)
               ("*A word on notation: the note itself, set smaller" :gap 3
                :height 0.010)
               ("and running one more line" :height 0.010 :x1 0.40)))
            :to-equal
            (concat "body line one, closing a sentence on a marker.[fn:1-star]"
                    " then body line two filling the column continues the"
                    " paragraph body line three ends here"
                    "\n\n"
                    "[fn:1-star] A word on notation: the note itself,"
                    " set smaller and running one more line")))

  (it "renders a numbered marker, the label keeping the page's numeral"
    (expect (pdf-text-tests--render
             '(("the class began in the fall of 2005.^{1} It went on and on"
                :height 0.015)
               ("body line two filling the column right along here")
               ("body line three ends here" :x1 0.40)
               ("1. Of course, each author did most of the work." :gap 3
                :height 0.010 :x1 0.60)))
            :to-equal
            (concat "the class began in the fall of 2005.[fn:1-1] It went on"
                    " and on body line two filling the column right along here"
                    " body line three ends here"
                    "\n\n"
                    "[fn:1-1] Of course, each author did most of the work.")))

  (it "does not pair an exponent with a look-alike block"
    (let ((render (pdf-text-tests--render
                   '(("the powers of ten run up to 10^{3} and beyond it"
                      :height 0.015)
                     ("body line two filling the column right along here")
                     ("body line three ends here" :x1 0.40)
                     ("3. A note that nothing in the body cites." :gap 3
                      :height 0.010 :x1 0.60)))))
      (expect render :to-match "10\\^{3}")
      (expect render :not :to-match (rx "[fn:"))))

  (it "leaves a marker without an answering block as the superscript it was"
    (expect (pdf-text-tests--render
             '(("a sentence closing on a dangling marker.^{*} and more text"
                :height 0.015)
               ("body line two filling the column right along here")
               ("body line three ends here" :x1 0.40)))
            :to-match "marker\\.\\^{\\*}"))

  (it "leaves a small trailing block the body never cites as text"
    (let ((render (pdf-text-tests--render
                   '(("body line one filling the column with plain prose"
                      :height 0.015)
                     ("body line two filling the column right along here")
                     ("body line three ends here" :x1 0.40)
                     ("*A note nothing cites, set small at the foot" :gap 3
                      :height 0.010 :x1 0.60)))))
      (expect render :not :to-match (rx "[fn:"))
      (expect render :to-match "\\*A note nothing cites")))

  (it "gives the size gate slack to a marker-opening block at the foot"
    ;; the alignment paper's notes reach 0.906 of the body height -
    ;; over the gate, under the slack - and the marker plus the body's
    ;; citation carry the decision
    (expect (pdf-text-tests--render
             '(("body prose citing its own footnote here.^{*} and onward"
                :height 0.015)
               ("body line two filling the column right along here")
               ("body line three ends here" :x1 0.40)
               ("*A note set a hair over the gate at the page's foot" :gap 3
                :height 0.0136 :x1 0.60)))
            :to-match (rx bol "[fn:1-star] A note set a hair over")))

  (it "gives no slack to a block without a marker"
    (expect (pdf-text-tests--render
             '(("body prose above the foot going about its business"
                :height 0.015)
               ("body line two filling the column right along here")
               ("body line three ends here" :x1 0.40)
               ("A closing paragraph set a hair small, no marker" :gap 3
                :height 0.0136 :x1 0.60)))
            :not :to-match (rx "[fn:")))

  (it "reads a flat trailing symbol as the citation the text layer wrote"
    ;; the alignment paper's title carries its star in-record with no
    ;; size contrast for the glyph pass to dissect: "Dynamic Programming*"
    (let ((render (pdf-text-tests--render
                   '(("Using Bilingual Dictionaries and Dynamic Programming*"
                      :height 0.019 :x1 0.70)
                     ("body prose under the title fills the column" :gap 2
                      :height 0.015)
                     ("body line two filling the column right along here")
                     ("body line three ends here" :x1 0.40)
                     ("*Work done under partial support of the government"
                      :gap 3 :height 0.011 :x1 0.80)))))
      (expect render :to-match (rx "Dynamic Programming[fn:1-star]"))
      (expect render :to-match (rx bol "[fn:1-star] Work done under"))))

  (it "marks an unmarked page-foot block as a note without a label"
    ;; bornstein's author note: no marker anywhere in the body, set
    ;; small at the foot - the face without an invented [fn:]
    (let ((render (pdf-text-tests--render
                   '(("body prose above the note going about its business"
                      :height 0.015 :base 0.70)
                     ("body line two filling the column right along here")
                     ("body line three ends here" :x1 0.40)
                     ("I would like to thank the reviewers for their help"
                      :gap 3 :height 0.011 :x1 0.80)))))
      (expect render :not :to-match (rx "[fn:"))
      (expect render :to-match "I would like to thank")
      (expect (get-text-property (string-match "I would like" render)
                                 'pdf-text-note render)
              :to-be t)
      (expect (get-text-property 0 'pdf-text-note render) :to-be nil)))

  (it "carries the note property on a definition line, body clean"
    (let ((render (pdf-text-tests--render
                   '(("body line one, closing a sentence on a marker.^{*} then"
                      :height 0.015)
                     ("body line two filling the column continues the paragraph")
                     ("body line three ends here" :x1 0.40)
                     ("*A word on notation: the note itself, set smaller" :gap 3
                      :height 0.010)
                     ("and running one more line" :height 0.010 :x1 0.40)))))
      (expect (get-text-property (string-match (rx bol "[fn:1-star]") render)
                                 'pdf-text-note render)
              :to-be t)
      (expect (get-text-property 0 'pdf-text-note render) :to-be nil)))

  (it "leaves a small block high on the page without the note face"
    (let ((render (pdf-text-tests--render
                   '(("body prose above the block going about its business"
                      :height 0.015)
                     ("body line two filling the column right along here")
                     ("body line three ends here" :x1 0.40)
                     ("a small caption set high, not a note at the foot"
                      :gap 3 :height 0.011 :x1 0.80)))))
      (expect (get-text-property (string-match "a small caption" render)
                                 'pdf-text-note render)
              :to-be nil)))

  (it "wants body prose above before dimming an unmarked block"
    ;; a listing page with a small line at its foot has no prose to be
    ;; a note to
    (let ((render (pdf-text-tests--render
                   '(("(define (f x) (* x x))" :cv 0.02 :x1 0.40)
                     ("(define (g x) (+ x 1))" :cv 0.02 :x1 0.40)
                     ("a small line at the foot of a listing page"
                      :base 0.80 :height 0.011 :x1 0.60)))))
      (expect (get-text-property (string-match "a small line" render)
                                 'pdf-text-note render)
              :to-be nil)))

  (it "does not read a body-size block as a footnote"
    (let ((render (pdf-text-tests--render
                   '(("a sentence closing on a marker of its own.^{*} yes it"
                      :height 0.015)
                     ("body line two filling the column right along here")
                     ("body line three ends here" :x1 0.40)
                     ("* A dinkus or a bullet at body size stays put" :gap 3)))))
      (expect render :not :to-match (rx "[fn:"))))

  (it "breaks a smaller-type block off a body line ending on a wrap hyphen"
    ;; the size break must outrank the hyphen join, or the block is
    ;; swallowed before the footnote pass can see it (DSB page 19)
    (expect (pdf-text-tests--render
             '(("body text citing a source in passing.^{1} More prose here"
                :height 0.015)
               ("body line two of full measure that ends on a hyphen fol-")
               ("1. The note the hyphenated line above cites." :gap 3
                :height 0.010 :x1 0.60)))
            :to-equal
            (concat "body text citing a source in passing.[fn:1-1] More prose"
                    " here body line two of full measure that ends on a hyphen"
                    " fol-"
                    "\n\n"
                    "[fn:1-1] The note the hyphenated line above cites.")))

  (it "converts several footnotes, each to its own label"
    (let ((render (pdf-text-tests--render
                   '(("first marker in the body of this very page.^{*} and a"
                      :height 0.015)
                     ("second right after it before the line closes.^{†} More")
                     ("body line three ends here" :x1 0.40)
                     ("*The first note, set small at the foot of the page"
                      :gap 3 :height 0.010 :x1 0.60)
                     ("†The second note, set just as small below it"
                      :gap 2 :height 0.010 :x1 0.60)))))
      (expect render :to-match (rx "page.[fn:1-star]"))
      (expect render :to-match (rx "closes.[fn:1-dagger]"))
      (expect render :to-match (rx bol "[fn:1-star] The first note"))
      (expect render :to-match (rx bol "[fn:1-dagger] The second note"))))

  (it "numbers labels by the page the book gives the render"
    (let* ((pages (list (pdf-text-tests--page
                         '(("a body line citing its own note here.^{*} yes so"
                            :height 0.015)
                           ("body line two filling the column right along")
                           ("body line three ends here" :x1 0.40)
                           ("*The note at the foot" :gap 3 :height 0.010
                            :x1 0.60)))))
           (render (car (pdf-text-render-lines pages nil 18))))
      (expect render :to-match (rx "here.[fn:18-star]"))
      (expect render :to-match (rx bol "[fn:18-star] The note at the foot")))))

(describe "pdf-text footnote escape and display"
  (it "breaks a literal [fn: so only generated footnotes parse"
    (expect (pdf-text--escape-literals "see [fn:note] in the org source")
            :to-equal "see [\u200Bfn:note] in the org source"))

  (it "parses the generated definition and reference, and no literal"
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (pdf-text-mode)
        (insert "prose citing a note.[fn:18-star] more prose\n\n"
                "[fn:18-star] The note itself.\n\n"
                (pdf-text--escape-literals "[fn:plain] a literal off the page\n")))
      (let ((tree (org-element-parse-buffer)))
        (expect (org-element-map tree 'footnote-reference
                  (lambda (ref) (org-element-property :label ref)))
                :to-equal '("18-star"))
        (expect (org-element-map tree 'footnote-definition
                  (lambda (def) (org-element-property :label def)))
                :to-equal '("18-star")))))

  (it "jumps from the reference to its definition"
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (pdf-text-mode)
        (insert "prose citing a note.[fn:18-star] more prose\n\n"
                "[fn:18-star] The note itself.\n"))
      (goto-char (point-min))
      (search-forward "note.[fn:18-star")
      (org-open-at-point)
      (expect (buffer-substring-no-properties (line-beginning-position)
                                              (line-end-position))
              :to-match "The note itself")))

  (it "dims a note span through font-lock, refontification included"
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (pdf-text-mode)
        (insert "plain body prose here\n\n"
                (propertize "[fn:18-star] The note itself." 'pdf-text-note t)
                "\n"))
      (font-lock-ensure)
      (let ((note (text-property-any (point-min) (point-max)
                                     'pdf-text-note t)))
        (expect (memq 'pdf-text-footnote-face
                      (ensure-list (get-text-property note 'face)))
                :to-be-truthy)
        (expect (get-text-property (point-min) 'face) :to-be nil))
      ;; a refontification strips faces; the rule reapplies ours off
      ;; the property, which survives
      (font-lock-flush)
      (font-lock-ensure)
      (let ((note (text-property-any (point-min) (point-max)
                                     'pdf-text-note t)))
        (expect (memq 'pdf-text-footnote-face
                      (ensure-list (get-text-property note 'face)))
                :to-be-truthy)))))

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
            :to-equal '("a" "b")))

  (it "leaves a heading the page already carries where the render put it"
    ;; the reflow places a heading at the line naming it; prepending it
    ;; again is the Round 4 defect - the section then owns nothing and
    ;; the line it names reads as prose below it
    (expect (pdf-text--interleave-outline
             '("body before\n\n** Sec\n\nbody after")
             '(((depth . 2) (title . "Sec") (page . 1))))
            :to-equal '("body before\n\n** Sec\n\nbody after")))

  (it "reads a placed heading behind the section number the page sets"
    ;; the render keeps the page's own number on the heading it places;
    ;; the outline entry carries the title bare, and prepending it
    ;; again would give the page the same section twice
    (expect (pdf-text--interleave-outline
             '("* 1 Introduction\n\nbody")
             '(((depth . 1) (title . "Introduction") (page . 1))))
            :to-equal '("* 1 Introduction\n\nbody")))

  (it "keeps a leftover below the section the page did place"
    ;; the outline opens Deep after Sec: stacked above the page's own
    ;; Sec heading it owns Sec's text, heading and all, and the reader
    ;; folds the page's sections in the wrong order
    (expect (pdf-text--interleave-outline
             '("body before\n\n* Sec\n\nbody after" "next page")
             '(((depth . 1) (title . "Sec") (page . 1))
               ((depth . 2) (title . "Deep") (page . 1))
               ((depth . 1) (title . "Later") (page . 2))))
            :to-equal '("body before\n\n* Sec\n\nbody after\n** Deep"
                        "* Later\nnext page")))

  (it "stops a leftover just above the next heading the page carries"
    ;; the range the outline leaves it: after the section placed above
    ;; it, before the one placed below - and inside that range as late
    ;; as it can go, so the placed sections keep their own text
    (expect (pdf-text--interleave-outline
             '("* One\nfirst body\n* Three\nthird body")
             '(((depth . 1) (title . "One") (page . 1))
               ((depth . 1) (title . "Two") (page . 1))
               ((depth . 1) (title . "Three") (page . 1))))
            :to-equal '("* One\nfirst body\n* Two\n* Three\nthird body")))

  (it "keeps leftovers in outline order around the headings placed"
    (expect (pdf-text--interleave-outline
             '("* Two\nbody")
             '(((depth . 1) (title . "One") (page . 1))
               ((depth . 1) (title . "Two") (page . 1))
               ((depth . 1) (title . "Three") (page . 1))
               ((depth . 1) (title . "Four") (page . 1))))
            :to-equal '("* One\n* Two\nbody\n* Three\n* Four"))))

(describe "pdf-text-page-headings"
  (it "lines the outline's headings up with the pages a render is given"
    (expect (pdf-text-page-headings
             '(((depth . 1) (title . "One") (page . 1))
               ((depth . 2) (title . "Deep") (page . 3))
               ((depth . 2) (title . "Deeper") (page . 3))
               ((depth . 1) (title . "Two") (page . 5)))
             2 3)
            :to-equal '(nil ("** Deep" "** Deeper") nil)))

  (it "has nothing to line up without an outline"
    (expect (pdf-text-page-headings nil 1 2) :to-equal '(nil nil))))

(describe "pdf-text--normalize-title"
  (it "reads through case, spacing and ligatures"
    (expect (pdf-text--normalize-title "The ﬁnal Word")
            :to-equal (pdf-text--normalize-title "THE  FINAL  WORD")))

  (it "reads through the punctuation a title is set with"
    (expect (pdf-text--normalize-title "A note on the starred, “curvy road” sections")
            :to-equal (pdf-text--normalize-title
                       "A note on the starred curvy road sections")))

  (it "keeps two titles that differ by a word apart"
    (expect (pdf-text--normalize-title "Sections and Notation")
            :not :to-equal (pdf-text--normalize-title "Sections and Notations"))))

(describe "headings placed at the line they name"
  (it "renders the line an outline title names as the heading itself"
    (expect (pdf-text-tests--render
             '(("The section before this one ends on this line." :x1 0.62)
               ("Other Skills and Concepts" :x1 0.45 :height 0.024 :gap 1.6)
               ("There are many other concepts and skills that a" :x1 0.90)
               ("practical data scientist needs to know." :x1 0.50))
             '("** Other Skills and Concepts"))
            :to-equal
            (string-join
             '("The section before this one ends on this line."
               ""
               "** Other Skills and Concepts"
               ""
               "There are many other concepts and skills that a practical data scientist needs to know.")
             "\n")))

  (it "matches the title through the small caps the line is set in"
    (expect (pdf-text-tests--render
             '(("OTHER SKILLS AND CONCEPTS" :x1 0.45 :height 0.024)
               ("There are many other concepts and skills that a" :x1 0.90)
               ("practical data scientist needs to know." :x1 0.50))
             '("** Other Skills and Concepts"))
            :to-equal
            (string-join
             '("** Other Skills and Concepts"
               ""
               "There are many other concepts and skills that a practical data scientist needs to know.")
             "\n")))

  (it "places each of a page's headings at its own line"
    (expect (pdf-text-tests--render
             '(("Other Skills and Concepts" :x1 0.45 :height 0.024)
               ("There are many other concepts and skills to know." :x1 0.60)
               ("Sections and Notation" :x1 0.41 :height 0.024 :gap 1.6)
               ("In addition to occasional footnotes, the book" :x1 0.90)
               ("contains boxed sidebars." :x1 0.35))
             '("** Other Skills and Concepts" "** Sections and Notation"))
            :to-equal
            (string-join
             '("** Other Skills and Concepts"
               ""
               "There are many other concepts and skills to know."
               ""
               "** Sections and Notation"
               ""
               "In addition to occasional footnotes, the book contains boxed sidebars.")
             "\n")))

  (it "leaves a line the outline has no heading for as the prose it is"
    (expect (pdf-text-tests--render
             '(("Some Other Big Line" :x1 0.45 :height 0.024)
               ("Body text follows the line above it here." :x1 0.55)))
            :to-equal "Some Other Big Line\n\nBody text follows the line above it here."))

  (it "matches the title behind the section number the page numbers it with"
    ;; a paper numbers every section its outline names bare, so nothing
    ;; matched and Introduction landed on the paper's display title
    (expect (pdf-text-tests--render
             '(("Alignment of Paragraphs in Bilingual Texts"
                :x0 0.20 :x1 0.80 :height 0.0192)
               ("Alexander Gelbukh and Grigori Sidorov" :x1 0.60 :gap 2)
               ("1 Introduction" :x1 0.28 :height 0.0164 :gap 2)
               ("Given the same text in two different languages, the" :x1 0.90)
               ("task consists in deciding which elements align." :x1 0.55))
             '("* Introduction"))
            :to-equal
            (string-join '("Alignment of Paragraphs in Bilingual Texts"
                           ""
                           "Alexander Gelbukh and Grigori Sidorov"
                           ""
                           "* 1 Introduction"
                           "Given the same text in two different languages, the task consists in deciding which elements align.")
                         "\n")))

  (it "matches a numbered title at every depth the paper sets"
    (expect (pdf-text-tests--render
             '(("3 Distance Measure" :x1 0.35 :height 0.0164)
               ("To assign the weight to a hyperarc we need the" :x1 0.90)
               ("similarity between two sets of paragraphs." :x1 0.50)
               ("3.1 Baseline Distance Measure" :x1 0.42 :height 0.0164 :gap 1.6)
               ("Common sense suggests that the corresponding" :x1 0.90)
               ("pieces of text sit at the same relative distance." :x1 0.55))
             '("* Distance Measure" "** Baseline Distance Measure"))
            :to-equal
            (string-join '("* 3 Distance Measure"
                           "To assign the weight to a hyperarc we need the similarity between two sets of paragraphs."
                           ""
                           "** 3.1 Baseline Distance Measure"
                           "Common sense suggests that the corresponding pieces of text sit at the same relative distance.")
                         "\n")))

  (it "gives a title no line spells out the line the page sets as a heading"
    ;; the outline carries the chapter number the page does not, so the
    ;; words never match; the size of the line is what is left to go on,
    ;; and the page's own words stay under the heading rather than go
    (expect (pdf-text-tests--render
             '(("Body of the section before it ends on this line." :x1 0.62)
               ("Similarity, Neighbors, and Clusters" :x1 0.45 :height 0.024 :gap 1.6)
               ("Body text follows the line above it here." :x1 0.55))
             '("* Chapter 6. Similarity, Neighbors, and Clusters"))
            :to-equal
            (string-join '("Body of the section before it ends on this line."
                           ""
                           "* Chapter 6. Similarity, Neighbors, and Clusters"
                           ""
                           "Similarity, Neighbors, and Clusters"
                           ""
                           "Body text follows the line above it here.")
                         "\n")))

  (it "reads no heading into a line the page sets like its body"
    (expect (pdf-text-tests--render
             '(("Body of the section before it ends on this line." :x1 0.62)
               ("A line no larger than the body" :x1 0.45 :gap 1.6)
               ("Body text follows the line above it here." :x1 0.55))
             '("* Chapter 6. Similarity, Neighbors, and Clusters"))
            :to-equal
            (string-join '("Body of the section before it ends on this line."
                           ""
                           "A line no larger than the body"
                           "Body text follows the line above it here.")
                         "\n")))

  (it "keeps a placed heading unescaped, so org folds the section"
    (let ((page (car (pdf-text-render-lines
                      (list (pdf-text-tests--page
                             '(("Sections and Notation" :x1 0.41 :height 0.024)
                               ("In addition to occasional footnotes, the" :x1 0.90)
                               ("book contains boxed sidebars." :x1 0.40))))
                      '(("** Sections and Notation"))))))
      (expect page :to-equal
              "** Sections and Notation\n\nIn addition to occasional footnotes, the book contains boxed sidebars.")
      (with-temp-buffer
        (insert page)
        (org-mode)
        (font-lock-ensure)
        (expect (org-element-map (org-element-parse-buffer 'headline)
                    'headline (lambda (h) (org-element-property :raw-value h)))
                :to-equal '("Sections and Notation"))))))

(describe "passages set in from the column margin"
  (it "sets a boxed sidebar in as one unit, its title line included"
    ;; the box is smaller than the body and starts at its own left edge;
    ;; its first line is its title, and printing that line flush would
    ;; leave the box a quotation with no head
    (expect (pdf-text-tests--render
             '(("Body prose at the column margin runs the full measure")
               ("and carries on across a second line of the paragraph")
               ("and a third and a fourth line at the same measure")
               ("and a fifth line of the very same paragraph")
               ("and a sixth before it ends." :x1 0.62)
               ("A note on the starred sections" :x0 0.25 :x1 0.55
                :height 0.012 :gap 1.6)
               ("The occasional mathematical details are relegated to" :x0 0.25
                :x1 0.78 :height 0.012)
               ("optional starred sections." :x0 0.25 :x1 0.42 :height 0.012)))
            :to-equal
            (string-join
             '("Body prose at the column margin runs the full measure and carries on across a second line of the paragraph and a third and a fourth line at the same measure and a fifth line of the very same paragraph and a sixth before it ends."
               ""
               "  A note on the starred sections"
               "  The occasional mathematical details are relegated to optional starred sections.")
             "\n")))

  (it "reads a lone inset line the body follows as the heading it is"
    ;; one line in from the margin with no box under it is a centred
    ;; heading or an attribution, and reads flush
    (expect (pdf-text-tests--render
             '(("Body prose at the column margin runs the full measure")
               ("and carries on across a second line of the paragraph")
               ("and a third and a fourth line at the same measure")
               ("and a fifth line of the very same paragraph")
               ("and a sixth before it ends." :x1 0.62)
               ("A Centred Line" :x0 0.25 :x1 0.55 :gap 1.6)
               ("Body prose resumes at the margin and runs on." :x1 0.62 :gap 1.6)))
            :to-equal
            (string-join
             '("Body prose at the column margin runs the full measure and carries on across a second line of the paragraph and a third and a fourth line at the same measure and a fifth line of the very same paragraph and a sixth before it ends."
               ""
               "A Centred Line"
               ""
               "Body prose resumes at the margin and runs on.")
             "\n"))))

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
            '("3 apples fell\nA long full width line sets the page wrap column for this page")))

  (it "leaves a numbered entry trailing its leader fill alone"
    ;; a TOC whose folio broke off leaves the entry closing on its
    ;; fill; the repetition names it an entry as surely as the folio
    (expect (pdf-text--synthesize-headings
             '("1.2 A brief history : : : : : : :\nA long full width prose line sets the wrap column for the page"))
            :to-equal
            '("1.2 A brief history : : : : : : :\nA long full width prose line sets the wrap column for the page"))
    (expect (pdf-text--synthesize-headings
             '("2.2 Arguments. . . . . . . .\nA long full width prose line sets the wrap column for the page"))
            :to-equal
            '("2.2 Arguments. . . . . . . .\nA long full width prose line sets the wrap column for the page"))))

(describe "pdf-text--dotted-number-level"
  (it "reads the dot count as the org level"
    (expect (pdf-text--dotted-number-level "2.2 Arguments") :to-equal 2)
    (expect (pdf-text--dotted-number-level "1.2.2.1 Improper Premise") :to-equal 4))

  (it "tolerates a trailing dot on the number"
    (expect (pdf-text--dotted-number-level "1.2. Background") :to-equal 2))

  (it "rejects a single number: exercises and footnotes open with one"
    (expect (pdf-text--dotted-number-level "1. Of course each author") :to-be nil))

  (it "rejects a trailing page number: that is a TOC entry"
    (expect (pdf-text--dotted-number-level "2.2 Arguments 20") :to-be nil)))

(describe "pdf-text--synth-page-tuples"
  (it "keeps dotted candidates off a contents page"
    ;; fpio p2: a wrapped entry's first line carries no folio, and the
    ;; dotted rule would read it as a section head; the page's own
    ;; entry lines say it is a contents page
    (let* ((lines (pdf-text-tests--page
                   '(("2.1 Functional models" :height 0.020 :x1 0.45
                      :base 0.30)
                     ("Alpha entry 3" :gap 2)
                     ("Beta entry 4")
                     ("Gamma entry 5"))))
           (profile (pdf-text--profile (list lines))))
      (dolist (l (cdr lines)) (setf (pdf-text-line-kind l) 'entry))
      (let ((page (pdf-text--page-profile lines profile)))
        (expect (cl-some (lambda (tuple) (plist-get tuple :dotted))
                         (pdf-text--synth-page-tuples
                          (pdf-text--blocks lines page) page
                          (pdf-text--hyphenated-words (list lines))))
                :not :to-be-truthy))))

  (it "keeps reading dotted candidates where no contents run stands"
    (let* ((lines (pdf-text-tests--page
                   '(("2.1 Functional models" :height 0.020 :x1 0.45
                      :base 0.30)
                     ("A body paragraph line runs the full column width here"
                      :gap 2))))
           (profile (pdf-text--profile (list lines)))
           (page (pdf-text--page-profile lines profile)))
      (expect (cl-some (lambda (tuple) (plist-get tuple :dotted))
                       (pdf-text--synth-page-tuples
                        (pdf-text--blocks lines page) page
                        (pdf-text--hyphenated-words (list lines))))
              :to-be-truthy))))

(describe "pdf-text--heading-clusters"
  (it "keeps styles heading enough distinct pages, tallest first"
    (expect (pdf-text--heading-clusters
             (append (cl-loop for page from 1 to 6
                              collect (list 0.021 '("CMBX12" . t) page))
                     (cl-loop for page from 1 to 8
                              collect (list 0.018 '("CMBX12" . t) page)))
             0.015)
            :to-equal '((0.021 0.021 "CMBX12" t) (0.018 0.018 "CMBX12" t))))

  (it "drops a style on too few pages however many blocks it has"
    ;; a cover spread sets many display lines on two pages
    (expect (pdf-text--heading-clusters
             (append (make-list 4 (list 0.030 '("Display") 1))
                     (make-list 4 (list 0.030 '("Display") 2))
                     (cl-loop for page from 1 to 5
                              collect (list 0.018 '("CMBX12" . t) page)))
             0.015)
            :to-equal '((0.018 0.018 "CMBX12" t))))

  (it "reads nearby heights as one style"
    (expect (pdf-text--heading-clusters
             (cl-loop for page from 1 to 6
                      collect (list (if (cl-oddp page) 0.0210 0.0215)
                                    '("CMBX12" . t) page))
             0.015)
            :to-equal '((0.0210 0.0215 "CMBX12" t))))

  (it "splits one height into styles by face, and support judges each"
    ;; fast-and-loose: author names share the section heads' height,
    ;; roman on one page against bold across eight
    (expect (pdf-text--heading-clusters
             (append (make-list 4 (list 0.0122 '("CMR10") 1))
                     (cl-loop for page from 1 to 8
                              collect (list 0.0122 '("CMBX10" . t) page)))
             0.01)
            :to-equal '((0.0122 0.0122 "CMBX10" t)))))

(describe "pdf-text--cluster-level"
  (it "ranks a height by its cluster, capped at pdf-text-synth-levels"
    (let ((clusters '((0.030 0.030 "F" t) (0.021 0.022 "F" t)
                      (0.018 0.018 "F" t) (0.016 0.016 "F" t)
                      (0.015 0.015 "F" t))))
      (expect (pdf-text--cluster-level 0.0215 '("F" . t) clusters) :to-equal 2)
      (expect (pdf-text--cluster-level 0.015 '("F" . t) clusters)
              :to-equal pdf-text-synth-levels)
      (expect (pdf-text--cluster-level 0.014 '("F" . t) clusters) :to-be nil)))

  (it "matches the face where a cluster carries one, any face where not"
    ;; a bare (MIN . MAX) range is the shape captures stored before
    ;; faces joined the key, and it must keep matching
    (let ((clusters '((0.021 0.022 "CMBX10" t) (0.018 . 0.018))))
      (expect (pdf-text--cluster-level 0.0215 '("CMR10") clusters) :to-be nil)
      (expect (pdf-text--cluster-level 0.0215 '("CMBX10" . t) clusters)
              :to-equal 1)
      (expect (pdf-text--cluster-level 0.018 '("Anything") clusters)
              :to-equal 2))))

(describe "geometry heading synthesis"
  (cl-flet ((synth (pages) (pdf-text-render-lines
                            (mapcar #'pdf-text-tests--page pages) nil nil t))
            (body-page (&rest head-specs)
              (append head-specs
                      '(("prose fills the column all the way to the right edge" :base 0.5)
                        ("and continues to a second full line of body text here")
                        ("and a third full body line keeps the profile honest")))))
    (it "promotes a numbered body-size line and leaves single numbers alone"
      (let ((page (car (synth (list (body-page
                                     '("2.2 Arguments" :x1 0.40 :base 0.30)
                                     '("1. Of course this line stays" :x1 0.60 :gap 2)))))))
        (expect page :to-match "^\\*\\* 2\\.2 Arguments")
        (expect page :not :to-match "^\\*+ 1\\. Of course")))

    (it "promotes a recurring display style at its cluster's rank"
      (let ((pages (synth
                    (cl-loop for n in '("One" "Two" "Three" "Four" "Five")
                             collect (body-page
                                      (list (concat "Alpha " n)
                                            :x1 0.40 :base 0.30 :height 0.021))))))
        (expect (car pages) :to-match "^\\* Alpha One")
        (expect (nth 4 pages) :to-match "^\\* Alpha Five")))

    (it "promotes a bold section at the body size by its face"
      ;; applicative: sections bold at 0.95 body, where size sees
      ;; nothing; the lead face carries the bold a sans identifier
      ;; hides from the dominant slot
      (let ((pages (synth
                    (cl-loop for n in '("One" "Two" "Three" "Four" "Five")
                             collect (body-page
                                      (list (concat "Applicative " n)
                                            :x0 0.40 :x1 0.60 :base 0.30
                                            :height 0.0143
                                            :font "KEYTKS+CMSS10"
                                            :lead-font "ABHJSZ+CMBX10"
                                            :lead-bold t))))))
        (expect (car pages) :to-match "^\\* Applicative One")
        (expect (nth 4 pages) :to-match "^\\* Applicative Five")))

    (it "keeps a bold line set tight against the text above it prose"
      ;; a workbook's bold vocabulary label sits one leading under its
      ;; flow; a real bold head stands a paragraph gap clear
      (let ((pages (synth
                    (cl-loop for n in '("One" "Two" "Three" "Four" "Five")
                             collect
                             `(("prose fills the column all the way to the right edge" :base 0.5)
                               ("and continues to a second full line of body text here")
                               (,(concat "Palabra " n)
                                :x0 0.10 :x1 0.30 :height 0.0143
                                :font "NimbusRomNo9L-Medi"
                                :lead-font "NimbusRomNo9L-Medi"
                                :lead-bold t)
                               ("and a third full body line keeps the profile honest"))))))
        (expect (car pages) :not :to-match "^\\*")))

    (it "gives a bold dotted line its dot depth, not a cluster rank"
      ;; LNCS sets numbered subsection heads bold a shade under the
      ;; body; the dot count is their depth
      (let ((page (car (synth
                        (list (body-page
                               '("2.1 Discrete and Continuous Modifications"
                                 :x0 0.10 :x1 0.55 :base 0.30 :height 0.0143
                                 :font "NREEDL+NimbusRomNo9L-Medi"
                                 :lead-font "NREEDL+NimbusRomNo9L-Medi"
                                 :lead-bold t)))))))
        (expect page :to-match "^\\*\\* 2\\.1 Discrete")))

    (it "keeps roman lines off a bold heading style at their height"
      ;; fast-and-loose: author names share the section heads' height,
      ;; roman against bold, and only the heads recur across pages
      (let ((pages (synth
                    (cons (body-page
                           '("Nils Anders Danielsson" :x0 0.30 :x1 0.55
                             :base 0.26 :height 0.018 :font "JBZUTU+CMR10"
                             :lead-font "JBZUTU+CMR10")
                           '("Abstract" :x0 0.44 :x1 0.56 :base 0.34
                             :height 0.018 :font "QKNFQX+CMBX10"
                             :lead-font "QKNFQX+CMBX10" :lead-bold t))
                          (cl-loop for n in '("Two" "Three" "Four" "Five" "Six")
                                   collect (body-page
                                            (list (concat "Section " n)
                                                  :x0 0.40 :x1 0.60 :base 0.30
                                                  :height 0.018
                                                  :font "QKNFQX+CMBX10"
                                                  :lead-font "QKNFQX+CMBX10"
                                                  :lead-bold t)))))))
        (expect (car pages) :not :to-match "^\\*+ Nils")
        (expect (car pages) :to-match "^\\* Abstract")
        (expect (nth 3 pages) :to-match "^\\* Section Four")))

    (it "leaves a one-spread display style prose: no supported cluster"
      (let ((pages (synth
                    (cons (body-page '("Cover Display Type" :x1 0.5 :base 0.30
                                       :height 0.030))
                          (cl-loop for n in '("Two" "Three" "Four")
                                   collect (body-page))))))
        (expect (car pages) :to-match "^Cover Display Type")
        (expect (car pages) :not :to-match "\\*+ Cover")))

    (it "merges a bare display number with the title it belongs to"
      (let ((pages (synth
                    (cl-loop for n in '("One" "Two" "Three" "Four" "Five")
                             collect (body-page
                                      '("3.1" :x1 0.14 :base 0.30 :height 0.021)
                                      (list (concat "Concepts " n)
                                            :x0 0.20 :x1 0.45 :gap 0 :height 0.021))))))
        (expect (car pages) :to-match "^\\*\\* 3\\.1 Concepts One")
        ;; the consumed half renders nowhere else
        (expect (car pages) :not :to-match "^Concepts One")))

    (it "merges a worded eyebrow into the larger title under it"
      (let ((pages (synth
                    (cl-loop for n in '("One" "Two" "Three" "Four" "Five")
                             collect (body-page
                                      '("Chapter 2" :x1 0.30 :base 0.26 :height 0.022)
                                      (list (concat "Steps " n)
                                            :x1 0.40 :gap 3 :height 0.0265))))))
        (expect (car pages) :to-match "^\\* Chapter 2 Steps One")))

    (it "never promotes a line opening inside the margin band"
      ;; a running head the marginal rules kept: base in the band, not
      ;; detached from the body below it
      (let ((page (car (synth (list (body-page
                                     '("1.2. FUNCTIONAL PROGRAMMING" :x1 0.5
                                       :base 0.06 :height 0.021)
                                     '("body right under the head keeps it attached"
                                       :base 0.08)))))))
        (expect page :to-match "FUNCTIONAL PROGRAMMING")
        (expect page :not :to-match "\\*+ 1\\.2\\. FUNCTIONAL")))

    (it "never promotes display-set code: operator glyphs no title carries"
      (let ((pages (synth
                    (cl-loop for n from 1 to 5
                             collect (body-page
                                      '("= { applying ∗ }" :x0 0.3 :x1 0.5
                                        :base 0.30 :height 0.021))))))
        (expect (car pages) :to-match "applying")
        (expect (car pages) :not :to-match "\\*+ =")))

    (it "escapes a literal star line while its own headings parse"
      (let ((page (car (synth (list (body-page
                                     '("2.2 Arguments" :x1 0.40 :base 0.30)
                                     '("* bullet stays text" :x1 0.40 :gap 2)))))))
        (expect page :to-match "^\\*\\* 2\\.2 Arguments")
        (expect page :to-match "\u200B\\* bullet stays text")))

    (it "falls back to the text-only rule when no page has geometry"
      (expect (pdf-text-render-lines
               (list (list (pdf-text-line-create
                            :text "2.2 Arguments")
                           (pdf-text-line-create
                            :text "A long full width prose line sets the wrap column here")))
               nil nil t)
              :to-equal
              '("** 2.2 Arguments\n\nA long full width prose line sets the wrap column here")))

    (it "takes the document's clusters from the seed when one is bound"
      (let* ((pages (list (body-page '("Solo Display" :x1 0.40 :base 0.30
                                       :height 0.021))))
             (bare (car (synth pages)))
             (seeded (let ((pdf-text-extra-heading-levels '((0.021 . 0.021))))
                       (car (synth pages)))))
        (expect bare :not :to-match "\\* Solo Display")
        (expect seeded :to-match "^\\* Solo Display")))))

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
      (expect (invisible-p (1- (point))) :to-equal nil)))

  (it "reveals every section the page shows, not just the landing lineage"
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (pdf-text-mode)
        (pdf-text--insert-pages
         '("front matter"
           "* One\ntail of section one\n** Two\ntwo opens mid-page"
           "* Three\nthree body overleaf"))
        (setq-local pdf-text--has-outline t)
        (org-cycle-overview))
      (goto-char (pdf-text--page-start 2))
      (pdf-text--reveal-page 2)
      (goto-char (point-min))
      (search-forward "tail of section one")
      (expect (invisible-p (1- (point))) :to-equal nil)
      (search-forward "two opens mid-page")
      (expect (invisible-p (1- (point))) :to-equal nil)
      (search-forward "three body overleaf")
      (expect (invisible-p (1- (point))) :to-be-truthy))))

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

(describe "pdf-text-sync-mode"
  (it "arms hooks on both buffers and disarms them on toggle"
    (let ((pdf (generate-new-buffer " *sync-pdf*"))
          (companion (generate-new-buffer " *sync-text*")))
      (unwind-protect
          (with-current-buffer companion
            (pdf-text-mode)
            (setq pdf-text--pdf-buffer pdf)
            (pdf-text-sync-mode 1)
            (expect (memq #'pdf-text-sync--follow-text post-command-hook)
                    :to-be-truthy)
            (expect (memq #'pdf-text-sync--follow-pdf
                          (buffer-local-value 'pdf-view-after-change-page-hook
                                              pdf))
                    :to-be-truthy)
            (pdf-text-sync-mode -1)
            (expect (memq #'pdf-text-sync--follow-text post-command-hook)
                    :to-be nil)
            (expect (memq #'pdf-text-sync--follow-pdf
                          (buffer-local-value 'pdf-view-after-change-page-hook
                                              pdf))
                    :to-be nil))
        (kill-buffer companion)
        (kill-buffer pdf))))

  (it "re-enabling arms a reopened source buffer"
    (let ((pdf1 (generate-new-buffer " *sync-pdf1*"))
          (pdf2 (generate-new-buffer " *sync-pdf2*"))
          (companion (generate-new-buffer " *sync-text*")))
      (unwind-protect
          (with-current-buffer companion
            (pdf-text-mode)
            (setq pdf-text--pdf-buffer pdf1)
            (pdf-text-sync-mode 1)
            (kill-buffer pdf1)
            (setq pdf-text--pdf-buffer pdf2)
            (pdf-text-sync-mode 1)
            (expect pdf-text-sync-mode :to-be-truthy)
            (expect (memq #'pdf-text-sync--follow-pdf
                          (buffer-local-value 'pdf-view-after-change-page-hook
                                              pdf2))
                    :to-be-truthy))
        (when (buffer-live-p pdf1) (kill-buffer pdf1))
        (kill-buffer pdf2)
        (kill-buffer companion))))

  (it "refuses outside pdf-text and without a live source"
    (with-temp-buffer
      (expect (pdf-text-sync-mode 1) :to-throw 'user-error))
    (let ((companion (generate-new-buffer " *sync-text*")))
      (unwind-protect
          (with-current-buffer companion
            (pdf-text-mode)
            (expect (pdf-text-sync-mode 1) :to-throw 'user-error)
            (expect pdf-text-sync-mode :to-be nil))
        (kill-buffer companion)))))

(defun pdf-text-tests--pair ()
  "A paired companion and PDF buffer, as (COMPANION . PDF)."
  (let ((pdf (generate-new-buffer " *kill-pdf*"))
        (companion (generate-new-buffer " *kill-text*")))
    (with-current-buffer companion (pdf-text-mode))
    (pdf-text--pair companion pdf)
    (cons companion pdf)))

(defun pdf-text-tests--drop-pair (pair)
  "Kill whichever half of PAIR is still alive, the pairing switched off."
  (let ((pdf-text-kill-together nil))
    (dolist (buf (list (car pair) (cdr pair)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(describe "pdf-text--pair"
  (it "points both halves at each other and arms the kill hook on each"
    (let ((pair (pdf-text-tests--pair)))
      (unwind-protect
          (progn
            (expect (buffer-local-value 'pdf-text--pdf-buffer (car pair))
                    :to-be (cdr pair))
            (expect (buffer-local-value 'pdf-text--companion (cdr pair))
                    :to-be (car pair))
            (dolist (buf (list (car pair) (cdr pair)))
              (expect (memq #'pdf-text--kill-partner
                            (buffer-local-value 'kill-buffer-hook buf))
                      :to-be-truthy)))
        (pdf-text-tests--drop-pair pair))))

  (it "leaves one hook a side however often it re-pairs"
    ;; `pdf-view-as-text' pairs on the reuse path too, so a reader who
    ;; opens the same book ten times must not stack ten hooks
    (let ((pair (pdf-text-tests--pair)))
      (unwind-protect
          (progn
            (pdf-text--pair (car pair) (cdr pair))
            (pdf-text--pair (car pair) (cdr pair))
            (dolist (buf (list (car pair) (cdr pair)))
              (expect (cl-count #'pdf-text--kill-partner
                                (buffer-local-value 'kill-buffer-hook buf))
                      :to-equal 1)))
        (pdf-text-tests--drop-pair pair))))

  (it "follows the companion to a reopened PDF buffer"
    ;; the reader closes the PDF and opens it again; the next
    ;; `pdf-view-as-text' re-pairs, and the kill has to take the live
    ;; half rather than reach for the buffer that is already gone
    (let* ((pair (pdf-text-tests--pair))
           (companion (car pair))
           (reopened (generate-new-buffer " *kill-pdf2*")))
      (unwind-protect
          (progn
            (let ((pdf-text-kill-together nil)) (kill-buffer (cdr pair)))
            (pdf-text--pair companion reopened)
            (kill-buffer companion)
            (expect (buffer-live-p reopened) :to-be nil))
        (pdf-text-tests--drop-pair pair)
        (let ((pdf-text-kill-together nil))
          (when (buffer-live-p reopened) (kill-buffer reopened)))))))

(describe "pdf-text-kill-together"
  (it "takes the other half with it, whichever half goes"
    ;; and the kill returns at all: each half's hook fires on the way
    ;; out, so without the guard the partner's hook comes back for the
    ;; buffer already dying and the recursion never bottoms out
    (let ((pair (pdf-text-tests--pair)))
      (unwind-protect
          (progn (kill-buffer (car pair))
                 (expect (buffer-live-p (cdr pair)) :to-be nil)
                 (expect pdf-text--killing :to-be nil))
        (pdf-text-tests--drop-pair pair)))
    (let ((pair (pdf-text-tests--pair)))
      (unwind-protect
          (progn (kill-buffer (cdr pair))
                 (expect (buffer-live-p (car pair)) :to-be nil)
                 (expect pdf-text--killing :to-be nil))
        (pdf-text-tests--drop-pair pair))))

  (it "carries the kill one way only under `from-pdf'"
    (let ((pdf-text-kill-together 'from-pdf)
          (pair (pdf-text-tests--pair)))
      (unwind-protect
          (progn (kill-buffer (car pair))
                 (expect (buffer-live-p (cdr pair)) :to-be t))
        (pdf-text-tests--drop-pair pair)))
    (let ((pdf-text-kill-together 'from-pdf)
          (pair (pdf-text-tests--pair)))
      (unwind-protect
          (progn (kill-buffer (cdr pair))
                 (expect (buffer-live-p (car pair)) :to-be nil))
        (pdf-text-tests--drop-pair pair))))

  (it "carries the kill the other way only under `from-text'"
    (let ((pdf-text-kill-together 'from-text)
          (pair (pdf-text-tests--pair)))
      (unwind-protect
          (progn (kill-buffer (cdr pair))
                 (expect (buffer-live-p (car pair)) :to-be t))
        (pdf-text-tests--drop-pair pair)))
    (let ((pdf-text-kill-together 'from-text)
          (pair (pdf-text-tests--pair)))
      (unwind-protect
          (progn (kill-buffer (car pair))
                 (expect (buffer-live-p (cdr pair)) :to-be nil))
        (pdf-text-tests--drop-pair pair))))

  (it "leaves both halves alone when nil"
    (let ((pdf-text-kill-together nil)
          (pair (pdf-text-tests--pair)))
      (unwind-protect
          (progn (kill-buffer (car pair))
                 (expect (buffer-live-p (cdr pair)) :to-be t))
        (pdf-text-tests--drop-pair pair))))

  (it "is read at the kill, so a change reaches an existing pair"
    (let ((pair (let ((pdf-text-kill-together nil)) (pdf-text-tests--pair))))
      (unwind-protect
          (progn (kill-buffer (car pair))
                 (expect (buffer-live-p (cdr pair)) :to-be nil))
        (pdf-text-tests--drop-pair pair))))

  (it "says nothing when the partner is already gone"
    (let ((pair (pdf-text-tests--pair)))
      (unwind-protect
          (progn
            (let ((pdf-text-kill-together nil)) (kill-buffer (cdr pair)))
            (kill-buffer (car pair))
            (expect (buffer-live-p (car pair)) :to-be nil))
        (pdf-text-tests--drop-pair pair))))

  (it "takes the pair down with the sync armed on both buffers"
    ;; `pdf-text-sync-mode' puts a hook in each buffer, and the pdf-side
    ;; one removes itself once the companion dies - adjacent code that
    ;; must not fire into a half-killed pair, and no hook may outlive
    ;; the buffers into the session's own `post-command-hook'
    (let ((pair (pdf-text-tests--pair)))
      (unwind-protect
          (progn
            (with-current-buffer (car pair) (pdf-text-sync-mode 1))
            (expect (memq #'pdf-text-sync--follow-pdf
                          (buffer-local-value 'pdf-view-after-change-page-hook
                                              (cdr pair)))
                    :to-be-truthy)
            (kill-buffer (car pair))
            (expect (buffer-live-p (cdr pair)) :to-be nil)
            (expect (memq #'pdf-text-sync--follow-text post-command-hook)
                    :to-be nil))
        (pdf-text-tests--drop-pair pair)))))

(describe "pdf-text--pdf-page"
  (it "reads the page from image-mode window properties"
    (with-temp-buffer
      (require 'image-mode)
      (setq-local image-mode-winprops-alist
                  (list (cons t (list (cons 'page 7)))))
      (expect (pdf-text--pdf-page) :to-equal 7))))

(describe "pdf-text--with-render-gc"
  (it "raises the thresholds for the body's extent only"
    (let ((threshold gc-cons-threshold)
          (percentage gc-cons-percentage))
      (pdf-text--with-render-gc
        (expect gc-cons-threshold :to-equal pdf-text-gc-cons-threshold)
        (expect gc-cons-percentage :to-equal 0.6))
      (expect gc-cons-threshold :to-equal threshold)
      (expect gc-cons-percentage :to-equal percentage)))

  (it "restores the thresholds when the body signals"
    (let ((threshold gc-cons-threshold)
          (percentage gc-cons-percentage))
      (expect (pdf-text--with-render-gc
                (user-error "The render refused"))
              :to-throw 'user-error)
      (expect gc-cons-threshold :to-equal threshold)
      (expect gc-cons-percentage :to-equal percentage)))

  (it "returns the body's value"
    (expect (pdf-text--with-render-gc 42) :to-equal 42)))
