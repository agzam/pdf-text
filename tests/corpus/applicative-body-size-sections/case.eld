;; What this case exercises; edit by hand.  `bb corpus-accept' leaves it alone.
(:book "Applicative"
 :source "Applicative.pdf"
 :page 3
 :exercises "The same paper's inside page: a body-size section heading among the
code and the display maths that share its measure.

\"2 The Applicative class\" is a single centred line at h 0.0105 -
0.95 of the modal body - and its dominant run is the sans identifier
\"Applicative\", not bold at all: the record's lead face (CMBX10,
bold) is what admits it as a heading candidate, and the book-wide
face cluster is what earns it the level.  Around it sit the lines
that must NOT be promoted: centred code fragments in their listing
run, display maths at 0.0105-0.0123, and the running head
\"Functional pearl\" with its folio - a short centred capitalised
line with air on both sides, the false positive any position-based
heading rule has to survive.  The furniture goes first, by the text
area and the folio on its baseline, so the heading rules never meet
it.  Captured red for the surviving furniture from daaab05 to the
Round 12 unit 4 fix.

The mid-sentence budget is the law paragraph broken by its own
display maths."
 :budget (:mid-sentence 1 :glued 0))
