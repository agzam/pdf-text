;; What this case exercises; edit by hand.  `bb corpus-accept' leaves it alone.
(:book "Applicative"
 :source "Applicative.pdf"
 :page 3
 :exercises "The same paper's inside page, captured red: a body-size section
heading among the code and the display maths that share its measure.

\"2 The Applicative class\" is a single centred line at h 0.0105 -
0.95 of the modal body - carrying a single section number, which the
dotted rule rejects on purpose (exercises and footnotes open with
one).  Around it sit the lines that must NOT be promoted: centred
code fragments in their listing run, display maths at 0.0105-0.0123,
and the running head \"Functional pearl\" with its folio, which is
itself a short centred capitalised line with air on both sides - the
false positive any position-based heading rule has to survive, and
the reason the furniture goes first.

The mid-sentence budget is the law paragraph broken by its own
display maths."
 :budget (:mid-sentence 1 :glued 0))
