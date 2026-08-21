;; What this case exercises; edit by hand.  `bb corpus-accept' leaves it alone.
(:book "Data Science for Business"
 :source "Language Agnostic/[Data.Science.for.Business(2013.7)].Foster.Provost.pdf"
 :page 20
 :exercises "The page of the Round 4 report.  Two outline entries name sections
that start mid-page - \"Other Skills and Concepts\" at line 4, \"Sections and
Notation\" at line 12 - and each heading has to sit at the line it names, so
that the first section owns its paragraph instead of folding shut on nothing.
A boxed sidebar sits inset and smaller than the body from \"A note on the
starred\"; its title line belongs to the box and is set in with it."
 :budget (:mid-sentence 0))
;; No :tolerates, and none is coming: this page is the Round 4 defect
;; itself.  A licence here would be a licence for the bug under repair.
