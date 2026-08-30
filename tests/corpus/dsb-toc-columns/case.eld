;; What this case exercises; edit by hand.  `bb corpus-accept' leaves it alone.
(:book "[Data.Science.for.Business(2013.7)].Foster.Provost"
 :source "Language Agnostic/[Data.Science.for.Business(2013.7)].Foster.Provost.pdf"
 :page 11
 :exercises "A table of contents whose folios MuPDF serves inline: every entry
arrives as one full-measure line closing on its page number, so no
numeric lane exists to pair rows from (poppler served the numbers as
their own column, and this case once rendered as an org table).  The
contents-run rule keeps the entries from chaining into paragraphs -
three or more neighbours closing on a short digit group render as an
org list, one item per entry - and Summary 108, the entry the margin
rules once ate, reads inline like the rest.  The declared drops are
the running foot (Table of Contents, roman folio v).  The chapter
lines carry their spaced-dot leader fill and folio inline in one
record - the other shape of the fill - and the strip leaves \"- 5.
Overfitting and Its Avoidance 111\" paired as an item of its own,
the italic description block no longer glued after the folio."
 :budget (:mid-sentence 0))
