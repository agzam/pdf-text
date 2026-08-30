;; What this case exercises; edit by hand.  `bb corpus-accept' leaves it alone.
(:book "[Data.Science.for.Business(2013.7)].Foster.Provost"
 :source "Language Agnostic/[Data.Science.for.Business(2013.7)].Foster.Provost.pdf"
 :page 11
 :exercises "A table of contents whose folios MuPDF serves inline: every entry
arrives as one full-measure line closing on its page number, so no
numeric lane exists to pair rows from (poppler served the numbers as
their own column, and this case once rendered as an org table).  The
contents-run rule keeps the entries from chaining into paragraphs -
three or more neighbours closing on a short digit group each keep
their own line - and Summary 108, the entry the margin rules once
ate, reads inline like the rest.  The declared drops are the running
foot (Table of Contents, roman folio v).  The chapter lines with
their dot leaders stay prose."
 :budget (:mid-sentence 0))
