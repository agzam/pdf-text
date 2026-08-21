;; What this case exercises; edit by hand.  `bb corpus-accept' leaves it alone.
(:book "Data Science for Business"
 :source "Language Agnostic/[Data.Science.for.Business(2013.7)].Foster.Provost.pdf"
 :page 17
 :exercises "The bulleted list that started Round 3: a blank line before it and
after it, one item per line, the third item its own item rather than a tail of
the second.  Also the wrap hyphen the book resolves elsewhere - well-known keeps
its hyphen, which only the captured vocabulary can decide."
 :budget (:mid-sentence 0))
;; No :tolerates.  The page opens the Preface, so it also guards the
;; heading placement: the chapter title reads once, as the heading.
