;; What this case exercises; edit by hand.  `bb corpus-accept' leaves it alone.
(:book "A new reference grammar of modern Spanish (2019)"
 :source "Espanol/A new reference grammar of modern Spanish (2019).pdf"
 :page 11
 :exercises "Future-work 11 evidence: a true table.  The Phonetic Symbols table
sets a header row (Symbol / Phonetic description / Remarks) over rows
whose cells arrive as separate records in fixed x-lanes, wrapped
cells continuing indented.  Until real column detection lands, the
render interleaves the lanes - header cells as lone lines, a cell's
continuation detached from its row - and this golden records that
salad deliberately; the org-table rendering FW11 plans will rewrite
it.  The prose half of the page above the table must keep flowing as
paragraphs.  The budget is the table's lane interleaving, all ten
breaks inside it."
 :budget (:mid-sentence 10))
