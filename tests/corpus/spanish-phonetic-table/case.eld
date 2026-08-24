;; What this case exercises; edit by hand.  `bb corpus-accept' leaves it alone.
(:book "A new reference grammar of modern Spanish (2019)"
 :source "Espanol/A new reference grammar of modern Spanish (2019).pdf"
 :page 11
 :exercises "A true table, the FW11 fix landed.  The Phonetic Symbols table sets
a header row (Symbol / Phonetic description / Remarks) over rows
whose cells arrive as separate records in fixed x-lanes - some rows
tab-joined into one record, wrapped cells continuing indented or
tab-prefixed.  The multicolumn pass reads the lanes off the
persistent gutters and renders org table rows, cells padded to their
lane, continuations joined into their cell.  The prose half of the
page above the table keeps flowing as paragraphs."
 :budget (:mid-sentence 0))
