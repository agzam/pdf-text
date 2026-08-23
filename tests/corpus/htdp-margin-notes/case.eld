;; What this case exercises; edit by hand.  `bb corpus-accept' leaves it alone.
(:book "Felleisen M., Findler R.B., Flatt M., Krishnamurthi Sh. - How to Design Programs, 2nd edition - 2018"
 :source "Felleisen M., Findler R.B., Flatt M., Krishnamurthi Sh. - How to Design Programs, 2nd edition - 2018.pdf"
 :page 184
 :exercises "A margin note beside the body column.  The note sits right of the
column in smaller type; its lines must wrap against their own measure
- the block's widest line, not the column's edge - and join into one
inset paragraph at the end of the page instead of braiding into the
body or breaking at every line.  Around it, the page carries what
this book is made of: cond skeletons rendered verbatim, an inset
Sample Problem block, and a define-struct data definition.  Known
wart, no invariant sees it: the BSL comment block after define-struct
munges its continuation semicolons (`; A UFO is a structure: ;'), the
reading-order residual in comment columns."
 :budget (:mid-sentence 0))
