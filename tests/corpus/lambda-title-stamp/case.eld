;; What this case exercises; edit by hand.  `bb corpus-accept' leaves it alone.
(:book "Tutorial Introduction to Lambda Calculus"
 :source "Tutorial Introduction to Lambda Calculus.pdf"
 :page 1
 :exercises "A rotated arXiv stamp in the left margin, and a title-page
footnote whose star is not an asterisk.

The stamp reads \"arXiv:1503.09060v1 [cs.LO] 28 Mar 2015\" up the
left margin.  Its record arrives with a lying horizontal box - x
0.018-0.061 for 40 characters, a density no type can set - and every
rule downstream believes it, so it survives to the reader as a line
of the page and drags the note above it into a mid-sentence pair.
MuPDF knows better: `beginLine' reports the line's direction, [0,-1]
here against [1,0] on every horizontal line of the page, and a line
that does not run along the page's own direction is furniture.

The footnote has both halves and pairs neither.  The author line
sets \"Raul Rojas^{*}\" and the foot of the page opens \"^{*}Send
corrections or suggestions...\", but the star is U+2217 ASTERISK
OPERATOR, which the marker set never listed - TeX's footnote symbol
comes out of a math font.  The stamp compounds it: the foot walk
starts at the page's last block, the stamp is that block and is not
small type, so the walk stops before it reaches the note at all.

The dotless bare-number join reads here too: \"1\" and
\"Definition\" are two CMBX12 records on one baseline, and the
heading places at the line they make rather than over the abstract.

The capture also caught a measure, not a render: an outline that
numbers its own titles - \"1 Definition\" - could never match the
line it names, because the placement count stripped the number from
the page's line and not from the title."
 :budget (:mid-sentence 0 :glued 0))
