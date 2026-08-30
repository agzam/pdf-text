;; What this case exercises; edit by hand.  `bb corpus-accept' leaves it alone.
(:book "Fast and Loose Reasoning is Morally Correct"
 :source "Fast and Loose Reasoning is Morally Correct.pdf"
 :page 1
 :exercises "Author names at the section heads' exact height, told apart by face.

The names (CMR10 roman, h 0.0122) and the paper's real heads -
\"Abstract\", \"1.\" + \"Introduction\" in CMBX10 bold - share the
quad height 0.0122 to the digit, so a height-only cluster promoted
the names to level-1 headings braided into the head matter.  The
face key holds them out: the roman style heads one page and earns no
level, the bold style heads enough pages and does.

\"1.\" and \"Introduction\" arrive as two bold records on one
baseline - the same split-number class as denotational's - and the
bare-number branch rejoins them before the synthesis sees the page.
The ACM run-in heads (\"Categories and Subject Descriptors\", CMBX9
bold at the body size) tuple through the bold gate and then fail
cluster support on their single page, so they stay run-in prose.
The display title survives the recurring rule that would read it as
its own later running heads, and its ^{*} marker pairs with the
funding note at the foot.

The budgets are the page's own: the display equation \"curry ◦
uncurry = id\" breaks its sentence - the standing display-math
residual - and the compressed citation keys (\"BdM96\", \"BdBH^{+}91\")
read as glued transitions."
 :budget (:mid-sentence 1 :glued 2))
