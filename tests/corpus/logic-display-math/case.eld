;; What this case exercises; edit by hand.  `bb corpus-accept' leaves it alone.
(:book "Logic_in_computer_science__Benji_MO"
 :source "Logic_in_computer_science__Benji_MO.pdf"
 :page 101
 :exercises "Inline math fragmented by script stacks, and a displayed axiom
block.  The big-operator limits of a proof paragraph arrive as their own
poppler lines - a subscript run at a near-zero baseline step, a raised m that
even reads as a roman folio - and must rejoin the sentence they belong to.
The Hilbert axioms H1-H3 and MP are display lines, inset and math-dense; each
keeps its own line instead of merging into a paragraph."
 :budget (:mid-sentence 1))
;; The budgeted break is not math: the section heading and the first
;; paragraph line group as one block (a single-line block has no body
;; margin for the indent rule to fire against), so the split lands one
;; line late.  A heading-placement residual, not a display-math one.
