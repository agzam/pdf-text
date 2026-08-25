;; What this case exercises; edit by hand.  `bb corpus-accept' leaves it alone.
(:book "Russell S.J., Norvig P. - Artificial Intelligence. A Modern Approach, 4th Global Edition (Pearson Series In Artificial Intelligence) - 2022"
 :source "Russell S.J., Norvig P. - Artificial Intelligence. A Modern Approach, 4th Global Edition (Pearson Series In Artificial Intelligence) - 2022.pdf"
 :page 44
 :exercises "Mirrored margins.  Facing pages set the body column 0.11 apart -
even pages 0.095-0.795, odd 0.205-0.905 - and the document's modal
edges match the odd parity, so this even page reads correctly only
because the per-page profile recomputes its own column.  Against the
document edges every line here would sit a dedent left of the margin
and the first-line indents would vanish.  The page is justified
prose with indent-marked paragraphs, an inset block quote joined as
one passage, and the book's outer-margin term labels (Hidden Markov
models, Bayesian network) kept as detached lines at the page's end
rather than braided into the paragraphs they annotate."
 :budget (:mid-sentence 0 :glued 3))
