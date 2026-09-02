;; What this case exercises; edit by hand.  `bb corpus-accept' leaves it alone.
(:book "MonadTransformers"
 :source "MonadTransformers.pdf"
 :page 2
 :exercises "Footnote definitions set in a code face, two notes in one block.

The body cites lhs2TeX^{1} and homepage^{2}, and the page's foot
answers with two URLs in CMTT10, a typeset line each, that arrive as
one mono block - by advance variation, since the opening run of each
line is the CMR6 marker and the name test wants the lead face mono
too.  The footnote walk over the page's trailing blocks read only
prose and item blocks, so it never opened on the pair, and both
notes reached the reader raw: `^{1}http://www.iai.uni-bonn.de/~loeh/'
as a line of the page.  A mono block joins the foot as a stack of
marker-opened lines the body cites every one of, one definition per
line, and both notes pair: [fn:2-1] and [fn:2-2] at their markers,
the definitions at the foot.

The glued budget is the paper's own camelCase - lhs2TeX, FunVal -
which the emphasis markers and the label ride inside.  The Haskell
blocks in the text italic face still join as prose with emphasis
markers in their identifiers (the module and import block, the data
declarations): the code-in-a-text-face residual, recorded and not
licensed here since no invariant fires on it."
 :budget (:mid-sentence 0 :glued 2))
