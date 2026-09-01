;; What this case exercises; edit by hand.  `bb corpus-accept' leaves it alone.
(:book "MonadTransformers"
 :source "MonadTransformers.pdf"
 :page 3
 :exercises "Haskell set in text italic, read as an org table.

The eval0 definition arrives as aligned record pairs - \"eval0 env
(Lit i)\" at x 0.165 beside \"= IntVal i\" at x 0.353 - over three
rows with a clean gutter, so the lane pass claims the region and
emits \"| /eval0/ | :: /Env -> Exp -> Value/ |\" rows, emphasis
markers and all.  Neither decline can see it: the cells are CMTI10,
TeX's text italic, so the monospace name test fails, the advance
variation is a proportional font's, and the glyphs are Haskell, not
maths.  This is the AIMA pseudocode residual of Round 3 in lane
form.

The face and what the lanes align on are the signal.  Every cell is
one face the body never uses, and the second lane opens on an
operator - `::', `=' - on every row: code aligns on operators, a
table aligns on nothing in particular.  Declined, the rows read as
the listing the page set.

No invariant names this shape - the rows are a legal org table of
the page's own words - so the golden is the proof, the way the
Linnaean paper's split heads were.

The definition below the block stays as it was: MuPDF serves those
lines welded, one record each, and no lane sits under them to read.
That is the p12 class of the previous round, recorded there.

Budget: the snippets between the prose (\"eval0 Map.empty
exampleExp\", the example expression) break their lead-in sentence,
which is the recorded code-inside-prose residual; the healed block
adds two of its own, each seam a line of Haskell against the next,
and the glued count reads the emphasis markers the italic runs put
inside identifiers."
 :budget (:mid-sentence 4 :glued 4))
