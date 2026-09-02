;; What this case exercises; edit by hand.  `bb corpus-accept' leaves it alone.
(:book "fpio - functional programming and IO - Gordon 1994"
 :source "/Users/ryl/SyncMobile/Papers/fpio - functional programming and IO - Gordon 1994.pdf"
 :page 20
 :exercises "Type 3 math glyphs in running prose: the lazy λ-calculus and
π-calculus discussion sets λ, Ω and π from cmmi subsets whose
Differences name glyphs by byte position, so every Greek letter
read U+FFFD until the walker fingerprinted the fonts by their
canonical widths and decoded the byte positions through the cmmi
table - the golden now reads the letters as the page prints them.
The budget is the script-fragment class, not the decode: the math
records sit apart from their lines' baselines, so \"his lazy /
λ-calculus\" and the (λx.Ω) display still break their sentences
with the glyphs decoded, exactly as the leaders unit recorded for
the p2 entries.  The orphan it tolerates is the page's true first
line, which continues the page before: the running head \"1.3.
SEMANTICS OF FUNCTIONAL LANGUAGES\" survives above it - the folio
alone drops - so the continuation reads as stranded under a closed
line.  It retires with the head."
 :budget (:mid-sentence 6)
 :tolerates (orphan))
