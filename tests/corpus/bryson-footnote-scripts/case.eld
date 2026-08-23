;; What this case exercises; edit by hand.  `bb corpus-accept' leaves it alone.
(:book "A Short History of Nearly Everything"
 :source "A Short History of Nearly Everything by Bill Bryson.pdf"
 :page 18
 :exercises "Superscripts in running prose, and a footnote under its marker.
The body cites 10^-43 twice and closes a sentence on an asterisk marker;
each raised run must render as ^{...} attached to its base, not mush into
the digits before it.  The footnote block at the page's foot must survive
the marginal rules; its own exponents (1010, 6.5 x 106) are flat in the
PDF's text layer - same size, same baseline - so no geometry can raise
them, and they stay as the page's text has them."
 :budget (:mid-sentence 0))
