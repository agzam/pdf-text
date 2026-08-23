;; What this case exercises; edit by hand.  `bb corpus-accept' leaves it alone.
(:book "[Data.Science.for.Business(2013.7)].Foster.Provost"
 :source "Language Agnostic/[Data.Science.for.Business(2013.7)].Foster.Provost.pdf"
 :page 19
 :exercises "A numbered footnote under its marker.  The body cites 2005.^{1} and
the answering block sits at the page's foot in smaller type, opening
with a flat `1.' - the marker digit is the page's own text, so the
org label must give it back to the reader and to the survival stream.
The body's last line above the block ends on a wrap hyphen (prob-),
which must not pull the smaller-type block into the paragraph: the
size break outranks the hyphen join.  The paragraph itself continues
overleaf and stays split, the known cross-page residual."
 :budget (:mid-sentence 0))
