;; What this case exercises; edit by hand.  `bb corpus-accept' leaves it alone.
(:book "Aligment of Paragraphs in Bilingual Texts"
 :source "Aligment of Paragraphs in Bilingual Texts.pdf"
 :page 1
 :exercises "A paper's page-1 footnotes, both markers raised as records of their
own: the `*' note off the title's flat star and the `1' note off a
generated available.^{1} superscript.  The script-fragment merge must
attach each leading marker record to its note's first line as ^{*}/
^{1} so the block opens with its marker; the citation side must read
the title's word-attached flat star as well as the generated form.
Both notes then convert to org footnotes, [fn:] at both ends, and the
imprint lines at the very foot carry the note face unlabeled.  The
mid-sentence budget is the affiliation block's address lines; the
glued budget is CONACyT, an acronym's own lowercase-uppercase seam.
Tolerated until the heading-placement unit: the outline's
Introduction heading stacks at the page top above the paper title
(added + outline), and dropped.txt licenses the title line the
running-head rule eats - a real defect of the same unit, recorded
there as the auto-licensing fact."
 :budget (:mid-sentence 2 :glued 1)
 :tolerates (added outline))
