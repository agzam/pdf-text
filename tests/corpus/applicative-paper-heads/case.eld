;; What this case exercises; edit by hand.  `bb corpus-accept' leaves it alone.
(:book "Applicative"
 :source "Applicative.pdf"
 :page 1
 :exercises "A paper whose text block sits low on a large page.

The furniture goes by the text area: the journal line and the folio
share the baseline 0.1494, outside `pdf-text-margin-band' 0.12 of the
physical page and 1.48-1.78 leadings off the body - under the
detachment - so neither the band nor the detachment ever sees them.
The profile's :text-top does: both sit above the modal body start,
the folio is a bare page marker there, and the journal line shares
its baseline.  Captured red for exactly this furniture from daaab05
to the Round 12 unit 4 fix.

The sections carry the other half of that unit: this paper has no
outline, and Abstract and \"1 Introduction\" are set bold at 0.95 of
the modal body - under every size gate - so only the bold lead face
earns them their org level, with the whole book's pages as the
cluster's support.  The display title above them stays prose: its
face heads one page and no cluster survives on that.

The authors' small-caps lines and the sans code fragments must stay
prose, and the code keeps the restored operator spaces
(\"[IO a ] → IO [a ]\") the run-gap rule reads out of the em size."
 :budget (:mid-sentence 0 :glued 0))
