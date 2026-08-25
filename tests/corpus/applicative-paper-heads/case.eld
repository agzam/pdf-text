;; What this case exercises; edit by hand.  `bb corpus-accept' leaves it alone.
(:book "Applicative"
 :source "Applicative.pdf"
 :page 1
 :exercises "A paper whose text block sits low on a large page, captured red.
Two defects the reader reported, both on this page.

The furniture renders: the journal line and the folio share the
baseline 0.1494, which is outside `pdf-text-margin-band' 0.12 of the
physical page, so neither becomes a margin candidate and the
recurrence they both show book-wide never gets a say.  Detachment
misses independently - head-to-body gaps run 1.48-1.78 leadings
against `pdf-text-margin-detachment' 1.8.  The band and the
detachment want the text block, not the page.

The sections stay prose: this paper has no outline at all, and its
headings are set at the body size - Abstract and \"1 Introduction\"
measure h 0.0105 against a modal body of 0.0110, under the 0.98 gate
the numbered rule keeps - so the size synthesis cannot see them.
What sets them apart is where they sit and the air around them:
centred in a column that starts at 0.1963, two to four leadings
clear.  The display title above them is the one thing the size rule
does catch, at 1.65 body, and it must not become a section heading of
its own."
 :budget (:mid-sentence 0 :glued 0))
