;; What this case exercises; edit by hand.  `bb corpus-accept' leaves it alone.
(:book "2017saner-eventsourcing"
 :source "2017saner-eventsourcing.pdf"
 :page 9
 :exercises "A two-column page whose lane run closed six times on overfull lines.

The gutter is 3.4 spaces wide and a handful of the left column's
justified lines run half a space overfull - x1 0.4931 against the
column's 0.4902 - each on a baseline the right column also sets.
Such a pair's gap reads 2.9 spaces against the 3-space gutter
measure, so the row is neither clean nor a single cell, and the run
closed there: six regions down one page, each unfolded left-then-
right on its own, the reader getting the columns braided six ways.
Both cells of such a row nest inside the run's lanes and neither
crosses the channel, so the row extends the run without a vote, as a
single nested cell always did; the page is one region and reads left
column whole, then right.

The foot stacks two notes in the body face, a URL each, that arrive
as one block; joined as one definition the second rode inside the
first with its marker still on.  A block stacking several
marker-opened lines the body cites every one of gives a definition
per line: [fn:9-1] and [fn:9-2] at their markers, the URLs as
definitions at the foot."
 :budget (:mid-sentence 0 :glued 0))
