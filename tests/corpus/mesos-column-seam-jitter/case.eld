;; What this case exercises; edit by hand.  `bb corpus-accept' leaves it alone.
(:book "mesos_tech_report"
 :source "mesos_tech_report.pdf"
 :page 5
 :exercises "A two-column page whose region split in two at a hairline.

Every justified line of the left column ends at x1 0.48529, and one
- \"be soft state, i.e., the master can reconstruct completely\" -
ends at 0.48530, on a baseline the right column leaves empty between
two bullet items.  Read with no tolerance, that line crossed the
gutter and closed the lane run; the page unfolded as two regions,
left-then-right twice over, and the reader got 3.6's opening
paragraph, then 4.1 and its bullets from the right column, then the
rest of 3.6 and section 4 - which folded looked like 3.6 truncated
and 4 empty.  A cell crosses a gutter only when its ink spans the
channel past `pdf-text-lane-overrun'; the page is one region, and
3.6, 4, 4.1 and 4.2 read in order.

The orphan measure was written for this shape: the mid-sentence pair
never sees the two halves of the paragraph side by side, but the
tail opens lowercase after a closed line.

The glued budget is ZooKeeper, the page's own word.  Residuals the
page still shows, no invariant firing: the first and third metric
bullets keep their raw • where the second reads as a list item, and
\"Framework ramp-up time:\" is served apart from \"time it takes a
new\" on one baseline - the italic label ends on a colon, so the
split-line clause test refuses the join and the item renders as a
lone label over its text."
 :budget (:mid-sentence 0 :glued 1))
