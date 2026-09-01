;; What this case exercises; edit by hand.  `bb corpus-accept' leaves it alone.
(:book "NIST.SP.800-61r2"
 :source "NIST.SP.800-61r2.pdf"
 :page 51
 :exercises "The Incident Handling Checklist loses its step numbers and
welds its rows.

The table is set in Arial 0.0127 on a page whose body is Times
0.015, so every cell of the region is smaller than the body height.
`pdf-text--lane-evict-foot-blocks' reads that as the author-note
shape it was written for: the step-number lane resumes after more
than a paragraph gap - the rows between carry no number - and each
number is \"small type\", so 2. through 9. are evicted from the
region and render as bare lines under the table.  Smallness has to
be measured against the region's own cells; a footnote at the foot
of a column is smaller than the prose it shares its region with,
which is what the rule is actually about.

What stays behind welds: sub-steps 1.1-1.4 sit in from step 1 in
both lanes, and `pdf-text--lane-continuation-p' reads an indent in
every cell as a wrapped line, so five rows of the checklist arrive
as one cell.  A cell that opens with its own enumerator starts a
row, however far it is indented.

Healed, the table reads step by step, every number in its own cell.
What still rides where the page did not put it: the two bold phase
heads inside the table (\"Containment, Eradication, and Recovery\",
\"Post-Incident Activity\") run the full measure with no cell beside
them, so they land at the end of the row above instead of standing
as their own - recorded, not licensed."
 :budget (:mid-sentence 0 :glued 0))
