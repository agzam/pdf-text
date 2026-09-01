;; What this case exercises; edit by hand.  `bb corpus-accept' leaves it alone.
(:book "testing_and_staying_sane"
 :source "testing_and_staying_sane.pdf"
 :page 12
 :exercises "A CMTT9 listing the lane machinery reads as a table.

The ticket_test snippet's pattern-match lines arrive as aligned
record pairs - \"1\"/\"2\"/\"3\" beside \"= take_ticket (),\" - a
clean gutter over 3+1 rows, and the lane pass emits
\"| 1 | = take_ticket (), |\" rows out of code while the full-width
lines between stay outside the region.  The cell-cv decline cannot
see it: the left lane is single digits, too short to vote.  The font
name can - CMTT9 names itself - and a lane region set in a
monospace-named font is a listing, not a table.

The parallel-test diagram lower down is the extractor's weld -
\"take ticket() take ticket()\" served as ONE record, alignment
spaces collapsed at record birth - and stays a recorded limit: no
lane pass can see a gutter inside one record.  It renders as
verbatim mono lines, structure unmarked.

Budget: QuickCheck is the prose's own camelCase, and the rejoined
listing's \"take_ticket (),\" / \"ok = reset (),\" seam reads as one
lowercase pair."
 :budget (:mid-sentence 1 :glued 2))
