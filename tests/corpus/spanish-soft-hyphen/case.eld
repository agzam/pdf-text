;; What this case exercises; edit by hand.  `bb corpus-accept' leaves it alone.
(:book "A new reference grammar of modern Spanish (2019)"
 :source "Espanol/A new reference grammar of modern Spanish (2019).pdf"
 :page 19
 :exercises "Characters the page never prints, and a dense book's running head.
The 1.2.9 heading wraps with no hyphen, but the text layer carries
the discretionary U+00AD at the head of the continuation line and a
BELL inside the heading; en spaces (U+2002) pad headings and table
rows all over.  All of it is display junk in Emacs and none of it is
ink: soft hyphens strip everywhere but line ends, C0 goes wherever it
sits, the typographic space family reads as plain spaces.  The head
(`6 Gender of nouns', folio merged into the line) stands 1.9 leadings
off the body - under the old two-leading detachment - and is the
declared drop.  The multicolumn pass reads the mid-page gender table
as lanes - feminine forms, masculine forms, glosses, each line its
own - and renders the bottom common-gender list as a two-lane table:
its ragged 1:1 rows read as pairs, though the list is really 2-up
alphabetical, a recorded residual with self-contained cells.  The
mid-sentence budget is the lane item lines, the listings class."
 :budget (:mid-sentence 21))
