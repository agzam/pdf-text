;; What this case exercises; edit by hand.  `bb corpus-accept' leaves it alone.
(:book "fpio - functional programming and IO - Gordon 1994"
 :source "/Users/ryl/SyncMobile/Papers/fpio - functional programming and IO - Gordon 1994.pdf"
 :page 1
 :exercises "The Type 3 kern shatter: every chunk its own record, healed wordwise.

A dvips-era Type 3 bitmap-font document serves every kern chunk as
its own record on one baseline - \"Univ\" | \"ersit\" | \"y\",
\"Cam\" | \"bridge\" - with gaps at or under zero (\"Univ\" ends x1
0.5765, \"ersit\" opens x0 0.5759) where word gaps run half a modal
space and up.  The em sizes are garbage (0.0001 against glyph
heights 0.0168-0.0314, the Type 3 FontMatrix scale reaching MuPDF
raw), the fonts bare DVI names (T1, T2, T3), and the shipped
ToUnicode CMaps cover only the letters, so MuPDF alone reads the
apostrophe of \"King's\" and the slash of \"Input/Output\" as
U+FFFD and the year 1992 as raw byte codes.  The walker completes
each Type 3 font's CMap from the font's own glyph names - dvips
names a glyph by its character (/' /1 /#2F) - which is what this
golden's punctuation, digits and ligatures prove.

Two branches of the split-line join heal the page, both measured in
the line's own glyph height because gaps scale with the font.  The
kern branch makes words: same baseline, same font, same size, gap at
or under zero - or a hair over with a lowercase continuation, the
clause branch's own confirming signal - rejoined with no space.  The
shatter branch makes lines: under the Type 3 class the profile's :em
declares, word-gap neighbours within a height rejoin with the space
the page set, whatever case they open on, so the title reads whole
and the dissertation line flows as one paragraph."
 :budget (:mid-sentence 0))
