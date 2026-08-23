;; What this case exercises; edit by hand.  `bb corpus-accept' leaves it alone.
(:book "Blandy J., Orendorff J. - Programming Rust - 2018"
 :source "Rust/Blandy J., Orendorff J. - Programming Rust - 2018.pdf"
 :page 43
 :exercises "A page-long code listing rendered verbatim.  The post_gcd function
keeps one source line per rendered line, its nesting restored in the
listing's own space widths, and the blank lines the code itself sets.
Every lone closing brace survives: capturing this book caught the
recurring-form rule reading `}' - a detached narrow candidate at
enough page bottoms - as a running head and eating every listing's
braces book-wide.  A brace has no letters or digits, so the survival
stream never saw the loss; only a reader could.  The listing opens
mid-function because the block starts overleaf - the first line
renders flush, part of the known cross-page residual.  The budget is
the listing's own consecutive lowercase code lines."
 :budget (:mid-sentence 18))
