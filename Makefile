squares: squares.lisp
	sbcl --noinform --load squares.lisp >index.html
	open index.html
