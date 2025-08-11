squares: squares.lisp
	sbcl --noinform --load squares.lisp >index.html
	open index.html

pentagons: pentagons.lisp
	sbcl --noinform --load pentagons.lisp >index.html
	open index.html
