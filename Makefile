galax: src/*.lisp *.asd
	buildapp --output galax                \
		 --asdf-path .                 \
		 --asdf-tree ~/quicklisp/dists \
		 --load-system galax            \
		 --entry galax:main

clean:
	rm -f galax *.fasl README.html
