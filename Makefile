.PHONY: galax


LISP?=sbcl

# # Suggestion from vindarel@....org:
# build2:
# 	sbcl --non-interactive \
#              --eval '(ql:quickload :galax)' \
#              --eval '(asdf:make :galax)'
# Works, but doesn't work w/ the quicklisp and the asd stage:
# build2:
# 	$(LISP)  --non-interactive \
#         --eval '#-quicklisp (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))) (when (probe-file quicklisp-init) (load quicklisp-init)))' \
#         --load galax.asd \
#         --eval '(ql:quickload :galax)' \
#         --eval '(asdf:make :galax)'

galax:
	buildapp --output galax                \
		 --asdf-path .                 \
		 --asdf-tree ~/quicklisp/dists \
		 --load-system galax            \
		 --entry galax:main

clean:
	rm -f galax *.fasl README.html
