galax: src/*.lisp
	./build.sh

clean:
	rm -rf galax *.fasl README.html

install:
	cp galax ${HOME}/bin
