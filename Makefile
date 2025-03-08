galax: src/*.lisp
	./build.sh

clean:
	rm -rf galax *.fasl README.html

docker:
	docker build -t eigenhombre/galax .

# N.B.: `docker login` first:
dockerpush:
	docker push -a eigenhombre/galax

install:
	cp galax ${HOME}/bin
