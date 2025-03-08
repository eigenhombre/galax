FROM debian:bookworm-slim

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        git \
        libssl3 \
        make \
        sbcl

RUN curl -o quicklisp.lisp 'https://beta.quicklisp.org/quicklisp.lisp'

# Clone nominal and trivialtests as local projects (not in Quicklisp yet)
RUN git clone --depth=1 https://github.com/eigenhombre/nominal.git /nominal
RUN git clone --depth=1 https://github.com/eigenhombre/trivialtests.git /trivialtests

COPY src src
COPY *.asd .

RUN sbcl --non-interactive \
    --load quicklisp.lisp \
    --disable-debugger \
    --eval '(quicklisp-quickstart:install)' \
    --eval '(ql:quickload :asdf)' \
    --eval '(require :asdf)' \
    --eval '(pushnew (truename "/") asdf:*central-registry*)' \
    --eval '(pushnew (truename "/src/") asdf:*central-registry*)' \
    --eval '(pushnew (truename "/nominal/") asdf:*central-registry*)' \
    --eval '(pushnew (truename "/trivialtests/") asdf:*central-registry*)' \
    --eval '(ql:register-local-projects)' \
    --eval '(ql:quickload :galax)' \
    --eval '(progn (sb-ext:disable-debugger) (sb-ext:save-lisp-and-die "galax" :toplevel #'"'"'galax:main :executable t))'

CMD ["/galax"]
