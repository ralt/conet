QL_LOCAL=.quicklocal
LOCAL_OPTS=--noinform --noprint --disable-debugger --no-sysinit --no-userinit
QL_OPTS=--load $(QL_LOCAL)/setup.lisp

RM=rm -rf

BIN=../bin/conet-dhcp-server
SRC:=$(wildcard *.lisp) $(wildcard *.asd)

SYSTEM_NAME=conet-dhcp-server
SYSTEM_ENTRY=main

all: $(BIN)

deps: $(QL_LOCAL)/setup.lisp
	sbcl $(LOCAL_OPTS) $(QL_OPTS) \
		--eval '(push "$(PWD)/" asdf:*central-registry*)' \
		--eval '(ql:quickload :$(SYSTEM_NAME))' \
		--eval '(quit)'
	@touch $@

$(BIN): $(SRC) deps
	@buildapp \
		--asdf-tree $(QL_LOCAL)/dists \
		--asdf-path . \
		--load-system $(SYSTEM_NAME) \
		--eval '(setf *debugger-hook* (lambda (c h) (declare (ignore h)) (format t "~A~%" c) (uiop:quit -1)))' \
		--compress-core \
		--output $(BIN) --entry $(SYSTEM_NAME):$(SYSTEM_ENTRY)

$(QL_LOCAL)/setup.lisp: quicklisp.lisp
	@sbcl --noinform --noprint --disable-debugger --no-sysinit --no-userinit \
		--load quicklisp.lisp \
		--eval '(quicklisp-quickstart:install :path "$(QL_LOCAL)")' \
		--eval '(quit)'

quicklisp.lisp:
	@wget https://beta.quicklisp.org/quicklisp.lisp
	@echo '4a7a5c2aebe0716417047854267397e24a44d0cce096127411e9ce9ccfeb2c17 *quicklisp.lisp' | shasum -c -

clean:
	-$(RM) deps $(QL_LOCAL) $(BIN) quicklisp.lisp

.PHONY: clean
