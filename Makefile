EMACS ?= emacs -nw
CASK ?= cask
all:
	${MAKE} clean
	${MAKE} compile
	${MAKE} test
	${MAKE} clean
compile:
	${CASK} exec ${EMACS} -Q -batch -L .cask -eval "(batch-byte-compile)" crowi.el
test:
	${CASK} exec ${EMACS} -Q -l tests/run-test.el
clean:
	rm -f *.elc
	rm -f tests/*.elc
.PHONY: all compile test clean
