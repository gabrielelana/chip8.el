# Run with $ EMACS=~/opt/emacs-native/bin/emacs make foo

EMACS ?= emacs
CASK ?= cask

SOURCE = $(shell ls *.el)

all: clean prepare test compile test clean

prepare:
	${CASK} install

test:
	${CASK} emacs --batch -L . -L test \
		-l example-test \
		-f ert-run-tests-batch

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile ${SOURCE}

clean:
	${CASK} clean-elc

.PHONY: prepare test compile clean
