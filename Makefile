# Run with $ EMACS=~/opt/emacs-native/bin/emacs make foo

EMACS ?= emacs
CASK ?= cask

SOURCE = $(shell ls *.el)

all: clean prepare test compile test clean

prepare:
	${CASK} install

test:
	${CASK} exec ert-runner --load ${SOURCE} test/*-test.el

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile ${SOURCE}

clean:
	${CASK} clean-elc

.PHONY: prepare test compile clean
