EMACS ?= emacs
CASK ?= cask
ECUKES ?= $(shell find .cask/*/elpa/ecukes-*/bin/ecukes | tail -1)
ECUKES_OPTS ?= --tags ~@known --no-win

test: unit-tests ecukes-features

unit-tests: elpa
	${CASK} exec ${EMACS} -Q -batch -L . -l test-e2wm-pst-class.el \
		-f ert-run-tests-batch-and-exit

ecukes-features: elpa
	${CASK} exec ${ECUKES} ${ECUKES_OPTS} features

elpa:
	mkdir -p elpa
	${CASK} install 2> elpa/install.log

clean-elpa:
	rm -rf elpa

clean-elc:
	rm -f *.elc

clean: clean-elpa clean-elc

print-deps:
	${EMACS} --version
	@echo CASK=${CASK}
	@echo ECUKES=${ECUKES}

travis-ci: print-deps test
