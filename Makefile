EMACS ?= emacs
CARTON ?= carton
ECUKES ?= $(shell find elpa/ecukes-*/ecukes | tail -1)
ECUKES_OPTS ?= --tags ~@known

test: unit-tests ecukes-features

unit-tests: elpa
	${CARTON} exec ${EMACS} -Q -batch -L . -l test-e2wm-pst-class.el \
		-f ert-run-tests-batch-and-exit

ecukes-features: elpa
	${CARTON} exec ${ECUKES} ${ECUKES_OPTS} features

elpa:
	mkdir -p elpa
	${CARTON} install 2> elpa/install.log

clean-elpa:
	rm -rf elpa

clean-elc:
	rm -f *.elc

clean: clean-elpa clean-elc

print-deps:
	${EMACS} --version
	@echo CARTON=${CARTON}
	@echo ECUKES=${ECUKES}

travis-ci: print-deps test
