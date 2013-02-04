CARTON ?= carton
ECUKES ?= $(shell find elpa/ecukes-*/ecukes | tail -1)

ecukes-features: elpa
	${CARTON} exec ${ECUKES} features

elpa:
	mkdir -p elpa
	${CARTON} install 2> elpa/install.log

clean-elpa:
	rm -rf elpa

clean-elc:
	rm -f *.elc

clean: clean-elpa clean-elc
