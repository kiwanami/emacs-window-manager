CARTON ?= carton
ECUKES ?= $(shell find elpa/ecukes-*/ecukes | tail -1)

ecukes-features:
	${CARTON} exec ${ECUKES} features

elpa:
	mkdir -p elpa
	${CARTON} install 2> elpa/install.log

clean-elpa:
	rm -rf elpa
