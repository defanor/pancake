VERSION := $(shell grep '^version:' pancake.cabal | cut -d ' '  -f14- -)
NAME ?= "pancake-${VERSION}"

deb:
	cabal configure --prefix=/usr
	cabal build
	cabal copy --destdir=deb
	dpkg-deb --build deb/ ${NAME}.deb

bin:
	cabal configure --prefix=/usr/local
	cabal build
	cabal copy --destdir=${NAME}-bin
	tar -cz -f ${NAME}-bin.tgz -P ${NAME}-bin

clean:
	rm -rf ${NAME}.deb ${NAME}-bin.tgz ${NAME}-bin/ deb/usr/ dist/

.PHONY: deb bin clean
