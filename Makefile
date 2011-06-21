
PROJECT := s-protobuf
VERSION := $(shell grep  Version control | cut -d ' ' -f 2)

SRC_INSTALL_DIR := ./debian/usr/share/common-lisp/source/$(PROJECT)
SYS_INSTALL_DIR := ./debian/usr/share/common-lisp/systems

deb:
	mkdir -p debian/DEBIAN
	mkdir -p $(SRC_INSTALL_DIR)
	mkdir -p $(SYS_INSTALL_DIR)
	cp control debian/DEBIAN
	rsync --delete -r \
		--include="**/*.lisp" --include="**/*.asd"  --exclude="*" \
		src/ $(SRC_INSTALL_DIR)
	(cd $(SYS_INSTALL_DIR) && find ../source/$(PROJECT) -name '*.asd' \
		-exec ln -fvs '{}' ';')
	fakeroot dpkg-deb --build ./debian $(PROJECT)_$(VERSION)-1.deb


clean:
	rm -rf debian *.deb src/*.fasl
