.PHONY: run test
.DELETE_ON_ERROR:

TGT=Main

GHCOPTS=-prof
RTSOPTS=+RTS -xc

$(TGT): build

build:
	cabal build

interp:
	cabal repl

run: build
	cabal run

include testing.mk
test: build vm
	$(MAKE) test_all

vm:
	$(MAKE) -C vm

clean:
	rm tests/ok/**/*check_*
	rm tests/ok/**/*actual_out*
	rm tests/ok/**/*.c
	rm tests/ok/**/*.exe
	rm tests/ok/**/*.bc32*


.PHONY: vm
