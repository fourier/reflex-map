# -*- Mode: makefile; -*-

CCL_PATH := ccl64
SBCL = sbcl --noinform
LW = ~/Development/lw-console

.PHONY: all
.DEFAULT_GOAL := all

all: ccl

ccl: 
	$(CCL_PATH) --load build.lisp --batch

lw:
	$(LW) -build_lw.lisp --batch

sbcl:
	$(SBCL) --load $(BUILD_SRC)


test:
	$(CCL_PATH) --load build-tests.lisp --batch
	./reflex-map-test.exe
