
MLKIT_ROOT = $(shell dirname $$(which mlkit))/../lib/smltojs

SMLTOJS ?= smltojs

SRC_ROOT=$(shell pwd)

JS_PATH_FLAGS=-js_path_compress -js_path_relative_to $(MLKIT_ROOT)

.PHONY: all
all:
	$(SMLTOJS) university.mlb

lib: sml.pkg
	smlpkg sync

.PHONY: clean
clean:
	rm -rf *~ MLB run.html *.ico deploy data/*~ data/*/*~

.PHONY: realclean
realclean:
	$(MAKE) clean
	rm -rf lib

deploy: university.mlb university.sml Makefile favicon.ico lib
	rm -rf deploy
	$(SMLTOJS) $(JS_PATH_FLAGS) -o index $<
	mkdir -p deploy deploy/MLB/Js deploy/basis/MLB/Js
	mkdir -p deploy/lib/github.com/diku-dk/sml-setmap/map/MLB/Js
	mkdir -p deploy/lib/github.com/diku-dk/sml-json/MLB/Js
	cp index.html favicon.ico *.png *.gif $(MLKIT_ROOT)/prims.js deploy/
	cp MLB/Js/*.js deploy/MLB/Js/
	cp lib/github.com/diku-dk/sml-setmap/map/MLB/Js/*.js deploy/lib/github.com/diku-dk/sml-setmap/map/MLB/Js/
	cp lib/github.com/diku-dk/sml-json/MLB/Js/*.js deploy/lib/github.com/diku-dk/sml-json/MLB/Js/
	cp $(MLKIT_ROOT)/basis/MLB/Js/*.js deploy/basis/MLB/Js/
	cp -a data deploy/

.PHONY: serve
serve: deploy
	(cd deploy && python3 -m http.server 8000)

favicon.ico: smltojs_logo_no_text.png
	magick smltojs_logo_no_text.png \
            \( -clone 0 -resize 16x16 \) \( -clone 0 -resize 32x32 \) \
            \( -clone 0 -resize 48x48 \) \( -clone 0 -resize 64x64 \) \
            favicon.ico

.PHONY: dodeploy
dodeploy: deploy
	rm -rf ../../wolfgame
	cp -pa deploy ../../wolfgame
