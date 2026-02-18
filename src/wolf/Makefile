
MLKIT_ROOT = ~/gits/mlkit/js

SMLTOJS ?= smltojs

SRC_ROOT=$(shell pwd)

JS_PATH_FLAGS=-js_path_compress -js_path_relative_to $(MLKIT_ROOT)

.PHONY: all
all:
	$(SMLTOJS) university.mlb

.PHONY: clean
clean:
	rm -rf *~ MLB run.html *.ico

deploy: Makefile favicon.ico
	rm -rf deploy
	$(SMLTOJS) $(JS_PATH_FLAGS) -o index university.mlb
	mkdir -p deploy deploy/MLB/Js deploy/basis/MLB/Js
	cp index.html favicon.ico *.png *.gif $(MLKIT_ROOT)/prims.js deploy/
	cp MLB/Js/*.js deploy/MLB/Js/
	cp $(MLKIT_ROOT)/basis/MLB/Js/*.js deploy/basis/MLB/Js/

favicon.ico: smltojs_logo_no_text.png
	magick smltojs_logo_no_text.png \
            \( -clone 0 -resize 16x16 \) \( -clone 0 -resize 32x32 \) \
            \( -clone 0 -resize 48x48 \) \( -clone 0 -resize 64x64 \) \
            favicon.ico
