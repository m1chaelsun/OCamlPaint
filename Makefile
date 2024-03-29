SUBMIT  := gctx.ml gctx.mli widget.ml widget.mli paint.ml 

OCB	:= ocamlbuild -r -tag debug -use-ocamlfind -pkg unix -pkg graphics -pkg js_of_ocaml -pkg js_of_ocaml-lwt -pkg js_of_ocaml-lwt.graphics

DEPENDS := \
	assert.ml \
	assert.mli \
	deque.ml \
	deque.mli \
	eventloop.ml \
	eventloop.mli \
	g-js.ml.x \
	g-native.ml.x \
	gctx.ml \
	gctx.mli \
	gdemo.html \
	gdemo.ml \
	gdemo.mli \
	lightbulb.html \
	lightbulb.ml \
	lightbulb.mli \
	paint.html \
	paint.ml \
	paint.mli \
	widget.ml \
	widget.mli \
	widgetTest.ml \
	widgetTest.mli \
	notifierdemo.html \
	notifierdemo.mli \
	notifierdemo.ml

NAME := OCamlPaint
ts := $(shell /bin/date "+%Y-%m-%d-%H:%M:%S")

ZIPNAME := $(NAME)-submit($(ts)).zip

.PHONY: all  clean zip

all: gdemo.js lightbulb.js paint.js notifierdemo.js widgetTest.native 

%.js: %.byte
	js_of_ocaml  $<

%.native: $(DEPENDS)
	rm -rf g.ml
	cp g-native.ml.x g.ml
	$(OCB) $@
	rm -rf g.ml

%.byte: $(DEPENDS)
	rm -rf g.ml
	cp g-js.ml.x g.ml
	$(OCB) $@
	rm -rf g.ml

run% : %.native $(DEPENDS)
	OCAMLRUNPARAM=b ./$<

zip: $(SUBMIT)
	zip '$(ZIPNAME)' $(SUBMIT)

clean:
	ocamlbuild -clean
	rm -f *.js
	rm -rf *.zip
