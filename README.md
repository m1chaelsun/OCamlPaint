# paint

**INSTRUCTIONS:** Run paint.ml

**FILE DESCRIPTIONS:**

- Gctx (graphics context) module: gctx.ml, gctx.mli
- Widget module: widget.ml, widget.mli
- Eventloop module: eventloop.ml, eventloop.mli
- Deque implementation: deque.ml, deque.mli
- Unit tests: widgetTest.ml (uses the Assert module)
- Paint application: paint.ml
- g-native.ml.x and g-js.ml.x: wrappers for the Graphics module (see below)

The modules gctx, widget, and eventloop are the main components of the GUI toolkit. The gdemo and lightbulb programs test some of the functions you must add to the gctx and widget modules. Finally, paint is the paint application itself.

gdemo.ml and lightbulb.ml are provided to test the some of the functions found within the gctx and widget modules.

This project heavily uses OCaml's Graphics module, available at https://ocaml.github.io/graphics/graphics/Graphics/index.html#point-and-line-drawing
