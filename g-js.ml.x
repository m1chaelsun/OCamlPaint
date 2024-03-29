(** This file is a "wrapper" for Graphics

- Graphics_js: the version of the library needed when we build our GUI
  programs to run in the web browser as a Javascript program.  *)

(* This flag records whether the program is running in Javascript or native
   mode.  It is needed in Gctx to configure parameters related to
   creating a new GUI window and setting the fonts (since those details differ
   between native and web mode. *)
let js_mode = true
;; include Graphics_js
