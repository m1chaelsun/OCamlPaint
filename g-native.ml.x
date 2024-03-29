(** This file is a "wrapper" for two different graphics libraries:

- (OCaml's) Graphics: the version of the library needed when be build
  the GUI program for native executation (i.e. linux, mac, or
  windows).  *)

(* This flag records whether the program is running in Javascript or native
   mode.  It is needed in Gctx to configure parameters related to
   creating a new GUI window and setting the fonts (since those details differ
   between native and web mode. *)
let js_mode = false
;; include Graphics

(* The native library doesn't provide the top-level event loop, so we 
   provide one here: *)
let rec loop (es:event list) (f : status -> unit) : unit =
  let status = wait_next_event es in
  (f status); loop es f
