(** The main paint application *)

;; open Gctx
;; open Widget

(******************************************)
(**    SHAPES, MODES, and PROGRAM STATE   *)
(******************************************)

(** A location in the paint_canvas widget *)
type point = position (* from Gctx *)

(** The shapes that are visible in the paint canvas -- these make up the
    picture that the user has drawn, as well as any other "visible" elements
    that must show up in the canvas area (e.g. a "selection rectangle"). At
    the start of the homework, the only available shape is a line.  *)
type shape = 
  | Line of { color: color; p1: point; p2: point; thickness: thickness }
  | Points of { color: Gctx.color; points: point list; thickness: thickness }
  | Ellipse of 
    { color: Gctx.color; center: point; r1: int; r2: int; thickness: thickness}

(** These are the possible interaction modes that the paint program might be
    in. Some interactions require two modes. For example, the GUI might
    recognize the first mouse click as starting a line and a second mouse
    click as finishing the line.

    To start out, there are only two modes:

    - LineStartMode means the paint program is waiting for the user to make
        the first click to start a line.

    - LineEndMode means that the paint program is waiting for the user's
        second click. The point associated with this mode stores the location
        of the user's first mouse click.  *)
type mode = 
  | LineStartMode
  | LineEndMode of point
  | PointMode
  | EllipseStartMode
  | EllipseEndMode of point

(** The state of the paint program. *)
type state = {
  (** The sequence of all shapes drawn by the user, in order from
      least recent (the head) to most recent (the tail). *)
  shapes : shape Deque.deque;

  (** The input mode the Paint program is in. *)
  mutable mode : mode;

  (** The currently selected pen color. *)
  mutable color : color;

  (* TODO: You will need to add new state for Tasks 2, 3, 5, and *)
  (* possibly 6 *) 
  mutable preview : shape option;
  
  (** The currently selected line thickness *)
  mutable thickness : thickness;
}

(** Initial values of the program state. *)
let paint : state = {
  shapes = Deque.create ();
  mode = LineStartMode;
  color = black;
  preview = None;
  thickness = thick_lines;
}

(** This function creates a graphics context with the appropriate
    pen color.
*)
let with_params (g: gctx) (c: color) (t: thickness) : gctx =
  let g = with_color g c in
  let g = with_thickness g t in
  g

(*********************************)
(**    MAIN CANVAS REPAINTING    *)
(*********************************)

(** The paint_canvas repaint function.

    This function iterates through all the drawn shapes (in order of least
    recent to most recent so that they are layered on top of one another
    correctly) and uses the Gctx.draw_xyz functions to display them on the
    canvas.  *)
let repaint (g: gctx) : unit =
  let draw_shape (s: shape) : unit =
    begin match s with
      | Line l -> draw_line (with_params g l.color l.thickness) l.p1 l.p2
      | Points p -> draw_points (with_params g p.color p.thickness) p.points
      | Ellipse e -> 
        draw_ellipse (with_params g e.color e.thickness) e.center e.r1 e.r2
    end in
   let draw_preview (ps: shape option) : unit =
     begin match ps with 
      | None -> ()
      | Some x -> draw_shape x
     end in 
  Deque.iterate draw_shape paint.shapes; draw_preview paint.preview

(** Create the actual paint_canvas widget and its associated
    notifier_controller . *)
let ((paint_canvas : widget), (paint_canvas_controller : notifier_controller)) =
  canvas (600, 350) repaint

(************************************)
(**  PAINT CANVAS EVENT HANDLER     *)
(************************************)

(** The paint_action function processes all events that occur
    in the canvas region. *)
let paint_action (gc:gctx) (event:event) : unit =
  let p  = event_pos event gc in  (* mouse position *)
  begin match (event_type event) with
    | MouseDown ->
      (* This case occurs when the mouse has been clicked in the
         canvas, but before the button has been released. How we
         process the event depends on the current mode of the paint
         canvas.  *)
      (begin match paint.mode with 
         | LineStartMode ->
           (* The paint_canvas was waiting for the first click of a line,
              so change it to LineEndMode, recording the starting point of
              the line. *)
           paint.mode <- LineEndMode p
         | LineEndMode p1 ->
           (* The paint_canvas was waiting for the second click of a line,
              so create the line and add it to the deque of shapes. Go back
              to waiting for the first click. *)
           ()
         | PointMode ->
         (* The paint_canvas was waiting for the first click of a point,
              so it begins to create the type point, recording the positions
              of the point(s) to be drawn. *)
           paint.preview <- Some (Points {color=paint.color; points=[p]; 
           thickness=paint.thickness})
         
         | EllipseStartMode -> 
         (* The paint_canvas was waiting for the first click of an ellipse,
              so change it to EllipseEndMode, recording the starting point of
              the ellipse. *)
         paint.mode <- EllipseEndMode p
         
         | EllipseEndMode p1 -> 
         (* The paint_canvas was waiting for the second click of an ellipse,
              so create the ellipse and add it to the deque of shapes. Go back
              to waiting for the first click. *)
         ()
       end)
         
    | MouseDrag ->
      (* In this case, the mouse has been clicked, and it's being dragged
         with the button down. *)
      (begin match paint.mode with
          | LineStartMode ->
            paint.preview <- None
          | LineEndMode p1 ->
            paint.preview 
            <- Some (Line {color=paint.color; p1=p1;p2=p; 
            thickness=paint.thickness})
          | PointMode ->
            let points_list = 
              begin match paint.preview with
                | Some (Points ps) -> ps.points
                | _ -> []
              end in 
            paint.preview <- Some (Points {color = paint.color; 
                                            points = points_list @ [p]; 
                                            thickness = paint.thickness})
          | EllipseStartMode -> paint.preview <- None
          | EllipseEndMode p1 -> 
            let (w, x), (y, z) = p1, p in
            let center = ((y + w) / 2 , (x + z) / 2) in
            let (w, _), (y, _ ) = p1, p in let r1 = if (w - y) > 0 
                                                    then (w - y) / 2 
                                                    else (y - w) / 2 in
            let (_, x), (_, z) = p1, p in
            let r2 = if (x - z) > 0 then (x - z) / 2 
                     else (z - x) / 2 in 
            paint.preview <- Some (Ellipse {color = paint.color; 
                                              center = center;
                                              r1 = r1; r2 = r2;
                                              thickness = paint.thickness})
            end)
    | MouseUp ->
      (* In this case there was a mouse button release event. *)
      (begin match paint.mode with
          | LineStartMode ->
            paint.preview <- None
          | LineEndMode p1 ->
            Deque.insert_tail
             (Line {color=paint.color; p1=p1; p2=p; thickness=paint.thickness}) 
             paint.shapes;
             paint.mode <- LineStartMode;
             paint.preview <- None
          | PointMode ->
            begin match paint.preview with
              | Some (Points ps) -> Deque.insert_tail
                                    (Points {color=paint.color; points=ps.points
                                    @ [p]; 
                                    thickness = paint.thickness})
                                    paint.shapes;
                                    paint.preview <- None
              | _ -> ()
            end
          | EllipseStartMode -> paint.preview <- None
          | EllipseEndMode p1 -> 
            let (w, x), (y, z) = p1, p in
            let center = ((y + w) / 2 , (x + z) / 2) in
            let (w, _), (y, _ ) = p1, p in let r1 = if (w - y) > 0 
                                   then (w - y) / 2 
                                   else (y - w) / 2 in
            let (_, x), (_, z) = p1, p in
            let r2 = if (x - z) > 0 then (x - z) / 2 
                     else (z - x) / 2 in Deque.insert_tail (Ellipse {
                     color = paint.color;
                                        center = center;
                                        r1 = r1;
                                        r2 = r2;
                                        thickness = paint.thickness}) 
                                        paint.shapes;
                                        paint.mode <- EllipseStartMode;
                                        paint.preview <- None
                                            
            end)
    | _ -> ()
    (* This catches the MouseMove event (where the user moved the mouse over
       the canvas without pushing any buttons) and the KeyPress event (where
       the user typed a key when the mouse was over the canvas). *)
  end

(** Add the paint_action function as a listener to the paint_canvas *)
;; paint_canvas_controller.add_event_listener paint_action

(**************************************)
(** TOOLBARS AND PAINT PROGRAM LAYOUT *)
(**************************************)

(** This part of the program creates the other widgets for the
    paint program -- the buttons, color selectors, etc., and
    lays them out in the top - level window. *)

(** Create the Undo button *)
let (w_undo, lc_undo, nc_undo) = button "Undo"

(** This function runs when the Undo button is clicked.
    It simply removes the last shape from the shapes deque. *)
let undo () : unit =
  if Deque.is_empty paint.shapes then () else
    ignore (Deque.remove_tail paint.shapes);
  paint.preview <- None;
  begin match paint.mode with
    | LineStartMode -> ()
    | LineEndMode _ -> paint.mode <- LineStartMode
    | PointMode -> ()
    | EllipseStartMode -> ()
    | EllipseEndMode _ -> paint.mode <- EllipseStartMode
  end

;; nc_undo.add_event_listener (mouseclick_listener undo)

(** A spacer widget *)
let spacer : widget = space (10,10)

(** Create the Line button *)
let (w_line, lc_line, nc_line) = button "Line"
;; nc_line.add_event_listener (mouseclick_listener (fun () ->
      paint.mode <- LineStartMode ))

(** Create the Point button *)
let (w_point, lc_point, nc_point) = button "Point"
;; nc_point.add_event_listener (mouseclick_listener (fun () ->
      paint.mode <- PointMode ))

(** Create the Ellipse button *)
let (w_ellipse, lc_ellipse, nc_ellipse) = button "Ellipse"
;; nc_ellipse.add_event_listener (mouseclick_listener (fun () ->
      paint.mode <- EllipseStartMode))

(** Create the Thin Lines button *)
let thickness_toggle (g: gctx) : widget = 
  let (w, lc) = checkbox false "Thin Lines" in
  lc.add_change_listener (fun a -> if a
                                   then paint.thickness <- thin_lines
                                   else paint.thickness <- thick_lines);
  w

(** Create the Thickness Slider *)
let thickness_sliders : widget = 
  let (w, vc) = thickness_slider 30 "Thickness Slider" in
  vc.add_change_listener (fun a -> paint.thickness <- {t = a});
  w

(** The mode toolbar, initially containing just the Undo button. *)
let mode_toolbar : widget = hlist [w_undo; spacer; w_line; spacer; w_point; 
                                   spacer; w_ellipse; spacer; thickness_toggle 
                                   top_level]

(* The color selection toolbar. *)
(* This toolbar contains an indicator for the currently selected color
   and some buttons for changing it. Both the indicator and the buttons
   are small square widgets built from this higher-order function. *)
(** Create a widget that displays itself as colored square with the given
    width and color specified by the [get_color] function. *)
let colored_square (width:int) (get_color:unit -> color)
  : widget * notifier_controller =
  let repaint_square (gc:gctx) =
    let c = get_color () in
    fill_rect (with_color gc c) (0, 0) (width-1, width-1) in
  canvas (width,width) repaint_square

(** The color_indicator repaints itself with the currently selected
    color of the paint application. *)
let color_indicator =
  let indicator,_ = colored_square 24 (fun () -> paint.color) in
  let lab, _ = label "Current Color" in
  border (hpair lab indicator)
  

(** color_buttons repaint themselves with whatever color they were created
    with. They are also installed with a mouseclick listener
    that changes the selected color of the paint app to their color. *)
let color_button (c: color) : widget =
  let w,nc = colored_square 10 (fun () -> c) in
  nc.add_event_listener (mouseclick_listener (fun () ->
      paint.color <- c ));
  w

(** The color selection toolbar. Contains the color indicator and
    buttons for several different colors. *)
let color_toolbar : widget =
  hlist [color_button black; spacer;
         color_button white; spacer;
         color_button red; spacer;
         color_button green; spacer;
         color_button blue; spacer;
         color_button yellow; spacer;
         color_button cyan; spacer;
         color_button magenta; spacer;
         color_indicator]

(** The top-level paint program widget: a combination of the
    mode_toolbar, the color_toolbar and the paint_canvas widgets.
*)
let paint_widget =
  vlist [paint_canvas; spacer;
         mode_toolbar; spacer;
         color_toolbar; spacer;
         thickness_sliders]

(**************************************)
(**      Start the application        *)
(**************************************)

(** Run the event loop to process user events. *)
;; Eventloop.run paint_widget
