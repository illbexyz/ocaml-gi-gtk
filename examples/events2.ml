(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    This code is in the public domain.                                  *)
(*    You may freely copy parts of it in your application.                *)
(*                                                                        *)
(**************************************************************************)

(* $Id$ *)

open GIGtk

(* See comments in events.ml *)

let _ =
  GMain.init ();
  let window = WindowG.window () in
  window#connect#destroy ~callback:GMain.quit;

  let text = TextViewG.text_view ~width_request:200 ~height_request:100 ~packing:window#add () in
  text#connect#button_press_event ~callback:
    begin fun ev ->
      GdkEvent.Button.button (Obj.magic ev)(*XXX*) = 3 &&
      GdkEvent.get_type (Obj.magic ev)(*XXX*) = `BUTTON_PRESS &&
      begin
	let win = match text#get_window_text_view `WIDGET with
	  | None -> assert false
	  | Some w -> w
	in
	let x,y = Gdk.Window.get_pointer_location (Obj.magic win)(*XXX*) in
	let b_x,b_y = text#window_to_buffer_coords `WIDGET x y in
	(*XXX let clicked_pos = text#get_iter_at_location ~x:b_x ~y:b_y in
	Printf.printf "Position is %d.\n" clicked_pos#offset;*) prerr_endline "XXX";
	flush stdout;
	true;
      end
    end;
  window#show;
  GMain.main ()
