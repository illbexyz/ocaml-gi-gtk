(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    This code is in the public domain.                                  *)
(*    You may freely copy parts of it in your application.                *)
(*                                                                        *)
(**************************************************************************)

(* $Id$ *)

open GIGtk

let main () =
  GMain.init ();
  let w = WindowG.window ~border_width:2 ~show:true () in
  w#realize;
  let hbox = HBoxG.h_box ~packing:w#add () in
  LabelG.label ~label:"hello <b>world</b>" ~use_markup:true ~packing:hbox#add ();
  w#connect#destroy ~callback:GMain.quit;
  GMain.main ()

let () = main ()
