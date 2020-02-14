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
  let window = WindowG.window ~title: "radio buttons" ~border_width: 0 () in
  window#connect#destroy ~callback:GMain.quit;

  let box1 = VBoxG.v_box ~packing: window#add () in

  let box2 = VBoxG.v_box ~spacing:10 ~border_width: 10 ~packing: box1#add () in

  let button1 = RadioButtonG.radio_button ~label:"button1" ~packing: box2#add () in
  button1#connect#clicked ~callback:(fun () -> prerr_endline "button1");

(*XXX
  let button2 = RadioButtonG.radio_button ~group:button1#group ~label:"button2"
      ~active:true ~packing: box2#add () in
  button2#connect#clicked ~callback:(fun () -> prerr_endline "button2");

  let button3 = RadioButtonG.radio_button
      ~group:button1#group ~label:"button3" ~packing: box2#add () in
  button3#connect#clicked ~callback:(fun () -> prerr_endline "button3");
*)

  let separator =
    HSeparatorG.h_separator ~packing: box1#add () in

  let box3 = VBoxG.v_box ~spacing: 10 ~border_width: 10
      ~packing: box1#add () in

  let button = ButtonG.button ~label: "close" ~packing: box3#add () in
  button#connect#clicked ~callback:GMain.quit;
  button#misc#grab_default ();

  window#misc#show ();

  GMain.main ()

let _ = main ()
