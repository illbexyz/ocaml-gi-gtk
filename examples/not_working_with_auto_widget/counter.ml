(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    This code is in the public domain.                                  *)
(*    You may freely copy parts of it in your application.                *)
(*                                                                        *)
(**************************************************************************)

(* $Id$ *)

open GIGtk

let _ = GMain.init()

let w = WindowG.window ()

let vb = VBoxG.v_box ~packing:w#add ()

let lbl = LabelG.label ~packing:vb#add ()

let hb = HBoxG.h_box ~packing:vb#add ()
let decB = ButtonG.button ~label:"Dec" ~packing:hb#add ()
let incB = ButtonG.button ~label:"Inc" ~packing:hb#add ()

let adj =
  AdjustmentG.adjustment ~lower:0. ~upper:100. ~step_increment:1. ~page_increment:10. ()

let sc = HScaleG.h_scale ~adjustment:(adj#as_adjustment) ~draw_value:false
    ~packing:vb#add ()

let counter = new Lablgtk3Compat.GUtil.variable 0

let _ =
  decB#connect#clicked
    ~callback:(fun () -> adj#set_value (float(counter#get-1)));
  incB#connect#clicked
    ~callback:(fun () -> adj#set_value (float(counter#get+1)));
  sc#connect#change_value
    ~callback:(fun _ v -> Printf.printf "drag: %i\n%!" (truncate v); false);
  adj#connect#value_changed
    ~callback:(fun () -> counter#set (truncate adj#value));
  counter#connect#changed ~callback:(fun n -> lbl#set_text (string_of_int n));
  counter#set 0;
  w#connect#destroy ~callback:GMain.quit;
  w#misc#show ();
  GMain.main ()
