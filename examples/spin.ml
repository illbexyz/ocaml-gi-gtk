(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    This code is in the public domain.                                  *)
(*    You may freely copy parts of it in your application.                *)
(*                                                                        *)
(**************************************************************************)

open GIGtk

let run () =
  GMain.init ();
  let w = DialogG.dialog  ~title:"Go to page" ~modal:true ~window_position:`CENTER () 
  in
  ignore (LabelG.label ~label:"Page: " ~packing:w#get_content_area#add ());
  let sb = 
    SpinButtonG.spin_button ~packing:w#get_content_area#add ~digits:0 ~numeric:true ~wrap:true ()
  in
  let adj = AdjustmentG.adjustment ~lower:0. ~upper:50.0 ~step_increment:1. () in
  sb#set_adjustment adj#as_adjustment;
  sb#set_value 22.;
  sb#connect#wrapped (fun () -> prerr_endline "Wrapped!");
  let encode,decode = Lablgtk3Compat.encode_decode () in
  w#add_button (GtkStock.convert_id `OK) (encode `OK);
  w#add_button (GtkStock.convert_id `CANCEL) (encode `CANCEL);
  w#set_default_response (encode `OK);
  let on_ok () = Format.printf "Ok...@." ; w#destroy () in
  match decode w#run with
    | `OK -> on_ok ()
    | `DELETE_EVENT | `CANCEL -> w#destroy ()

let () = run ()
