(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    This code is in the public domain.                                  *)
(*    You may freely copy parts of it in your application.                *)
(*                                                                        *)
(**************************************************************************)

(* $Id$ *)

open GIGtk

let xpm_label_box ~(window : #ContainerG.container)
    ~file ~text ?packing ?(show=true) () =
  if not (Sys.file_exists file) then failwith (file ^ " does not exist");
  let box = HBoxG.h_box ~border_width: 2 ?packing ~show:false () in
  let image = ImageG.image ~file ~packing:(Lablgtk3Compat.pack box ~padding:3) () in
  LabelG.label ~label:text ~packing:(Lablgtk3Compat.pack box ~padding:3) ();
  if show then box#misc#show ();
  new GObj.widget_full box#as_widget

let main () =
  GMain.init ();
  let window = WindowG.window ~title:"Pixmap'd Buttons!" ~border_width:10 () in
  window#connect#destroy ~callback:GMain.quit;
  let hbox = HBoxG.h_box ~packing:window#add () in
  let button = ButtonG.button ~packing:(Lablgtk3Compat.pack hbox ~padding:5) () in
  button#connect#clicked ~callback:
    (fun () -> prerr_endline "Hello again - cool button was pressed");
  xpm_label_box ~window:(window :> ContainerG.container) ~file:"test.xpm" ~text:"cool button"
    ~packing:button#add ();
  let button = ButtonG.button (*XXX~use_mnemonic:true*) ~label:"_Coucou" ~packing:(Lablgtk3Compat.pack hbox ~padding:5) () in
  button#connect#clicked ~callback:
    (fun () -> prerr_endline "Coucou");
  let button = ButtonG.button (*~stock:`HOME*) ~packing:(Lablgtk3Compat.pack hbox ~padding:5) () in
  button#connect#clicked ~callback:
    (fun () -> prerr_endline "Stock buttons look nice");
  window#misc#show ();
  GMain.main ()

let _ = main ()
