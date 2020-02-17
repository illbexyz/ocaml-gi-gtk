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
  let w = WindowG.window ~title:"Socket example" () in
  w#connect#destroy ~callback:GMain.quit;
  let vbox = VBoxG.v_box ~packing:w#add () in
  let label = LabelG.label ~packing:(Lablgtk3Compat.pack vbox) () in
  w#misc#show ();
  let socket = SocketG.socket ~packing:vbox#add ~height:40 () in
  label#set_text ("XID to plug into this socket: 0x" ^ 
                  Int32.format "%x" socket#get_xid);
  GMain.main ()

let _ = main ()
