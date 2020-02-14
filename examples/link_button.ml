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

  let window = WindowG.window ~title: "Link button" ~border_width: 0 () in

  let box = VBoxG.v_box ~packing: window#add () in

  let button = LinkButtonG.link_button 
    ~uri:"http://HELLO.ORG" 
    ~label:"BYE" ~packing:box#add () 
  in
  button#set_uri "GHHHHH";
  Format.printf "Got:%a@." GUtil.print_widget button;
  button#connect#activate_link
    (fun () -> Format.printf "Got url '%s'@." button#get_uri;   button#set_uri "AGAIN");
  window#connect#destroy GMain.quit;
  window#misc#show ();
  GMain.main ()

let _ = main ()
