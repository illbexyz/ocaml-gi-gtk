(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    This code is in the public domain.                                  *)
(*    You may freely copy parts of it in your application.                *)
(*                                                                        *)
(**************************************************************************)

(* $Id: $ *)

open GIGtk

let main () =
  GMain.init ();

  let assistant = AssistantG.assistant () in

  let box = VBoxG.v_box () in
  ignore (assistant#append_page box);
  assistant#set_page_complete box true;
  prerr_endline "Complete";
  assistant#set_page_type box `SUMMARY;
  let button = LinkButtonG.link_button 
    ~uri:"http://HELLO.ORG" 
    ~label:"BYE" ~packing:box#add () 
  in
  button#set_uri "GHHHHH";
  Format.printf "Got:%a@." GUtil.print_widget button;
  button#connect#activate_link
    (fun () -> Format.printf "Got url '%s'@." button#uri;   button#set_uri "AGAIN");
  assistant#connect#close GMain.quit;
  assistant#misc#show ();
  GMain.main ()

let _ = main ()

