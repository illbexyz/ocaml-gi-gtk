(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    This code is in the public domain.                                  *)
(*    You may freely copy parts of it in your application.                *)
(*                                                                        *)
(**************************************************************************)

(* $Id$ *)

open GIGtk

open Printf

let enter_callback entry =
  printf "Entry contents: %s\n" entry#get_text;
  flush stdout

let entry_toggle_editable button entry =
  entry#set_editable button#get_active

let entry_toggle_visibility button entry =
  entry#set_visibility button#get_active

let main () =
  GMain.init ();
  let window =
    WindowG.window ~title: "GTK Entry" ~width_request: 200 ~height_request: 100 () in
  window#connect#destroy ~callback:GMain.quit;

  let vbox = VBoxG.v_box ~packing: window#add () in

  let entry = EntryG.entry ~max_length: 50 ~packing: vbox#add () in
  entry#connect#activate ~callback:(fun () -> enter_callback entry);
  entry#set_text "Hello";
  (* Appending text now requires getting the underlying buffer, and
   * entry#buffer is not exposed in the bindings yet *)
  (new EntryBufferG.entry_buffer entry#get_buffer)#set_text
   ((new EntryBufferG.entry_buffer entry#get_buffer)#get_text ^ " world");
  entry#ieditable#select_region 0 entry#get_text_length;

  let hbox = HBoxG.h_box ~packing: vbox#add () in

  let check = CheckButtonG.check_button ~label: "Editable" ~active: true
      ~packing: hbox#add () in
  check#connect#toggled
    ~callback:(fun () -> entry_toggle_editable check entry);

  let check =
    CheckButtonG.check_button ~label:"Visible" ~active:true ~packing:hbox#add () in
  check#connect#toggled
    ~callback:(fun () -> entry_toggle_visibility check entry);

  let button = ButtonG.button ~label: "Close" ~packing: vbox#add () in
  button#connect#clicked ~callback:(fun () -> window#destroy);
  button#grab_default;

  window#show;

  GMain.main ()

let _ = main ()
