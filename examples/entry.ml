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
  printf "Entry contents: %s\n" entry#text;
  flush stdout

let entry_toggle_editable button entry =
  entry#set_editable button#active

let entry_toggle_visibility button entry =
  entry#set_visibility button#active

let main () =
  GMain.init ();
  let window =
    WindowG.window ~title: "GTK Entry" ~width: 200 ~height: 100 () in
  window#connect#destroy ~callback:GMain.quit;

  let vbox = VBoxG.v_box ~packing: window#add () in

  let entry = EntryG.entry ~max_length: 50 ~packing: vbox#add () in
  entry#connect#activate ~callback:(fun () -> enter_callback entry);
  entry#set_text "Hello";
  (* Appending text now requires getting the underlying buffer, and
   * entry#buffer is not exposed in the bindings yet *)
  (new EntryBufferG.entry_buffer entry#buffer)#set_text
   ((new EntryBufferG.entry_buffer entry#buffer)#text ^ " world");
  entry#ieditable#select_region 0 entry#text_length;

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
  button#connect#clicked ~callback:window#destroy;
  button#misc#grab_default ();

  window#misc#show ();

  GMain.main ()

let _ = main ()
