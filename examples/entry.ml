(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    This code is in the public domain.                                  *)
(*    You may freely copy parts of it in your application.                *)
(*                                                                        *)
(**************************************************************************)

(* $Id$ *)

open Printf
open GIGtk

let enter_callback entry =
  printf "Entry contents: %s\n" entry#get_text;
  flush stdout

let entry_toggle_editable button entry =
  entry#set_editable button#get_active

let entry_toggle_visibility button entry =
  entry#set_visibility button#get_active

let main () =
  let _ = GMain.init () in
  let window = WindowG.window ~show:false () in
  window#set_title "Gtk Entry";
  let _ = window#connect#destroy ~callback:GMain.quit in

  let vbox = VBoxG.v_box ~packing: window#add () in

  let entry = EntryG.entry ~packing: vbox#add () in
  let _ = entry#set_max_length 50 in
  let _ = entry#connect#activate ~callback:(fun () -> enter_callback entry) in
  let _ = entry#set_text "Hello" in

  let hbox = HBoxG.h_box ~packing: vbox#add () in

  let check1 = CheckButtonG.check_button ~label:"Editable" ~packing:hbox#add () in
  check1#set_active true;
  let _ = check1#connect#toggled
      ~callback:(fun () -> entry_toggle_editable check1 entry) in

  let check2 =
    CheckButtonG.check_button ~label:"Visible" ~packing:hbox#add () in
  let _ = check2#set_active true in
  let _ = check2#connect#toggled
      ~callback:(fun () -> entry_toggle_visibility check2 entry) in

  let button = GButton.button ~label:"Close" ~packing:vbox#add () in
  let _ = button#connect#clicked ~callback:window#destroy in
  let _ = button#grab_default () in

  window#set_default_size 1280 720;
  window#misc#show ();
  GMain.main ()

let _ = main ()
