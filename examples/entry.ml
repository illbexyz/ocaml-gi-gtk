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
  entry#set_editable button#active

let entry_toggle_visibility button entry =
  entry#set_visibility button#active

let main () =
  let _ = GMain.init () in
  let window = WindowG.window () in
  let _ = window#set_title "Gtk Entry" in
  let _ = window#set_default_size 1280 720 in
  let _ = window#connect#destroy ~callback:GMain.quit in
  printf "%d" window#get_default_width; flush stdout;

  let vbox = VBoxG.v_box ~packing: window#add () () in

  let entry = EntryG.entry ~packing: vbox#add () in
  let _ = entry#set_max_length 50 in
  let _ = entry#connect#activate ~callback:(fun () -> enter_callback entry) in
  let _ = entry#set_text "Hello" in

  let hbox = HBoxG.h_box ~packing: vbox#add () () in

  let check = CheckButtonG.check_button ~label: "Editable" ~packing: hbox#add () in
  let _ = check#connect#toggled
      ~callback:(fun () -> entry_toggle_editable check entry) in

  let check =
    CheckButtonG.check_button ~label:"Visible" ~active:true ~packing:hbox#add () in
  let _ = check#connect#toggled
      ~callback:(fun () -> entry_toggle_visibility check entry) in

  let button = GButton.button ~label: "Close" ~packing: vbox#add () in
  let _ = button#connect#clicked ~callback:window#destroy in
  let _ = button#grab_default () in

  GMain.main ()

let _ = main ()
