(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    This code is in the public domain.                                  *)
(*    You may freely copy parts of it in your application.                *)
(*                                                                        *)
(**************************************************************************)

(* $Id$ *)

open Printf

let enter_callback entry =
  printf "Entry contents: %s\n" entry#text;
  flush stdout

let entry_toggle_editable button entry =
  entry#set_editable button#active

let entry_toggle_visibility button entry =
  entry#set_visibility button#active

let main () =
  let _ = GMain.init () in
  let window =
    GWindow.window ~title: "GTK Entry" ~width: 200 ~height: 100 () in
  let _ = window#connect#destroy ~callback:GMain.quit in

  let vbox = GPack.vbox ~packing: window#add () in

  let entry = Objects.EntryG.entry ~packing: vbox#add () in
  let _ = entry#set_max_length 50 in
  let _ = entry#connect#activate ~callback:(fun () -> enter_callback entry) in
  let _ = entry#set_text "Hello" in

  let hbox = GPack.hbox ~packing: vbox#add () in

  let check = GButton.check_button ~label: "Editable" ~active: true
      ~packing: hbox#add () in
  let _ = check#connect#toggled
      ~callback:(fun () -> entry_toggle_editable check entry) in

  let check =
    GButton.check_button ~label:"Visible" ~active:true ~packing:hbox#add () in
  let _ = check#connect#toggled
      ~callback:(fun () -> entry_toggle_visibility check entry) in

  let button = GButton.button ~label: "Close" ~packing: vbox#add () in
  let _ = button#connect#clicked ~callback:window#destroy in
  let _ = button#grab_default () in

  let _ = window#show () in

  GMain.main ()

let _ = main ()
