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
  let window = DialogG.dialog ~title: "dialog"
      ~border_width: 10 ~width: 300 ~height: 300 () in
  window#connect#destroy ~callback:GMain.quit;

  let scrolled_window = ScrolledWindowG.scrolled_window
      ~border_width: 10 ~hscrollbar_policy: `AUTOMATIC ~packing: window#get_content_area#add ()
  in

  let table = TableG.table ~n_rows:10 ~n_columns:10
      ~row_spacing: 10 ~column_spacing: 10
      ~packing: scrolled_window#add_with_viewport ()
  in

  for i = 0 to 9 do
    for j = 0 to 9 do
      let label = Printf.sprintf "button (%d,%d)\n" i j in
      ToggleButtonG.toggle_button ~label
        ~packing:(Lablgtk3Compat.attach table ~left: i ~top: j ~expand: `BOTH) ()
    done
  done;

  let button_box = new ButtonBoxG.button_box (Gobject.try_cast window#get_action_area#as_widget "GtkButtonBox") in
  let button =
    ButtonG.button ~label: "close" ~packing: button_box#add () in
  button#connect#clicked ~callback: GMain.quit;
  button#misc#grab_default ();
  window#misc#show ();
  GMain.main ()

let _ = main ()
