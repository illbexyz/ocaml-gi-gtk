(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    This code is in the public domain.                                  *)
(*    You may freely copy parts of it in your application.                *)
(*                                                                        *)
(**************************************************************************)

(* $Id$ *)

open GIGtk

let rec fix ~f ~eq x =
  let x' = f x in
  if eq x x' then x
  else fix ~f ~eq x'

let eq_float x y = abs_float (x -. y) < 1e-13

let _ =
  let _ = GMain.init () in
  let top = WindowG.window ~resizable: false () in
  top#connect#destroy ~callback:GMain.quit;
  let vbox = VBoxG.v_box ~packing: top#add () in
  let label = LabelG.label ~label: "Fixed point of cos(x)" ~packing: vbox#add () in
  let entry = EntryG.entry ~max_length: 20 ~packing: vbox#add () in
  entry#set_tooltip_text "Initial value for fix-point";
  let result =
    EntryG.entry ~max_length: 20 ~editable: false ~packing: vbox#add () in

  entry#connect#activate ~callback:
    begin fun () ->
      let x = try float_of_string entry#text with _ -> 0.0 in
      entry#set_text (string_of_float (cos x));
      let res = fix ~f:cos ~eq:eq_float x in
      result#set_text (string_of_float res)
    end;
  top#misc#show ();
  GMain.main ()
