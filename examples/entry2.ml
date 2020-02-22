(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    This code is in the public domain.                                  *)
(*    You may freely copy parts of it in your application.                *)
(*                                                                        *)
(**************************************************************************)

open GIGtk

let _ = GMain.init ()
let w = WindowG.window ~show:true ()
let e = EntryG.entry ~packing:w#add ()

let () =
  e#connect#after#insert_text
    (fun _ pos ->
      if e#get_text_length > 5 then e#set_secondary_icon_stock (GtkStock.convert_id `DIALOG_WARNING)
      else e#set_secondary_icon_name "");
  w#connect#delete_event (fun _ -> GMain.quit (); true);
  GMain.main ()
