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
  let window = WindowG.window ~border_width: 10 () in
  window#connect#destroy ~callback:GMain.quit;

  let table = TableG.table ~n_rows:3 ~n_columns:2 ~packing: window#add () in
  
  LabelG.label ~label:"Progress Bar Example" ()
    ~packing:(Lablgtk3Compat.attach table ~left:0 ~right:2 ~top:0 ~expand:`X ~shrink:`BOTH);
  
  let pbar =
    ProgressBarG.progress_bar ~pulse_step:0.01 ()
      ~packing:(Lablgtk3Compat.attach table ~left:0 ~right:2 ~top:1
                  ~expand:`BOTH ~fill:`X ~shrink:`BOTH) in

  let ptimer = GMain.Timeout.add ~ms:50 ~callback:(fun () -> pbar#pulse; true) in

  let button = ButtonG.button ~label:"Reset" ()
      ~packing:(Lablgtk3Compat.attach table ~left:0 ~top:2
                  ~expand:`NONE ~fill:`X ~shrink:`BOTH) in
  button#connect#clicked ~callback:(fun () -> pbar#set_fraction 0.);

  let button = ButtonG.button ~label:"Cancel" ()
      ~packing:(Lablgtk3Compat.attach table ~left:1 ~top:2
                  ~expand:`NONE ~fill:`X ~shrink:`BOTH) in
  button#connect#clicked ~callback:GMain.quit;

  window#show ;
  GMain.main ()

let _ = main ()
