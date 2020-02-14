(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    This code is in the public domain.                                  *)
(*    You may freely copy parts of it in your application.                *)
(*                                                                        *)
(**************************************************************************)

(* $Id$ *)

open GIGtk

let _ = GMain.init ()

let window = WindowG.window ~border_width: 10 ()

let button = ButtonG.button ~label:"Hello World" ~packing: window#add ()

let main () =
  (*XXX window#event#connect#delete 
    ~callback:(fun _ -> prerr_endline "Delete event occured"; true);*)
  window#connect#destroy ~callback:GMain.quit;
  button#connect#clicked ~callback:(fun () -> prerr_endline "Hello World");
  button#connect#clicked ~callback:window#destroy;
  window#misc#show ();
  GMain.main ()

let _ = Printexc.print main ()
