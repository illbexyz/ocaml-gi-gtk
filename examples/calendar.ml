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
  let window = WindowG.window () in
  window#connect#destroy ~callback:GMain.quit;

  let calendar = CalendarG.calendar ~packing:window#add () in
  calendar#connect#day_selected ~callback:
    begin fun () ->
      let (year,month,day) = calendar#get_date in
      Printf.printf "You selected %d/%d/%02d.\n"
        day (month+1) (year mod 100);
      flush stdout
    end;

  window#show;
  GMain.main ()

let _ = main ()
