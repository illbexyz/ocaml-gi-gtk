(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    This code is in the public domain.                                  *)
(*    You may freely copy parts of it in your application.                *)
(*                                                                        *)
(**************************************************************************)

(* $Id$ *)

open GIGtk

(* lablgtk2 -thread -nothinit dialog-thread.ml *)

let _ = GMain.init ()
let window = WindowG.window ~border_width: 10 ()

let button = ButtonG.button ~label:"Open Dialog" ~packing: window#add ()

let mythread =
  Thread.create
    (fun () -> while true do Thread.delay 2.0; prerr_endline "running." done)
    ()

let main () =
  Glib.Timeout.add ~ms:100 ~callback:GtkThread.do_jobs;
  window#connect#destroy ~callback:GMain.quit;
  button#connect#clicked ~callback:(fun () ->
    let dialog = 
      MessageDialogG.message_dialog ~title:"Quit ?"
        ~message_type:`QUESTION ~message:"Quit the application ?"
        ~buttons:yes_no ()
    in match dialog#run () with
      `YES -> GMain.quit ()
    | `NO | `DELETE_EVENT -> dialog#destroy ());
  window#misc#show ();
  GtkThread.main ()

let _ = Printexc.print main ()
