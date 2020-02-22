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
(*XXX BUGGED: it should encode enum GtkResponseType using numbers from -1 to -12
and the same used by C because "MessageDialogG.message_dialog ~buttons" expects
this behaviour. Moreover one cannot add the buttons later to MessageDialogs*)
  let encode,decode = Lablgtk3Compat.encode_decode () in
  button#connect#clicked ~callback:(fun () ->
    let dialog = 
      MessageDialogG.message_dialog ~title:"Quit ?"
        ~message_type:`QUESTION ~text:"Quit the application ?"
        ~buttons:`YES_NO () in
    match decode dialog#run with
      `YES -> prerr_endline "Yes"; GMain.quit ()
    | `NO -> prerr_endline "No"; dialog#destroy
    | `DELETE_EVENT -> prerr_endline "Delete"; dialog#destroy);
  window#show;
  GtkThread.main ()

let _ = Printexc.print main ()
