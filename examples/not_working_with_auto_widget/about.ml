(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    This code is in the public domain.                                  *)
(*    You may freely copy parts of it in your application.                *)
(*                                                                        *)
(**************************************************************************)

(* $Id$ *)

open GIGtk

let show () =
  let dialog = 
    AboutDialogG.about_dialog 
      ~program_name:"Name" 
      (*~authors:["Me" ; 
                "Myself"; 
               ]*)
      ~copyright:"Copyright: copyleft"
      ~license:"Open"
      ~website:"http://www.world.com"
      ~website_label:"Questions and support"
      ~version:"0.0"
      ()
  in
  ignore (dialog#connect#response 
            ~callback:(fun _ -> dialog#misc#show ()
                       ));

  ignore (dialog#run)


let () = 
  GMain.init ();
  show ()
    
