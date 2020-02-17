(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    This code is in the public domain.                                  *)
(*    You may freely copy parts of it in your application.                *)
(*                                                                        *)
(**************************************************************************)

open GIGtk

let default d = function
  | None -> d
  | Some v -> v

let all_files () =
  let f = FileFilterG.file_filter ~name:"All" () in
  f#add_pattern "*" ;
  f

let is_string_prefix s1 s2 =
  let l1 = String.length s1 in
  let l2 = String.length s2 in
  l1 <= l2 && s1 = String.sub s2 0 l1

let image_filter () =
  let f = FileFilterG.file_filter ~name:"Images" () in
  f#add_custom [ `MIME_TYPE ]
    (fun info ->
      let mime = List.assoc `MIME_TYPE info in
      is_string_prefix "image/" mime) ;
  f

let text_filter () = 
  FileFilterG.file_filter 
    ~name:"Caml source code" 
    ~patterns:[ "*.ml"; "*.mli"; "*.mly"; "*.mll" ] ()

let ask_for_file parent =
  let dialog = FileChooserDialogG.file_chooser_dialog 
      ~action:`OPEN
      ~title:"Open File"
      ~parent () in
  dialog#add_button_stock `CANCEL `CANCEL ;
  dialog#add_select_button_stock `OPEN `OPEN ;
  dialog#add_filter (all_files ()) ;
  dialog#add_filter (image_filter ()) ;
  dialog#add_filter (text_filter ()) ;
  begin match dialog#run () with
  | `OPEN ->
      print_string "filename: " ;
      print_endline (default "<none>" dialog#filename) ;
      flush stdout
  | `DELETE_EVENT | `CANCEL -> ()
  end ;
  dialog#destroy ()

let main () =
  GMain.init ();
  let w = WindowG.window ~title:"FileChooser demo" () in
  w#connect#destroy GMain.quit ;

  let b = ButtonG.button ~stock:`OPEN ~packing:w#add () in
  b#connect#clicked
    (fun () -> ask_for_file w) ;

  w#misc#show () ;
  GMain.main ()

let _ = main ()

(* Local Variables: *)
(* compile-command: "ocamlc -I ../src -w s lablgtk.cma filechooser.ml" *)
(* End: *)
