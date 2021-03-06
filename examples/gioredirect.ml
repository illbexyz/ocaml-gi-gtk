(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    This code is in the public domain.                                  *)
(*    You may freely copy parts of it in your application.                *)
(*                                                                        *)
(**************************************************************************)

open GIGtk

open StdLabels

(* On Windows, the channel will be set to non blocking mode. 
   The argument given to [callback] may no be UTF-8 encoded.
   The redirection stops as soon as [callbacks] return [false]
   or an error occured *)
let channel_redirector channel callback = 
  let cout,cin = Unix.pipe () in
  Unix.dup2 cin channel ;
  let channel = GMain.Io.channel_of_descr cout in
  let len = 80 in
  let buf = Bytes.create len in
  GMain.Io.add_watch channel ~prio:0 ~cond:[`IN; `HUP; `ERR] ~callback:
    begin fun cond -> 
      try if List.mem `IN cond then begin
	(* On Windows, you must use Io.read *)
	let len = GMain.Io.read channel ~buf ~pos:0 ~len in
	len >= 1 && (callback (Bytes.sub_string buf ~pos:0 ~len)) 
      end
      else false
      with e -> callback 
       ("Channel redirector got an exception: " ^ (Printexc.to_string e)); 
       false
     end

let () = 
  let _l = GMain.init () in
  let w = WindowG.window ~width:300 ~height:200 () in
  let notebook = NotebookG.notebook ~packing:w#add () in
  let redirect channel name = 
    let buffer = TextBufferG.text_buffer () in
    let sw = ScrolledWindowG.scrolled_window ~hscrollbar_policy:`AUTOMATIC ~vscrollbar_policy:`AUTOMATIC 
             () 
    in
    let label = LabelG.label ~use_markup:true ~label:name () in
    let _ = notebook#prepend_page sw (Some label) in
    let _text = TextViewG.text_view ~buffer:buffer#as_text_buffer ~editable:false ~packing:sw#add () in
    channel_redirector channel (fun c -> buffer#insert_at_cursor c ~-1; true )
  in	
  redirect Unix.stdout "Std Out";
  redirect Unix.stderr "Std Error";
  let _ = 
    GMain.Timeout.add 500 (fun () -> try
                         Pervasives.print_endline "Hello print_endline";
                         true
                         with e -> prerr_endline (Printexc.to_string e); false),
    GMain.Timeout.add 500 (fun () -> 
                Printf.printf "Hello printf\n%!";
                        true),
    GMain.Timeout.add 500 (fun () -> 
                        Format.printf "Hello format@.";
                        true),
    GMain.Timeout.add 5000 (fun () ->
                         Pervasives.prerr_endline "Hello prerr_endline";
                         true)
  in
  let _ = w#connect#destroy GMain.quit in
  w#misc#show ();
  GMain.main ()
