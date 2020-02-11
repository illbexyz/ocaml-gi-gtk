open GIGtk

let main () =
  let _ = GMain.init () in

  let window = WindowG.window ~show:false () in
  window#set_title "Notebook";
  window#set_border_width 10;
  ignore @@ window#connect#destroy ~callback:GMain.quit;

  let notebook = NotebookG.notebook ~packing:window#add () in
  let button1 = ButtonG.button ~label:"Page 1" ~packing:(fun w -> ignore (notebook#append_page w None)) () in
  ignore @@ button1#connect#clicked ~callback:(fun () -> prerr_endline "Hello again - cool button 1 was pressed");

  let button2 = ButtonG.button ~label:"Page 2" ~packing:(fun w -> ignore (notebook#append_page w None)) () in
  ignore @@ button2#connect#clicked ~callback:(fun () -> prerr_endline "Hello again - cool button 1 was pressed");

  ignore @@ notebook#connect#switch_page ~callback:(fun _ i -> prerr_endline ("Page switch to " ^ string_of_int i));

  ignore @@ button2#connect#clicked ~callback:(fun () -> prerr_endline "Coucou");

  window#misc#show ();
  GMain.main ()

let _ = main ()