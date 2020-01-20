let main () =
  let _ = GMain.init () in
  let w = GWindow.window ~border_width:2 () in
  let _ = w#misc#realize () in
  let hbox = GPack.hbox ~spacing:10 ~packing:w#add () in
  let label = GIGtk.LabelG.label ~packing:hbox#add () in
  let _ = label#set_text "hello world" in
  let _ = w#show () in
  let _ = w#connect#destroy ~callback:GMain.quit in
  GMain.main ()

let () = main ()