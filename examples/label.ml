open GIGtk

let main () =
  let _ = GMain.init () in
  let w = WindowG.window ~show:true () in
  let _ = w#set_border_width 2 in
  let _ = w#misc#realize () in
  let hbox = HBoxG.h_box ~packing:w#add () () in
  let label = LabelG.label ~packing:hbox#add () () in
  let _ = label#set_text "hello world" in
  let _ = w#connect#destroy ~callback:GMain.quit in
  GMain.main ()

let () = main ()