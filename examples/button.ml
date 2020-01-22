let main () =
  let _ = GMain.init () in
  let window = GWindow.window ~title:"Buttons" ~border_width:10 () in
  let _ = window#connect#destroy ~callback:GMain.quit in
  let hbox = GPack.hbox ~packing:window#add () in
  let button = GIGtk.ButtonG.button ~packing:(hbox#pack ~padding:5) () in
  let _ = button#set_label "One" in
  let _ = button#connect#clicked ~callback:
      (fun () -> prerr_endline "Button one clicked") in
  let button = GIGtk.ButtonG.button ~use_underline:true ~label:"Two" ~packing:(hbox#pack ~padding:5) () in
  let _ = button#connect#clicked ~callback:
      (fun () ->
        let () = prerr_endline "Button two clicked, GC" in
        Gc.compact ()
      ) in
  let button = GIGtk.ButtonG.button ~use_stock:true ~packing:(hbox#pack ~padding:5) () in
  let _ = button#set_label "HOME" in
  let _ = button#connect#clicked ~callback:
      (fun () -> prerr_endline "HOME button clicked") in
  let _ = window#show () in
  GMain.main ()


let _ = main ()
