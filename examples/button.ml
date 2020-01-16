let main () =
  let _ = GMain.init () in
  let window = GWindow.window ~title:"Pixmap'd Buttons!" ~border_width:10 () in
  let _ = window#connect#destroy ~callback:GMain.quit in
  let hbox = GPack.hbox ~packing:window#add () in
  let button = Objects.GButton.button ~packing:(hbox#pack ~padding:5) () in
  let _ = button#connect#clicked ~callback:
      (fun () -> prerr_endline "Hello again - cool button was pressed") in
  let button = Objects.GButton.button ~use_underline:true ~label:"_Coucou" ~packing:(hbox#pack ~padding:5) () in
  let _ = button#connect#clicked ~callback:
      (fun () -> prerr_endline "Coucou") in
  let button = Objects.GButton.button ~packing:(hbox#pack ~padding:5) () in
  let _ = button#connect#clicked ~callback:
      (fun () -> prerr_endline "Stock buttons look nice") in
  let _ = window#show () in
  GMain.main ()


let _ = main ()