open GIGtk

let _ = GMain.init ()

let w = WindowG.window ~show:true ()

let vb = VBoxG.v_box ~packing:w#add ()

let lbl = LabelG.label ~packing:vb#add ()

let hb = HBoxG.h_box ~packing:vb#add ()

let decB = ButtonG.button ~label:"Dec" ~packing:hb#add ()

let incB = ButtonG.button ~label:"Inc" ~packing:hb#add ()

let adj = AdjustmentG.adjustment ~lower:0. ~upper:100. ~step_increment:1. ~page_increment:10. ()

let sc = ScaleG.scale ~adjustment:adj#as_adjustment ~packing:vb#add ()

let counter = ref 0

let _ =
  ignore @@ decB#connect#clicked ~callback:(fun () -> adj#set_value (float(!counter-1)));
  ignore @@ incB#connect#clicked ~callback:(fun () -> adj#set_value (float(!counter+1)));

  ignore @@ sc#connect#change_value ~callback:(fun _ v -> Printf.printf "drag: %i\n%!" (truncate v));
  ignore @@ adj#connect#value_changed ~callback:(fun () -> counter := truncate (adj#get_value));

  ignore @@ w#connect#destroy ~callback:GMain.quit;
  w#misc#show ();
  GMain.main ()
