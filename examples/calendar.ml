open GIGtk

let main () =
  let _ = GMain.init () in

  let window = WindowG.window () in
  ignore @@ window#connect#destroy ~callback:GMain.quit;

  let calendar = CalendarG.calendar ~packing:window#add () in
  ignore @@ calendar#connect#day_selected ~callback:(fun () ->
      let (_, year,month,day) = calendar#get_date in
      Printf.printf "You selected %d/%d/%02d.\n" day (month+1) (year mod 100);
      flush stdout
    );

  GMain.main()

let _ = main ()