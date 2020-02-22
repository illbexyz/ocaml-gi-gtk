(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    This code is in the public domain.                                  *)
(*    You may freely copy parts of it in your application.                *)
(*                                                                        *)
(**************************************************************************)

(* $Id$ *)

open GIGtk

open Cairo

let polygon = 
    [ 10,100; 35,35; 100,10; 165,35; 190,100;
      165,165; 100,190; 35,165; 10,100 ]

let draw_polygon cr = function
    [] -> ()
  | (x,y)::tl ->
      set_source_rgba cr 0. 0. 0. 1.;
      move_to cr (float x) (float y);
      List.iter (fun (x,y) -> line_to cr (float x) (float y)) tl;
      set_source_rgba cr 0.5 0. 0. 0.5;
      fill cr

let expose drawing_area cr =
  draw_polygon cr polygon ;
  true

let () =
  GMain.init ();
  let w = WindowG.window ~title:"Drawing demo" ~width_request:500 ~height_request:400 () in
  ignore(w#connect#destroy ~callback:GMain.quit);

  let d = DrawingAreaG.drawing_area ~packing:w#add () in
  ignore(d#connect#draw ~callback:(Obj.magic (expose d)))(*XXX Obj.magic for Cairo*);

  w#show;
  GMain.main ()
