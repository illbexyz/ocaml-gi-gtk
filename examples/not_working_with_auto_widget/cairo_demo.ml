(* File: cairo.ml
   Originally part of library ocaml-cairo by Christophe Troestler
   Ported to lablgtk3 by Claudio Sacerdoti Coen

   Copyright (C) 2009

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umh.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

open GIGtk

open Cairo

let pi2 = 8. *. atan 1.

let draw cr width height =
  let r = 0.25 *. width in
  set_source_rgba cr 0. 1. 0. 0.5;
  arc cr (0.5 *. width) (0.35 *. height) ~r ~a1:0. ~a2:pi2;
  fill cr;
  set_source_rgba cr 1. 0. 0. 0.5;
  arc cr (0.35 *. width) (0.65 *. height) ~r ~a1:0. ~a2:pi2;
  fill cr;
  set_source_rgba cr 0. 0. 1. 0.5;
  arc cr (0.65 *. width) (0.65 *. height) ~r ~a1:0. ~a2:pi2;
  fill cr;
;;

let expose drawing_area cr =
  let allocation = drawing_area#allocation in
  draw cr (float allocation.Gtk.width) (float allocation.Gtk.height);
  true

let () =
  let _ = GMain.init () in
  let w = WindowG.window ~title:"Cairo demo" ~width_request:500 ~height_request:400 () in
  ignore(w#connect#destroy ~callback:GMain.quit);

  let d = DrawingAreaG.drawing_area ~packing:w#add () in
  ignore(d#connect#draw ~callback:(expose d));

  w#show;
  GMain.main()
