(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    This code is in the public domain.                                  *)
(*    You may freely copy parts of it in your application.                *)
(*                                                                        *)
(**************************************************************************)

(* $Id$ *)

(* これを実行する前にLC_ALL=ja_JP.EUCなどと指定しなければならない *)

open GIGtk

let _ = GMain.init ()
let window = WindowG.window ()
let box = VBoxG.v_box ~packing: window#add ()
let text = TextViewG.text_view ~packing: box#add ()
let button = ButtonG.button ~label: "終了" ~packing: box#add ()
let label = LabelG.label ~label:"これには影響しない" ~packing: box#add ()

let _ =
  window#connect#destroy ~callback:GMain.quit;
  (new TextBufferG.text_buffer text#get_buffer)#insert_at_cursor "こんにちは" ~-1;(*XXX*)
  text#set_size_chars ~width:20 ~height:5 ();
  let style = (new StyleG.style button#get_style)#copy in
  button#set_style style#as_style;
  style#set_bg [`NORMAL,`NAME "green"; `PRELIGHT,`NAME "red"];
  button#connect#clicked ~callback:GMain.quit

let _ =
  window#show ;
  GMain.main ()
