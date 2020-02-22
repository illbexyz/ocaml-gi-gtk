(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    This code is in the public domain.                                  *)
(*    You may freely copy parts of it in your application.                *)
(*                                                                        *)
(**************************************************************************)

(* $Id$ *)

module S = Stack
open GIGtk

open StdLabels
open Printf

let _ = GMain.init ()

let file_dialog ~title ~callback ?filename () =
callback "foo" (*XXX
  let sel = FileChooserDialogG.file_chooser_dialog ~action:`OPEN ~title ?filename () in
  let encode,decode = Lablgtk3Compat.encode_decode () in
  sel#add_button (GtkStock.convert_id `CANCEL) (encode `CANCEL);
  sel#add_button (GtkStock.convert_id `OPEN) (encode `OPEN);
  begin match decode sel#run with
  | `OPEN -> begin
      match sel#filename with
      | Some name -> callback name
      | _ -> ()
    end
  | `DELETE_EVENT | `CANCEL -> ()
  end ;
  sel#destroy ()
*)

let w = WindowG.window ~title:"Okaimono" ()
let vb = VBoxG.v_box ~packing:w#add ()

let menubar = MenuBarG.menu_bar ~packing:(Lablgtk3Compat.pack vb) ()
let factory = new Lablgtk3Compat.factory menubar
let file_menu = factory#add_submenu "File"
let edit_menu = factory#add_submenu "Edit"

let sw = ScrolledWindowG.scrolled_window ~height_request:200 ~packing:vb#add
    ~hscrollbar_policy:`AUTOMATIC ~vscrollbar_policy:`AUTOMATIC ()
let vp = ViewportG.viewport ~width_request:340 ~shadow_type:`NONE ~packing:sw#add ()
let table = TableG.table ~n_columns:4 ~n_rows:256 ~packing:vp#add ()
let _ = table#set_focus_vadjustment vp#get_vadjustment

let top = ref 0
and left = ref 0
let add_to_table  w =
  Lablgtk3Compat.attach table ~left:!left ~top:!top ~expand:`X w;
  incr left;
  if !left >= 4 then (incr top; left := 0)

let entry_list = ref []

let add_entry () =
  let entry =
    List.map [40;200;40;60]
      ~f:(fun width -> EntryG.entry ~packing:add_to_table ~width_request:width ())
  in entry_list := entry :: !entry_list

let _ =
  List.iter2 ["Number";"Name";"Count";"Price"] [40;200;40;60] ~f:
    begin fun label width ->
      ignore (ButtonG.button ~label ~packing:add_to_table ~width_request:width ())
    end;
  for i = 1 to 9 do add_entry () done

let split ~sep s =
  let len = String.length s in
  let rec loop pos =
    let next =
      try String.index_from s pos sep with Not_found -> len
    in
    let sub = String.sub s ~pos ~len:(next-pos) in
    if next = len then [sub] else sub::loop (next+1)
  in loop 0

let load name =
  try
    let ic = open_in name in
    List.iter !entry_list
      ~f:(fun l -> List.iter l ~f:(fun e -> e#set_text ""));
    let entries = S.create () in
    List.iter !entry_list ~f:(fun x -> S.push x entries);
    try while true do
      let line = input_line ic in
      let fields = split ~sep:'\t' line in
      let entry =
	try S.pop entries
	with S.Empty ->
	  add_entry (); List.hd !entry_list
      in
      List.fold_left fields ~init:entry ~f:
	begin fun acc field ->
	  (List.hd acc)#set_text field;
	  List.tl acc
	end
    done
    with End_of_file -> close_in ic
  with Sys_error _ -> ()
    

let save name =
  try
    let oc = open_out name in
    List.iter (List.rev !entry_list) ~f:
      begin fun entry ->
	let l = List.map entry ~f:(fun e -> e#get_text) in
	if List.exists l ~f:((<>) "") then
	  let rec loop = function
	      [] -> ()
	    | [x] -> fprintf oc "%s\n" x
	    | x::l -> fprintf oc "%s\t" x; loop l
	  in loop l
      end;
    close_out oc
  with Sys_error _ -> ()

open GdkKeysyms

let _ =
  w#connect#destroy ~callback:GMain.quit;
  w#connect#key_press_event ~callback:
    begin fun ev ->
      let key = GdkEvent.Key.keyval (Obj.magic ev)(*XXX*) and adj = vp#get_vadjustment in
      if key = _Page_Up then
	adj#set_value (adj#get_value -. adj#get_page_increment)
      else if key = _Page_Down then
	adj#set_value (min (adj#get_value +. adj#get_page_increment)
			 (adj#get_upper -. adj#get_page_size));
      false
    end;
  w#add_accel_group factory#accel_group;
  let ff = new Lablgtk3Compat.factory (file_menu :> MenuShellG.menu_shell) ~accel_group:factory#accel_group in
  ff#add_item ~key:_O "Open..."
    ~callback:(file_dialog ~title:"Open data file" ~callback:load);
  ff#add_item ~key:_S "Save..."
    ~callback:(file_dialog ~title:"Save data" ~callback:save);
  ff#add_separator ();
  ff#add_item ~key:_Q "Quit" ~callback:(fun () -> w#destroy);
  let ef = new Lablgtk3Compat.factory (edit_menu :> MenuShellG.menu_shell) ~accel_group:factory#accel_group in
  ef#add_item ~key:_A "Add line" ~callback:add_entry;
  w#show;
  GMain.main ()
