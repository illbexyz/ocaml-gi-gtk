open GIGtk

(* Dialog *)

let rec list_rassoc k = function
  | (a, b) :: _ when b = k -> a
  | _ :: l -> list_rassoc k l
  | [] -> raise Not_found

(*XXX BUGGED: it should encode enum GtkResponseType using numbers from -1 to -12
and the same used by C because "MessageDialogG.message_dialog ~buttons" expects
this behaviour. Moreover one cannot add the buttons later to MessageDialogs*)
let encode_decode () =
 let resp = Gpointer.encode_variant GtkEnums.Conv.response_tbl in
 let rnone = resp `NONE
 and rreject = resp `REJECT
 and raccept = resp `ACCEPT
 and rdelete = resp `DELETE_EVENT
 and rok = resp `OK
 and rcancel = resp `CANCEL
 and rclose = resp `CLOSE
 and ryes = resp `YES
 and rno = resp `NO
 and rapply = resp `APPLY
 and rhelp = resp `HELP in
 let tbl = ref [rdelete, `DELETE_EVENT] in
 let id = ref 0 in
 let encode (v : 'a) = list_rassoc v !tbl in
 let decode r = 
  try 
    List.assoc r !tbl 
  with Not_found -> 
    Format.eprintf 
      "Warning: unknown response id:%d in dialog. \
                Please report to lablgtk dev team.@." 
      r;
    `DELETE_EVENT in
 encode,decode

(* Table *)

type expand_type = [`NONE | `X | `Y | `BOTH]

let has_x : expand_type -> bool =
  function `X|`BOTH -> true | `Y|`NONE -> false
let has_y : expand_type -> bool =
  function `Y|`BOTH -> true | `X|`NONE -> false

let attach (tbl : #TableG.table) ~left ~top ?(right=left+1) ?(bottom=top+1)
    ?(expand=`NONE) ?(fill=`BOTH) ?(shrink=`NONE)
    ?(xpadding=0) ?(ypadding=0) w =
    let xoptions = if has_x shrink then [`SHRINK] else [] in
    let xoptions = if has_x fill then `FILL::xoptions else xoptions in
    let xoptions = if has_x expand then `EXPAND::xoptions else xoptions in
    let yoptions = if has_y shrink then [`SHRINK] else [] in
    let yoptions = if has_y fill then `FILL::yoptions else yoptions in
    let yoptions = if has_y expand then `EXPAND::yoptions else yoptions in
  tbl#attach w left right top bottom xoptions yoptions xpadding ypadding

(* Box *)

let pack (box : #BoxG.box) ?(expand=false) ?(fill=true) ?(padding=0) w =
 box#pack_start w expand fill padding

(* Assistant *)

let append_page (assistant : #AssistantG.assistant) ?page_type ?title ?header_image ?side_image ?complete w =
  let n = assistant#append_page w in
  Gaux.may (assistant#set_page_type w) page_type;
  Gaux.may (assistant#set_page_title w) title;
  Gaux.may (assistant#set_page_header_image w) header_image;
  Gaux.may (assistant#set_page_side_image w) side_image;
  Gaux.may (assistant#set_page_complete w) complete;
  n


(* Old Lablgtk3 module GUtil *)
module GUtil =
struct
(**************************************************************************)
(*                Lablgtk                                                 *)
(*                                                                        *)
(*    This program is free software; you can redistribute it              *)
(*    and/or modify it under the terms of the GNU Library General         *)
(*    Public License as published by the Free Software Foundation         *)
(*    version 2, with the exception described in file COPYING which       *)
(*    comes with the library.                                             *)
(*                                                                        *)
(*    This program is distributed in the hope that it will be useful,     *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*    GNU Library General Public License for more details.                *)
(*                                                                        *)
(*    You should have received a copy of the GNU Library General          *)
(*    Public License along with this program; if not, write to the        *)
(*    Free Software Foundation, Inc., 59 Temple Place, Suite 330,         *)
(*    Boston, MA 02111-1307  USA                                          *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id$ *)

open StdLabels
open GObj

let print_widget ppf (o : #WidgetG.widget) = 
    Format.fprintf ppf "<%s@@0x%x>" (Gobject.Type.name (Gobject.get_type o#as_widget)) o#get_oid

class ['a] memo () = object
  val tbl : (int, 'a) Hashtbl.t = Hashtbl.create 7
  method add (obj : 'a) = Hashtbl.add tbl obj#get_oid obj
  method find (obj : WidgetG.widget) = Hashtbl.find tbl obj#get_oid
  method remove (obj : WidgetG.widget) = Hashtbl.remove tbl obj#get_oid
end

let signal_id = ref 0

let next_callback_id () : GtkSignal.id =
  decr signal_id; Obj.magic (!signal_id : int)

class ['a] signal () = object (self)
  val mutable callbacks : (GtkSignal.id * ('a -> unit)) list = []
  method callbacks = callbacks
  method connect ~after ~callback =
    let id = next_callback_id () in
    callbacks <-
      if after then callbacks @ [id,callback] else (id,callback)::callbacks;
    id
  method call arg =
    List.exists callbacks ~f:
      begin fun (_,f) ->
        let old = GtkSignal.push_callback () in
        try f arg; GtkSignal.pop_callback old
        with exn -> GtkSignal.pop_callback old; raise exn
      end;
    ()
  method disconnect key =
    List.mem_assoc key ~map:callbacks &&
    (callbacks <- List.remove_assoc key callbacks; true)
end

class virtual ml_signals disconnectors =
  object (self)
    val after = false
    method after = {< after = true >}
    val mutable disconnectors : (GtkSignal.id -> bool) list = disconnectors
    method disconnect key =
      ignore (List.exists disconnectors ~f:(fun f -> f key))
  end

class virtual add_ml_signals obj disconnectors =
  object (self)
    val mutable disconnectors : (GtkSignal.id -> bool) list = disconnectors
    method disconnect key =
      if List.exists disconnectors ~f:(fun f -> f key) then ()
      else GtkSignal.disconnect obj key
  end

class ['a] variable_signals ~(set : 'a signal) ~(changed : 'a signal) =
  object
    inherit ml_signals [changed#disconnect; set#disconnect]
    method changed = changed#connect ~after
    method set = set#connect ~after
  end

class ['a] variable x =
  object (self)
    val changed = new signal ()
    val set = new signal ()
    method connect = new variable_signals ~set ~changed
    val mutable x : 'a = x
    method get = x
    method set = set#call
    method private equal : 'a -> 'a -> bool = (=)
    method private real_set y =
      let x0 = x in x <- y;
      if changed#callbacks <> [] && not (self#equal x x0)
      then changed#call y
    initializer
      ignore (set#connect ~after:false ~callback:self#real_set)
  end
end

(* Deprecated, from Gtk2 *)
class ['a] factory
    ?(accel_group=AccelGroupG.accel_group ())
    ?(accel_path="<DEFAULT ROOT>/")
    ?(accel_modi=[`CONTROL])
    ?(accel_flags=[`VISIBLE]) (menu_shell : 'a) =
  object (self)
    val menu_shell : #MenuShellG.menu_shell = menu_shell
    val group = accel_group
    val m = accel_modi
    val flags = (accel_flags:Gtk.Tags.accel_flag list)
    val accel_path = accel_path
    method menu = menu_shell
    method accel_group = group
    method private bind ?(modi=m) ?key ?callback (item : #MenuItemG.menu_item) label =
      menu_shell#append item;
      let accel_path = accel_path ^ label ^ "/" in
      (* Default accel path value *)
      GtkData.AccelMap.add_entry accel_path ?key ~modi:m;
      (* Register this accel path *)
      item#set_accel_path (Some accel_path) (Some accel_group);
      Gaux.may callback ~f:(fun callback -> item#connect#activate ~callback)
    method add_item ?key ?callback ?submenu label =
      let item = MenuItemG.menu_item  (*~use_mnemonic:true*)(*XXX REQUIRES ALT. CONSTRUCTOR*) ~label () in
      self#bind item ?key ?callback label;
      Gaux.may (submenu : MenuG.menu option) ~f:(fun m -> item#set_submenu m#as_menu)(*XXX Type mismatch*);
      item
    method add_check_item ?active ?key ?callback label =
      let item = CheckMenuItemG.check_menu_item ~label (*~use_mnemonic:true*)(*XXX*) ?active () in
      self#bind (item : CheckMenuItemG.check_menu_item :> MenuItemG.menu_item) label ?key
        ?callback:(Gaux.may_map callback ~f:(fun f () -> f item#get_active));
      item
    method add_radio_item ?group ?active ?key ?callback label =
      let item = RadioMenuItemG.radio_menu_item ~label (*~use_mnemonic:true*)(*XXX*) ?group ?active () in
      self#bind (item : RadioMenuItemG.radio_menu_item :> MenuItemG.menu_item) label ?key
        ?callback:(Gaux.may_map callback ~f:(fun f () -> f item#get_active));
      item
    method add_separator () = SeparatorMenuItemG.separator_menu_item ~packing:menu_shell#append ()
    method add_submenu ?key (label : string) =
      let item = MenuItemG.menu_item (*~use_mnemonic:true*)(*XXX*) ~label () in
      self#bind item ?key label;
      MenuG.menu ~packing:(fun m -> item#set_submenu m#as_menu)(*XXX Type mismatch*) ()
end
