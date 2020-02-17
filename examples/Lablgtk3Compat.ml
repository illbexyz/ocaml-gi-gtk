open GIGtk

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

let pack (box : #BoxG.box) ?(expand=false) ?(fill=true) ?(padding=0) w =
 box#pack_start w expand fill padding

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

let print_widget ppf (o : #widget) = 
    Format.fprintf ppf "<%s@@0x%x>" o#misc#get_type o#get_oid

class ['a] memo () = object
  val tbl : (int, 'a) Hashtbl.t = Hashtbl.create 7
  method add (obj : 'a) = Hashtbl.add tbl obj#get_oid obj
  method find (obj : widget) = Hashtbl.find tbl obj#get_oid
  method remove (obj : widget) = Hashtbl.remove tbl obj#get_oid
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
