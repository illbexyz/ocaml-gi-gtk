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
open Gaux
open Gobject
open Gtk

type color = [
  | `COLOR of Gdk.color
  | `WHITE
  | `BLACK
  | `NAME of string
  | `RGB of int * int * int
]

let color (c : color) =
  match c with
  | `COLOR col -> col
  | `WHITE -> Gdk.Color.color_parse "white"
  | `BLACK -> Gdk.Color.color_parse "black"
  | `NAME s -> Gdk.Color.color_parse s
  | `RGB (r,g,b) -> Gdk.Color.color_parse (Printf.sprintf "#%04X%04X%04X" r g b)

module Cairo = Gdk.Cairo

(* GObject *)

class ['a] gobject_signals (obj : 'a obj) = object
  (*val obj : 'a obj = obj*)
  val after = false
  method after = {< after = true >}
  method private connect : 'b. ('a,'b) GtkSignal.t -> callback:'b -> _ =
    fun sgn ~callback -> GtkSignal.connect obj ~sgn ~after ~callback
  method private notify : 'b. ('a, 'b) property -> callback:('b -> unit) -> _ =
    fun prop ~callback -> GtkSignal.connect_property obj ~prop ~callback
end

class gobject_ops obj = object
  method get_oid = get_oid obj
  method get_type = Type.name (get_type obj)
  method disconnect = GtkSignal.disconnect obj
  method handler_block = GtkSignal.handler_block obj
  method handler_unblock = GtkSignal.handler_unblock obj
  method set_property : 'a. string -> 'a data_set -> unit =
    Property.set_dyn obj
  method get_property = Property.get_dyn obj
  method freeze_notify () = Property.freeze_notify obj
  method thaw_notify () = Property.thaw_notify obj
end

(* GtkObject *)

(*
class type ['a] objvar =
  object val obj : 'a obj end
*)

class gtkobj obj = object
  method get_oid = get_oid obj
end

class type gtkobj_signals =
  object ('a) method after : 'a end

let pack_return self ~packing ~show =
  may packing ~f:(fun f -> ((f self) : unit));
  if show <> Some false then self#show;
  self
