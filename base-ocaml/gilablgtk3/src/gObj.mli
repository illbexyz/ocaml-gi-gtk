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

open Gtk

(** {3 Colors} *)

module Cairo : sig
  val create : Gdk.window -> Gdk.cairo
end

(** Base classes for objects and widgets *)

(** {3 GObject} *)

class gobject_ops : 'a Gobject.obj ->
  object
    method get_oid : int
    method get_type : string
    method disconnect : GtkSignal.id -> unit
    method handler_block : GtkSignal.id -> unit
    method handler_unblock : GtkSignal.id -> unit
    method set_property : 'a. string -> 'a Gobject.data_set -> unit
    method get_property : string -> Gobject.data_get
    method freeze_notify : unit -> unit
    method thaw_notify : unit -> unit
  end

class ['a] gobject_signals : 'a Gobject.obj ->
  object ('b)
    val after : bool
    method after : 'b
    method private connect :
      'c. ('a,'c) GtkSignal.t -> callback:'c -> GtkSignal.id
    method private notify :
      'b. ('a, 'b) Gobject.property -> callback:('b -> unit) -> GtkSignal.id
  end

(** {3 GtkObject} *)

(*
class type ['a] objvar = object
  (* needed for pre 3.10
  method private obj : 'a Gobject.obj
  *)
end
*)

class gtkobj : 'a Gobject.obj ->
  object
    method get_oid : int
  end

class type gtkobj_signals =
  object ('a) method after : 'a end

(** @gtkdoc gdk gdk-Colormaps-and-Colors *)
type color =
  [ `COLOR of Gdk.color
  | `WHITE
  | `BLACK
  | `NAME of string
  | `RGB of int * int * int]

val color : color -> Gdk.color

val pack_return :
 (< show : unit; ..> as 'a) ->
   packing:('a -> unit) option -> show:bool option -> 'a
    (* To use in initializers to provide a ?packing: option *)
