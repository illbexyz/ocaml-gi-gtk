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

open Gdk

(** Offscreen drawables *)

(** {3 Colors} *)

(** @gtkdoc gdk gdk-Colormaps-and-Colors *)
type color =
  [ `COLOR of Gdk.color
  | `WHITE
  | `BLACK
  | `NAME of string
  | `RGB of int * int * int]

val color : color -> Gdk.color

module Cairo : sig
  val create : window -> cairo
end

(** {3 GdkDragContext} *)

(** @gtkdoc gdk gdk-Drag-and-Drop *)
class drag_context : Gdk.drag_context ->
  object
    val context : Gdk.drag_context
    method status : ?time:int32 -> Tags.drag_action option -> unit
    method suggested_action : Tags.drag_action
    method targets : string list
  end
