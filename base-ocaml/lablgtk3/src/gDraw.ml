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

open Gaux
open Gobject
open Gdk

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

module Cairo = Cairo

class drag_context context = object
  val context = context
  method status ?(time=Int32.zero) act = DnD.drag_status context act ~time
  method suggested_action = DnD.drag_context_suggested_action context
  method targets = List.map Gdk.Atom.name (DnD.drag_context_targets context)
end
