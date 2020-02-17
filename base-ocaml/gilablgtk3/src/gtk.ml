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

open Gobject

exception Error of string

(*
type 'a optobj = 'a obj Gpointer.optboxed
type clampf = float
*)

module Tags = struct
  type expand_type = [ `X|`Y|`BOTH|`NONE ]
  include (GtkEnums : module type of GtkEnums
                      with module Conv := GtkEnums.Conv)
  type position = position_type
end
open Tags

(*
type gtk_class
*)

type accel_group
type clipboard

type style = [`style] obj
(*
type 'a group = 'a obj option

type statusbar_message
type statusbar_context
*)
type selection_data

type rectangle  = { x: int; y: int; width: int; height: int }
type target_entry = { target: string; flags: target_flags list; info: int }
(*
type box_packing =
    { expand: bool; fill: bool; padding: int; pack_type: pack_type }
*)

type orientable = [`giu|`orientable]
type adjustment = [`giu|`adjustment]
(*
type tooltips = [`giu|`tooltips]
*)
type widget = [`giu|`widget]
type container = [widget|`container]
type container' = container obj
type bin = [container|`bin]
type invisible = [bin|`invisible]
type window = [bin|`window]
type dialog = [window|`dialog]
type message_dialog = [dialog|`messagedialog]
type plug = [window|`plug]
type socket = [container|`socket]


type icon_source
type icon_set
type icon_factory = [`iconfactory] obj

(*
type size_group = [`sizegroup] obj
*)
type about_dialog = [dialog|`aboutdialog]

(* New widgets in 2.12 *)
type tooltip = [`tooltip] obj

(* re-export Gobject.obj *)
type 'a obj = 'a Gobject.obj
  (* constraint 'a = [> `giu] *)
  (* *Props modules break this *)
