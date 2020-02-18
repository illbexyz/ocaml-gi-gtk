(**************************************************************************)
(*     Lablgtk - Applications                                             *)
(*                                                                        *)
(*    * You are free to do anything you want with this code as long       *)
(*      as it is for personal use.                                        *)
(*                                                                        *)
(*    * Redistribution can only be "as is".  Binary distribution          *)
(*      and bug fixes are allowed, but you cannot extensively             *)
(*      modify the code without asking the authors.                       *)
(*                                                                        *)
(*    The authors may choose to remove any of the above                   *)
(*    restrictions on a per request basis.                                *)
(*                                                                        *)
(*    Authors:                                                            *)
(*      Jacques Garrigue <garrigue@kurims.kyoto-u.ac.jp>                  *)
(*      Benjamin Monate  <Benjamin.Monate@free.fr>                        *)
(*      Olivier Andrieu  <oandrieu@nerim.net>                             *)
(*      Jun Furuse       <Jun.Furuse@inria.fr>                            *)
(*      Hubert Fauque    <hubert.fauque@wanadoo.fr>                       *)
(*      Koji Kagawa      <kagawa@eng.kagawa-u.ac.jp>                      *)
(*                                                                        *)
(**************************************************************************)

(* $Id$ *)

let get_files_in_directory dir =
  try
  let dirh = Unix.opendir dir in
  let rec get_them () =
    try
      let x = Unix.readdir dirh in
      x :: get_them ()
    with
      _ -> Unix.closedir dirh; [] 
  in
    List.sort String.compare (get_them ())
  with Unix.Unix_error _ -> []

open Unix

let is_directory name =
  try
    (stat name).st_kind = S_DIR
  with _ -> false

let get_directories_in_files ~path =
  List.filter (fun x -> is_directory  (path ^ "/" ^ x))

(************************************************** Subshell call *)
let subshell ~cmd =
  let rc = open_process_in cmd in
  let rec it () =
    try 
      let x = input_line rc in x :: it ()
    with _ -> []
  in 
  let answer = it () in
  ignore (close_process_in rc);
  answer
