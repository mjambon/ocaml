(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                             Martin Jambon                              *)
(*                                                                        *)
(*   Copyright 2025 Martin Jambon                                         *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
(**
   Manage warnings
*)

type status = On | Off | Error

(** Warnings that can be turned on and off on the command line *)
type conf = {
  illegal_backslash: status;
  unescaped_newline: status;
  missing_case: status;
}

(* Command-line processing *)

(** Read the argument of a [-warn-error] option *)
val add_warn_error_warnings : string -> unit

(** Read the argument of a [-warn-on] option *)
val add_warn_on_warnings : string -> unit

(** Read the argument of a [-warn-off] option *)
val add_warn_off_warnings : string -> unit

(** Get the warning statuses once the command-line arguments have been
    processed *)
val get_conf : unit -> conf

(** Print a warning or error message and a location.
    If [is_error] is true, the program will exit with code 4.

    The message may span multiple lines but should not be terminated by
    a newline. *)
val emit : status -> Syntax.location -> string -> unit
