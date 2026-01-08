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
(*
   Manage warnings
*)

open Printf

type t =
  | Illegal_backslash
  | Unescaped_newline
  | Missing_case

type status = On | Off | Error

module Warnings = Set.Make (struct type nonrec t = t let compare = compare end)

type conf = {
  illegal_backslash: status;
  unescaped_newline: status;
  missing_case: status;
}

(*
   Globals populated during command-line parsing and read by 'get_conf'

   Inconsistencies such as requesting both '-warn-off foo' and
   'warn-error foo' are reported during 'get_conf ()'.
*)
let warn_error = ref Warnings.empty
let warn_on = ref Warnings.empty
let warn_off = ref Warnings.empty

let show_warning = function
  | Illegal_backslash -> "illegal-backslash"
  | Unescaped_newline -> "unescaped-newline"
  | Missing_case -> "missing-case"

let cli_error msg =
  eprintf
    "invalid ocamllex command:\n%s\n%!"
    msg;
  exit 3

let is_malformed str =
  let looks_like_an_option =
    str <> "" && str.[0] = '-'
  in
  let contains_bad_characters =
    String.for_all (function 'a'..'z' | '-' -> false | _ -> true) str
  in
  looks_like_an_option || contains_bad_characters

let parse_warning str : t list =
  match str with
  | "illegal-backslash" -> [Illegal_backslash]
  | "unescaped-newline" -> [Unescaped_newline]
  | "missing-case" -> [Missing_case]
  | "all" -> [Illegal_backslash; Unescaped_newline; Missing_case]
  | _ ->
      if is_malformed str then
        cli_error (sprintf "malformed warning: '%s'" str)
      else
        (* silently ignore future or misspelled warnings *)
        []

let parse_comma_sep_warnings str : t list =
  String.split_on_char ',' str
  |> List.concat_map parse_warning

let add_warning acc str =
  acc :=
    Warnings.union !acc
      (Warnings.of_list (parse_comma_sep_warnings str))

let add_warn_error_warnings str =
  add_warning warn_error str

let add_warn_on_warnings str =
  add_warning warn_on str

let add_warn_off_warnings str =
  add_warning warn_off str

let determine_warning_status ?(default = On) warning : status =
  match Warnings.mem warning !warn_on,
        Warnings.mem warning !warn_off,
        Warnings.mem warning !warn_error with
  | false, false, false -> default
  | true, false, false -> On
  | false, true, false -> Off
  | false, false, true -> Error
  | _ ->
      cli_error (
        sprintf
          "don't know how to treat warning '%s'"
          (show_warning warning)
      )

let get_conf () : conf =
  {
    illegal_backslash = determine_warning_status Illegal_backslash;
    unescaped_newline = determine_warning_status Unescaped_newline;
    missing_case = determine_warning_status Missing_case;
  }

let emit (status : status) loc msg =
  match status with
  | Off -> ()
  | _ ->
      let kind, is_error =
        match status with
        | Error -> "error", true
        | _ -> "warning", false
      in
      eprintf
        "ocamllex %s:\n\
         %s: %s\n"
        kind
        (Syntax.show_location loc) msg;
      flush stderr;
      if is_error then
        exit 4
