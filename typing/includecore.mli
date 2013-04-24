(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Inclusion checks for the core language *)

open Typedtree
open Types

exception Dont_match

type type_mismatch =
    Arity
  | Privacy
  | New
  | Kind
  | Constraint
  | Manifest
  | Variance
  | Field_type of Ident.t
  | Field_mutable of Ident.t
  | Field_arity of Ident.t
  | Field_names of int * Ident.t * Ident.t
  | Field_missing of bool * Ident.t
  | Record_representation of bool

val value_descriptions:
    Env.t -> value_description -> value_description -> module_coercion
val type_declarations:
    ?equality:bool ->
      Env.t -> string ->
        type_declaration -> Ident.t -> type_declaration -> type_mismatch list
val exception_declarations:
    Env.t -> exception_declaration -> exception_declaration -> bool
(*
val class_types:
        Env.t -> class_type -> class_type -> bool
*)

val report_type_mismatch:
    string -> string -> string -> Format.formatter -> type_mismatch list -> unit
