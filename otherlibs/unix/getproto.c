/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS

#ifndef _WIN32
#include <netdb.h>
#endif

static value alloc_proto_entry(struct protoent *entry)
{
  value res;
  value name = Val_unit, aliases = Val_unit;

  Begin_roots2 (name, aliases);
    name = copy_string(entry->p_name);
    aliases = copy_string_array((const char**)entry->p_aliases);
    res = caml_alloc_3(0, name, aliases,
                       Val_int(entry->p_proto));
  End_roots();
  return res;
}

CAMLprim value unix_getprotobyname(value name)
{
  struct protoent * entry;
  if (! caml_string_is_c_safe(name)) raise_not_found();
  entry = getprotobyname(String_val(name));
  if (entry == (struct protoent *) NULL) raise_not_found();
  return alloc_proto_entry(entry);
}

CAMLprim value unix_getprotobynumber(value proto)
{
  struct protoent * entry;
  entry = getprotobynumber(Int_val(proto));
  if (entry == (struct protoent *) NULL) raise_not_found();
  return alloc_proto_entry(entry);
}

#else

CAMLprim value unix_getprotobynumber(value proto)
{ invalid_argument("getprotobynumber not implemented"); }

CAMLprim value unix_getprotobyname(value name)
{ invalid_argument("getprotobyname not implemented"); }

#endif
