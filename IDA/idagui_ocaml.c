#include <caml/memory.h>
#include <caml/alloc.h>

value IDAOCaml_askfile_c(value b, value str)
{
  CAMLparam1(str);
  CAMLlocal1(retval);

  const char *ret = wrap_askfile_c(Int_val(b), String_val(str));

  if(ret != NULL)
  {
    retval = caml_alloc_small(1, 0); 
    Field(retval, 0) = caml_copy_string(ret); /* Some */
  }
  else
    retval = Val_int(0);

  CAMLreturn(retval);
}
