#include <memory.h>
#include <compatibility.h>

#include <ida.hpp>
#include <idp.hpp>
#include <loader.hpp>
#include <kernwin.h>

value IDAOCaml_get_screen_ea(value unit)
{
  CAMLparam1(unit);
  CAMLreturn(caml_copy_int32(get_screen_ea()));
}

