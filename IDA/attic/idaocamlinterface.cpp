#include <memory.h>
#include <alloc.h>

#include <ida.hpp>
#include <kernwin.hpp>

value IDAOCaml_get_screen_ea(value unit)
{
  CAMLparam1(unit);
  CAMLreturn(caml_copy_int32(get_screen_ea()));
}

