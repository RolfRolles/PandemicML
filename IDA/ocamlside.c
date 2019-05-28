#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/printexc.h>

#include <stdio.h>

#include "idaside.hpp"

#define Ea_val(ea) Int32_val(ea)

void IDAOCaml_init_interpreter(char **argv)
{
  caml_startup(argv);
  caml_callback(*caml_named_value("RolfInnerLoopInit"), Val_unit);
}

void IDAOCaml_invoke_interpreter()
{
  caml_callback(*caml_named_value("RolfInnerLoop"), Val_unit);
}

value IDAOCaml_get_screen_ea(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(ret);
  ret = caml_copy_int32(wrap_get_screen_ea());
  CAMLreturn(ret);
}

value IDAOCaml_set_cmt(value ea, value str, value rpt)
{
  CAMLparam3(ea,str,rpt);
  wrap_set_cmt(Ea_val(ea), String_val(str), Bool_val(rpt));
  CAMLreturn(Val_unit);
}

value IDAOCaml_set_xterior_cmt(value ea, value str, value rpt)
{
  CAMLparam3(ea,str,rpt);
  wrap_set_xterior_cmt(Ea_val(ea), String_val(str), Bool_val(rpt));
  CAMLreturn(Val_unit);
}

value IDAOCaml_set_func_cmt(value ea, value str, value rpt)
{
  CAMLparam3(ea,str,rpt);
  wrap_set_func_cmt(Ea_val(ea), String_val(str), Bool_val(rpt));
  CAMLreturn(Val_unit);
}

value IDAOCaml_msg(value str)
{
  CAMLparam1(str);
  wrap_msg(String_val(str));
  CAMLreturn(Val_unit);
}

value IDAOCaml_warning(value str)
{
  CAMLparam1(str);
  wrap_warning(String_val(str));
  CAMLreturn(Val_unit);
}

value IDAOCaml_find_func_begin(value ea)
{
  CAMLparam1(ea);
  CAMLlocal1(retval);
  ea_t fb;
  fb = wrap_find_func_begin(Ea_val(ea));
  if(fb != -1)
  {
    retval = caml_alloc_small(1, 0); 
    Field(retval, 0) = caml_copy_int32(fb); /* Some */
  }
  else
    retval = Val_int(0);
  CAMLreturn(retval);  
}

value IDAOCaml_get_byte(value ea)
{
  CAMLparam1(ea);
  CAMLlocal1(ret);
  ret = caml_copy_int32(wrap_get_byte(Ea_val(ea)));
  CAMLreturn(ret);
}

value IDAOCaml_put_byte(value ea, value x)
{
  CAMLparam2(ea,x);
  CAMLlocal1(ret);

  ret = caml_copy_int32(wrap_put_byte(Ea_val(ea), Int32_val(x)));

  if(ret)
    CAMLreturn(Val_int(1));

  else
    CAMLreturn(Val_int(0));
}

value IDAOCaml_jumpto(value ea)
{
  CAMLparam1(ea);
  CAMLlocal1(ret);
  
  ret = wrap_jumpto(Ea_val(ea));

  if(ret)
    CAMLreturn(Val_int(1));

  else
    CAMLreturn(Val_int(0));
}

value IDAOCaml_get_mem_byte(value ea)
{
  CAMLparam1(ea);
  CAMLlocal1(ret);
  ret = caml_copy_int32(get_mem_byte(Ea_val(ea)));
  CAMLreturn(ret);
}

value IDAOCaml_add_ocaml_hotkey(value hotkey, value n)
{
  char line[100], err[256], funcname[50];
  CAMLparam2(hotkey,n);
  
  sprintf(funcname, "__IDCOCaml%d", Int_val(n));
  sprintf(line, "static %s() { OCamlCallback(%d); }", funcname, Int_val(n));
  if(wrap_CompileLine(line,err,sizeof(err)))
  {
    // Returns 0 on success
    if(!wrap_add_idc_hotkey(String_val(hotkey),funcname))
      CAMLreturn(Val_int(1));
  }
  else
    wrap_msg(err);
  
  CAMLreturn(Val_int(0));
}

value IDAOCaml_del_ocaml_hotkey(value hotkey)
{
  CAMLparam1(hotkey);
  
  if(!wrap_del_idc_hotkey(String_val(hotkey)))
    CAMLreturn(Val_int(1));

  else
    CAMLreturn(Val_int(0));
}

void IDAOCaml_invoke_hotkey_callback(int i)
{
  CAMLlocal1(ret);

  ret = caml_callback_exn(*caml_named_value("HotkeyCallback"), Val_int(i));
  if(Is_exception_result(ret))
  {
    char buf[1024];
    char *exn = caml_format_exception(Extract_exception(ret));
    sprintf(buf, "[E] Function bound to hotkey (internal %d) threw exception (value %08lx): %s\n", 
      i, 
      Extract_exception(ret), 
      exn);
    wrap_msg(buf);
    free(exn);
  }
}

value IDAOCaml_step_into(value unit)
{
  CAMLparam1(unit);

  if(!wrap_step_into())
    CAMLreturn(Val_int(0));

  else
    CAMLreturn(Val_int(1));
}