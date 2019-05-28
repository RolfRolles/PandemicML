#include <ida.hpp>
#include <kernwin.hpp>
#include <bytes.hpp>
#include <funcs.hpp>
#include <expr.hpp>
#include <dbg.hpp>

extern "C" {

unsigned long wrap_get_screen_ea()
{
  return get_screen_ea();
}

void wrap_set_cmt(ea_t ea, char *str, bool b)
{
  set_cmt(ea, str, b);
}

void wrap_set_xterior_cmt(ea_t ea, char *str, bool isprev)
{
  describe(ea,isprev,"%s",str);
}

void wrap_set_func_cmt(ea_t ea, char *str, bool rpt)
{
  func_t *f;
  if((f = get_func(ea)) != NULL)
    set_func_cmt(f, str, rpt);
}

void wrap_msg(char *str)
{
  msg("%s", str);
}

void wrap_warning(char *str)
{
  warning("%s", str);
}

unsigned long wrap_find_func_begin(ea_t ea)
{
  func_t *f;
  return (f = get_func(ea)) != NULL ? f->startEA : -1;
}

unsigned long wrap_get_byte(ea_t ea)
{
  return get_byte(ea);
}

unsigned long wrap_put_byte(ea_t ea, unsigned long x)
{
  return put_byte(ea, x);
}


char wrap_jumpto(ea_t ea)
{
  return jumpto(ea);
}

unsigned long get_mem_byte(unsigned long ea)
{
  invalidate_dbgmem_contents(ea, 1);
  return get_byte(ea);
}

char wrap_CompileLine(const char *line, char *err, unsigned long err_sz)
{
  return CompileLine(line,err,err_sz,NULL);
}

char wrap_add_idc_hotkey(char *hotkey,char *funcname)
{
  return add_idc_hotkey(hotkey,funcname);
}

char wrap_del_idc_hotkey(char *hotkey)
{
  return del_idc_hotkey(hotkey);
}

char wrap_step_into()
{
  return step_into();
}

};