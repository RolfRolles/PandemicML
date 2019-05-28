#ifndef __IDASIDE_HPP__
#define __IDASIDE_HPP__

typedef unsigned long ea_t;

ea_t wrap_get_screen_ea();
void wrap_set_cmt(ea_t, char *, char);
void wrap_set_xterior_cmt(ea_t, char *str, char);
void wrap_set_func_cmt(ea_t ea, char *str, char);
void wrap_msg(char *str);
void wrap_warning(char *str);
unsigned long wrap_get_byte(ea_t ea);
char wrap_put_byte(ea_t ea, unsigned long x);
char wrap_jumpto(ea_t ea);
unsigned long wrap_find_func_begin(ea_t ea);
char wrap_CompileLine(const char *line, char *err, unsigned long err_sz);
char wrap_add_idc_hotkey(char *hotkey,char *funcname);

// Move these into IDADebug namespace
unsigned long get_mem_byte(ea_t);
char wrap_step_into();

#endif
