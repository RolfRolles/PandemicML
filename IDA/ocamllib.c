#include <caml/memory.h>
#include <caml/alloc.h>

void OCamlRegisterGlobalRoot(value *root)   { caml_register_global_root(root); }
void   OCamlRemoveGlobalRoot(value *root)   {   caml_remove_global_root(root); }
void WrapOCamlRegisterGlobalRoot(void *val) { OCamlRegisterGlobalRoot((value *)val); }
void   WrapOCamlRemoveGlobalRoot(void *val) {   OCamlRemoveGlobalRoot((value *)val); }
