#pragma once

void   WrapOCamlRemoveGlobalRoot(void *val);
void WrapOCamlRegisterGlobalRoot(void *val);

// Hours and hours worth of debugging, and it comes down to this single
// sentence which is clearly stated in the reference manual.  Note to self:
// read reference manual.

// 18.5.1 Registration of a global variable v is achieved by calling 
// caml_register_global_root(&v) just before a valid value is stored in v 
// for the first time.
#define REGISTERGLOBALROOT(lval,rval) \
{ lval = NULL;  \
  WrapOCamlRegisterGlobalRoot(&lval); \
  lval = rval;  }
