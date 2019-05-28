#ifndef __IDATOP_OCAML_H__
#define __IDATOP_OCAML_H__

#include <mlvalues.h>

CAMLprim value OCaml_ReaderCallbackNowait(value buffer, value length);
CAMLprim value OCaml_WriteBuffer(value buffer, value offset, value length);
CAMLprim value OCaml_FlushBuffer(value unit);

#endif
