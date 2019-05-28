#pragma once

#include "idadebug_common.h"

char WrapOCamlProcess(void *ocaml_object, unsigned long ea);
void   WrapOCamlTracerInitialize(void *ocaml_object, RegisterState *rs);
