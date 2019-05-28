#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/printexc.h>

#include "idahack.h"
#include "idadebug_common.h"
#include "idaside.hpp"

#define TRACER_OBJECT_FIELD(x)  Field(x,0)
#define TRACER_PROCESS_FIELD(x) Field(x,1)
#define TRACER_INIT_FIELD(x)    Field(x,2)

#define X86_EAX_FIELD(x) Field(x,0)
#define X86_ECX_FIELD(x) Field(x,1)
#define X86_EDX_FIELD(x) Field(x,2)
#define X86_EBX_FIELD(x) Field(x,3)
#define X86_ESP_FIELD(x) Field(x,4)
#define X86_EBP_FIELD(x) Field(x,5)
#define X86_ESI_FIELD(x) Field(x,6)
#define X86_EDI_FIELD(x) Field(x,7)

#define X86_CS_FIELD(x) Field(x, 8)
#define X86_DS_FIELD(x) Field(x, 9)
#define X86_ES_FIELD(x) Field(x,10)
#define X86_FS_FIELD(x) Field(x,11)
#define X86_GS_FIELD(x) Field(x,12)
#define X86_SS_FIELD(x) Field(x,13)

#define X86_SF_FIELD(x) Field(x,14)
#define X86_CF_FIELD(x) Field(x,15)
#define X86_PF_FIELD(x) Field(x,16)
#define X86_ZF_FIELD(x) Field(x,17)
#define X86_AF_FIELD(x) Field(x,18)
#define X86_OF_FIELD(x) Field(x,19)
#define X86_DF_FIELD(x) Field(x,20)

#define X86_NUM_FIELDS 21

int OCamlProcess(value tracer, value ea)
{
  CAMLparam2(tracer,ea);
  CAMLlocal1(ret);
  ret = caml_callback2_exn(TRACER_PROCESS_FIELD(tracer), TRACER_OBJECT_FIELD(tracer), ea);
  if(Is_exception_result(ret))
  {
    char buf[1024];
    char *exn = caml_format_exception(Extract_exception(ret));
    sprintf(buf, "[E] Tracer \"process\" callback threw exception (value %08lx): %s\n", Extract_exception(ret), exn);
    wrap_msg(buf);
    free(exn);
    CAMLreturnT(int,0);
  }
  CAMLreturnT(int,Int_val(ret));
}

char WrapOCamlProcess(void *ocaml_object, unsigned long ea)
{
  return OCamlProcess((value)ocaml_object, caml_copy_int32(ea));
}

value OCamlTrace(value tracer)
{
  char ret;
  CAMLparam1(tracer);
  CAMLlocal1(boole);
  
  ret = CreateTracer((void *)tracer);
  boole = ret ? Val_true : Val_false;

  CAMLreturn(boole);
}

int OCamlTracerInitialize(value tracer, struct RegisterState *rs)
{
  CAMLparam1(tracer);
  CAMLlocal2(init,ret);
  
  // Allocate and initialize the OCaml register state object
  init = caml_alloc_tuple(X86_NUM_FIELDS);
  X86_EAX_FIELD(init) = caml_copy_int32(rs->eax);
  X86_ECX_FIELD(init) = caml_copy_int32(rs->ecx);
  X86_EDX_FIELD(init) = caml_copy_int32(rs->edx);
  X86_EBX_FIELD(init) = caml_copy_int32(rs->ebx);
  X86_ESP_FIELD(init) = caml_copy_int32(rs->esp);
  X86_EBP_FIELD(init) = caml_copy_int32(rs->ebp);
  X86_ESI_FIELD(init) = caml_copy_int32(rs->esi);
  X86_EDI_FIELD(init) = caml_copy_int32(rs->edi);
                  
  X86_CS_FIELD(init) = caml_copy_int32(rs->cs);
  X86_DS_FIELD(init) = caml_copy_int32(rs->ds);
  X86_ES_FIELD(init) = caml_copy_int32(rs->es);
  X86_FS_FIELD(init) = caml_copy_int32(rs->fs);
  X86_GS_FIELD(init) = caml_copy_int32(rs->gs);
  X86_SS_FIELD(init) = caml_copy_int32(rs->ss);
                  
  X86_SF_FIELD(init) = Int_val(rs->sf);
  X86_CF_FIELD(init) = Int_val(rs->cf);
  X86_PF_FIELD(init) = Int_val(rs->pf);
  X86_ZF_FIELD(init) = Int_val(rs->zf);
  X86_AF_FIELD(init) = Int_val(rs->af);
  X86_OF_FIELD(init) = Int_val(rs->of);
  X86_DF_FIELD(init) = Int_val(rs->df);

  // Callback into OCaml to initialize
  ret = caml_callback_exn(TRACER_INIT_FIELD(tracer), init);
  if(Is_exception_result(ret))
  {
    char buf[1024];
    char *exn = caml_format_exception(Extract_exception(ret));
    sprintf(buf, "[E] Tracer initialization callback threw exception (value %08lx): %s\n", Extract_exception(ret), exn);
    wrap_msg(buf);
    free(exn);
  }
  CAMLreturnT(int,0);
}

void WrapOCamlTracerInitialize(void *ocaml_object, struct RegisterState *rs)
{
  OCamlTracerInitialize((value)ocaml_object, rs);
}

