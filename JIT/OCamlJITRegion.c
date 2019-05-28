#include <caml/memory.h>
#include <caml/alloc.h>

#include "CPPJITRegion.h"

#include <stdio.h>

#define PAGE_SIZE 0x1000
value OCaml_JITRegion_allocate(value sz)
{
  CAMLparam1 (sz);
  CAMLlocal2 (retval,str);
  int size;
  unsigned long memaddr;
  
  size = (Int_val(sz) + (PAGE_SIZE-1)) & (~(PAGE_SIZE-1));
  memaddr = CPP_allocate(size);
  if(memaddr == 0)
    retval = Val_int(0);

  else
  {
    retval = caml_alloc(1,0);
    str = caml_alloc_tuple(2);
    Store_field(str, 0,caml_copy_int32(memaddr));
    Store_field(str, 1,Val_int(size));
    Store_field(retval,0,str);
  }
  CAMLreturn(retval);
}

#define GET_INT32(a,i) Int32_val(Field(a,i))
#define GET_ADDR(t) GET_INT32(t,0)
#define GET_SIZE(t) Int_val(Field(t,1))

value OCaml_JITRegion_deallocate(value t)
{
  CAMLparam1(t);
  CPP_deallocate(GET_ADDR(t));
  CAMLreturn0;
}

value OCaml_JITRegion_blit_at(value t, value vpos, value varray)
{
  CAMLparam3(t,vpos,varray);
  unsigned long addr, arrlen,i;
  char bret;
  char *c_array;

  arrlen = Wosize_val(varray);
  c_array = malloc(arrlen);
  for(i = 0; i < arrlen; ++i)
    c_array[i] = (GET_INT32(varray,i)) & 0xFF;
  
  bret = CPP_blit_at(GET_ADDR(t), GET_SIZE(t), Int_val(vpos), c_array, arrlen);
  free(c_array);
  CAMLreturn(Val_bool(bret));
}

typedef void (__stdcall *JITFP)(void *in, void *out);
typedef void (__stdcall *JITVFP)();

void invoke_jitted_region(void *in, void *out, JITFP fp) { fp(in,out); }
void invoke_jitted_region_alternate(JITVFP fp) { fp(); }

typedef struct 
{
  unsigned long lhs_in;
  unsigned long rhs_in;
  unsigned long input3;
  unsigned long flags_in;
} JITInput;

typedef struct 
{
  unsigned long lhs_out;
  unsigned long output2;
  unsigned long flags_out;
} JITOutput;

typedef struct
{
  unsigned long espval;
  unsigned long regs[8];
  unsigned long eflags;
} JITStateStruct;

#define N_FIRSTREG 0
#define N_EAX    0
#define N_ECX    1
#define N_EDX    2
#define N_EBX    3
#define N_ESP    4
#define N_EBP    5
#define N_ESI    6
#define N_EDI    7
#define N_LASTREG 7
#define N_EFLAGS 8
#define N_LAST   8

typedef struct
{
  JITStateStruct states[3];	
} JITStateContainer;

/*
*/

value OCaml_JITRegion_read_output_ctx(value t)
{
  CAMLparam1(t);
  CAMLlocal1(outstate);
  unsigned long jit_where;
  JITStateContainer *cont;
  JITStateStruct *output; 
  int i;

  jit_where = GET_ADDR(t);  
  cont = (JITStateContainer *)jit_where;
  output = &cont->states[2];

  outstate = caml_alloc_tuple(N_LAST+1);
  for(i = N_EAX; i <= N_EDI; ++i)
    if(i != N_ESP)
      Store_field(outstate,i,caml_copy_int32(output->regs[N_LASTREG-i]));
  
  Store_field(outstate,N_ESP,caml_copy_int32(output->espval));
  
  Store_field(outstate,N_EFLAGS,caml_copy_int32(output->eflags));
  CAMLreturn(outstate);
}

value OCaml_JITRegion_write_input_ctx(value t, value ctx)
{
  CAMLparam2(t,ctx);
  unsigned long jit_where;
  JITStateContainer *cont;
  JITStateStruct *input;
  int i;

  jit_where = GET_ADDR(t);
  cont = (JITStateContainer *)jit_where;
  input = &cont->states[1];

  for(i = N_EAX; i <= N_EDI; ++i)
    input->regs[N_LASTREG-i] = GET_INT32(ctx,i);
  input->espval = GET_INT32(ctx,N_ESP);
  input->eflags = GET_INT32(ctx,N_EFLAGS);
  
  CAMLreturn0;
}

value OCaml_JITRegion_execute_at_complex(value t, value pos)
{
  CAMLparam2(t,pos);

  int c_pos;
  unsigned long jit_where;
  JITVFP fp;
  
  c_pos = Int_val(pos);
  jit_where = GET_ADDR(t) + c_pos;
  fp = (JITVFP)jit_where;

  fp();

  CAMLreturn0;
}

value OCaml_JITRegion_execute_at(value t, value pos, value instate)
{
  CAMLparam3(t,pos,instate);
  CAMLlocal1(outstate);
  JITInput instruct;
  JITOutput outstruct;
  int c_pos;
  unsigned long jit_where;
  JITFP fp;
  
  c_pos = Int_val(pos);
  jit_where = GET_ADDR(t) + c_pos;
  fp = (JITFP)jit_where;
  memset(&outstruct, 0, sizeof(outstruct));

  instruct.lhs_in = GET_INT32(instate,0);
  instruct.rhs_in = GET_INT32(instate,1);
  instruct.input3 = GET_INT32(instate,2);
  instruct.flags_in = Int_val(Field(instate,3));
/*
  printf("instruct: lhs_in %lx rhs_in %lx input3 %lx flags_in %lx\n",
    instruct.lhs_in,
    instruct.rhs_in,
    instruct.input3,
    instruct.flags_in);
*/
  invoke_jitted_region(&instruct,&outstruct,fp);
/*
  printf("outstruct: lhs_out %lx output2 %lx flags_out %lx\n",
    outstruct.lhs_out,
    outstruct.output2,
    outstruct.flags_out);
*/
  outstate = caml_alloc_tuple(3);
  Store_field(outstate,0,caml_copy_int32(outstruct.lhs_out));
  Store_field(outstate,1,caml_copy_int32(outstruct.output2));
  Store_field(outstate,2,Val_int(outstruct.flags_out));
  CAMLreturn(outstate);
}