#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/threads.h>
#include <caml/printexc.h>

#include "idahack.h"
#include "idagraph_common.h"

#define GRAPH_VIEWER_GRAPH_FIELD(x) Field(x,0)
#define GRAPH_VIEWER_SIZER_FIELD(x) Field(x,1)
#define GRAPH_VIEWER_TEXT_FIELD(x)  Field(x,2)
#define GRAPH_VIEWER_EDGES_FIELD(x) Field(x,3)
#define GRAPH_VIEWER_UNIQ_FIELD(x)  Field(x,4)


int OCamlSizer(value graph_viewer)
{
  CAMLparam1(graph_viewer);
  CAMLlocal1(ret);
  caml_leave_blocking_section();
  ret = caml_callback_exn(GRAPH_VIEWER_SIZER_FIELD(graph_viewer), GRAPH_VIEWER_GRAPH_FIELD(graph_viewer));
  if(Is_exception_result(ret))
  {
    char buf[1024];
    char *exn = caml_format_exception(Extract_exception(ret));
    sprintf(buf, "[E] Graph sizer callback threw exception.  Returning 0.\n Exception (value %08lx): %s\n", Extract_exception(ret), exn);
    wrap_msg(buf);
    free(exn);
    CAMLreturnT(int,0);
  }

  caml_enter_blocking_section();
  CAMLreturnT(int,Int_val(ret));
}

struct EdgeListEntry *c_list_of_ocaml_int_pair_list( value ml_list )
{
  struct EdgeListEntry *ret = NULL, *curr;
  CAMLparam1( ml_list );
  CAMLlocal1( head );

  while(ml_list != Val_emptylist)
  {
    head = Field(ml_list, 0);
    curr = new_edge_list_entry_vals(Int_val(Field(head,0)),Int_val(Field(head,1)));
    if(ret == NULL) ret = curr; else { curr->next = ret; ret = curr; }
    ml_list = Field(ml_list, 1);
  }

  CAMLreturnT(struct EdgeListEntry *, ret);
}

struct EdgeListEntry *OCamlEdges(value graph_viewer)
{
  struct EdgeListEntry *retval;
  CAMLparam1(graph_viewer);
  CAMLlocal1(ocedgelist);
  caml_leave_blocking_section();
  ocedgelist = caml_callback_exn(GRAPH_VIEWER_EDGES_FIELD(graph_viewer), GRAPH_VIEWER_GRAPH_FIELD(graph_viewer));

  if(Is_exception_result(ocedgelist))
  {
    char buf[1024];
    char *exn = caml_format_exception(Extract_exception(ocedgelist));
    sprintf(buf, "[E] Graph edges callback threw exception; returning [].\n Exception (value %08lx): %s\n", Extract_exception(ocedgelist), exn);
    wrap_msg(buf);
    free(exn);
    CAMLreturnT(struct EdgeListEntry *,NULL);
  }

  retval = c_list_of_ocaml_int_pair_list(ocedgelist);
  caml_enter_blocking_section();
  CAMLreturnT(struct EdgeListEntry *,retval);
}

char *OCamlText(value graph_viewer, value i)
{
  char *retval;
  CAMLparam2(graph_viewer, i);
  CAMLlocal1(ocstring);
  caml_leave_blocking_section();
  ocstring = caml_callback2_exn(GRAPH_VIEWER_TEXT_FIELD(graph_viewer), GRAPH_VIEWER_GRAPH_FIELD(graph_viewer), i);
  if(Is_exception_result(ocstring))
  {
    char buf[1024];
    char *exn = caml_format_exception(Extract_exception(ocstring));
    sprintf(buf, "[E] Graph text callback threw exception; returning NULL.\n Exception (value %08lx): %s\n", Extract_exception(ocstring), exn);
    wrap_msg(buf);
    free(exn);
    CAMLreturnT(char *,NULL);
  }
  retval = c_strdup(String_val(ocstring));
  caml_enter_blocking_section();
  CAMLreturnT(char *,retval);
}

value OCamlShowGraph(value graph_viewer, value str)
{
  char ret;
  CAMLparam2(str, graph_viewer);
  CAMLlocal1(boole);
  caml_leave_blocking_section();  
  ret = CreateGraphViewer(String_val(str),(void *)graph_viewer);
  caml_enter_blocking_section();
  boole = ret ? Val_true : Val_false;
  CAMLreturn(boole);
}

int WrapOCamlSizer(void *graph_viewer)
{
  return OCamlSizer((value)graph_viewer);
}

struct EdgeListEntry *WrapOCamlEdges(void *graph_viewer)
{
  return OCamlEdges((value)graph_viewer);
}

char *WrapOCamlText(void *graph_viewer, int i)
{
  return OCamlText((value)graph_viewer, Val_int(i));
}

