#include "idagraph_common.h"
#include "idagraph_ocaml.h"

int CPPSizer(void *graph_viewer)
{
  return WrapOCamlSizer(graph_viewer);
}

struct EdgeListEntry *CPPEdges(void *graph_viewer)
{
  return WrapOCamlEdges(graph_viewer);
}

char *CPPText(void *graph_viewer, int i)
{
  return WrapOCamlText(graph_viewer, i);
}

