extern "C" {
#include "idahack.h"
#include "idagraph_c.h"
#include "ocamllib.h"
};

#include <windows.h>

#include <ida.hpp>
#include <idp.hpp>
#include <graph.hpp>
#include <kernwin.hpp>

#include <map>

struct OCamlGraph
{
  std::map<int, qstring> m_VertexText;
  void *ocaml_graph_structure_object;
};

static int idaapi callback(void *gr, int code, va_list va)
{
  OCamlGraph *graph = (OCamlGraph *)gr;
  int result = 0;

  switch ( code )
  {
    case grcode_user_refresh: // refresh user-defined graph nodes and edges
                              // in:  mutable_graph_t *g
                              // out: success
    {
      mutable_graph_t *g = va_arg(va, mutable_graph_t *);

      if ( g->empty() )
      {
        int size = CPPSizer(graph->ocaml_graph_structure_object);

        if(size!=0)
          g->resize(size);

        else
        {
          result = false;
          break;
        }
      }

      EdgeListEntry *curr, *head;
      head = CPPEdges(graph->ocaml_graph_structure_object);
      curr = head;
      while(curr)
      {
        g->add_edge(curr->label1, curr->label2, NULL);
        curr = curr->next;
      }
      delete_edge_list(head);
      result = true;
    }
    break;


    case grcode_user_gentext: // generate text for user-defined graph nodes
                              // in:  mutable_graph_t *g
                              // out: must return 0
    {
       mutable_graph_t *g = va_arg(va, mutable_graph_t *);

       graph->m_VertexText.clear();
       for ( node_iterator p=g->begin(); p != g->end(); ++p )
       {
         int n = *p;
         char *text = CPPText(graph->ocaml_graph_structure_object,n);
         graph->m_VertexText[n] = text != NULL ? text : "";
         if(text != NULL)
           c_strdel(text);
       }
       result = true;
    }
    break;

    case grcode_user_text:    // retrieve text for user-defined graph node
                              // in:  mutable_graph_t *g
                              //      int node
                              //      const char **result
                              //      bgcolor_t *bg_color (maybe NULL)
                              // out: must return 0, result must be filled
                              // NB: do not use anything calling GDI!
    {
       mutable_graph_t *g = va_arg(va, mutable_graph_t *);
       int node           = va_arg(va, int);
       const char **text  = va_arg(va, const char **);
       bgcolor_t *bgcolor = va_arg(va, bgcolor_t *);

       *text = graph->m_VertexText[node].c_str();
       if ( bgcolor != NULL )
         *bgcolor = DEFCOLOR;
       result = true;
       qnotused(g);
    }
    break;
    case grcode_destroyed:
    {
      WrapOCamlRemoveGlobalRoot(&graph->ocaml_graph_structure_object);
      delete graph;
    }

  }
  return result;
}

char CPPCreateGraphViewer(const char *title, void *data)
{
  HWND hwnd = NULL;
  TForm *form = create_tform(title, &hwnd);
  if ( hwnd != NULL )
  {
    OCamlGraph *g = new OCamlGraph;

    // get a unique graph id
    netnode id;
    id.create();

    REGISTERGLOBALROOT(g->ocaml_graph_structure_object, data);

    graph_viewer_t *gv = create_graph_viewer(form, id, callback, g, 0);
    open_tform(form, FORM_MDI|FORM_TAB|FORM_MENU);
    if ( gv != NULL )
    {
      viewer_fit_window(gv);
      return 1;
    }
    else
      WrapOCamlRemoveGlobalRoot(&g->ocaml_graph_structure_object);

  }
  else
    close_tform(form, 0);

  return 0;
}

extern "C" {

void CPPInitGraphViewer()
{
  CreateGraphViewer = &CPPCreateGraphViewer;
}

};