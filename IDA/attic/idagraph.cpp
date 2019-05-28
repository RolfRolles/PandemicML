/************************************************
 *
 * Author: Rolf Rolles
 * Date: 2.12.2010
 *
 ************************************************/

#include <windows.h>

#include <ida.hpp>
#include <idp.hpp>
#include <loader.hpp>
#include <graph.hpp>
#include <allins.hpp>

#include <map>
#include <list>

/*********************************************************************
* Function: init
*
* init is a plugin_t function. It is executed when the plugin is
* initially loaded by IDA.
* Three return codes are possible:
*    PLUGIN_SKIP - Plugin is unloaded and not made available
*    PLUGIN_KEEP - Plugin is kept in memory
*    PLUGIN_OK   - Plugin will be loaded upon 1st use
*
* Check are added here to ensure the plug-in is compatible with
* the current disassembly.
*********************************************************************/
int init(void)
{
	return PLUGIN_OK;
}

/*********************************************************************
* Function: term
*
* term is a plugin_t function. It is executed when the plugin is
* unloading. Typically cleanup code is executed here.
*********************************************************************/
void term(void)
{

	return;
}

struct BlockComponent
{
  bool operator < (BlockComponent rhs)
  {
    return startEA < rhs.startEA;
  }
  bool operator== (BlockComponent rhs)
  {
    return startEA == rhs.startEA;
  }
  ea_t startEA;
  ea_t endEA;
};

enum EdgeType
{
  CondTrue,
  CondFalse,
  Uncond
};

struct Edge
{
  EdgeType type;
  int src, dest;
};

struct Vertex
{
  std::vector<BlockComponent> m_Constituents;
  std::list<ea_t> m_Instructions;
};

struct Graph
{
  ea_t m_StartAddress;
  std::vector<Vertex> m_Vertices;
  graph_viewer_t *m_Viewer;
  std::map<int, std::list<std::pair<int, EdgeType> > > m_Edges;
  std::map<int, qstring> m_VertexText;
};

void print_graph(Graph *g)
{
  msg("%lx:  graph begins\n", g->m_StartAddress);
  int n = 0;
  for(std::vector<Vertex>::iterator i = g->m_Vertices.begin(); i != g->m_Vertices.end(); ++i, ++n)
  {
    if(i->m_Instructions.empty())
	  msg("Empty vertex %d\n", n);

	else
	{
      msg("Vertex %d\n", n);
	  for(std::list<ea_t>::iterator l = i->m_Instructions.begin(); l != i->m_Instructions.end(); ++l)
      {
        char buf[MAXSTR];
        generate_disasm_line(*l, buf, MAXSTR);
        tag_remove(buf, buf, MAXSTR);
        msg("%lx: %s\n", *l, buf);
      }
	}

	std::map<int, std::list<std::pair<int, EdgeType> > >::iterator j = g->m_Edges.find(n);
	if(j == g->m_Edges.end())
	{
	  msg("[no edges]\n");
	}
	else
	{
	  for(std::list<std::pair<int, EdgeType> >::iterator k = j->second.begin(); k != j->second.end(); ++k)
	  {
	    const char *et = "wtf?";
	    switch(k->second)
		{
		  case Uncond: et = "unconditional"; break;
		  case CondTrue: et = "conditional (true)"; break;
		  case CondFalse: et = "conditional (false)"; break;
		}
		msg("%d -> %d %s\n", n, k->first, et);
	  }
	}
  }
}

typedef std::map<int, std::list<int> > blocks;

bool can_collapse_predecessor(const gdl_graph_t * const g, int i)
{
  return 
   (g != NULL &&
    g->size() > 0 &&
    g->npred(i) == 1 &&
    g->nsucc(g->pred(i,0)) == 1);
}

bool is_uncond_jump(ea_t ea)
{
  // xrefblk_t x;

  // return (x.first_from(ea, XREF_ALL) && x.iscode && (x.type == fl_JF || x.type == fl_JN) && !x.next_from());
  ua_ana0(ea);
  return (cmd.itype == NN_jmp || cmd.itype == NN_jmpni || cmd.itype == NN_jmpfi);
}

void get_branch_targets_and_types(ea_t ea, std::list<std::pair<ea_t, EdgeType> > &list)
{
  if(is_uncond_jump(ea))
  {
    list.push_back(std::make_pair<ea_t, EdgeType>(get_first_cref_from(ea), Uncond));
    return;
  }
	
  xrefblk_t x;
  bool ok = x.first_from(ea, XREF_ALL);
  while(ok)
  {
    if(x.iscode)
	  list.push_back(std::make_pair<ea_t, EdgeType>(x.to, x.to == ea + get_item_size(ea) ? CondFalse : CondTrue));

	ok = x.next_from();  
  }
}

void coalesce_step_one(qflow_chart_t *graph, blocks &newblocks)
{
  std::map<int, int> newparent2child;
  std::list<int> cant_merge;
  
  // Parent # -> child #
  for(int i = 0; i < graph->node_qty(); ++i)
    if(can_collapse_predecessor(graph,i))
      newparent2child[graph->pred(i,0)] = i;
	else
	  cant_merge.push_back(i);

  /* Walk from all top-nodes forwards */
  while(!cant_merge.empty())
  {
    int current_top = cant_merge.front(); cant_merge.pop_front();

    /* The block that we will put all subsequent entries into is this one */
    /* The number that we are looking for in the map is this one */
    int block = current_top, currno = current_top;
    
	while(true)
    {
//      msg("Looking to see whether %d has a child in the map\n", currno);
      // Try to find the child in the newparent2child map
	  std::map<int, int>::iterator j = newparent2child.find(currno);
      
	  // If it does not exist...
	  if(j == newparent2child.end())
		break;
      
      // If the child did exist, then roll it up into the parent
	  newblocks[block].push_back(j->first);
      currno = j->second;
    }
	newblocks[block].push_back(currno);
  }
}

Graph *make_collapsed_graph(ea_t where)
{
  qflow_chart_t *graph = new qflow_chart_t("Deobfuscated graph", get_func(where), BADADDR, BADADDR, FC_PREDS);
  
  blocks newblocks;

  coalesce_step_one(graph, newblocks);
  


  Graph *myg = new Graph;
  myg->m_StartAddress = where;
  myg->m_Viewer = NULL;
  myg->m_Vertices.resize(graph->node_qty());

  std::vector<int> is_own_block;
  is_own_block.resize(graph->node_qty());

  for(int i = 0; i < graph->node_qty(); ++i)
    is_own_block[i] = 1;

  std::map<int, int> idavertex2myvertex;
  int n = 0;
  BlockComponent bc;
  for(std::map<int, std::list<int> >::iterator i = newblocks.begin(); i != newblocks.end(); ++i, ++n)
  {
	for(std::list<int>::iterator j = i->second.begin(); j != i->second.end(); ++j)
	{
	  bc.startEA = graph->blocks[*j].startEA;
	  bc.endEA   = graph->blocks[*j].endEA;
      myg->m_Vertices[n].m_Constituents.push_back(bc);
	  is_own_block[*j] = 0;
	}
	idavertex2myvertex[i->first] = n;
    is_own_block[i->first] = 2;
  }
  for(int i = 0; i < graph->node_qty(); ++i)
  {
    if(is_own_block[i] == 1)
    {
      bc.startEA = graph->blocks[i].startEA;
      bc.endEA   = graph->blocks[i].endEA;
      myg->m_Vertices[n].m_Constituents.push_back(bc);
      idavertex2myvertex[i] = n++;
    }
  }

  // msg("%d vertices total\n", n);
  myg->m_Vertices.resize(n);

  for(int i = 0; i != myg->m_Vertices.size(); ++i)
    for(std::vector<BlockComponent>::iterator j = myg->m_Vertices[i].m_Constituents.begin(); j != myg->m_Vertices[i].m_Constituents.end(); ++j)
      for(ea_t beg = j->startEA; beg < j->endEA; beg = beg + get_item_size(beg))
        myg->m_Vertices[i].m_Instructions.push_back(beg);

  // Start Address -> Vertex
  std::map<ea_t, int> address2block;
  for(int i = 0; i < graph->node_qty(); ++i)
    address2block[graph->blocks[i].startEA] = i;

  for(int i = 0; i < myg->m_Vertices.size(); ++i)
  {
    ea_t last_insn;
    if(myg->m_Vertices[i].m_Instructions.empty())
      continue;

    last_insn = *(myg->m_Vertices[i].m_Instructions.rbegin());
    std::list<std::pair<ea_t, EdgeType> > list;
    get_branch_targets_and_types(last_insn, list);
    for(std::list<std::pair<ea_t, EdgeType> >::iterator j = list.begin(); j != list.end(); ++j)
    {
      std::map<ea_t, int>::iterator it = address2block.find(j->first);
      if(it == address2block.end())
      {
        msg("%lx:  can't find destination vertex in address2block\n", j->first);
        continue;
      }
      int dest_vertex_ida_num = it->second;
      std::map<int, int>::iterator itt = idavertex2myvertex.find(dest_vertex_ida_num);
      if(itt == idavertex2myvertex.end())
      {
        msg("%d:  can't find destination vertex in my graph (%lx)\n", dest_vertex_ida_num, graph->blocks[dest_vertex_ida_num].startEA);
        continue;
      }
      int dest_vertex_num = itt->second;
      msg("Adding edge %d->%d (%d)\n", i, dest_vertex_num, j->second);
      myg->m_Edges[i].push_back(std::make_pair<ea_t, EdgeType>(dest_vertex_num, j->second));
    }
    // Erase all unconditional jumps
    myg->m_Vertices[i].m_Instructions.erase(
      std::remove_if(
        myg->m_Vertices[i].m_Instructions.begin(),
        myg->m_Vertices[i].m_Instructions.end(),
        is_uncond_jump),
      myg->m_Vertices[i].m_Instructions.end());
  }
  delete graph;
  return myg;
}

qstring generate_vertex_text(Graph *graph, int which)
{
  qstring out;
  for(std::list<ea_t>::iterator i = graph->m_Vertices[which].m_Instructions.begin(); i != graph->m_Vertices[which].m_Instructions.end(); ++i)
  {
    char dbuf[MAXSTR];
    generate_disasm_line(*i, dbuf, sizeof(dbuf));
    out += dbuf;
  }
  return out;
}

static int idaapi callback(void *gr, int code, va_list va)
{
  Graph *graph = (Graph *)gr;
  int result = 0;
  switch ( code )
  {
    case grcode_calculating_layout:
                              // calculating user-defined graph layout
                              // in: mutable_graph_t *g
                              // out: 0-not implemented
                              //      1-graph layout calculated by the plugin
      msg("calculating graph layout...\n");
      break;

    case grcode_changed_current:
                              // a new graph node became the current node
                              // in:  graph_viewer_t *gv
                              //      int curnode
                              // out: 0-ok, 1-forbid to change the current node
     {
       graph_viewer_t *v = va_arg(va, graph_viewer_t *);
       int curnode       = va_argi(va, int);
       msg("%x: current node becomes %d\n", v, curnode);
     }
     break;

    case grcode_clicked:      // a graph has been clicked
                              // in:  graph_viewer_t *gv
                              //      selection_item_t *current_item
                              // out: 0-ok, 1-ignore click
     {
       /*graph_viewer_t *v   =*/ va_arg(va, graph_viewer_t *);
       va_arg(va, selection_item_t *);
       graph_item_t *m = va_arg(va, graph_item_t *);
       msg("clicked on ");
       switch ( m->type )
       {
         case git_none:
           msg("background\n");
           break;
         case git_edge:
           msg("edge (%d, %d)\n", m->e.src, m->e.dst);
           break;
         case git_node:
           msg("node %d\n", m->n);
           break;
         case git_tool:
           msg("toolbutton %d\n", m->b);
           break;
         case git_text:
           msg("text (x,y)=(%d,%d)\n", m->p.x, m->p.y);
           break;
         case git_elp:
           msg("edge layout point (%d, %d) #%d\n", m->elp.e.src, m->elp.e.dst, m->elp.pidx);
           break;
       }
     }
     break;

    case grcode_dblclicked:   // a graph node has been double clicked
                              // in:  graph_viewer_t *gv
                              //      selection_item_t *current_item
                              // out: 0-ok, 1-ignore click
     {
       graph_viewer_t *v   = va_arg(va, graph_viewer_t *);
       selection_item_t *s = va_arg(va, selection_item_t *);
       msg("%x: %sclicked on ", v, code == grcode_clicked ? "" : "dbl");
       if ( s == NULL )
         msg("background\n");
       else if ( s->is_node )
         msg("node %d\n", s->node);
       else
         msg("edge (%d, %d) layout point #%d\n", s->elp.e.src, s->elp.e.dst, s->elp.pidx);
     }
     break;

    case grcode_creating_group:
                              // a group is being created
                              // in:  mutable_graph_t *g
                              //      intset_t *nodes
                              // out: 0-ok, 1-forbid group creation
     {
       mutable_graph_t *g = va_arg(va, mutable_graph_t *);
       intset_t &nodes    = *va_arg(va, intset_t *);
       msg("%x: creating group", g);
       for ( intset_t::iterator p=nodes.begin(); p != nodes.end(); ++p )
         msg(" %d", *p);
       msg("...\n");
     }
     break;

    case grcode_deleting_group:
                              // a group is being deleted
                              // in:  mutable_graph_t *g
                              //      int old_group
                              // out: 0-ok, 1-forbid group deletion
     {
       mutable_graph_t *g = va_arg(va, mutable_graph_t *);
       int group          = va_argi(va, int);
       msg("%x: deleting group %d\n", g, group);
     }
     break;

    case grcode_group_visibility:
                              // a group is being collapsed/uncollapsed
                              // in:  mutable_graph_t *g
                              //      int group
                              //      bool expand
                              // out: 0-ok, 1-forbid group modification
     {
       mutable_graph_t *g = va_arg(va, mutable_graph_t *);
       int group          = va_argi(va, int);
       bool expand        = va_argi(va, bool);
       msg("%x: %scollapsing group %d\n", g, expand ? "un" : "", group);
     }
     break;

    case grcode_gotfocus:     // a graph viewer got focus
                              // in:  graph_viewer_t *gv
                              // out: must return 0
     {
       mutable_graph_t *g = va_arg(va, mutable_graph_t *);
       msg("%x: got focus\n", g);
     }
     break;

    case grcode_lostfocus:    // a graph viewer lost focus
                              // in:  graph_viewer_t *gv
                              // out: must return 0
     {
       mutable_graph_t *g = va_arg(va, mutable_graph_t *);
       msg("%x: lost focus\n", g);
     }
     break;

    case grcode_user_refresh: // refresh user-defined graph nodes and edges
                              // in:  mutable_graph_t *g
                              // out: success
     {
       mutable_graph_t *g = va_arg(va, mutable_graph_t *);
       msg("%x: refresh\n", g);

       if ( g->empty() )
         g->resize(graph->m_Vertices.size());

       for(int i = 0; i < graph->m_Vertices.size(); ++i)
         for(std::list<std::pair<int, EdgeType> >::iterator j = graph->m_Edges[i].begin(); j != graph->m_Edges[i].end(); ++j)
           g->add_edge(i, j->first, NULL);

       result = true;
     }
     break;


    case grcode_user_gentext: // generate text for user-defined graph nodes
                              // in:  mutable_graph_t *g
                              // out: must return 0
     {
       mutable_graph_t *g = va_arg(va, mutable_graph_t *);
       msg("%x: generate text for graph nodes\n", g);
       graph->m_VertexText.clear();
       for ( node_iterator p=g->begin(); p != g->end(); ++p )
       {
         int n = *p;
         graph->m_VertexText[n] = generate_vertex_text(graph, n);
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


    case grcode_user_size:    // calculate node size for user-defined graph
                              // in:  mutable_graph_t *g
                              //      int node
                              //      int *cx
                              //      int *cy
                              // out: 0-did not calculate, ida will use node text size
                              //      1-calculated. ida will add node title to the size
     msg("calc node size - not implemented\n");
     // ida will calculate the node size based on the node text
     break;

    case grcode_user_title:   // render node title of a user-defined graph
                              // in:  mutable_graph_t *g
                              //      int node
                              //      rect_t *title_rect
                              //      int title_bg_color
                              //      HDC dc
                              // out: 0-did not render, ida will fill it with title_bg_color
                              //      1-rendered node title
     // ida will draw the node title itself
     break;

    case grcode_user_draw:    // render node of a user-defined graph
                              // in:  mutable_graph_t *g
                              //      int node
                              //      rect_t *node_rect
                              //      HDC dc
                              // out: 0-not rendered, 1-rendered
                              // NB: draw only on the specified DC and nowhere else!
     // ida will draw the node text itself
     break;

    case grcode_user_hint:    // retrieve hint for the user-defined graph
                              // in:  mutable_graph_t *g
                              //      int mousenode
                              //      int mouseedge_src
                              //      int mouseedge_dst
                              //      char **hint
                              // 'hint' must be allocated by qalloc() or qstrdup()
                              // out: 0-use default hint, 1-use proposed hint
     {
       mutable_graph_t *g = va_arg(va, mutable_graph_t *);
       int mousenode      = va_argi(va, int);
       int mouseedge_src  = va_argi(va, int);
       int mouseedge_dst  = va_argi(va, int);
       char **hint        = va_arg(va, char **);
       char buf[MAXSTR];
       buf[0] = '\0';
       if ( mousenode != -1 )
         qsnprintf(buf, sizeof(buf), "My fancy hint for node %d", mousenode);
       else if ( mouseedge_src != -1 )
         qsnprintf(buf, sizeof(buf), "Hovering on (%d,%d)", mouseedge_src, mouseedge_dst);
       if ( buf[0] != '\0' )
         *hint = qstrdup(buf);
       result = true; // use our hint
       qnotused(g);
     }
     break;
  }
  return result;
}

/*********************************************************************
* Function: run
*
* run is a plugin_t function. It is executed when the plugin is run.
*
* The argument 'arg' can be passed by adding an entry in
* plugins.cfg or passed manually via IDC:
*
*   success RunPlugin(string name, long arg);
*********************************************************************/
void run(int arg)
{
  if (arg == -1)
  {
    PLUGIN.flags |= PLUGIN_UNL;
    msg("Unloading deob_graph plugin...\n");
    return;
  }
  ea_t where = get_screen_ea();
  Graph *g = make_collapsed_graph(where);
  print_graph(g);
  HWND hwnd = NULL;
  TForm *form = create_tform("Deobfuscated Graph", &hwnd);
  if ( hwnd != NULL )
  {
    // get a unique graph id
    netnode id;
    id.create();
    graph_viewer_t *gv = create_graph_viewer(form,  id, callback, g, 0);
    open_tform(form, FORM_MDI|FORM_TAB|FORM_MENU);
    if ( gv != NULL )
    {
      viewer_fit_window(gv);
    }
  }
  else
  {
    close_tform(form, 0);
  }
  return;
}

char comment[] 	= "Short one line description about the plugin";
char help[] 	= "My plugin:\n"
                  "\n"
                  "Multi-line\n"
                  "description\n";

/* Plugin name listed in (Edit | Plugins) */
char wanted_name[] 	= "deob_graph";

/* plug-in hotkey */
char wanted_hotkey[] 	= "";

/* defines the plugins interface to IDA */
plugin_t PLUGIN =
{
  IDP_INTERFACE_VERSION,
  0,              // plugin flags
  init,           // initialize
  term,           // terminate. this pointer may be NULL.
  run,            // invoke plugin
  comment,        // comment about the plugin
  help,           // multiline help about the plugin
  wanted_name,    // the preferred short name of the plugin
  wanted_hotkey   // the preferred hotkey to run the plugin
};
