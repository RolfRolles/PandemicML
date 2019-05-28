#include "idagraph_common.h"

struct EdgeListEntry *new_edge_list_entry()
{
  struct EdgeListEntry *ret = (struct EdgeListEntry *)malloc(sizeof(struct EdgeListEntry));
  memset(ret,0,sizeof(*ret));
  return ret;
}

struct EdgeListEntry *new_edge_list_entry_vals(unsigned long label1, unsigned long label2)
{
  struct EdgeListEntry *ret = new_edge_list_entry();
  ret->label1 = label1; ret->label2 = label2;
  return ret;
}

void delete_edge_list(struct EdgeListEntry *list)
{
  struct EdgeListEntry *curr = list;
  while(curr)
  {
    struct EdgeListEntry *curr2 = curr;
    curr = curr->next;
    free(curr2);
  }
}

char *c_strdup(char *what)
{
  char *str = (char *)malloc(strlen(what)+1);
  strcpy(str,what);
  return str;
}

void c_strdel(char *what)
{
  free(what);
}