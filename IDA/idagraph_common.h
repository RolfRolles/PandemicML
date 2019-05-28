#pragma once

struct EdgeListEntry
{
  unsigned long label1, label2;
  struct EdgeListEntry *next;
};

struct EdgeListEntry *new_edge_list_entry();
struct EdgeListEntry *new_edge_list_entry_vals(unsigned long label1, unsigned long label2);
void delete_edge_list(struct EdgeListEntry *list);
char *c_strdup(char *what);
void c_strdel(char *what);
