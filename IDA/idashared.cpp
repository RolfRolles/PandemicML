#define USE_DANGEROUS_FUNCTIONS 
#include <string.h>
#include <kernwin.hpp>

extern "C" {

void do_msg(const char *outgoing, unsigned int len)
{
  char buf[100];
  sprintf(buf, "%%.0%ds", len);
  msg(buf, outgoing);
}

};
