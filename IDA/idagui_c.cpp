#include <ida.hpp>
#include <kernwin.hpp>

extern "C" {

const char *wrap_askfile_c(bool b, const char *titlestr)
{
  return askfile_c(b,NULL,"%s",titlestr);
}

};