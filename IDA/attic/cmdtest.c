#include <stdio.h>
#include <callback.h>

int main(int argc, char **argv)
{
  caml_startup(argv);
  caml_callback(*caml_named_value("RolfTopLoop"), Val_unit);
  fflush(stdout);
}

