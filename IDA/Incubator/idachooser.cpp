/*
type 'a chooser =
{ columns: (int * string * ('a -> string)) array;
  elements: 'a array;
  del:     ('a -> int32)  option;
  ins:     (unit -> unit) option;
  update:  ('a -> int32)  option;
  edit:    ('a -> unit)   option;
  enter:   ('a -> unit)   option;
  destroy: (unit -> unit) option;
}

(* ' *)
*/

#define USE_DANGEROUS_FUNCTIONS

#include <ida.hpp>
#include <kernwin.hpp>

#include "ocamllib.h"
#include "idaside.hpp"

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/printexc.h>

extern "C" {

#define CHOOSER_COLUMNS(x)  Field(x,0)
#define CHOOSER_ELEMENTS(x) Field(x,1)
#define CHOOSER_DEL(x)      Field(x,2)
#define CHOOSER_INS(x)      Field(x,3)
#define CHOOSER_UPDATE(x)   Field(x,4)
#define CHOOSER_EDIT(x)     Field(x,5)
#define CHOOSER_ENTER(x)    Field(x,6)
#define CHOOSER_DESTROY(x)  Field(x,7)

#define SOME_VALUE(x)          Field(x,0)
#define TUPLE_ELEMENT(tuple,i) Field(tuple,i)
#define ARRAY_ELEMENT(arr,i)   Field(arr,i)
#define NUM_ELEMENTS(x)        Wosize_val(x)

#define Val_none Val_int(0)

struct OCamlChooser
{
  void *oobj;
  int ncols;
  unsigned int *widths;
  
  OCamlChooser(void *oobj) : ncols(0), widths(NULL) 
  {
    REGISTERGLOBALROOT(this->oobj, oobj);
  };
  ~OCamlChooser()
  {
    WrapOCamlRemoveGlobalRoot(oobj);
  };
};

int OCamlChooserSizer(void *obj)
{
  OCamlChooser *occ = (OCamlChooser *)obj;
  return NUM_ELEMENTS(CHOOSER_ELEMENTS(occ->oobj));
}

void OCamlGetl(void *obj,ulong n, char * const * arrptr)
{
  CAMLlocal3(cols,colel,colname);
  CAMLlocal3(elsarr,arrayel,ret);
  int ncols;
  OCamlChooser *occ = (OCamlChooser *)obj;

  cols  = CHOOSER_COLUMNS(occ->oobj);
  ncols = NUM_ELEMENTS(cols);
  if ( n == 0 ) // generate the column headers
  {
    for ( int i=0; i < ncols; i++ )
    {
      colel = ARRAY_ELEMENT(cols,i);
      // Actually getting the second element of the tuple here
      colname = TUPLE_ELEMENT(colel,1);
      strncpy(arrptr[i], String_val(colname), MAXSTR);
    }
    return;
  }

  elsarr  = CHOOSER_ELEMENTS(occ->oobj);
  arrayel = ARRAY_ELEMENT(elsarr,0);
  for(int i = 0; i < ncols; i++)
  {
    // Get the i'th element of the column array, and extract the third 
    // element from the tuple (the callback).  Call it on the array element.
    ret = caml_callback_exn(Field(Field(cols,i),2), arrayel);
    if(Is_exception_result(ret))
    {
      char buf[1024];
      char *exn = caml_format_exception(Extract_exception(ret));
      sprintf(buf, "[E] choose2 desc%d callback threw exception.  Returning 0.\nException (value %08lx): %s\n", i, Extract_exception(ret), exn);
      wrap_msg(buf);
      free(exn);
      return;
    }
    strncpy(arrptr[i], String_val(ret), MAXSTR);
  }
}

};