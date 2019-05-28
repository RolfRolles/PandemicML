#define CHOOSER_COLUMNS(x)  Field(x,0)
#define CHOOSER_DESCRIBE(x) Field(x,1)
#define CHOOSER_ELEMENTS(x) Field(x,2)
#define CHOOSER_DEL(x)      Field(x,3)
#define CHOOSER_INS(x)      Field(x,4)
#define CHOOSER_EDIT(x)     Field(x,5)
#define CHOOSER_ENTER(x)    Field(x,6)
#define CHOOSER_DESTROY(x)  Field(x,7)

#define USE_DANGEROUS_FUNCTIONS

#include <ida.hpp>
#include <kernwin.hpp>

extern "C" {

#include "ocamllib.h"
#include "idaside.hpp"

#define asize_t ocamlasize_t
#define color_t ocamlcolor_t

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/printexc.h>
#include <caml/threads.h>

#undef ocamlasize_t
#undef ocamlcolor_t

#define SOME_VALUE(x)          Field(x,0)
#define TUPLE_ELEMENT(tuple,i) Field(tuple,i)
#define ARRAY_ELEMENT(arr,i)   Field(arr,i)
#define NUM_ELEMENTS(x)        Wosize_val(x)

#define Val_none Val_int(0)

struct OCamlChooser
{
  void *oobj;
  int ncols;
  int *widths;
  
  OCamlChooser(void *oobj) : ncols(0), widths(NULL) 
  {
    CAMLparam0();
    CAMLlocal1(cols);
    REGISTERGLOBALROOT(this->oobj, oobj);
    
    cols   = CHOOSER_COLUMNS(oobj);

    // Get the number of columns
    ncols  = NUM_ELEMENTS(cols);
    widths = new int[ncols+1];
    
    // Get the widths of the columns
    for ( int i=0; i < ncols; i++ )
      widths[i] = Int_val(TUPLE_ELEMENT(ARRAY_ELEMENT(cols,i),0));
     
    CAMLreturn0;
  };
  ~OCamlChooser()
  {
    WrapOCamlRemoveGlobalRoot(oobj);
    delete[] widths;
  };
};

static ulong __stdcall OCamlSizer(void *obj)
{
  OCamlChooser *occ = (OCamlChooser *)obj;
  return NUM_ELEMENTS(CHOOSER_ELEMENTS(occ->oobj));
}

// So the problem here is that CHOOSER_COLUMNS seems to actually be returning
// a pointer to the first element of the 
static void __stdcall OCamlGetl(void *obj,ulong n, char * const * arrptr)
{
  CAMLparam0();
  CAMLlocal3(cols,colel,colname);
  CAMLlocal3(elsarr,arrayel,ret);
  CAMLlocal1(head);
  int ncols;
  int i;
  OCamlChooser *occ = (OCamlChooser *)obj;

  cols  = CHOOSER_COLUMNS(occ->oobj);
  ncols = NUM_ELEMENTS(cols);
  if ( n == 0 ) // generate the column headers
  {
    for ( int i=0; i < ncols; i++ )
    {
      // Get the column element from the descriptions
      colel = ARRAY_ELEMENT(cols,i);
      
      // Get the name of the column from the array (2nd in tuple)
      colname = TUPLE_ELEMENT(colel,1);
      strncpy(arrptr[i], String_val(colname), MAXSTR);
    }
    CAMLreturn0;
  }

  elsarr  = CHOOSER_ELEMENTS(occ->oobj);
  arrayel = ARRAY_ELEMENT(elsarr,n-1);

  ret = caml_callback_exn(CHOOSER_DESCRIBE(occ->oobj), arrayel);
  if(Is_exception_result(ret))
  {
    char buf[1024];
    char *exn = caml_format_exception(Extract_exception(ret));
    sprintf(buf, "[E] choose2 desc%d callback threw exception.  Returning 0.\nException (value %08lx): %s\n", n-1, Extract_exception(ret), exn);
    wrap_msg(buf);
    free(exn);
    CAMLreturn0;
  }

  i = 0;
  while ( ret != Val_emptylist )
  {
    head = Field(ret, 0);
    if( i >= ncols )
      msg("BUG in describe: too many strings %d/%d %s\n", i, ncols, String_val(head));

    else
      strncpy(arrptr[i++], String_val(head), MAXSTR);

    ret = Field(ret, 1);
  }
  if(i < ncols)
  {
    while(i < ncols)
      sprintf(arrptr[i++], "BUG in describe: too few strings. %d/%d", i, ncols);
  }
  CAMLreturn0;
}

static ulong __stdcall OCamlDel(void *obj, ulong n)
{
  CAMLparam0();
  CAMLlocal1(ret);
  OCamlChooser *occ = (OCamlChooser *)obj;
  if(CHOOSER_DEL(occ->oobj) != Val_none)
  {
    ret = caml_callback2_exn(SOME_VALUE(CHOOSER_DEL(occ->oobj)), (value)occ->oobj, Val_int(n));
    if(Is_exception_result(ret))
    {
      char buf[1024];
      char *exn = caml_format_exception(Extract_exception(ret));
      sprintf(buf, "[E] choose2 del(%d) callback threw exception.  Returning 0.\nException (value %08lx): %s\n", n, Extract_exception(ret), exn);
      wrap_msg(buf);
      free(exn);
      CAMLreturn(0);
    }
    CAMLreturn(Int32_val(ret));
  }
  CAMLreturn(0);
}

static void __stdcall OCamlIns(void *obj)
{
  CAMLparam0();
  CAMLlocal1(ret);
  OCamlChooser *occ = (OCamlChooser *)obj;
  if(CHOOSER_INS(occ->oobj) != Val_none)
  {
    ret = caml_callback_exn(SOME_VALUE(CHOOSER_INS(occ->oobj)), (value)occ->oobj);
    if(Is_exception_result(ret))
    {
      char buf[1024];
      char *exn = caml_format_exception(Extract_exception(ret));
      sprintf(buf, "[E] choose2 ins callback threw exception.  Returning 0.\nException (value %08lx): %s\n", Extract_exception(ret), exn);
      wrap_msg(buf);
      free(exn);
    }
  }
  CAMLreturn0;
}

static void __stdcall OCamlEdit(void *obj, ulong n)
{
  CAMLparam0();
  CAMLlocal1(ret);
  OCamlChooser *occ = (OCamlChooser *)obj;
  if(CHOOSER_EDIT(occ->oobj) != Val_none)
  {
    ret = caml_callback2_exn(SOME_VALUE(CHOOSER_EDIT(occ->oobj)), (value)occ->oobj, Val_int(n));
    if(Is_exception_result(ret))
    {
      char buf[1024];
      char *exn = caml_format_exception(Extract_exception(ret));
      sprintf(buf, "[E] choose2 edit(%d) callback threw exception.  Returning 0.\nException (value %08lx): %s\n", n, Extract_exception(ret), exn);
      wrap_msg(buf);
      free(exn);
    }
  }
  CAMLreturn0;
}

static void __stdcall OCamlEnter(void *obj, ulong n)
{
  CAMLparam0();
  CAMLlocal1(ret);
  OCamlChooser *occ = (OCamlChooser *)obj;
  if(CHOOSER_ENTER(occ->oobj) != Val_none)
  {
    ret = caml_callback2_exn(SOME_VALUE(CHOOSER_ENTER(occ->oobj)), (value)occ->oobj, Val_int(n));
    if(Is_exception_result(ret))
    {
      char buf[1024];
      char *exn = caml_format_exception(Extract_exception(ret));
      sprintf(buf, "[E] choose2 edit(%d) callback threw exception.  Returning 0.\nException (value %08lx): %s\n", n, Extract_exception(ret), exn);
      wrap_msg(buf);
      free(exn);
    }
  }
  CAMLreturn0;
}

static void __stdcall OCamlDestroy(void *obj)
{
  CAMLparam0();
  CAMLlocal1(ret);
  OCamlChooser *occ = (OCamlChooser *)obj;
  if(CHOOSER_DESTROY(occ->oobj) != Val_none)
  {
    ret = caml_callback_exn(SOME_VALUE(CHOOSER_DESTROY(occ->oobj)), (value)occ->oobj);
    if(Is_exception_result(ret))
    {
      char buf[1024];
      char *exn = caml_format_exception(Extract_exception(ret));
      sprintf(buf, "[E] choose2 destroy callback threw exception.  Returning 0.\nException (value %08lx): %s\n", Extract_exception(ret), exn);
      wrap_msg(buf);
      free(exn);
    }
  }
  delete occ;
  CAMLreturn0;
}

static int do_choose2(const char *title, void *obj, int modal)
{
  OCamlChooser *occ = new OCamlChooser(obj);
  if(occ->widths == NULL)
  {
    delete occ;
    return 0;
  }

  return
    choose2(modal,                // non-modal window
            -1, -1, -1, -1,       // position is determined by Windows
            occ,                  // pass the created netnode to the window
            occ->ncols,           // number of columns
            occ->widths,          // widths of columns
            &OCamlSizer,          // function that returns number of lines
            &OCamlGetl,           // function that generates a line
            title,                // window title
            -1,                   // use the default icon for the window
            0,                    // position the cursor on the first line
            &OCamlDel,            // "kill" callback
            &OCamlIns,            // "new" callback
            NULL,                 // "update" callback
            &OCamlEdit,           // "edit" callback
            &OCamlEnter,          // function to call when the user pressed Enter
            &OCamlDestroy,        // function to call when the window is closed
            NULL,                 // use default popup menu items
            NULL);                // use the same icon for all lines
}

value OCamlShowChoose2(value obj, value str, value modal)
{
  int ret;
  CAMLparam3(obj, str, modal);
  CAMLlocal1(boole);
  caml_leave_blocking_section();  
  ret = do_choose2(String_val(str),(void *)obj, Int_val(modal));
  caml_enter_blocking_section();
  CAMLreturn(Val_int(ret));
}

};