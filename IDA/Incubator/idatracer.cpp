#include <ida.hpp>
#include <idp.hpp>
#include <loader.hpp>
#include <dbg.hpp>

extern "C" {
#include "idahack.h"
#include "idadebug_common.h"
#include "idadebug_ocaml.h"
#include "idadebug_c.h"
#include "ocamllib.h"
};

struct OCamlValue
{
  void *val;
};

int idaapi callback(void* data, int notification_code, va_list va)
{
  OCamlValue *ocaml_object = (OCamlValue *)data;
  switch (notification_code)
  {
    case dbg_trace:    // A step occured (one instruction was executed). This event
                       // notification is only generated if step tracing is enabled.
                       // Parameter:  none
    {
      thid_t ti = va_arg(va, thid_t);
      ea_t ip   = va_arg(va, ea_t);
      // msg("%lx: tracing\n", ip);
      if(!WrapOCamlProcess(ocaml_object->val,ip))
      {
        // stop the trace mode
        enable_step_trace(false);
        suspend_thread(ti);
        goto cleanup;
      }
      else
        request_step_into();
    }
    break;
    case dbg_process_exit:
    {
      msg("[E] Process exiting\n");
      goto cleanup;
    }
    break;

    cleanup:
    unhook_from_notification_point(HT_DBG, callback, ocaml_object);
    WrapOCamlRemoveGlobalRoot(&ocaml_object->val);
    delete ocaml_object;
  }
  return 0;
}

char CPPCreateTracer(void *data)
{
  if(get_process_state() == DSTATE_NOTASK)
  {
    msg("[E] No process is currently being debugged, tracing failed\n");
    return 0;
  }
  OCamlValue *v = new OCamlValue;
  REGISTERGLOBALROOT(v->val, data);
  if(!hook_to_notification_point(HT_DBG, callback, v))
  {
    msg("[E] Could not attach to the debugger notification point, tracing failed\n");
    WrapOCamlRemoveGlobalRoot(&v->val);
    return 0;
  }
  struct RegisterState rs;
  GetRegisterValues(&rs);
  
  WrapOCamlTracerInitialize(data, &rs);

  enable_step_trace();
  step_into();
  
  return 1;
}

extern "C" {

void CPPInitTracer()
{
  CreateTracer = &CPPCreateTracer;
}

};