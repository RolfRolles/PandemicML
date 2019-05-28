#include <ida.hpp>
#include <idp.hpp>
#include <loader.hpp>
#include <dbg.hpp>

#include "idadebug_common.h"

extern "C" 
{

void GetRegisterValues(RegisterState *out)
{
  regval_t eax, ecx, edx, ebx, esp, ebp, esi, edi;
  regval_t sf, cf, pf, zf, of, af, df;
  regval_t es, cs, ss, ds, fs, gs;
  
  get_reg_val("EAX", &eax);
  get_reg_val("ECX", &ecx);
  get_reg_val("EDX", &edx);
  get_reg_val("EBX", &ebx);
  get_reg_val("ESP", &esp);
  get_reg_val("EBP", &ebp);
  get_reg_val("ESI", &esi);
  get_reg_val("EDI", &edi);
  
  out->eax = eax.ival;
  out->ecx = ecx.ival;
  out->edx = edx.ival;
  out->ebx = ebx.ival;
  out->esp = esp.ival;
  out->ebp = ebp.ival;
  out->esi = esi.ival;
  out->edi = edi.ival;

  get_reg_val("SF", &sf);
  get_reg_val("CF", &cf);
  get_reg_val("PF", &pf);
  get_reg_val("ZF", &zf);
  get_reg_val("OF", &of);
  get_reg_val("AF", &af);
  get_reg_val("DF", &df);
  
  out->sf = sf.ival & 1;
  out->cf = cf.ival & 1;
  out->pf = pf.ival & 1;
  out->zf = zf.ival & 1;
  out->of = of.ival & 1;
  out->af = af.ival & 1;
  out->df = df.ival & 1;
  
  get_reg_val("ES", &es);
  get_reg_val("CS", &cs);
  get_reg_val("SS", &ss);
  get_reg_val("DS", &ds);
  get_reg_val("FS", &fs);
  get_reg_val("GS", &gs);
  
  out->es = es.ival;
  out->cs = cs.ival;
  out->ss = ss.ival;
  out->ds = ds.ival;
  out->fs = fs.ival;
  out->gs = gs.ival;
}

};