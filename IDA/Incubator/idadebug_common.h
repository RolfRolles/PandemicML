#pragma once

struct RegisterState
{
  unsigned long eax, ecx, edx, ebx, esp, ebp, esi, edi;
  unsigned char sf, cf, pf, zf, of, af, df;
  unsigned short es, cs, ss, ds, fs, gs;
};

