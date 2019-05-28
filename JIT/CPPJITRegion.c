#include <windows.h>
#include <stdlib.h>
#include <stdio.h>

unsigned long CPP_allocate(unsigned long size)
{
  return (unsigned long)VirtualAlloc(NULL,size,MEM_COMMIT,PAGE_EXECUTE_READWRITE);
}

void CPP_deallocate(unsigned long addr)
{
  VirtualFree((LPVOID)addr,0,MEM_RELEASE);
}

char CPP_blit_at(unsigned long addr, unsigned long addr_size, unsigned long pos, const char * const bytes, unsigned long bytes_size)
{
  if((pos + bytes_size) > addr_size)
    return 0;
  
  memcpy((void *)(addr+pos), bytes, bytes_size);
  FlushInstructionCache(GetCurrentProcess(), addr, addr_size);
  return 1;
}

