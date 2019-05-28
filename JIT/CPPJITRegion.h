unsigned long CPP_allocate(unsigned long size);
void CPP_deallocate(unsigned long addr);
char CPP_blit_at(unsigned long addr, unsigned long addr_size, unsigned long pos, const char * const bytes, unsigned long bytes_size);
