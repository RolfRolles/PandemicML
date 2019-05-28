#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/compatibility.h>
#include <caml/callback.h>

#include <stdio.h>
#include "IDAshared.h"
#include "IDAdata.h"

value OCaml_ReaderCallbackNowait(value buffer, value length)
{
  CAMLparam2 (buffer, length);
  CAMLlocal1 (integer);
  int i, len = Int_val(length);

  // Kind of a hack, but if you don't do this, when you enter an unterminated
  // command the parser will keep coming back for more, leading to an infinite
  // loop that allocates memory
  // if(IDACpp_incoming_buffer_pos == 0 && IDACpp_CmdToExecute[0] == '\0')
  if(IDACpp_FinishedLastCommand)
  {
    IDACpp_FinishedLastCommand = 0;
    Byte(buffer,0) = ';';
    Byte(buffer,1) = ';';
    Byte(buffer,2) = '\0';
    integer = Val_int(3);
    CAMLreturn(integer);
  }
  
  // Replace this with a memcpy if possible
  for(i = 0; 
      i < len && 
     (i+IDACpp_incoming_buffer_pos) < IDACpp_incoming_buffer_len && 
      IDACpp_CmdToExecute[IDACpp_incoming_buffer_pos + i] != 0; 
      ++i)
    Byte(buffer,i) = IDACpp_CmdToExecute[IDACpp_incoming_buffer_pos + i];

  if(i < len)
  {
    IDACpp_incoming_buffer_pos = 0;
    // IDACpp_CmdToExecute[0] = '\0';
    IDACpp_FinishedLastCommand = 1;
  }
  else
  {
    IDACpp_incoming_buffer_pos += len;
    IDACpp_FinishedLastCommand = 0;
  }
      
  integer = Val_int(i);
  CAMLreturn(integer);
}

CAMLprim value OCaml_WriteBuffer(value buffer, value offset, value length)
{
  CAMLparam3(buffer, offset, length);
  char *str = String_val(buffer);
  int ofs = Long_val(offset);
  size_t len = (size_t)Long_val(length);
  
  // Is the length of what we want to print greater than what would fit in the
  // buffer under any circumstances?  If so, flush the current buffer, and then
  // print the outgoing buffer directly.
  if( len >= (IDACpp_outgoing_buffer_len-2) )
  {
    if( IDACpp_outgoing_buffer_pos != 0 )
    {
      do_msg(IDACpp_outgoing_buffer, IDACpp_outgoing_buffer_pos);
      IDACpp_outgoing_buffer_pos = 0;
    }
    do_msg(str, len);
  }
  
  // Otherwise, we are guaranteed that the data fits in the outgoing buffer if
  // the outgoing buffer were empty.  We check to see if the addition of the data
  // would cause the buffer to overflow.  If it would, we flush the current buffer.
  // We then copy the data into the buffer and return.
  else
  if( IDACpp_outgoing_buffer_pos + len >= (IDACpp_outgoing_buffer_len-2) )
  {
    if( IDACpp_outgoing_buffer_pos != 0 )
    {
      do_msg(IDACpp_outgoing_buffer, IDACpp_outgoing_buffer_pos);
      IDACpp_outgoing_buffer_pos = 0;
    }
    memcpy(IDACpp_outgoing_buffer, str, len);
    IDACpp_outgoing_buffer_pos += len;
  }

  // Otherwise, the data will fit snugly into the outgoing buffer.  Simply copy
  // the data into it at the current position.
  else
  {
    memcpy(&IDACpp_outgoing_buffer[IDACpp_outgoing_buffer_pos], str, len);
    IDACpp_outgoing_buffer_pos += len;
  }

  CAMLreturn(Val_unit);
}

CAMLprim value OCaml_FlushBuffer(value unit)
{
  CAMLparam1(unit);

  do_msg(IDACpp_outgoing_buffer, IDACpp_outgoing_buffer_pos);
  IDACpp_outgoing_buffer_pos = 0;

  CAMLreturn(Val_unit);
}

