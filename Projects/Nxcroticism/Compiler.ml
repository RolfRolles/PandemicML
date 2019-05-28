type encodable =
{
  next_return_address: int32;
  size_of_frame: int;
  return_address_position: int;
  extra_dwords: (int * int32) list;
}

