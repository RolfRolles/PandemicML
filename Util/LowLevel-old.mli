val ( ~~<$ ) : string -> int32
val ( ~~>$ ) : int32 -> string
val ( ~~<@ ) : int -> int32
val ( ~~>@ ) : int32 -> int
val ( ~~~ ) : int32 -> int32
val ( ~~+ ) : int32 -> int32 -> int32
val ( ~~- ) : int32 -> int32 -> int32
val ( ~~/ ) : int32 -> int32 -> int32
val ( ~~* ) : int32 -> int32 -> int32
val ( ~~% ) : int32 -> int32 -> int32
val ( ~~& ) : int32 -> int32 -> int32
val ( ~~| ) : int32 -> int32 -> int32
val ( ~~^ ) : int32 -> int32 -> int32
val ( ~~< ) : Int32.t -> Int32.t -> int
val ( ~~<< ) : int32 -> int -> int32
val ( ~~>> ) : int32 -> int -> int32
val zero_extend_byte_word : 'a -> 'a
val zero_extend_byte_dword : int -> int32
val sign_extend_word_dword : int -> int32
val sign_extend_byte_dword : int -> int32
val not_byte : int -> int
val dec_byte : int -> int
val inc_byte : int -> int
val xor_byte : int -> int -> int
val ror_byte : int -> int -> int
val rol_byte : int -> int -> int
val and_byte : int -> int -> int
val neg_byte : int -> int
val add_byte : int -> int -> int
val sub_byte : int -> int -> int
val xchg_word : int -> int
val inc_word : int -> int
val rol_word : int -> int -> int
val ror_word : int -> int -> int
val xor_word : int -> int -> int
val neg_word : int -> int
val add_word : int -> int -> int
val sub_word : int -> int -> int
val bswap_dword : int32 -> int32
val sub_dword : int32 -> int32 -> int32
val add_dword : int32 -> int32 -> int32
val ror_dword : int32 -> int32 -> int32
val rol_dword : int32 -> int32 -> int32
val neg_dword : int32 -> int32
val xor_dword : int32 -> int32 -> int32
val take_low_byte : int32 -> int
val replace_byte : int -> int32 -> int32
val take_low_word : int32 -> int
val replace_word : int -> int32 -> int32
