(* In fact, I have run out of options, and have no choice but to declare the 
   operators with "~~", as stupid as this seems. *)
let (~~<$) a   = Int32.of_string           a  ;;
let (~~>$) a   = Int32.to_string           a  ;;
let (~~<@) a   = Int32.of_int              a  ;;
let (~~>@) a   = Int32.to_int              a  ;;

let (~~-)  a   = Int32.neg                 a  ;;
let (~~~)  a   = Int32.lognot              a  ;;
let (~~+)  a b = Int32.add                 a b;;
let (~~-)  a b = Int32.sub                 a b;;
let (~~/)  a b = Int32.div                 a b;;
let (~~*)  a b = Int32.mul                 a b;;
let (~~%)  a b = Int32.rem                 a b;;
let (~~&)  a b = Int32.logand              a b;;
let (~~|)  a b = Int32.logor               a b;;
let (~~^)  a b = Int32.logxor              a b;;
let (~~<)  a b = Int32.compare             a b;;
let (~~<<) a b = Int32.shift_left          a b;;
let (~~>>) a b = Int32.shift_right         a b;;

let zero_extend_byte_word  b = b
let zero_extend_byte_dword b = Int32.of_int b

let sign_extend_word_dword w = if (w land 0x8000) == 0 
                                 then (Int32.of_int w) 
                                 else Int32.logor (Int32.shift_left (Int32.of_int   0xffff) 16) (Int32.of_int w)
let sign_extend_byte_dword b = if (b land 0x80  ) == 0 
                                 then (Int32.of_int b) 
                                 else Int32.logor (Int32.shift_left (Int32.of_int 0xffffff)  8) (Int32.of_int b)

let mk_int  q = Int32.to_int q

let mk_byte b = Int32.logand 0xffl b

let not_byte b   =  mk_byte (Int32.logxor b 0xffffffffl)
let dec_byte b   =  mk_byte (Int32.pred b)
let inc_byte b   =  mk_byte (Int32.succ b)
let neg_byte b   =  mk_byte (Int32.neg b)
let add_byte b s =  mk_byte (Int32.add b s)
let and_byte b s =  mk_byte (Int32.logand b s)
let sub_byte b s =  mk_byte (Int32.sub b s)
let xor_byte b s =  mk_byte (Int32.logxor b s)
let or_byte  b s =  mk_byte (Int32.logor b s)
let ror_byte b s =  mk_byte (Int32.logor (Int32.shift_right_logical b (mk_int s)) (Int32.shift_left          b (8-(mk_int s))))
let rol_byte b s =  mk_byte (Int32.logor (Int32.shift_left          b (mk_int s)) (Int32.shift_right_logical b (8-(mk_int s))))
let shl_byte b s =  mk_byte (Int32.shift_left b (mk_int s))
let shr_byte b s =  mk_byte (Int32.shift_right_logical b (mk_int s))
let sar_byte b s =  invalid_arg "sar_byte: unimplemented"

let mk_word w = Int32.logand 0xffffl w

let not_word b   =  mk_word (Int32.logxor b 0xffffffffl)
let dec_word b   =  mk_word (Int32.pred b)
let inc_word b   =  mk_word (Int32.succ b)
let neg_word b   =  mk_word (Int32.neg b)
let add_word b s =  mk_word (Int32.add b s)
let and_word b s =  mk_word (Int32.logand b s)
let sub_word b s =  mk_word (Int32.sub b s)
let xor_word b s =  mk_word (Int32.logxor b s)
let or_word  b s =  mk_word (Int32.logor b s)
let ror_word b s =  mk_word (Int32.logor (Int32.shift_right_logical b (mk_int s)) (Int32.shift_left          b (16-(mk_int s))))
let rol_word b s =  mk_word (Int32.logor (Int32.shift_left          b (mk_int s)) (Int32.shift_right_logical b (16-(mk_int s))))
let shl_word b s =  mk_word (Int32.shift_left b (mk_int s))
let shr_word b s =  mk_word (Int32.shift_right_logical b (mk_int s))
let sar_word b s =  invalid_arg "sar_word: unimplemented"
let xchg_word w   = mk_word (Int32.logor (Int32.shift_left w 8) (Int32.shift_right_logical w 8))

let bswap_dword d = 
  Int32.logor
    (Int32.logor 
      (Int32.shift_left d 24)
      (Int32.logand (Int32.shift_left d  8)         (Int32.of_int 0xff0000)))
    (Int32.logor 
      (Int32.logand (Int32.shift_right_logical d 8) (Int32.of_int 0xff00   ))
                                 (Int32.shift_right_logical d 24))
let sub_dword d s = Int32.sub d s
let add_dword d s = Int32.add d s
let ror_dword d s = let s = Int32.to_int s in Int32.logor (Int32.shift_right_logical d s) (Int32.shift_left d (32-s))
let rol_dword d s = let s = Int32.to_int s in Int32.logor (Int32.shift_left d s) (Int32.shift_right_logical d (32-s))
let neg_dword d   = Int32.mul d (Int32.minus_one)
let xor_dword d s = Int32.logxor d s

let take_low_byte  d = Int32.to_int (Int32.logand d (Int32.of_int 0xff))                      
let replace_byte b d = Int32.logor (Int32.logand d (Int32.of_string "0xffffff00")) (Int32.of_int b)
let take_low_word  d = Int32.to_int (Int32.logand d (Int32.of_int 0xffff))
let replace_word w d = Int32.logor (Int32.logand d (Int32.of_string "0xffffff00")) (Int32.of_int w)