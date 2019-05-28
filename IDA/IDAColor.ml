type idacolor_marker = 
| COLOR_ON 
| COLOR_OFF

let int_of_idacolor_marker = function
| COLOR_ON  -> 0x1
| COLOR_OFF -> 0x2

type idacolor =
| COLOR_DEFAULT   
| COLOR_REGCMT    
| COLOR_RPTCMT    
| COLOR_AUTOCMT   
| COLOR_INSN      
| COLOR_DATNAME   
| COLOR_DNAME     
| COLOR_DEMNAME   
| COLOR_SYMBOL    
| COLOR_CHAR      
| COLOR_STRING    
| COLOR_NUMBER    
| COLOR_VOIDOP    
| COLOR_CREF      
| COLOR_DREF      
| COLOR_CREFTAIL  
| COLOR_DREFTAIL  
| COLOR_ERROR     
| COLOR_PREFIX    
| COLOR_BINPREF   
| COLOR_EXTRA     
| COLOR_ALTOP     
| COLOR_HIDNAME   
| COLOR_LIBNAME   
| COLOR_LOCNAME   
| COLOR_CODNAME   
| COLOR_ASMDIR    
| COLOR_MACRO     
| COLOR_DSTR      
| COLOR_DCHAR     
| COLOR_DNUM      
| COLOR_KEYWORD   
| COLOR_REG       
| COLOR_IMPNAME   
| COLOR_SEGNAME   
| COLOR_UNKNAME   
| COLOR_CNAME     
| COLOR_UNAME     
| COLOR_COLLAPSED 
| COLOR_ADDR      
| COLOR_OPND1     
| COLOR_OPND2     
| COLOR_OPND3     
| COLOR_OPND4     
| COLOR_OPND5     
| COLOR_OPND6     

let int_of_idacolor = function
| COLOR_DEFAULT   -> 0x01 
| COLOR_REGCMT    -> 0x02 
| COLOR_RPTCMT    -> 0x03 
| COLOR_AUTOCMT   -> 0x04 
| COLOR_INSN      -> 0x05 
| COLOR_DATNAME   -> 0x06 
| COLOR_DNAME     -> 0x07 
| COLOR_DEMNAME   -> 0x08 
| COLOR_SYMBOL    -> 0x09 
| COLOR_CHAR      -> 0x0A 
| COLOR_STRING    -> 0x0B 
| COLOR_NUMBER    -> 0x0C 
| COLOR_VOIDOP    -> 0x0D 
| COLOR_CREF      -> 0x0E 
| COLOR_DREF      -> 0x0F 
| COLOR_CREFTAIL  -> 0x10 
| COLOR_DREFTAIL  -> 0x11 
| COLOR_ERROR     -> 0x12 
| COLOR_PREFIX    -> 0x13 
| COLOR_BINPREF   -> 0x14 
| COLOR_EXTRA     -> 0x15 
| COLOR_ALTOP     -> 0x16 
| COLOR_HIDNAME   -> 0x17 
| COLOR_LIBNAME   -> 0x18 
| COLOR_LOCNAME   -> 0x19 
| COLOR_CODNAME   -> 0x1A 
| COLOR_ASMDIR    -> 0x1B 
| COLOR_MACRO     -> 0x1C 
| COLOR_DSTR      -> 0x1D 
| COLOR_DCHAR     -> 0x1E 
| COLOR_DNUM      -> 0x1F 
| COLOR_KEYWORD   -> 0x20 
| COLOR_REG       -> 0x21 
| COLOR_IMPNAME   -> 0x22 
| COLOR_SEGNAME   -> 0x23 
| COLOR_UNKNAME   -> 0x24 
| COLOR_CNAME     -> 0x25 
| COLOR_UNAME     -> 0x26 
| COLOR_COLLAPSED -> 0x27 
| COLOR_ADDR      -> 0x28 
| COLOR_OPND1     -> 0x29 
| COLOR_OPND2     -> 0x2A 
| COLOR_OPND3     -> 0x2B 
| COLOR_OPND4     -> 0x2C 
| COLOR_OPND5     -> 0x2D 
| COLOR_OPND6     -> 0x2E 

let int_of_idacolor_marker = function
| COLOR_ON  -> 0x1
| COLOR_OFF -> 0x2

let idacolorstr col str =
  let sb,se,sc = String.create 2, String.create 2, char_of_int (int_of_idacolor col) in
  sb.[0] <- char_of_int (int_of_idacolor_marker (COLOR_ON));
  sb.[1] <- sc;
  se.[0] <- char_of_int (int_of_idacolor_marker (COLOR_OFF));
  se.[1] <- sc;
  sb^str^se

let col_insn    = idacolorstr (COLOR_INSN)
let col_keyword = idacolorstr (COLOR_KEYWORD)
let col_symbol  = idacolorstr (COLOR_SYMBOL)
let col_reg     = idacolorstr (COLOR_REG)
let col_memimm  = idacolorstr (COLOR_VOIDOP)
let col_imm     = idacolorstr (COLOR_NUMBER)
let col_addr    = idacolorstr (COLOR_ADDR)

