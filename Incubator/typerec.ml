type tau_reg =
| Reg1_t
| Reg8_t
| Reg16_t
| Reg32_t

type tau_refined =
| Num8_t
| Num16_t
| Num32_t
| Uint8_t
| Uint16_t
| Uint32_t
| Int8_t
| Int16_t
| Int32_t
| Pointer of big_T
| Code_t

type tau_mem =
| ???

type tau_R =
| ???

type tau_fun = tau_mem * tau_R * big_T

type tau_base =
| Register of tau_reg
| Refined of tau_refined

type tau_data =
| Base of tau_base
| Memory of tau_mem

type big_T = 
| Data of tau_data
| Fun of tau_fun
| Top
| Bottom
| Union of big_T * big_T
| Intersection of big_T * big_T 
| Arrow of big_T * big_T

type constraint =
| Equals of big_T * big_T
| Subtype of big_T * big_T 
| Conjunction of constraint * constraint
| Disjunction of constraint * constraint

let meet t1 t2 = match (t1,t2) with
| Bottom,_ | _,Bottom -> Bottom
| 