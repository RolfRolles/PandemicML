let heap_region_ctr  = ref 0
let stack_region_ctr = ref 0

type region = 
| Register of IR.var
| Heap of int
| Stack of int
| Global

type offs = int32

type amemaddr = region * offs
type aloc = region * offs
type strided_interval = int32 * int32 * int32

type bool3 = AbsZero | AbsHalf | AbsOne

type valueset = region -> strided_interval
type aloc_env = Map from aloc -> valueset


type absenv = 
{ regs: IR.var -> valueset;
  flags: IR.var -> bool3;
  globals: Map from int32 to aloc_env;
  proc: Map from region to aloc_env;
  heap: Map from region to aloc_env;
  