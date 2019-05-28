exception IndirectJump of int32

module type Language = sig
  type t
  val disasm : int32 -> t * ASMUtil.cfsuccessors
  val disasm_ex : int32 -> int32 -> t * ASMUtil.cfsuccessors * int32
end

module type S = sig
  type lang
  module C : CFG.CFG with type language = lang list
  val merge_singleton_vertices : C.G.t -> C.G.t
  val remove_empty_vertices : C.G.t -> C.G.t
  val build : ?stopfun:(int32 -> bool) -> int32 -> C.G.t * (int32, unit) Hashtbl.t
  val build_ex : ?start_graph:C.G.t -> int32 -> int32 -> C.G.t
  val get_order : C.G.t -> C.G.V.t -> C.G.V.t list
end

module MakeGraphBuilder (Lang : Language) : S with type lang = Lang.t

type 'a s = (module S with type lang = 'a)