type labeltype = int32

(* The definition of the CFG module; we still need a Make module to produce one *)
module type CFG =
sig
  module V :
  sig
    type t = labeltype
    val hash : t -> int
    val equal : t -> t -> bool
    val compare : t -> t -> int
  end

  (* Include everything from Graph.Builder.S.  Our vertices are labeled with 
     integers (the number corresponding to the statement beginning the basic
     block), and our edges are labeled with the type of edge that they are. *)
  include Graph.Builder.S with type G.V.label = labeltype and type G.E.label = (*cfgedge*) unit

  (* Abstract out the type of the language, so we can use the same code for SSA *)
  type language
  
  (* Find a vertex in the graph, given its label *)
  val find_vertex   : G.t -> labeltype -> G.V.t
  
  (* Create a vertex within the CFG; have already added it to the underlying
     graph, so we don't need to pass in the label again.  Return a pair of the
     new graph and the new vertex. *)
  val create_vertex : G.t -> labeltype -> language -> G.t * G.V.t
  
  (* Set the IR on a given vertex. *)
  val set_ir : G.t -> G.V.t -> language -> G.t

  (* Get the IR from a given vertex. *)
  val get_ir : G.t -> G.V.t -> language
  
  val remove_edge : G.t -> G.V.t -> G.V.t -> G.t
  val remove_vertex : G.t -> G.V.t -> G.t
end

(** Functor building an implementation of the control flow graph structure, 
    given a language. *)
module Make (Language : sig type t end) : CFG with type language = Language.t list

type 'a cfg = (module CFG with type language = 'a)