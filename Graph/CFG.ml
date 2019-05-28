(*type cfgedge = Fallthrough | TrueBranch | Unconditional*)

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


(* Vertex structure which satisfies Sig.COMPARABLE *)
module Vertex =
struct
  type t = labeltype
  let compare = Int32.compare
  let hash x = Int32.to_int x
  let equal = (=)
end

(* This functor application produces half of a map.  We can create an entire
   map by declaring some of type `[value type] LabelMap.t` *)
module LabelMap = Map.Make(Vertex)

(* Edge structure which satisfies Sig.ORDERED_TYPE_DFT *)
module E =
struct
  type t = unit
  let compare = Pervasives.compare
  let default = (*Unconditional*) ()
end

(* The actual data type corresponding to the graph.  Used in Make.  We 
   abstract out the types because they differ depending upon what language is
   being used, e.g. IR vs. SSA.  *)
type ('a, 'b, 'c) ccfg = 
{
  (* The graph itself, which is whatever it's being represented as in 
     Graph.Persistent.Digraph.ConcreteLabeled(Vertex)(E) *)
  graph:  'a;
  
  (* A map from a label to the corresponding vertex. *)
  label2vertex:  'b;
  
  (* A map from a vertex to the statements thereupon. *)
  label2lang:  'c
}

(* ' *)

(* Functor responsible for creating a graph object. *)
module Make(Language : sig type t end) =
struct
  (* Include everything from persistent concrete digraph *)
  module G' = Graph.Persistent.Digraph.ConcreteLabeled(Vertex)(E) (* ' *)

  (* This way we can use the same code for regular IR and SSA *)
  type language = Language.t list
  
  (* We have to have a module G inside of our Make structure *)
  module G = 
  struct
    (* Copy the module V and vertex type *)
    module V = G'.V (* ' *)
    type vertex = V.t

    (* Copy the module E and edge type *)
    module E = G'.E (* ' *)
    type edge = E.t
  
    (* How graphs are represented.  Contains however G' is represented as the
       underlying graph representation, and two maps to conveniently map labels
       to vertices and vertices to their statements. *)
    type t = (G'.t, V.t LabelMap.t, language LabelMap.t) ccfg (* ' *)
    
    (* We have to wrap the functionality of the existing graph such that it's
       first-class present within this G submodule.  Is there a better way to
       do this? *)
    
    (* Wrappers for Sig.G *)
    let is_directed = true
    let is_empty       cfg = G'.is_empty       cfg.graph
    let nb_vertex      cfg = G'.nb_vertex      cfg.graph
    let nb_edges       cfg = G'.nb_edges       cfg.graph
    let out_degree     cfg = G'.out_degree     cfg.graph
    let in_degree      cfg = G'.in_degree      cfg.graph
    let mem_vertex     cfg = G'.mem_vertex     cfg.graph
    let mem_edge       cfg = G'.mem_edge       cfg.graph
    let mem_edge_e     cfg = G'.mem_edge_e     cfg.graph
    let find_edge      cfg = G'.find_edge      cfg.graph
    let succ           cfg = G'.succ           cfg.graph
    let pred           cfg = G'.pred           cfg.graph
    let succ_e         cfg = G'.succ_e         cfg.graph
    let pred_e         cfg = G'.pred_e         cfg.graph
    let iter_vertex  f cfg = G'.iter_vertex  f cfg.graph
    let iter_edges   f cfg = G'.iter_edges   f cfg.graph
    let fold_vertex  f cfg = G'.fold_vertex  f cfg.graph
    let fold_edges   f cfg = G'.fold_edges   f cfg.graph
    let iter_edges_e f cfg = G'.iter_edges_e f cfg.graph
    let fold_edges_e f cfg = G'.fold_edges_e f cfg.graph
    let iter_succ    f cfg = G'.iter_succ    f cfg.graph
    let iter_pred    f cfg = G'.iter_pred    f cfg.graph
    let fold_succ    f cfg = G'.fold_succ    f cfg.graph
    let fold_pred    f cfg = G'.fold_pred    f cfg.graph
    let iter_succ_e  f cfg = G'.iter_succ_e  f cfg.graph
    let fold_succ_e  f cfg = G'.fold_succ_e  f cfg.graph
    let iter_pred_e  f cfg = G'.iter_pred_e  f cfg.graph
    let fold_pred_e  f cfg = G'.fold_pred_e  f cfg.graph(* ' *)
    let map_vertex     cfg = failwith "map_vertex:  haven't decided how to do this"
    
    (* Wrappers for Persistent.S.Concrete *)
    let add_edge      cfg v1 v2 = { cfg with graph = G'.add_edge      cfg.graph v1 v2 }
    let add_edge_e    cfg e     = { cfg with graph = G'.add_edge_e    cfg.graph e     }
    let remove_edge   cfg v1 v2 = { cfg with graph = G'.remove_edge   cfg.graph v1 v2 }
    let remove_edge_e cfg e     = { cfg with graph = G'.remove_edge_e cfg.graph e     }
    let add_vertex    cfg v     = { cfg with graph = G'.add_vertex    cfg.graph v     }

    (* Have to remove from the maps as well *)
    let remove_vertex cfg v     = { graph = G'.remove_vertex  cfg.graph v;
                                    label2vertex = LabelMap.remove (V.label v) cfg.label2vertex;
                                    label2lang   = LabelMap.remove (V.label v) cfg.label2lang }

    (* ' *)
    let empty = { graph = G'.empty; label2vertex = LabelMap.empty; label2lang = LabelMap.empty }
    (* ' *)
    (* Functions that were introduced in the CFG signature *)
    let find_vertex cfg label = LabelMap.find label cfg.label2vertex
    let set_ir cfg v s = { cfg with label2lang = LabelMap.add (V.label v) s cfg.label2lang }
    let get_ir cfg v   = LabelMap.find (V.label v) cfg.label2lang
    let create_vertex cfg label s = let v   = V.create label in 
                                    let cfg = add_vertex cfg v in
                                    let cfg = set_ir cfg v s in
                                    ( {cfg with label2vertex = LabelMap.add (V.label v) v cfg.label2vertex}, v )
  end
  
  module V = Vertex
  (* Builder.P stuff *)
  let empty ()   = G.empty
  let copy g     = g
  let add_vertex = G.add_vertex
  let add_edge   = G.add_edge
  let add_edge_e = G.add_edge_e
  
  (* Persistent.S.Concrete stuff *)
  let remove_vertex = G.remove_vertex
  let remove_edge   = G.remove_edge
  let remove_edge_e = G.remove_edge_e
  
  (* CFG stuff *)
  let find_vertex (c: G.t) (l: labeltype) : (G.V.t) = G.find_vertex c l
  let get_ir (c: G.t) (v: G.V.t) : (language) = G.get_ir c v
  let set_ir (c: G.t) (v: G.V.t) (s: language) : (G.t) = G.set_ir c v s
  let create_vertex (c: G.t) (l: labeltype) s  : (G.t * G.V.t) = G.create_vertex c l s
end

type 'a cfg = (module CFG with type language = 'a)