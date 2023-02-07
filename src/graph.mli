(** Basic statistics on graphs (experimental) *)

(** [Dist] handles distances between vertices. The distance between
    two vertices is the length of a shortest path between those vertices. *)
module Dist : sig
  type t = Inf | Fin of int

  (** The distance between two vertices is [zero] iff they are equal. *)
  val zero : t

  (** The distance between two vertices is [one] iff they are adjacent. *)
  val one : t

  (** The distance between two vertices is [infty] iff they are disconnected. *)
  val infty : t

  (** Adding distances. *)
  val ( + ) : t -> t -> t

  (** Comparing distances. *)
  val ( > ) : t -> t -> bool

  (** Testing distance for equality. *)
  val ( = ) : t -> t -> bool

  (** Computing the [max] of two distances. *)
  val max : t -> t -> t

  (** Pretty printing. *)
  val pp : Format.formatter -> t -> unit
end

module type Graph_statistics = sig
  (** [t] is the type of (undirected) graphs. *)
  type t

  (** [vertex] is the type of vertices. *)
  type vertex

  type matrix = (int * int, float) Stats_intf.fin_fun

  (** Undirected edges. The [equal] and [hash] function are invariant under permutation
      of the vertices in the pair encoding the edge. *)
  module Undirected_edge : Basic_intf.Std with type t = vertex * vertex

  (** [Table] is a hashtable module with undirected edges as keys. *)
  module Table : Hashtbl.S with type key = Undirected_edge.t

  (** [Vertex_set] handles sets of vertices. *)
  module Vertex_set : Set.S with type elt = vertex

  (** [Vertex_table] is a hashtable module with vertices as keys. *)
  module Vertex_table : Hashtbl.S with type key = vertex

  (** Finite bijections between vertices and integers. *)
  module Vertex_bij : Finbij.S with type elt = vertex

  (** [adjacency_matrix g] computes the adjacency matrix of [g] as well as a
      bijection between the matrix dimensions and the vertices. The matrix is
      dense. Since the graph is undirected, it is also symmetric. *)
  val adjacency_matrix : t -> matrix * Vertex_bij.t

  (** [laplacian g] computes the laplacian of the adjacency matrix,
      following the definition in 'Spectral Graph Theory', by Fan Chung Graham.
      The dimensions are the same as the adjacency matrix.
      A finite bijection is returned as well. *)
  val laplacian : t -> matrix * Vertex_bij.t

  type distance_table = (vertex * vertex, Dist.t) Hashtbl.t

  (** Floyd-warshall algorithm. Complexity is O(V^3) where V is the number of
      vertices of the graph. Returns a table of all distances between pairs of
      vertices. *)
  val floyd_warshall : t -> Dist.t Table.t

  (** Computes the diameter of the graph, ie the maximum over all pair of vertices
      of the shortest-path distance between those vertices. *)
  val diameter : t -> Dist.t

  (** [volume g] computes the sum over all vertices of their degree. *)
  val volume : t -> int

  (** [connected_component g v predicate] produces the connected component of [v] in [g]
      that satisfies the given predicate. *)
  val connected_component : t -> vertex -> (vertex -> bool) -> vertex Seq.t

  (** See {!connected_component}. Returns a table instead of a {!Seq.t}. *)
  val connected_component_ :
    t -> vertex -> (vertex -> bool) -> unit Vertex_table.t

  (** [is_induced_subgraph_connected g predicate] is true if the subgraph of [g] induced by
      [predicate] is connected. *)
  val is_induced_subgraph_connected : t -> (vertex -> bool) -> bool

  (** [degree_dist g] computes the degree distribution of [g]. *)
  val degree_dist : t -> (int, float) Stats_intf.fin_prb

  (** [cut g set] computes the {e cut} associated to [set], i.e.
      the subset of edges that have an end in [set] and the other end
      in the complement of [set]. *)
  val cut : t -> Vertex_set.t -> (vertex * vertex) list

  (** [Tree] defines a type of trees and functions to construct, deconstruct and iterate on those trees. *)
  module Tree : sig
    type t

    (** [cons v sub] constructs a tree with root [v] and subtrees [sub]. *)
    val cons : vertex -> t list -> t

    (** [uncons t] deconstructs [t] into a pair of its root and its subtrees. *)
    val uncons : t -> vertex * t list

    (** [mem_vertex t v]  *)
    val mem_vertex : t -> vertex -> bool

    (** [iter_vertices t f] iterates [f] on the nodes of [t]. *)
    val iter_vertices : t -> (vertex -> unit) -> unit

    (** [iter_edges t f] iterates [f] on the edges of [t]. The edges are directed from each node towards
        its subtrees. *)
    val iter_edges : t -> (vertex * vertex -> unit) -> unit
  end

  (** [aldous_broder g v0 predicate] returns a uniform sampler
      for spanning trees in the subgraph containing [v0]
      and satisfying [predicate], using the Aldous-Broder algorithm. *)
  val aldous_broder : t -> vertex -> (vertex -> bool) -> Tree.t Gen.t
end

(** Graph statistics generic on an undirected [Graph] implementation. *)
module Make (Graph : Stats_intf.Graph) :
  Graph_statistics with type t = Graph.t and type vertex = Graph.vertex
