(* Statistics on (simple, undirected) graphs. *)

module Dist = struct
  type t = Inf | Fin of int

  let zero = Fin 0

  let one = Fin 1

  let infty = Inf

  let ( + ) d1 d2 =
    match (d1, d2) with
    | (Inf, _) | (_, Inf) -> Inf
    | (Fin i, Fin j) -> Fin (i + j)

  let ( > ) d1 d2 =
    match (d1, d2) with
    | (Inf, Inf) -> false
    | (Inf, _) -> true
    | (_, Inf) -> false
    | (Fin i1, Fin i2) -> i1 > i2

  let max d1 d2 = if d2 > d1 then d2 else d1

  let ( = ) d1 d2 =
    match (d1, d2) with
    | (Inf, Inf) -> true
    | (Fin d, Fin d') -> Int.equal d d'
    | _ -> false

  let pp fmtr d =
    match d with
    | Inf -> Format.fprintf fmtr "+inf"
    | Fin i -> Format.fprintf fmtr "%d" i
end

module type Graph_statistics = sig
  (* [t] is the type of (undirected) graphs. *)
  type t

  type vertex

  type matrix = (int * int, float) Stats_intf.fin_fun

  (* Undirected edges. The [equal] and [hash] function are invariant under permutation
     of the vertices in the pair encoding the edge. *)
  module Undirected_edge : Basic_intf.Std with type t = vertex * vertex

  module Table : Hashtbl.S with type key = Undirected_edge.t

  module Vertex_set : Set.S with type elt = vertex

  module Vertex_table : Hashtbl.S with type key = vertex

  module Vertex_bij : Finbij.S with type elt = vertex

  val adjacency_matrix : t -> matrix * Vertex_bij.t

  val laplacian : t -> matrix * Vertex_bij.t

  type distance_table = (vertex * vertex, Dist.t) Hashtbl.t

  val floyd_warshall : t -> Dist.t Table.t

  val diameter : t -> Dist.t

  val volume : t -> int

  val connected_component : t -> vertex -> (vertex -> bool) -> vertex Seq.t

  val connected_component_ :
    t -> vertex -> (vertex -> bool) -> unit Vertex_table.t

  val is_induced_subgraph_connected : t -> (vertex -> bool) -> bool

  val degree_dist : t -> (int, float) Stats_intf.fin_prb

  val cut : t -> Vertex_set.t -> (vertex * vertex) list

  module Tree : sig
    type t

    val cons : vertex -> t list -> t

    val uncons : t -> vertex * t list

    val mem_vertex : t -> vertex -> bool

    val iter_vertices : t -> (vertex -> unit) -> unit

    val iter_edges : t -> (vertex * vertex -> unit) -> unit
  end

  val aldous_broder : t -> vertex -> (vertex -> bool) -> Tree.t Gen.t
end

module Make (Graph : Stats_intf.Graph) :
  Graph_statistics with type t = Graph.t and type vertex = Graph.vertex = struct
  type t = Graph.t

  type vertex = Graph.vertex

  type matrix = (int * int, float) Stats_intf.fin_fun

  let canon v1 v2 =
    let c = Graph.V.compare v1 v2 in
    match c with -1 | 0 -> (v1, v2) | 1 -> (v2, v1) | _ -> assert false

  module Undirected_edge :
    Basic_intf.Std with type t = Graph.vertex * Graph.vertex = struct
    type t = Graph.vertex * Graph.vertex

    let equal (v1, v2) (v1', v2') =
      let (v1, v2) = canon v1 v2 in
      let (v1', v2') = canon v1' v2' in
      Graph.V.compare v1 v1' = 0 && Graph.V.compare v2 v2' = 0

    let compare (v1, v2) (v1', v2') =
      let (v1, v2) = canon v1 v2 in
      let c = Graph.V.compare v1 v1' in
      if c <> 0 then c else Graph.V.compare v2 v2'

    let hash (v1, v2) =
      let (v1, v2) = canon v1 v2 in
      Hashtbl.hash (Graph.V.hash v1, Graph.V.hash v2)

    let pp fmtr (v1, v2) =
      Format.fprintf fmtr "(%a, %a)" Graph.V.pp v1 Graph.V.pp v2
  end

  module Table = Hashtbl.Make (Undirected_edge)
  module Vertex_bij = Finbij.Make (Graph.V)
  module Vertex_set = Set.Make (Graph.V)
  module Vertex_table = Hashtbl.Make (Graph.V)

  let iter2 c r f =
    for i = 0 to c - 1 do
      for j = 0 to r - 1 do
        f (i, j)
      done
    done

  let adjacency_matrix graph : matrix * Vertex_bij.t =
    let nb_vertex = Graph.nb_vertex graph in
    let vertices = Graph.fold_vertex (fun v l -> v :: l) graph [] in
    let vbij = Vertex_bij.of_list vertices in
    let matrix =
      Vec.(
        Vec
          ( iter2 nb_vertex nb_vertex,
            fun (c, r) ->
              let vr = Vertex_bij.nth_exn vbij r in
              let vc = Vertex_bij.nth_exn vbij c in
              if Graph.mem_edge graph vr vc then 1.0 else 0.0 ))
    in
    (matrix, vbij)

  (* Following the definition in 'Spectral Graph Theory', Fan Chung Graham *)
  let laplacian graph : matrix * Vertex_bij.t =
    let nb_vertex = Graph.nb_vertex graph in
    let vertices = Graph.fold_vertex (fun v l -> v :: l) graph [] in
    let vbij = Vertex_bij.of_list vertices in
    let matrix =
      Vec.(
        Vec
          ( iter2 nb_vertex nb_vertex,
            fun (j, r) ->
              if r = j then
                let vr = Vertex_bij.nth_exn vbij r in
                let d = Graph.out_degree graph vr in
                if d = 0 then 0.0 else 1.0
              else
                let vr = Vertex_bij.nth_exn vbij r in
                let vj = Vertex_bij.nth_exn vbij j in
                if Graph.mem_edge graph vr vj then
                  let dr = float_of_int (Graph.out_degree graph vr) in
                  let dj = float_of_int (Graph.out_degree graph vj) in
                  ~-.1. /. sqrt (dr *. dj)
                else 0.0 ))
    in
    (matrix, vbij)

  type distance_table = (Graph.vertex * Graph.vertex, Dist.t) Hashtbl.t

  let floyd_warshall graph =
    let nb_vertex = Graph.nb_vertex graph in
    let table = Table.create (nb_vertex * nb_vertex * 2) in
    let find_dist table v1 v2 =
      match Table.find_opt table (canon v1 v2) with
      | None -> Dist.infty
      | Some dist -> dist
    in
    let set_dist table v1 v2 dist = Table.replace table (canon v1 v2) dist in
    Graph.iter_vertex
      (fun vi ->
        Graph.iter_vertex (fun vj -> set_dist table vi vj Dist.Inf) graph)
      graph ;
    Graph.iter_vertex (fun v -> Table.replace table (v, v) Dist.zero) graph ;
    Graph.iter_edges
      (fun v1 v2 -> Table.replace table (canon v1 v2) Dist.one)
      graph ;
    Graph.iter_vertex
      (fun vi ->
        Graph.iter_vertex
          (fun vj ->
            Graph.iter_vertex
              (fun vk ->
                let dij = find_dist table vi vj in
                let dik = find_dist table vi vk in
                let dkj = find_dist table vk vj in
                let len = Dist.(dik + dkj) in
                if Dist.(dij > len) then set_dist table vi vj len else ())
              graph)
          graph)
      graph ;
    table

  let diameter graph =
    Table.fold
      (fun _ dist acc -> Dist.max dist acc)
      (floyd_warshall graph)
      Dist.zero

  let volume graph =
    Graph.fold_vertex (fun v acc -> acc + Graph.out_degree graph v) graph 0

  let connected_component_ graph v0 predicate =
    if not (predicate v0) then
      invalid_arg "connected_component: input node does not satisfy predicate" ;
    let count = Graph.nb_vertex graph in
    let table = Vertex_table.create count in
    let rec loop v queue =
      if Vertex_table.mem table v then next queue
      else if not (predicate v) then ()
      else (
        Vertex_table.add table v () ;
        Graph.iter_succ
          (fun v ->
            if (not (Vertex_table.mem table v)) && predicate v then
              Queue.add v queue)
          graph
          v ;
        next queue)
    and next queue =
      match Queue.take_opt queue with None -> () | Some v' -> loop v' queue
    in
    loop v0 (Queue.create ()) ;
    table

  let connected_component graph v0 predicate =
    let table = connected_component_ graph v0 predicate in
    Vertex_table.to_seq_keys table

  let is_induced_subgraph_connected graph predicate =
    let vertices =
      Graph.fold_vertex (fun v l -> if predicate v then v :: l else l) graph []
    in
    match vertices with
    | [] -> true
    | v0 :: _ ->
        let table = connected_component_ graph v0 predicate in
        List.for_all (fun v -> Vertex_table.mem table v) vertices

  let incr graph v map =
    let deg = Graph.out_degree graph v in
    Basic_impl.Int_map.update
      deg
      (fun count_opt -> Some (Option.value ~default:0 count_opt + 1))
      map

  let degree_dist graph =
    let degrees =
      Graph.fold_vertex (incr graph) graph Basic_impl.Int_map.empty
    in
    let degrees =
      Basic_impl.Int_map.fold
        (fun deg count acc -> (deg, float_of_int count) :: acc)
        degrees
        []
      |> Array.of_list
    in
    Fin.Float.(
      normalize (measure (of_assoc (module Helpers.Int_table) degrees)))

  let cut graph subset =
    let set = Graph.fold_vertex Vertex_set.add graph Vertex_set.empty in
    if not (Vertex_set.subset subset set) then invalid_arg "cut" ;
    let co_subset = Vertex_set.diff set subset in
    Graph.fold_edges
      (fun v v' acc ->
        if
          (Vertex_set.mem v subset && Vertex_set.mem v' co_subset)
          || (Vertex_set.mem v' subset && Vertex_set.mem v co_subset)
        then (v, v') :: acc
        else acc)
      graph
      []

  module Tree = struct
    type t = { vertex : vertex; mutable subtrees : t list }

    let cons vertex subtrees = { vertex; subtrees }

    let uncons { vertex; subtrees } = (vertex, subtrees)

    let single vertex = { vertex; subtrees = [] }

    let rec mem_vertex tree v =
      Graph.V.equal tree.vertex v
      || List.exists (fun t -> mem_vertex t v) tree.subtrees

    let iter_vertices tree f =
      let rec iter_vertices tree f stack =
        f tree.vertex ;
        List.iter (fun t -> Stack.push t stack) tree.subtrees ;
        next f stack
      and next f stack =
        match Stack.pop_opt stack with
        | None -> ()
        | Some t -> iter_vertices t f stack
      in
      iter_vertices tree f (Stack.create ())

    let iter_edges tree f =
      let rec iter_edges tree f stack =
        List.iter
          (fun t ->
            f (tree.vertex, t.vertex) ;
            Stack.push t stack)
          tree.subtrees ;
        next f stack
      and next f stack =
        match Stack.pop_opt stack with
        | None -> ()
        | Some t -> iter_edges t f stack
      in
      iter_edges tree f (Stack.create ())
  end

  (* Uniform sampler on the neighbourhood of a vertex. *)
  let make_sampler graph vertex predicate =
    let neighbours =
      Graph.fold_succ
        (fun v l -> if predicate v then v :: l else l)
        graph
        vertex
        []
    in
    (match neighbours with [] -> assert false | _ -> ()) ;
    Array.of_list neighbours |> Gen.uniform

  let jump graph st vertex predicate =
    match Vertex_table.find_opt st vertex with
    | None ->
        let sampler = make_sampler graph vertex predicate in
        Vertex_table.add st vertex sampler ;
        sampler
    | Some sampler -> sampler

  let rec rwalk graph st vt v count predicate rng_state () =
    if count = 0 then Seq.Nil
    else if Vertex_table.mem vt v then
      Seq.Cons
        ( v,
          rwalk
            graph
            st
            vt
            (jump graph st v predicate rng_state)
            count
            predicate
            rng_state )
    else (
      Vertex_table.replace vt v () ;
      Seq.Cons
        ( v,
          rwalk
            graph
            st
            vt
            (jump graph st v predicate rng_state)
            (count - 1)
            predicate
            rng_state ))

  let rec reconstruct tt prev rest =
    match Seq.uncons rest with
    | Some (v, rest) ->
        if Vertex_table.mem tt v then reconstruct tt v rest
        else
          let n = Vertex_table.find tt prev in
          let n' = Tree.single v in
          Vertex_table.add tt v n' ;
          n.Tree.subtrees <- n' :: n.Tree.subtrees ;
          reconstruct tt v rest
    | None -> ()

  let reconstruct tt path =
    match Seq.uncons path with
    | None -> assert false
    | Some (v, rest) -> reconstruct tt v rest

  let aldous_broder graph v0 predicate =
    let vertices = connected_component graph v0 predicate |> List.of_seq in
    match vertices with
    | [] -> assert false
    | [v] ->
        assert (Graph.V.equal v v0) ;
        fun _ -> Tree.single v0
    | _ ->
        let vcount = List.length vertices in
        (* samplers table *)
        let st = Vertex_table.create vcount in
        fun rng_state ->
          (* visited table *)
          let vt = Vertex_table.create vcount in
          let path = rwalk graph st vt v0 vcount predicate rng_state in
          assert (Vertex_table.to_seq_keys vt |> Seq.for_all predicate) ;
          (* tree table *)
          let tt = Vertex_table.create vcount in
          let root = Tree.single v0 in
          Vertex_table.add tt v0 root ;
          reconstruct tt path ;
          root
end
