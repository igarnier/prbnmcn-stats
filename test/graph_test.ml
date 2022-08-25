module Vertex = struct
  include Int

  let hash = Hashtbl.hash
end

module G = struct
  include Graph.Imperative.Digraph.ConcreteBidirectional (Vertex)

  module V = struct
    include V

    let pp = Format.pp_print_int
  end
end

module Graph_stats = Stats.Graph.Make (G)
open Graph_stats

let linear_diameter () =
  let g = G.create () in
  (* 0 - 1 - 2 - 3 - 4 *)
  G.add_edge g 0 1 ;
  G.add_edge g 1 2 ;
  G.add_edge g 4 3 ;
  G.add_edge g 3 2 ;
  assert (Stats.Graph.Dist.(diameter g = Fin 4))

let inf_diameter () =
  let g = G.create () in
  (* 0 - 1 - 2   3 - 4 *)
  G.add_edge g 0 1 ;
  G.add_edge g 1 2 ;
  G.add_edge g 4 3 ;
  assert (Stats.Graph.Dist.(diameter g = Inf))

let dists () =
  let g = G.create () in
  (* 0 - 1 - 2   3 - 4 *)
  G.add_edge g 0 1 ;
  G.add_edge g 1 2 ;
  G.add_edge g 4 3 ;
  let dists = floyd_warshall g in
  let open Stats.Graph.Dist in
  let oracle =
    [ (0, 0, Fin 0);
      (1, 1, Fin 0);
      (2, 2, Fin 0);
      (3, 3, Fin 0);
      (4, 4, Fin 0);
      (0, 1, Fin 1);
      (1, 0, Fin 1);
      (1, 2, Fin 1);
      (2, 1, Fin 1);
      (2, 3, Inf);
      (3, 2, Inf);
      (3, 4, Fin 1);
      (4, 3, Fin 1) ]
  in
  assert (List.for_all (fun (x, y, d) -> Table.find dists (x, y) = d) oracle)

let cut () =
  let g = G.create () in
  (*
     0 -- 1
     | \  |
     |  \ |
     2 -- 3
  *)
  G.add_edge g 0 1 ;
  G.add_edge g 0 2 ;
  G.add_edge g 0 3 ;
  G.add_edge g 1 3 ;
  G.add_edge g 2 3 ;
  let edges = cut g (Vertex_set.of_list [0; 2]) in
  let edges = List.sort Stdlib.compare edges in
  assert (match edges with [(0, 1); (0, 3); (2, 3)] -> true | _ -> false)

let () = linear_diameter ()

let () = inf_diameter ()

let () = dists ()

let () = cut ()
