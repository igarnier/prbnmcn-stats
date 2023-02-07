module Vertex = struct
  include Int

  let hash = Hashtbl.hash
end

module G = struct
  include Graph.Imperative.Graph.Concrete (Vertex)

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
  assert (match edges with [(1, 0); (3, 0); (3, 2)] -> true | _ -> false)

let component () =
  let g = G.create () in
  (*
     0 -- 1
     | \  |
     |  \ |
     2 -- 3 -x- 4
  *)
  G.add_edge g 0 1 ;
  G.add_edge g 0 2 ;
  G.add_edge g 0 3 ;
  G.add_edge g 1 3 ;
  G.add_vertex g 4 ;
  let cc =
    connected_component g 0 (fun x -> x < 4)
    |> List.of_seq |> List.sort Int.compare
  in
  assert (match cc with [0; 1; 2; 3] -> true | _ -> false)

let subgraph_connected () =
  let g = G.create () in
  (*
     0 -- 1
     | \  |
     |  \ |
     2 -- 3 -x- 4
  *)
  G.add_edge g 0 1 ;
  G.add_edge g 0 2 ;
  G.add_edge g 0 3 ;
  G.add_edge g 1 3 ;
  G.add_vertex g 4 ;
  assert (is_induced_subgraph_connected g (fun x -> x <= 3)) ;
  assert (not @@ is_induced_subgraph_connected g (fun x -> x <> 1))

let () = linear_diameter ()

let () = inf_diameter ()

let () = dists ()

let () = cut ()

let () = component ()

let () = subgraph_connected ()

let iter_tree () =
  let open Tree in
  (*
     0 -- 1
     |    |
     |    |
     2    3 -- 4
  *)
  let tree = cons 0 [cons 2 []; cons 1 [cons 3 [cons 4 []]]] in
  let push r x = r := x :: !r in
  let verts =
    let acc = ref [] in
    iter_vertices tree (push acc) ;
    !acc
  in
  let edges =
    let acc = ref [] in
    iter_edges tree (push acc) ;
    !acc
  in
  (match List.sort Int.compare verts with
  | [0; 1; 2; 3; 4] -> ()
  | _ -> assert false) ;
  match List.sort Stdlib.compare edges with
  | [(0, 1); (0, 2); (1, 3); (3, 4)] -> ()
  | _ -> assert false

let spanning () =
  let g = G.create () in
  (*
     0 -- 1
     | \  |
     |  \ |
     2 -- 3 -- 4
  *)
  G.add_edge g 0 1 ;
  G.add_edge g 0 2 ;
  G.add_edge g 0 3 ;
  G.add_edge g 1 3 ;
  G.add_edge g 2 3 ;
  G.add_edge g 3 4 ;
  let edges = G.fold_edges_e (fun e acc -> e :: acc) g [] in
  let rng = Random.State.make [| 0x1337; 0x533D |] in
  for _ = 1 to 100 do
    let tree = aldous_broder g 0 (fun _ -> true) rng in
    let tree_edges =
      let acc = ref [] in
      let push r x = r := x :: !r in
      Tree.iter_edges tree (push acc) ;
      !acc
    in
    assert (
      List.for_all
        (fun (v, v') -> List.mem (v, v') edges || List.mem (v', v) edges)
        tree_edges)
  done

let () = iter_tree ()

let () = spanning ()
