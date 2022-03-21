open Helpers

(* ------------------------------------------------------------------------- *)
(* Generate finite approximations of various dists *)

(* Basic algebraic properties *)

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"KL: diagonal" ~count:1 QCheck.unit
  @@ fun () ->
  Fin.Float.Dist.kl int_table binned_gaussian binned_gaussian =. 0.0

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"KL: nontrivial" ~count:1 QCheck.unit
  @@ fun () ->
  not (Fin.Float.Dist.kl int_table binned_gaussian binned_exp =. 0.0)

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"Lp: diagonal" QCheck.pos_float
  @@ fun p ->
  let p = 1. +. p in
  Fin.Float.Dist.lp int_table ~p binned_gaussian binned_gaussian =. 0.0

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"Linf: diagonal" ~count:1 QCheck.unit
  @@ fun () ->
  Fin.Float.Dist.linf int_table binned_gaussian binned_gaussian =. 0.0

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"Lp: nontrivial" ~count:1 QCheck.pos_float
  @@ fun p ->
  let p = 1. +. p in
  not (Fin.Float.Dist.lp int_table ~p binned_gaussian binned_exp =. 0.0)

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"Linf: nontrivial" ~count:1 QCheck.unit
  @@ fun () ->
  not (Fin.Float.Dist.linf int_table binned_gaussian binned_exp =. 0.0)

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"Lp: triangular" QCheck.pos_float
  @@ fun p ->
  let p = 1. +. p in
  let d = Fin.Float.Dist.lp int_table ~p in
  d binned_gaussian binned_exp +. d binned_exp binned_exp'
  <=. d binned_gaussian binned_exp'

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"Linf: triangular" ~count:1 QCheck.unit
  @@ fun () ->
  let d = Fin.Float.Dist.linf int_table in
  d binned_gaussian binned_exp +. d binned_exp binned_exp'
  <=. d binned_gaussian binned_exp'

(* ------------------------------------------------------------------------- *)
(* Convergence test *)

let truth = [| ("a", 0.5); ("b", 0.001); ("c", 1. -. (0.5 +. 0.001)) |]

let truth_fin_prb = Fin.Float.(probability (of_assoc string_table truth))

let categorical = Gen.categorical truth

let convergent_sequence dist =
  ListLabels.map
    [1_000; 10_000; 100_000; 500_000; 1_000_000]
    ~f:(fun nsamples ->
      let emp = Emp.of_generative ~nsamples categorical rng_state in
      let normalized =
        Fin.Float.normalize @@ Fin.Float.counts_of_empirical string_table emp
      in
      dist
        (Fin.Float.as_measure normalized)
        (Fin.Float.as_measure truth_fin_prb))

let make_conv_test distname f =
  QCheck.Test.check_exn
  @@ QCheck.Test.make
       ~name:(Printf.sprintf "%s: convergence" distname)
       ~count:1
       QCheck.unit
  @@ fun () ->
  let dists = f () in
  let fst = List.hd dists in
  let lst = List.hd (List.rev dists) in
  lst /. fst <=. 0.3

let () =
  make_conv_test "kl" (fun () ->
      convergent_sequence (Fin.Float.Dist.kl string_table))

let () =
  make_conv_test "l1" (fun () ->
      convergent_sequence (Fin.Float.Dist.lp string_table ~p:1.))

let () =
  make_conv_test "l2" (fun () ->
      convergent_sequence (Fin.Float.Dist.lp string_table ~p:2.))

let () =
  make_conv_test "linf" (fun () ->
      convergent_sequence (Fin.Float.Dist.linf string_table))

(* ------------------------------------------------------------------------- *)
(* Poisson sampler converges to its pdf *)

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"poisson" ~count:10 (QCheck.float_range 0.1 10.)
  @@ fun lambda ->
  let approx =
    Emp.of_generative ~nsamples:10_000 (Gen.poisson ~lambda) rng_state
    |> Fin.Float.counts_of_empirical int_table
    |> Fin.Float.normalize
  in
  let (`Probability empirical) = Fin.Float.list_of_probability approx in
  let theoretical =
    List.map (fun (x, _) -> (x, Pdfs.poisson ~lambda ~k:x)) empirical
    |> Array.of_list
    |> Fin.Float.of_assoc int_table
    |> Fin.Float.measure |> Fin.Float.normalize
  in
  let kl_div =
    Fin.Float.Dist.kl
      int_table
      (Fin.Float.as_measure approx)
      (Fin.Float.as_measure theoretical)
  in
  kl_div <. 0.01
