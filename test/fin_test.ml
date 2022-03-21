module Fin = Stats.Fin.Rational

let empty = Fin.(measure (of_array [||]))

let data = [| Q.of_int 1; Q.of_int 2; Q.of_int 3; Q.of_int 4; Q.of_int 1 |]

let total_mass = 11

let mes = Fin.(measure (of_array data))

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"quantile err 1" ~count:1 QCheck.unit
  @@ fun () ->
  try
    ignore (Fin.quantile (module Std.Int) mes Q.minus_one) ;
    false
  with Invalid_argument _ -> true

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"quantile err 2" ~count:1 QCheck.unit
  @@ fun () ->
  try
    ignore (Fin.quantile (module Std.Int) mes (Q.of_float 1.1)) ;
    false
  with Invalid_argument _ -> true

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"quantile err 3" ~count:1 QCheck.unit
  @@ fun () ->
  try
    ignore (Fin.quantile (module Std.Int) empty (Q.of_float 0.5)) ;
    false
  with Invalid_argument _ -> true

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"quantile 0" ~count:1 QCheck.unit
  @@ fun () ->
  let i = Fin.quantile (module Std.Int) mes Q.zero in
  i = 0

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"quantile 1" ~count:1 QCheck.unit
  @@ fun () ->
  let i = Fin.quantile (module Std.Int) mes Q.(1 // 11) in
  i = 0

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"quantile 2" ~count:1 QCheck.unit
  @@ fun () ->
  let i = Fin.quantile (module Std.Int) mes Q.(2 // 11) in
  i = 1

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"quantile 3" ~count:1 QCheck.unit
  @@ fun () ->
  let i = Fin.quantile (module Std.Int) mes Q.(3 // 11) in
  i = 1

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"quantile 6" ~count:1 QCheck.unit
  @@ fun () ->
  let i = Fin.quantile (module Std.Int) mes Q.(6 // 11) in
  i = 2

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"quantile 6" ~count:1 QCheck.unit
  @@ fun () ->
  let i = Fin.quantile (module Std.Int) mes Q.(11 // 11) in
  i = 4
