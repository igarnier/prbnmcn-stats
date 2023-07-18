module Emp = Stats.Emp

let empty = [||]

let data () = [| 0; 1; 1; 2; 2; 2; 3; 3; 3; 3; 4 |]

let total_mass = 11.

let mes = data

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"quantile err 1" ~count:1 QCheck.unit
  @@ fun () ->
  try
    ignore (Emp.quantile (module Std.Int) (mes ()) (-1.)) ;
    false
  with Invalid_argument _ -> true

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"quantile err 2" ~count:1 QCheck.unit
  @@ fun () ->
  try
    ignore (Emp.quantile (module Std.Int) (mes ()) 1.1) ;
    false
  with Invalid_argument _ -> true

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"quantile err 3" ~count:1 QCheck.unit
  @@ fun () ->
  try
    ignore (Emp.quantile (module Std.Int) empty 0.5) ;
    false
  with Invalid_argument _ -> true

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"quantile 0" ~count:1 QCheck.unit
  @@ fun () ->
  let i = Emp.quantile (module Std.Int) (mes ()) 0.0 in
  i = 0

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"quantile 1" ~count:1 QCheck.unit
  @@ fun () ->
  let i = Emp.quantile (module Std.Int) (mes ()) (1. /. total_mass) in
  i = 0

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"quantile 2" ~count:1 QCheck.unit
  @@ fun () ->
  let i = Emp.quantile (module Std.Int) (mes ()) (2. /. total_mass) in
  i = 1

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"quantile 3" ~count:1 QCheck.unit
  @@ fun () ->
  let i = Emp.quantile (module Std.Int) (mes ()) (3. /. total_mass) in
  i = 1

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"quantile 6" ~count:1 QCheck.unit
  @@ fun () ->
  let i = Emp.quantile (module Std.Int) (mes ()) (6. /. total_mass) in
  i = 2

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"quantile 6" ~count:1 QCheck.unit
  @@ fun () ->
  let i = Emp.quantile (module Std.Int) (mes ()) (total_mass /. total_mass) in
  i = 4
