open! Core
open! Import

let%expect_test "final" =
  let mt = Mt19937.init 0 in
  let iter () =
    let _ = Mt19937.next mt in
    ()
  in
  let test () =
    Mt19937.next mt 
    |> Int.to_string
    |> print_endline
  in
  for _ = 1 to 999 do iter () done;
  test ();
  (* drawn from a reference implementation at http://www.math.sci.hiroshima-u.ac.jp/m-mat/MT/MT2002/CODES/mt19937ar.c *)
  [%expect {| 3043451800 |}]
