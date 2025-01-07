open! Core
open! Import

let slow_rand () =
  let before_wait_len = Random.int 960 + 40 in
  let after_wait_len = Random.int 960 + 40 in
  Core_unix.sleep before_wait_len;
  let seed = Util.sec_since_epoch () in
  let mt = Mt19937.init seed in
  Core_unix.sleep after_wait_len;
  print_endline (Int.to_string seed);
  Mt19937.next mt

(* Disabled because it takes up to half an hour to run :/ *)
(*
let%expect_test "final" =
  let first_result = slow_rand () in
  let rec try_backwards n =
    if first_result = Mt19937.(init n |> next) then
      n
    else
      try_backwards (n - 1)
  in
  let original_seed = try_backwards (sec_since_epoch ()) in
  print_endline (Int.to_string original_seed);
  [%expect {|
    1694984511
    1694984511 |}]
*)
