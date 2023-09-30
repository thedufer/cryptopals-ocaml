open! Core
open! Import

let (u, d) = (11, 0xFFFFFFFFl)
let (s, b) = (7,  0x9D2C5680l)
let (t, c) = (15, 0xEFC60000l)
let l = 18

let to_i x = Int.((of_int32_exn x) land ((1 lsl 32) - 1))

let of_i x =
  if x land (1 lsl 31) <> 0 then
    Int.((x lor (-1 lxor ((1 lsl 32) - 1))) |> to_int32_exn)
  else
    Int.to_int32_exn x

let temper y =
  let y = of_i y in
  let open Int32 in
  let y = y lxor ((y lsr u) land d) in
  let y = y lxor ((y lsl s) land b) in
  let y = y lxor ((y lsl t) land c) in
  let y = y lxor (y lsr l) in
  to_i y

let one_step ~shift ~mask y =
  let open Int32 in
  let rec aux acc y mask =
    if mask <> 0l then
      let y = shift y in
      aux (acc lxor (y land mask)) y (mask land shift mask)
    else
      acc
  in
  aux y y mask

let untemper y =
  let y = of_i y in
  let open Int32 in
  let y = y lxor (y lsr l) in
  let y = one_step ~shift:(fun x -> x lsl t) ~mask:c y in
  let y = one_step ~shift:(fun x -> x lsl s) ~mask:b y in
  let y = one_step ~shift:(fun x -> x lsr u) ~mask:d y in
  to_i y

let tag_exn tag f =
  try f () with
  | exn -> raise_s [%message tag (exn : exn)]

let clone rng =
  let state = Array.init 624 ~f:(fun _ -> Mt19937.next rng |> untemper |> of_i) in
  Mt19937.create_exn state

let%expect_test "final" =
  let seed = 0 in
  let orig = Mt19937.init seed in
  let cloned = clone orig in
  for _ = 0 to 10_000 do
    let orig_val = Mt19937.next orig in
    let cloned_val = Mt19937.next cloned in
    Expect_test_helpers_core.require_equal [%here] (module Int) orig_val cloned_val
  done;
  [%expect {| |}]
