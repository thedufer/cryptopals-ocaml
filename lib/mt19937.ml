open! Core
open Int32

let w = 32
let n = 624l
let m = 397l
let r = 31

let a = 0x9908B0DFl
let (u, d) = (11, 0xFFFFFFFFl)
let (s, b) = (7,  0x9D2C5680l)
let (t, c) = (15, 0xEFC60000l)
let l = 18

let f = 1812433253l

type t =
  { mt : int32 array
  ; mutable index : int32;
  }

let to_i = Int32.to_int_exn
let of_i = Int32.of_int_exn

module Array = struct
  include Array

  let get t i = get t (to_i i)
  let set t i a = set t (to_i i) a
end

let lower_mask = (1l lsl r) - 1l
let upper_mask = lnot lower_mask

let init seed =
  let mt = Array.init (to_i n) ~f:(const 0l) in
  let index = Int32.of_int_exn (to_i n) in
  mt.(0l) <- of_i seed;
  for i = 1 to Int.(to_i n - 1) do
    let i = of_i i in
    let prev = mt.(i - 1l) in
    mt.(i) <- Int32.(f * (prev lxor (prev lsr Int.(w - 2))) + i)
  done;
  { mt; index }

let create_exn mt =
  assert (Array.length mt |> of_i = n);
  { mt; index = n }

let twist t =
  for i = 0 to Int.(to_i n - 1) do
    let i = of_i i in
    let x = (t.mt.(i) land upper_mask) lor (t.mt.((i + 1l) % n) land lower_mask) in
    let xA = x lsr 1 in
    let xA =
      if (x % 2l) <> 0l then
        xA lxor a
      else
        xA
    in
    t.mt.(i) <- t.mt.((i + m) % n) lxor xA
  done;
  t.index <- 0l

let next' t_ =
  if t_.index >= n then twist t_;
  let y = t_.mt.(t_.index) in
  let y = y lxor ((y lsr u) land d) in
  let y = y lxor ((y lsl s) land b) in
  let y = y lxor ((y lsl t) land c) in
  let y = y lxor (y lsr l) in
  t_.index <- t_.index + 1l;
  y

let next t_ =
  let y = next' t_ in
  Int.((to_i y) land ((1 lsl 32) - 1))
