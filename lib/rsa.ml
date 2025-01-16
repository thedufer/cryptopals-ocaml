open! Core

type pub = [`Pub of Z.t * Z.t]
type priv = [`Priv of Z.t * Z.t]

let random_prime ~bytes =
  let init =
    String_util.random_bytes bytes |> Z.of_bits
  in
  let init =
    if Z.is_even init then Z.succ init else init
  in
  let rec aux candidate =
    if Z.probab_prime candidate 10 <> 0
    then candidate
    else aux (candidate |> Z.succ |> Z.succ)
  in
  aux init

let rec gen_rsa_keys ~bytes =
  let p, q = random_prime ~bytes, random_prime ~bytes in
  let n = Z.mul p q in
  let et = Z.mul (Z.pred p) (Z.pred q) in
  let e = Z.of_int 3 in
  try
    let d = Z.invert e et in
    (`Pub (e, n), `Priv (d, n))
  with
  | _ -> gen_rsa_keys ~bytes

let encrypt (`Pub (e, n)) msg =
  Z.powm msg e n

let decrypt (`Priv (d, n)) msg =
  Z.powm msg d n

let to_bits_trim z =
  Z.to_bits z |> String.rstrip ~drop:(function '\000' -> true | _ -> false)
