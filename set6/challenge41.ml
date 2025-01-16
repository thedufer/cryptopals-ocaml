open Core
open! Import

module Z = struct
  include Z
  include Sexpable.Of_stringable (Z)
end

module Z_hashset = Hash_set.Make (Z)

let oracle, cipher, pub =
  let seen = Z_hashset.create () in
  let pub, priv = Rsa.gen_rsa_keys ~bytes:32 in
  let oracle cipher =
    if Hash_set.mem seen cipher then
      Error `Already_seen
    else (
      Hash_set.add seen cipher;
      Ok (Rsa.decrypt priv cipher))
  in
  let cipher =
    let plain = "this is indeed the message you're looking for" in
    let cipher = Rsa.encrypt pub (Z.of_bits plain) in
    let plain_rt = oracle cipher |> Result.ok |> Option.value_exn in
    assert (String.equal plain (Rsa.to_bits_trim plain_rt));
    cipher
  in
  oracle, cipher, pub

let solve oracle (`Pub (e, n)) cipher =
  let s = Z.of_int 2 in
  let cipher' = Z.((powm s e n * cipher) mod n) in
  let result' = oracle cipher' in
  Result.map result' ~f:(fun plain' ->
      Z.((invert s n * plain') mod n)
      |> Rsa.to_bits_trim)

let%expect_test "final" =
  let result = oracle cipher in
  print_s [%sexp (result : (Z.t, [`Already_seen]) Result.t)];
  [%expect {| (Error Already_seen) |}];
  let new_result = solve oracle pub cipher in
  print_s [%sexp (new_result : (string, [`Already_seen]) Result.t)];
  [%expect {|
    (Ok "this is indeed the message you're looking for") |}]
