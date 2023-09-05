open! Core
open! Import

let parse str =
  String.split str ~on:'&'
  |> List.map ~f:(String.lsplit2_exn ~on:'=')

let serialize alist =
  List.map alist ~f:(fun (k, v) ->
      let clean =
        String.filter ~f:(function
            | '&' | '=' -> false
            | _ -> true)
      in
      clean k ^ "=" ^ clean v)
  |> String.concat ~sep:"&"

let profile_for email =
  serialize [("email", email); ("uid", "10"); ("role", "user")]

let profile_for_encrypted, decrypt_profile =
  let open Mirage_crypto.Cipher_block.AES.ECB in
  let key = String_util.random_bytes 16 |> Cstruct.of_string |> of_secret in
  let profile_for_encrypted email =
    profile_for email
    |> Ecb.encrypt ~blocksize:16 ~key ~cipher:encrypt
  in
  let decrypt_profile profile =
    Ecb.decrypt profile ~blocksize:16 ~key ~cipher:decrypt
    |> parse
  in
  profile_for_encrypted, decrypt_profile

let solve profile_for_encrypted =
  (* okay, so we know that our text gets inserted starting at position 6, and
     that the base length is 23
     we need to drop off at least 4 characters, so let's go for exactly 4
     we're going to aim for a final plaintext of 37 characters, that'll be 3 blocks
     the last block will be "admin" plus padding (11 0xbs), so we need to figure out how to encrypt that
     let's start there
  *)
  let final_block =
    let test_input =
      String.init 10 ~f:(const 'A') ^ "admin" ^ String.init 11 ~f:(const '\011')
    in
    let result = profile_for_encrypted test_input in
    String.sub result ~pos:16 ~len:16
  in
  (* now we just need the base
     that'll be 2 full blocks containing "email=fooba@baz.com&uid=10&role="
     easy.
  *)
  let base =
    let result = profile_for_encrypted "fooba@baz.com" in
    String.sub result ~pos:0 ~len:32
  in
  base ^ final_block

let%expect_test "k/v format" =
  parse "foo=bar&baz=qux&zap=zazzle" |> serialize |> print_endline;
  [%expect {| foo=bar&baz=qux&zap=zazzle |}];
  serialize [("foo&=bleah", "br&=re")] |> print_endline;
  [%expect {| foobleah=brre |}]

let%expect_test "profile_for" =
  profile_for "foo@bar.com" |> print_endline;
  [%expect {| email=foo@bar.com&uid=10&role=user |}];
  profile_for "" |> print_endline;
  [%expect {| email=&uid=10&role=user |}]

let%expect_test "final" =
  let ciphertext = solve profile_for_encrypted in
  decrypt_profile ciphertext
  |> [%sexp_of: (string * string) list]
  |> Sexp.to_string
  |> print_endline;
  [%expect {| ((email fooba@baz.com)(uid 10)(role admin)) |}]
