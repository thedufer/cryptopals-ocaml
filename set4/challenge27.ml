open! Core
open! Import

let ciphertext, decryptor, check_key =
  let open Mirage_crypto.AES.ECB in
  let iv = String_util.random_bytes 16 in
  let key = iv |> of_secret in
  let blocksize = 16 in
  let plaintext = "onetwothreefourfivesixseveneightnineteneleventwelve" in
  let ciphertext =
    Cbc.encrypt plaintext ~iv ~key ~cipher:encrypt ~blocksize
  in
  let decryptor ciphertext =
    let plaintext = Cbc.decrypt ciphertext ~iv ~key ~cipher:decrypt ~blocksize in
    print_endline plaintext;
    let is_valid = String.for_all plaintext ~f:(fun c -> (Char.to_int c) lxor (1 lsl 7) = 0) in
    if is_valid then Ok () else Error plaintext
  in
  let check_key key' = String.equal key' iv in
  ciphertext, decryptor, check_key

(* In CBC, c1 = encrypt (iv ^ p1)
           c2 = encrypt (c1 ^ p2)
           c3 = encrypt (c2 ^ p3)
   etc

   In decryption, p1 = (decrypt c1) ^ iv
                  p2 = (decrypt c2) ^ c1
                  p3 = (decrypt c3) ^ c2
   etc

   So if we ask it to decrypt c1 + 0 + c1 + c4..., the result will be
   p'1 + garbage + (decrypt c1)
   since p1 = (decrypt c1) ^ iv, p'1 ^ p'3 = iv

   All that remains is to pad out the message with something that will pass
   padding removal. The easiest way is to try adding 32 0 bytes to the end of
   the aforementioned 3-block message, and then increment the 16th byte until we
   find one that works (doing so iterates through all possible values of the
   _last_ byte of the plaintext, and eventually we'll see a byte \x01 which is
   valid padding on its own). *)
let solve ciphertext decryptor =
  let c1 = String.sub ciphertext ~pos:0 ~len:16 in
  let zero_block = String.make 16 '\000' in
  let new_ciphertext_prefix = c1 ^ zero_block ^ c1 in
  let decryptor_with_padding_fix prefix =
    List.init 256 ~f:Fn.id
    |> List.find_map ~f:(fun i ->
        Option.try_with (fun () ->
            decryptor
              (prefix ^ String.make 15 '\000' ^ (String.of_char (Char.of_int_exn i)) ^ zero_block)))
  in
  let plaintext =
    match decryptor_with_padding_fix new_ciphertext_prefix with
    | Some (Ok ()) | None -> assert false
    | Some (Error plaintext) -> plaintext
  in
  let p'1 = String.sub plaintext ~pos:0 ~len:16 in
  let p'3 = String.sub plaintext ~pos:32 ~len:16 in
  String_util.xor_raw p'1 p'3

let%expect_test "final" =
  let key = solve ciphertext decryptor in
  check_key key |> Bool.to_string |> print_endline;
  [%expect {|
    onetwothreefourfŠökñã©D‘úıy¦^y;H¬(]ÃOòÂÎsü$3å™g¾<Û¤¼A¯±YËÎõ-JiÌÒLûÖÔÁì
    true |}]
