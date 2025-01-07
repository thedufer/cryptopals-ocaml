open! Core
open! Import

let oracle, iv, check_padding =
  let open Mirage_crypto.AES.ECB in
  let key = String_util.random_bytes 16 |> of_secret in
  let iv = String.init 16 ~f:(const '\x00') in
  let blocksize = 16 in
  let data =
    In_channel.read_lines "challenge17.txt"
    |> List.map ~f:(fun line ->
        String.strip line |> String_util.base64_to_raw)
  in
  let oracle ~which =
    Cbc.encrypt (List.nth_exn data which) ~iv ~key ~cipher:encrypt ~blocksize
  in
  let check_padding ciphertext =
    match Cbc.decrypt ciphertext ~iv ~key ~cipher:decrypt ~blocksize with
    | exception _ -> false
    | _ -> true
  in
  oracle, iv, check_padding

let solve ciphertext ~iv check_padding =
  let determine_last_block ciphertext =
    let final_char =
      match
        List.init 256 ~f:Fn.id
        |> List.filter ~f:(fun i ->
            check_padding (String_util.map_byte_i ciphertext (String.length ciphertext - 17) ~f:((lxor) i)))
      with
      | [ 0 ] ->
        (* the only one that was valid was the base, which means that this must
           already be a 1, and the other possibilities aren't there *)
        1
      | [ 0; a ] | [ a ] ->
        let rec aux i =
          if String_util.map_bytes_i ciphertext
              [ String.length ciphertext - 17, ((lxor) a)
              ; String.length ciphertext - 17 - i, ((lxor) 1)]
             |> check_padding
          then
            a lxor i
          else
            aux (i + 1)
        in
        aux 1
      | _ -> assert false
    in
    let last_block_chars =
      List.init 15 ~f:Fn.id
      |> List.fold ~init:[final_char] ~f:(fun so_far idx ->
          let new_char =
            match
              List.init 256 ~f:Fn.id
              |> List.filter ~f:(fun i ->
                  String_util.map_bytes_i ciphertext
                    ((String.length ciphertext - 18 - idx, ((lxor) i))
                     :: List.mapi (List.rev so_far) ~f:(fun j c ->
                         String.length ciphertext - 17 - j, ((lxor) (c lxor (idx + 2)))))
                  |> check_padding)
            with
            | [ a ] -> a lxor (idx + 2)
            | _ -> assert false
          in
          new_char :: so_far)
    in
    List.map last_block_chars ~f:Char.of_int_exn |> String.of_list
  in
  List.init (String.length ciphertext / 16) ~f:Fn.id
  |> List.map ~f:(fun blocks_to_drop ->
        determine_last_block
          (iv ^ String.sub ciphertext ~pos:0 ~len:(String.length ciphertext - 16 * blocks_to_drop)))
  |> List.rev
  |> String.concat

let%expect_test "final" =
  List.init 10 ~f:Fn.id
  |> List.iter ~f:(fun which ->
      let ciphertext = oracle ~which in
      let plaintext = solve ciphertext ~iv check_padding |> String_util.remove_pkcs7_padding in
      print_s [%sexp (plaintext : string)]);
  [%expect {|
    "000000Now that the party is jumping"
    "000001With the bass kicked in and the Vega's are pumpin'"
    "000002Quick to the point, to the point, no faking"
    "000003Cooking MC's like a pound of bacon"
    "000004Burning 'em, if you ain't quick and nimble"
    "000005I go crazy when I hear a cymbal"
    "000006And a high hat with a souped up tempo"
    "000007I'm on a roll, it's time to go solo"
    "000008ollin' in my five point oh"
    "000009ith my rag-top down so my hair can blow" |}]
