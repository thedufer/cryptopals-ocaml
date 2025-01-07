open! Core
open! Import

let encrypt ~key plaintext =
  let blocks = String_util.to_blocks plaintext ~blocksize:4 in
  let rng = Mt19937.init key in
  List.map blocks ~f:(fun block ->
      let keyblocki = Mt19937.next' rng in
      let keyblock = Bytes.create 4 in
      Bytes.unsafe_set_int32 keyblock 0 keyblocki;
      let keyblock = Bytes.to_string keyblock |> String.subo ~len:(String.length block) in
      String_util.xor_raw block keyblock)
  |> String.concat

let suffix = "AAAAAAAAAAAAAA"

let plaintext () =
  String.init (Random.int 32) ~f:(fun _ -> Random.char ()) ^ suffix

let random_key () = Random.int (1 lsl 16)

let ciphertext ~key =
  encrypt ~key (plaintext ())

let get_key ciphertext ~min ~max =
  List.init (max - min) ~f:(fun i ->
      let i = i + min in
      i, encrypt ~key:i ciphertext)
  |> List.find ~f:(fun (_, plaintext) ->
      String.is_suffix ~suffix plaintext)
  |> Option.map ~f:fst

let password_token () =
  let key = Util.sec_since_epoch () in
  encrypt ~key (plaintext ())

let not_password_token () =
  String.init ((Random.int 32) + 41) ~f:(fun _ -> Random.char ())

let is_password_token ciphertext =
  let now = Util.sec_since_epoch () in
  get_key ciphertext ~min:(now - 100) ~max:(now + 100) |> Option.is_some

let%expect_test "encrypt" =
  let key = 0xa52b in
  let plaintext = "this is a sentence we can encrypt" in
  let ciphertext = encrypt ~key plaintext in
  let round_tripped = encrypt ~key ciphertext in
  print_s [%message (plaintext : string) (round_tripped : string)];
  [%expect {|
    ((plaintext "this is a sentence we can encrypt")
     (round_tripped "this is a sentence we can encrypt")) |}]

(* commented out because it takes too long *)
(*
let%expect_test "find key" =
  let key = random_key () in
  let ciphertext = ciphertext ~key in
  let key_guess = get_key ciphertext ~min:0 ~max:(1 lsl 16) in
  print_s [%message (key : int) (key_guess : int option)];
  [%expect {| ((key 54467) (key_guess (54467))) |}]
   *)

let%expect_test "is password token" =
  let test ciphertext result =
    let guess_result = is_password_token ciphertext in
    print_s [%message (result : bool) (guess_result : bool)];
  in
  let test_real () = test (password_token ()) true in
  let test_fake () = test (not_password_token ()) false in
  test_real ();
  test_fake ();
  test_real ();
  test_real ();
  test_fake ();
  test_real ();
  test_fake ();
  [%expect {|
    ((result true) (guess_result true))
    ((result false) (guess_result false))
    ((result true) (guess_result true))
    ((result true) (guess_result true))
    ((result false) (guess_result false))
    ((result true) (guess_result true))
    ((result false) (guess_result false)) |}]
