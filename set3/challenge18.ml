open! Core
open! Import

let%expect_test "final" =
  let open Mirage_crypto.AES.ECB in
  let input =
    String_util.base64_to_raw
      "L77na/nrFsKvynd6HzOoG7GHTLXsTVu9qvY/2syLXzhPweyyMTJULu/6/kXX0KSvoOLSFQ=="
  in
  Ctr.encrypt input ~nonce:0L
    ~key:("YELLOW SUBMARINE" |> of_secret)
    ~cipher:encrypt
  |> print_endline;
  [%expect {|
    Yo, VIP Let's kick it Ice, Ice, baby Ice, Ice, baby |}]
