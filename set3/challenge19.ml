open! Core
open! Import

let input =
  In_channel.read_lines "challenge19.txt"
  |> List.map ~f:(fun line ->
      String.strip line |> String_util.base64_to_raw)

let encrypt () =
  let open Mirage_crypto.AES.ECB in
  let key = String_util.random_bytes 16 |> of_secret in
  List.map input ~f:(Ctr.encrypt ~nonce:0L ~key ~cipher:encrypt)

let max_length = 38

let guess = "\x27f\162\169AJ\011\011\244\185\209\167\231\218srp\n\190\191\129|;1k \163\171\137\240\158\x74\x55\x7b\x82\x25\x96\x6b"

let plaintext_guesses () =
  encrypt ()
  |> List.map ~f:(fun c ->
      let guess_pref = String.sub guess ~pos:0 ~len:(String.length c) in
      String_util.xor_raw c guess_pref)

let%expect_test "guess from freq analysis" =
  let ciphertexts = encrypt () in
  let guess =
    List.init max_length ~f:(fun idx ->
        List.filter_map ciphertexts ~f:(fun ciphertext ->
            Option.try_with (fun () ->
                String.get ciphertext idx))
        |> List.map ~f:(fun c -> c, 1)
        |> Char.Map.of_alist_reduce ~f:( + ))
    |> List.map ~f:(fun map ->
        List.init 256 ~f:(fun i ->
            i,
            Char.Map.map_keys_exn map ~f:(fun c ->
                Char.of_int_exn (Char.to_int c lxor i)))
        |> List.filter ~f:(fun (_, map) ->
            Map.for_alli map ~f:(fun ~key ~data:_ -> Char.is_print key))
        |> List.max_elt ~compare:(Comparable.lift Int.compare ~f:(fun (_, map) ->
            Map.to_alist map
            |> List.map ~f:(fun (c, i) -> String.init i ~f:(const c))
            |> String.concat
            |> String_util.english_score))
        |> Option.value_exn
        |> fst
        |> Char.of_int_exn)
    |> String.of_list
  in
  print_s [%sexp (guess : string)];
  [%expect {|
     "\007f\162\169AJ\011\011\244\185\209\167\231\218srp\
    \n\190\191\129|;1k \163\171\137\240\158uTj\134\004\157\002" |}]

let%expect_test "final" =
  plaintext_guesses ()
  |> List.iter ~f:(fun c -> print_s [%sexp (c : string)]);
  [%expect {|
    "I have met them at close of day"
    "Coming with vivid faces"
    "From counter or desk among grey"
    "Eighteenth-century houses."
    "I have passed with a nod of the head"
    "Or polite meaningless words,"
    "Or have lingered awhile and said"
    "Polite meaningless words,"
    "And thought before I had done"
    "Of a mocking tale or a gibe"
    "To please a companion"
    "Around the fire at the club,"
    "Being certain that they and I"
    "But lived where motley is worn:"
    "All changed, changed utterly:"
    "A terrible beauty is born."
    "That woman's days were spent"
    "In ignorant good will,"
    "Her nights in argument"
    "Until her voice grew shrill."
    "What voice more sweet than hers"
    "When young and beautiful,"
    "She rode to harriers?"
    "This man had kept a school"
    "And rode our winged horse."
    "This other his helper and friend"
    "Was coming into his force;"
    "He might have won fame in the end,"
    "So sensitive his nature seemed,"
    "So daring and sweet his thought."
    "This other man I had dreamed"
    "A drunken, vain-glorious lout."
    "He had done most bitter wrong"
    "To some who are near my heart,"
    "Yet I number him in the song;"
    "He, too, has resigned his part"
    "In the casual comedy;"
    "He, too, has been changed in his turn,"
    "Transformed utterly:"
    "A terrible beauty is born." |}]
