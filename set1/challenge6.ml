open! Core
open! Import

let get_base64_data () =
  let input = In_channel.read_all "challenge6.txt" in
  String.filter input ~f:(fun c -> not (Char.is_whitespace c))


let get_keysize input ~min ~max =
  List.init (max - min + 1) ~f:(( + ) min)
  |> List.map ~f:(fun keysize ->
      let dists =
        List.init (String.length input / keysize - 1) ~f:(fun i ->
            let fst =
              String.sub input ~pos:(i * keysize) ~len:keysize
            in
            let snd =
              String.sub input ~pos:((i + 1) * keysize) ~len:keysize
            in
            String_util.hamming_distance fst snd)
      in
      keysize, (List.sum (module Int) dists ~f:Fn.id |> Int.to_float) /. (List.length dists |> Int.to_float) /. (Int.to_float keysize))
  |> List.min_elt ~compare:(Comparable.lift Float.compare ~f:snd)
  |> Option.value_exn
  |> fst

let solve_for_keysize input keysize =
  let num_whole_blocks = String.length input / keysize in
  let size_last_block = String.length input % keysize in
  let key =
    List.init keysize ~f:(fun i ->
        let str =
          List.init (num_whole_blocks + (if i < size_last_block then 1 else 0)) ~f:(fun j ->
              String.get input (j * keysize + i))
          |> String.of_list
        in
        Xor.find_single_char_cipher_key str)
    |> String.of_list
  in
  Xor.multi_char_cipher input ~key

let%expect_test "hamming" =
  String_util.hamming_distance "this is a test" "wokka wokka!!!"
  |> Int.to_string
  |> print_endline;
  [%expect {| 37 |}]

let%expect_test "base64 decode" =
  let orig = get_base64_data () in
  let round_tripped =
    String_util.base64_to_raw orig
    |> String_util.raw_to_base64
  in
  Expect_test_helpers_core.require_equal [%here] (module String) orig round_tripped;
  [%expect {||}]

let%expect_test "keysize" =
  get_base64_data ()
  |> String_util.base64_to_raw
  |> get_keysize ~min:2 ~max:40
  |> Int.to_string
  |> print_endline;
  [%expect {| 29 |}]

let%expect_test "final" =
  let input = get_base64_data () |> String_util.base64_to_raw in
  let keysize = get_keysize input ~min:2 ~max:40 in
  solve_for_keysize input keysize |> print_endline;
  [%expect {|
    I'm back and I'm ringin' the bell
    A rockin' on the mike while the fly girls yell
    In ecstasy in the back of me
    Well that's my DJ Deshay cuttin' all them Z's
    Hittin' hard and the girlies goin' crazy
    Vanilla's on the mike, man I'm not lazy.

    I'm lettin' my drug kick in
    It controls my mouth and I begin
    To just let it flow, let my concepts go
    My posse's to the side yellin', Go Vanilla Go!

    Smooth 'cause that's the way I will be
    And if you don't give a damn, then
    Why you starin' at me
    So get off 'cause I control the stage
    There's no dissin' allowed
    I'm in my own phase
    The girlies sa y they love me and that is ok
    And I can dance better than any kid n' play

    Stage 2 -- Yea the one ya' wanna listen to
    It's off my head so let the beat play through
    So I can funk it up and make it sound good
    1-2-3 Yo -- Knock on some wood
    For good luck, I like my rhymes atrocious
    Supercalafragilisticexpialidocious
    I'm an effect and that you can bet
    I can take a fly girl and make her wet.

    I'm like Samson -- Samson to Delilah
    There's no denyin', You can try to hang
    But you'll keep tryin' to get my style
    Over and over, practice makes perfect
    But not if you're a loafer.

    You'll get nowhere, no place, no time, no girls
    Soon -- Oh my God, homebody, you probably eat
    Spaghetti with a spoon! Come on and say it!

    VIP. Vanilla Ice yep, yep, I'm comin' hard like a rhino
    Intoxicating so you stagger like a wino
    So punks stop trying and girl stop cryin'
    Vanilla Ice is sellin' and you people are buyin'
    'Cause why the freaks are jockin' like Crazy Glue
    Movin' and groovin' trying to sing along
    All through the ghetto groovin' this here song
    Now you're amazed by the VIP posse.

    Steppin' so hard like a German Nazi
    Startled by the bases hittin' ground
    There's no trippin' on mine, I'm just gettin' down
    Sparkamatic, I'm hangin' tight like a fanatic
    You trapped me once and I thought that
    You might have it
    So step down and lend me your ear
    '89 in my time! You, '90 is my year.

    You're weakenin' fast, YO! and I can tell it
    Your body's gettin' hot, so, so I can smell it
    So don't be mad and don't be sad
    'Cause the lyrics belong to ICE, You can call me Dad
    You're pitchin' a fit, so step back and endure
    Let the witch doctor, Ice, do the dance to cure
    So come up close and don't be square
    You wanna battle me -- Anytime, anywhere

    You thought that I was weak, Boy, you're dead wrong
    So come on, everybody and sing this song

    Say -- Play that funky music Say, go white boy, go white boy go
    play that funky music Go white boy, go white boy, go
    Lay down and boogie and play that funky music till you die.

    Play that funky music Come on, Come on, let me hear
    Play that funky music white boy you say it, say it
    Play that funky music A little louder now
    Play that funky music, white boy Come on, Come on, Come on
    Play that funky music |}]
