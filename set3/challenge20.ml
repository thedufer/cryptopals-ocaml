open! Core
open! Import

let input =
  In_channel.read_lines "challenge20.txt"
  |> List.map ~f:(fun line ->
      String.strip line
      |> String_util.base64_to_raw)

let encrypt () =
  let open Mirage_crypto.AES.ECB in
  let key = String_util.random_bytes 16 |> of_secret in
  List.map input ~f:(Ctr.encrypt ~nonce:0L ~key ~cipher:encrypt)

let solve () =
  let ciphertexts = encrypt () in
  let min_size =
    List.map ciphertexts ~f:String.length
    |> List.min_elt ~compare:Int.compare
    |> Option.value_exn
  in
  let single_input =
    List.map ciphertexts ~f:(fun line ->
        String.sub line ~pos:0 ~len:min_size)
    |> String.concat
  in
  let key = Xor.find_multi_char_cipher_key single_input ~keysize:min_size in
  let plaintext = Xor.multi_char_cipher single_input ~key in
  String_util.to_blocks plaintext ~blocksize:min_size

let%expect_test "final" =
  solve ()
  |> List.iter ~f:print_endline;
  [%expect {|
    N'm rated "R"...this is a warning, ya better void / P
    Duz I came back to attack others in spite- / Strike l
    Eut don't be afraid in the dark, in a park / Not a sc
    ^a tremble like a alcoholic, muscles tighten up / Wha
    Tuddenly you feel like your in a horror flick / You g
    Jusic's the clue, when I come your warned / Apocalyps
    Oaven't you ever heard of a MC-murderer? / This is th
    Ceath wish, so come on, step to this / Hysterical ide
    Ariday the thirteenth, walking down Elm Street / You
    Shis is off limits, so your visions are blurry / All
    Serror in the styles, never error-files / Indeed I'm
    Aor those that oppose to be level or next to this / I
    Porse than a nightmare, you don't have to sleep a win
    Alashbacks interfere, ya start to hear: / The R-A-K-I
    Shen the beat is hysterical / That makes Eric go get
    Toon the lyrical format is superior / Faces of death
    JC's decaying, cuz they never stayed / The scene of a
    She fiend of a rhyme on the mic that you know / It's
    Jelodies-unmakable, pattern-unescapable / A horn if w
    N bless the child, the earth, the gods and bomb the r
    Oazardous to your health so be friendly / A matter of
    Thake 'till your clear, make it disappear, make the n
    Nf not, my soul'll release! / The scene is recreated,
    Duz your about to see a disastrous sight / A performa
    Kyrics of fury! A fearified freestyle! / The "R" is i
    Jake sure the system's loud when I mention / Phrases
    ^ou want to hear some sounds that not only pounds but
    Shen nonchalantly tell you what it mean to me / Stric
    Fnd I don't care if the whole crowd's a witness! / I'
    Wrogram into the speed of the rhyme, prepare to start
    Jusical madness MC ever made, see it's / Now an emerg
    Hpen your mind, you will find every word'll be / Furi
    Eattle's tempting...whatever suits ya! / For words th
    ^ou think you're ruffer, then suffer the consequences
    N wake ya with hundreds of thousands of volts / Mic-t
    Iovocain ease the pain it might save him / If not, Er
    ^o Rakim, what's up? / Yo, I'm doing the knowledge, E
    Pell, check this out, since Norby Walters is our agen
    Lara Lewis is our agent, word up / Zakia and 4th and
    Hkay, so who we rollin' with then? We rollin' with Ru
    Dheck this out, since we talking over / This def beat
    N wanna hear some of them def rhymes, you know what I
    Shinkin' of a master plan / 'Cuz ain't nuthin' but sw
    To I dig into my pocket, all my money is spent / So I
    To I start my mission, leave my residence / Thinkin'
    N need money, I used to be a stick-up kid / So I thin
    N used to roll up, this is a hold up, ain't nuthin' f
    Eut now I learned to earn 'cuz I'm righteous / I feel
    Tearch for a nine to five, if I strive / Then maybe I
    To I walk up the street whistlin' this / Feelin' out
    F pen and a paper, a stereo, a tape of / Me and Eric
    Aish, which is my favorite dish / But without no mone
     Cuz I don't like to dream about gettin' paid / So I
    To now to test to see if I got pull / Hit the studio,
    Uakim, check this out, yo / You go to your girl house
     Cause my girl is definitely mad / 'Cause it took us
    ^o, I hear what you're saying / So let's just pump th
    Fnd count our money / Yo, well check this out, yo Eli
    Surn down the bass down / And let the beat just keep
    Fnd we outta here / Yo, what happened to peace? / Pea |}]
