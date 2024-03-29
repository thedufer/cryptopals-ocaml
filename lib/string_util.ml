open! Core

let hex_char_to_int = function
  | '0'..'9' as c -> Char.to_int c - Char.to_int '0'
  | 'a'..'f' as c -> Char.to_int c - Char.to_int 'a' + 10
  | 'A'..'F' as c -> Char.to_int c - Char.to_int 'A' + 10
  | c -> raise_s [%message "not a hex character" (c : char)]

let char_to_hex_char c =
  let int =
    match c with
    | '\000'..'\009' as c -> Char.to_int c + Char.to_int '0'
    | '\010'..'\015' as c -> Char.to_int c - 10 + Char.to_int 'a'
    | c -> raise_s [%message "high bits set in char_to_hex_char" (c : char)]
  in
  Char.of_int_exn int

let hex_to_raw hex =
  String.to_list hex
  |> List.chunks_of ~length:2
  |> List.map ~f:(function
      | [a; b] -> (hex_char_to_int a * 16 + hex_char_to_int b) |> Char.of_int_exn
      | _ -> raise_s [%message "not a whole number of bytes in hex_to_raw"])
  |> String.of_list

let raw_to_hex raw =
  String.to_list raw
  |> List.concat_map ~f:(fun c ->
      let c = Char.to_int c in
      [ c lsr 4; c land 0xf ]
    )
  |> List.map ~f:(fun int -> Char.of_int_exn int |> char_to_hex_char)
  |> String.of_list

let raw_char_to_base64_char c =
  let int =
    match c with
    | '\000'..'\025' -> Char.to_int c + Char.to_int 'A'
    | '\026'..'\051' -> Char.to_int c - 26 + Char.to_int 'a'
    | '\052'..'\061' -> Char.to_int c - 52 + Char.to_int '0'
    | '\062' -> Char.to_int '+'
    | '\063' -> Char.to_int '/'
    | _ -> raise_s [%message "high bits set in raw_char_to_base64_char" (c : char)]
  in
  Char.of_int_exn int

let base64_char_to_raw_char c =
  let int =
    match c with
    | 'A'..'Z' -> Char.to_int c - Char.to_int 'A'
    | 'a'..'z' -> Char.to_int c - Char.to_int 'a' + 26
    | '0'..'9' -> Char.to_int c - Char.to_int '0' + 52
    | '+' -> Char.to_int '\062'
    | '/' -> Char.to_int '\063'
    | _ -> raise_s [%message "invalid base64 char" (c : char)]
  in
  Char.of_int_exn int

let raw_to_base64 base64 =
  let unpadded =
    String.to_list base64
    |> List.chunks_of ~length:3
    |> List.concat_map ~f:(fun chunk ->
        let ints =
          match chunk with
          | [a; b; c] ->
            let a = Char.to_int a in
            let b = Char.to_int b in
            let c = Char.to_int c in
            [ a lsr 2 
            ; ((a land 0x3) lsl 4) lor (b lsr 4)
            ; ((b land 0xf) lsl 2) lor (c lsr 6)
            ; c land 0x3f
            ]
          | [a; b] ->
            let a = Char.to_int a in
            let b = Char.to_int b in
            [ a lsr 2
            ; ((a land 0x3) lsl 4) lor (b lsr 4)
            ; ((b land 0xf) lsl 2)
            ]
          | [a] ->
            let a = Char.to_int a in
            [ a lsr 2 
            ; ((a land 0x3) lsl 4) 
            ]
          | _ -> assert false
        in
        List.map ints ~f:(fun int -> Char.of_int_exn int |> raw_char_to_base64_char))
  in
  let padded =
    unpadded @ (List.init ((-1 * List.length unpadded) % 4) ~f:(const '='))
  in
  String.of_list padded

let base64_to_raw base64 =
  let base64_char_to_raw_int c = base64_char_to_raw_char c |> Char.to_int in
  String.to_list base64
  |> List.chunks_of ~length:4
  |> List.concat_map ~f:(fun chunk ->
      match chunk with
      | [a; b; '='; '='] | [a; b] ->
        let a  = base64_char_to_raw_int a in
        let b  = base64_char_to_raw_int b in
        [ (a lsl 2) lor (b lsr 4) ]
      | [a; b; c; '='] | [a; b; c] ->
        let a  = base64_char_to_raw_int a in
        let b  = base64_char_to_raw_int b in
        let c  = base64_char_to_raw_int c in
        [ (a lsl 2) lor (b lsr 4)
        ; (b lsl 4) lor (c lsr 2)
        ]
      | [a; b; c; d] ->
        let a  = base64_char_to_raw_int a in
        let b  = base64_char_to_raw_int b in
        let c  = base64_char_to_raw_int c in
        let d  = base64_char_to_raw_int d in
        [ (a lsl 2) lor (b lsr 4)
        ; (b lsl 4) lor (c lsr 2)
        ; (c lsl 6) lor d
        ]
      | _ -> assert false)
  |> List.map ~f:(fun c ->
      (* we "shifted off" some high bits, but since we're thinking about them as
      chars they're actually still there *)
      c land 0xff |> Char.of_int_exn)
  |> String.of_list

let xor_raw a b =
  let zipped =
    match
      List.zip (String.to_list a) (String.to_list b)
    with
    | Ok l -> l
    | Unequal_lengths -> raise_s [%message "strings not of same length in xor_raw" (a : string) (b : string)]
  in
  List.map zipped ~f:(fun (a, b) ->
      (Char.to_int a lxor Char.to_int b)
      |> Char.of_int_exn)
  |> String.of_list

(* Letter frequencies:

   60	E	12	F
   45	T	  10	W Y
   40	A I N O S	9	G P
   32	H	  8	B
   31	R	  6	V
   22	D	  4	K
   20	L	  3	Q
   17	U   2	J X
   15	C M	1	Z
*)
let english_score text =
  String.lowercase text
  |> String.to_list
  |> List.map ~f:(function
      | 'A'..'Z' -> assert false
      | 'e' -> 60
      | 't' -> 45
      | 'a' | 'i' | 'n' | 'o' | 's' -> 40
      | 'h' -> 32
      | 'r' -> 31
      | 'd' -> 22
      | 'l' -> 20
      | 'u' -> 17
      | 'c' | 'm' -> 15
      | 'f' -> 12
      | 'w' | 'y' -> 10
      | 'g' | 'p' -> 9
      | 'b' -> 8
      | 'v' -> 6
      | 'k' -> 4
      | 'q' -> 3
      | 'j' | 'x' -> 2
      | 'z' -> 1
      (* non-printables *)
      | '\000'..'\008' | '\014'..'\027' | '\127' -> -10
      | '\128'..'\255' -> -50
      (* some whitespace *)
      | '\t' | '\r' | '\n' | '\011'..'\012' | '\028'..'\031' -> 0
      | '0'..'9' -> 10
      | ' ' -> 40
      (* punctuation *)
      | '\033'..'\047' | '\058'..'\064' | '\091'..'\096' | '\123'..'\126' -> 1
    )
  |> List.sum (module Int) ~f:Fn.id

(*
   n%4  pad 4-n%4
   0    0   4
   1    3   3
   2    2   2
   3    1   1

*)

let hamming_distance a b =
  let diff = xor_raw a b in
  let padding_length = 4 - (String.length diff % 4) in
  let padded = diff ^ (String.init padding_length ~f:(const '\000')) in
  List.init (String.length padded / 4) ~f:Fn.id
  |> List.sum (module Int) ~f:(fun i ->
      Stdlib.String.get_int32_ne padded (i * 4)
      |> Int32.popcount)

let pkcs7_padding input ~blocksize =
  let padding_length = blocksize - (String.length input % blocksize) in
  assert (padding_length >= 0);
  input ^ String.init padding_length ~f:(const (Char.of_int_exn padding_length))

let remove_pkcs7_padding input =
  let last_char = String.get input (String.length input - 1) in
  let last_char_i = Char.to_int last_char in
  assert (last_char_i > 0);
  let result_length = String.length input - last_char_i in
  List.init last_char_i ~f:(( + ) result_length)
  |> List.iter ~f:(fun i ->
      assert (Char.equal last_char (String.get input i)));
  String.sub input ~pos:0 ~len:result_length

let random_bytes length =
  let bytes = Bytes.create length in
  List.init length ~f:(fun _ -> Random.char ())
  |> List.iteri ~f:(fun i c -> Bytes.set bytes i c);
  Bytes.to_string bytes

let to_blocks input ~blocksize =
  String.to_list input |> List.chunks_of ~length:blocksize |> List.map ~f:String.of_list

let map_byte str idx ~f =
  let new_char = String.get str idx |> f in
  let bytes = Bytes.of_string str in
  Bytes.set bytes idx new_char;
  Bytes.to_string bytes

let map_byte_i str idx ~f =
  map_byte str idx ~f:(fun c -> f (Char.to_int c) |> Char.of_int_exn)

let map_bytes_i str updates =
  List.fold updates ~init:str ~f:(fun str (idx, f) -> map_byte_i str idx ~f)
