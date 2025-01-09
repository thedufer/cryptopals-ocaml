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

(* a common operation in hash functions *)
let leftrotate i n =
  let open Int32.O in
  assert (Int.between n ~low:0 ~high:32);
  (i lsl n) lor (i lsr Int.O.(32 - n))

let sha1_padding ~len =
  let padding =
    let len_with_length_suffix_and_extra_byte = len + 1 + 8 in
    let padding_len = 64 - len_with_length_suffix_and_extra_byte % 64 in
    let padding_len = if padding_len = 64 then 0 else padding_len in
    String.make padding_len '\000'
  in
  let len_suffix =
    let bs = Bigstring.create 8 in
    Bigstring.unsafe_set_int64_t_be bs ~pos:0 (Int64.of_int (len * 8));
    Bigstring.to_string bs
  in
  String.of_char '\x80' ^ padding ^ len_suffix

let sha1_seeded ~h0 ~h1 ~h2 ~h3 ~h4 ~prefix_len input =
  let input = input ^ sha1_padding ~len:(prefix_len + String.length input) in
  let h0, h1, h2, h3, h4 =
    to_blocks input ~blocksize:64
    |> List.fold ~init:(h0, h1, h2, h3, h4) ~f:(fun (h0, h1, h2, h3, h4) block ->
        let w = Array.create ~len:80 0l in
        let block_bs = Bigstring.of_string block in
        for i = 0 to 15 do
          w.(i) <- Bigstring.get_int32_t_be block_bs ~pos:(4 * i)
        done;
        for i = 16 to 79 do
          let (lxor) = Int32.(lxor) in
          w.(i) <- leftrotate (w.(i-3) lxor w.(i-8) lxor w.(i-14) lxor w.(i-16)) 1
        done;
        let a, b, c, d, e =
          Array.foldi w ~init:(h0, h1, h2, h3, h4) ~f:(fun i (a, b, c, d, e) w_i ->
              let open Int32.O in
              let f, k =
                if Int.between i ~low:0 ~high:19 then
                  (b land c) lor ((lnot b) land d), 0x5A827999l
                else if Int.between i ~low:20 ~high:39 then
                  b lxor c lxor d, 0x6ED9EBA1l
                else if Int.between i ~low:40 ~high:59 then
                  (b land c) lor (b land d) lor (c land d), 0x8F1BBCDCl
                else
                  b lxor c lxor d, 0xCA62C1D6l
              in
              (leftrotate a 5) + f + e + k + w_i , a, leftrotate b 30, c, d)
        in
        let open Int32.O in
        (h0 + a, h1 + b, h2 + c, h3 + d, h4 + e))
  in
  let result_bs = Bigstring.create 20 in
  Bigstring.set_int32_t_be result_bs ~pos:0 h0;
  Bigstring.set_int32_t_be result_bs ~pos:4 h1;
  Bigstring.set_int32_t_be result_bs ~pos:8 h2;
  Bigstring.set_int32_t_be result_bs ~pos:12 h3;
  Bigstring.set_int32_t_be result_bs ~pos:16 h4;
  Bigstring.to_string result_bs

let sha1 =
  let h0 = 0x67452301l in
  let h1 = 0xEFCDAB89l in
  let h2 = 0x98BADCFEl in
  let h3 = 0x10325476l in
  let h4 = 0xC3D2E1F0l in
  sha1_seeded ~h0 ~h1 ~h2 ~h3 ~h4 ~prefix_len:0

let md4_padding ~len =
  let padding =
    let len_with_length_suffix_and_extra_byte = len + 1 + 8 in
    let padding_len = 64 - len_with_length_suffix_and_extra_byte % 64 in
    let padding_len = if padding_len = 64 then 0 else padding_len in
    String.make padding_len '\000'
  in
  let len_suffix =
    let bs = Bigstring.create 8 in
    Bigstring.unsafe_set_int64_t_le bs ~pos:0 (Int64.of_int (len * 8));
    Bigstring.to_string bs
  in
  String.of_char '\x80' ^ padding ^ len_suffix

let md4_seeded ~a ~b ~c ~d ~prefix_len input =
  let open Int32.O in
  let input = input ^ md4_padding ~len:Int.(prefix_len + String.length input) in
  let f x y z = (x land y) lor ((lnot x) land z) in
  let g x y z = (x land y) lor (y land z) lor (z land x) in
  let h x y z = x lxor y lxor z in
  let a, b, c, d =
    to_blocks ~blocksize:64 input
    |> List.fold ~init:(a, b, c, d) ~f:(fun (aa, bb, cc, dd) block ->
        let block_bs = Bigstring.of_string block in
        let a, b, c, d = aa, bb, cc, dd in
        let get_word pos =
          Bigstring.get_int32_t_le block_bs ~pos:Int.O.(pos * 4)
        in
        (* round 1 *)
        let a = leftrotate (a + (f b c d) + (get_word 0)) 3 in
        let d = leftrotate (d + (f a b c) + (get_word 1)) 7 in
        let c = leftrotate (c + (f d a b) + (get_word 2)) 11 in
        let b = leftrotate (b + (f c d a) + (get_word 3)) 19 in
        let a = leftrotate (a + (f b c d) + (get_word 4)) 3 in
        let d = leftrotate (d + (f a b c) + (get_word 5)) 7 in
        let c = leftrotate (c + (f d a b) + (get_word 6)) 11 in
        let b = leftrotate (b + (f c d a) + (get_word 7)) 19 in
        let a = leftrotate (a + (f b c d) + (get_word 8)) 3 in
        let d = leftrotate (d + (f a b c) + (get_word 9)) 7 in
        let c = leftrotate (c + (f d a b) + (get_word 10)) 11 in
        let b = leftrotate (b + (f c d a) + (get_word 11)) 19 in
        let a = leftrotate (a + (f b c d) + (get_word 12)) 3 in
        let d = leftrotate (d + (f a b c) + (get_word 13)) 7 in
        let c = leftrotate (c + (f d a b) + (get_word 14)) 11 in
        let b = leftrotate (b + (f c d a) + (get_word 15)) 19 in
        (* round 2 *)
        let a = leftrotate (a + (g b c d) + (get_word 0) + 0x5a827999l) 3 in
        let d = leftrotate (d + (g a b c) + (get_word 4) + 0x5a827999l) 5 in
        let c = leftrotate (c + (g d a b) + (get_word 8) + 0x5a827999l) 9 in
        let b = leftrotate (b + (g c d a) + (get_word 12) + 0x5a827999l) 13 in
        let a = leftrotate (a + (g b c d) + (get_word 1) + 0x5a827999l) 3 in
        let d = leftrotate (d + (g a b c) + (get_word 5) + 0x5a827999l) 5 in
        let c = leftrotate (c + (g d a b) + (get_word 9) + 0x5a827999l) 9 in
        let b = leftrotate (b + (g c d a) + (get_word 13) + 0x5a827999l) 13 in
        let a = leftrotate (a + (g b c d) + (get_word 2) + 0x5a827999l) 3 in
        let d = leftrotate (d + (g a b c) + (get_word 6) + 0x5a827999l) 5 in
        let c = leftrotate (c + (g d a b) + (get_word 10) + 0x5a827999l) 9 in
        let b = leftrotate (b + (g c d a) + (get_word 14) + 0x5a827999l) 13 in
        let a = leftrotate (a + (g b c d) + (get_word 3) + 0x5a827999l) 3 in
        let d = leftrotate (d + (g a b c) + (get_word 7) + 0x5a827999l) 5 in
        let c = leftrotate (c + (g d a b) + (get_word 11) + 0x5a827999l) 9 in
        let b = leftrotate (b + (g c d a) + (get_word 15) + 0x5a827999l) 13 in
        (* round 3 *)
        let a = leftrotate (a + (h b c d) + (get_word 0) + 0x6ed9eba1l) 3 in
        let d = leftrotate (d + (h a b c) + (get_word 8) + 0x6ed9eba1l) 9 in
        let c = leftrotate (c + (h d a b) + (get_word 4) + 0x6ed9eba1l) 11 in
        let b = leftrotate (b + (h c d a) + (get_word 12) + 0x6ed9eba1l) 15 in
        let a = leftrotate (a + (h b c d) + (get_word 2) + 0x6ed9eba1l) 3 in
        let d = leftrotate (d + (h a b c) + (get_word 10) + 0x6ed9eba1l) 9 in
        let c = leftrotate (c + (h d a b) + (get_word 6) + 0x6ed9eba1l) 11 in
        let b = leftrotate (b + (h c d a) + (get_word 14) + 0x6ed9eba1l) 15 in
        let a = leftrotate (a + (h b c d) + (get_word 1) + 0x6ed9eba1l) 3 in
        let d = leftrotate (d + (h a b c) + (get_word 9) + 0x6ed9eba1l) 9 in
        let c = leftrotate (c + (h d a b) + (get_word 5) + 0x6ed9eba1l) 11 in
        let b = leftrotate (b + (h c d a) + (get_word 13) + 0x6ed9eba1l) 15 in
        let a = leftrotate (a + (h b c d) + (get_word 3) + 0x6ed9eba1l) 3 in
        let d = leftrotate (d + (h a b c) + (get_word 11) + 0x6ed9eba1l) 9 in
        let c = leftrotate (c + (h d a b) + (get_word 7) + 0x6ed9eba1l) 11 in
        let b = leftrotate (b + (h c d a) + (get_word 15) + 0x6ed9eba1l) 15 in
        (aa + a, bb + b, cc + c, dd + d))
  in
  let result_bs = Bigstring.create 16 in
  Bigstring.set_int32_t_le result_bs ~pos:0 a;
  Bigstring.set_int32_t_le result_bs ~pos:4 b;
  Bigstring.set_int32_t_le result_bs ~pos:8 c;
  Bigstring.set_int32_t_le result_bs ~pos:12 d;
  Bigstring.to_string result_bs

let md4 =
  let a = 0x67452301l in
  let b = 0xEFCDAB89l in
  let c = 0x98BADCFEl in
  let d = 0x10325476l in
  md4_seeded ~a ~b ~c ~d ~prefix_len:0
