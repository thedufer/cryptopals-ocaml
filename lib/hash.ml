open Core

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
    String_util.to_blocks input ~blocksize:64
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
    String_util.to_blocks ~blocksize:64 input
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
