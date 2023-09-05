open! Core

let get_blocksize_and_base_length oracle =
  let smallest_size = oracle "" |> String.length in
  let rec find_blocksize_and_suffix_length i =
    let ciphertext_length = oracle (String.init i ~f:(const 'A')) |> String.length in
    if ciphertext_length = smallest_size then
      find_blocksize_and_suffix_length (i + 1)
    else
      let blocksize = ciphertext_length - smallest_size in
      blocksize, ciphertext_length - blocksize - i
  in
  find_blocksize_and_suffix_length 1
