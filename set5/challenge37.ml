open Core
open Async
open! Import

module Z = struct
  include Z

  include Binable.Of_binable_with_uuid (String) (struct
      type t = Z.t
      let to_binable = Z.to_bits
      let of_binable = Z.of_bits
      let caller_identity = Bin_shape.Uuid.of_string "2720004d-7fcf-43eb-9c49-1e16a6e38d44"
    end)
end

module Protocol = struct
  type proposal = string * Z.t [@@deriving bin_io]

  type salt = string * Z.t [@@deriving bin_io]

  type login = string [@@deriving bin_io]

  type result =
    | Logged_in of string
    | Nope
  [@@deriving bin_io, sexp_of]
end

let read_bin_prot_exn reader b_reader =
  match%bind Reader.read_bin_prot reader b_reader with
  | `Eof -> raise_s [%message "EOF"]
  | `Ok x -> return x

let start_server ~email ~password =
  Tcp.Server.create
    ~on_handler_error:`Ignore
    Tcp.Where_to_listen.of_port_chosen_by_os
    (fun _ reader writer ->
       let context = Challenge36.s_init ~email ~password in
       let%bind proposal = read_bin_prot_exn reader Protocol.bin_reader_proposal in
       let context, salt = Challenge36.s_second_message context proposal in
       Writer.write_bin_prot writer Protocol.bin_writer_salt salt;
       let%bind login = read_bin_prot_exn reader Protocol.bin_reader_login in
       let result =
         match Challenge36.s_validate context login with
         | email -> Protocol.Logged_in email
         | exception _ -> Nope
       in
       Writer.write_bin_prot writer Protocol.bin_writer_result result;
       return ())

let normal_client ~port ~email ~password =
  let%bind _, reader, writer = Tcp.connect (Tcp.Where_to_connect.of_host_and_port {host="localhost"; port}) in
  let context, proposal = Challenge36.c_first_message ~email ~password in
  Writer.write_bin_prot writer Protocol.bin_writer_proposal proposal;
  let%bind salt = read_bin_prot_exn reader Protocol.bin_reader_salt in
  let login = Challenge36.c_third_message context salt in
  Writer.write_bin_prot writer Protocol.bin_writer_login login;
  let%bind result = read_bin_prot_exn reader Protocol.bin_reader_result in
  print_s [%sexp (result : Protocol.result)];
  return ()

let%expect_test "protocol" =
  let email = "test@example.com" in
  let password = "a secret" in
  let%bind server = start_server ~email ~password in
  let port = Tcp.Server.listening_on server in
  let%bind () = normal_client ~port ~email ~password in
  [%expect {| (Logged_in test@example.com) |}];
  let%bind () = Tcp.Server.close server in
  return ()

let client_S_0 ~port ~email ~big_a ~big_s =
  let%bind _, reader, writer = Tcp.connect (Tcp.Where_to_connect.of_host_and_port {host="localhost"; port}) in
  let proposal = (email, big_a) in
  Writer.write_bin_prot writer Protocol.bin_writer_proposal proposal;
  let%bind (salt, _big_b) = read_bin_prot_exn reader Protocol.bin_reader_salt in
  let big_k = Z.to_bits big_s |> Digestif.SHA256.digest_string |> Digestif.SHA256.to_raw_string in
  let login = Digestif.SHA256.hmac_string ~key:salt big_k |> Digestif.SHA256.to_raw_string in
  Writer.write_bin_prot writer Protocol.bin_writer_login login;
  let%bind result = read_bin_prot_exn reader Protocol.bin_reader_result in
  print_s [%sexp (result : Protocol.result)];
  return ()

let%expect_test "A = 0 mod n" =
  let email = "test@example.com" in
  let password = "a secret" in
  let%bind server = start_server ~email ~password in
  let port = Tcp.Server.listening_on server in
  (* note that the client doesn't have the password *)
  let%bind () = client_S_0 ~port ~email ~big_a:Z.zero ~big_s:Z.zero in
  [%expect {| (Logged_in test@example.com) |}];
  let%bind () = client_S_0 ~port ~email ~big_a:Challenge36.n ~big_s:Z.zero in
  [%expect {| (Logged_in test@example.com) |}];
  let%bind () = client_S_0 ~port ~email ~big_a:(Z.mul Challenge36.n (Z.of_int 2)) ~big_s:Z.zero in
  [%expect {| (Logged_in test@example.com) |}];
  let%bind () = Tcp.Server.close server in
  return ()
