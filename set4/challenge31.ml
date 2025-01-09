open Core
open Async
open Import

let%expect_test "hmac-sha1" =
  Hash.hmac_sha1 ~key:"key" "The quick brown fox jumps over the lazy dog"
  |> String_util.raw_to_hex
  |> print_endline;
  [%expect {| de7c9b85b8b78aa6bc8a7a36f70a90701c9db4d9 |}];
  return ()

let insecure_compare s1 s2 =
  let get_opt s i =
    if i < String.length s then Some (String.get s i) else None
  in
  let rec aux i =
    match get_opt s1 i, get_opt s2 i with
    | None, None -> return true
    | None, Some _ | Some _, None -> return false
    | Some c1, Some c2 ->
      if Char.equal c1 c2 then
        let%bind () = Clock_ns.after (Time_ns.Span.of_sec 0.05) in
        aux (i + 1)
      else return false
  in
  aux 0

let start_server () =
  let key = String_util.random_bytes 64 in
  let%bind server =
    Cohttp_async.Server.create
      ~on_handler_error:`Ignore
      Tcp.Where_to_listen.of_port_chosen_by_os
      (fun ~body:_ _ req ->
         let uri = Uri.of_string req.resource in
         match req.meth, Uri.path uri with
         | `GET, "/test" ->
           let file = Uri.get_query_param uri "file" in
           let signature = Uri.get_query_param uri "signature" in
           (match file, signature with
           | None, _ | _, None -> Cohttp_async.Server.respond_string ~status:`Bad_request "missing query param(s)"
           | Some file, Some signature ->
             let expected_sig = Hash.hmac_sha1 ~key file |> String_util.raw_to_hex in
             if%bind insecure_compare signature expected_sig then
               Cohttp_async.Server.respond_string "you did it!"
             else
               Cohttp_async.Server.respond_string ~status:`Internal_server_error "oh no")
         | _ -> Cohttp_async.Server.respond_string ~status:`Not_found "not found")
  in
  return (server, key)

let solve port ~file =
  let test signature =
    let start = Time_ns.now () in
    let%bind rep, _body =
      Cohttp_async.Client.get
        (Uri.of_string (sprintf "http://localhost:%d/test?file=%s&signature=%s" port file signature))
    in
    let time = Time_ns.diff (Time_ns.now ()) start in
    match rep.status with
    | `OK -> return (Either.First signature)
    | `Internal_server_error -> return (Either.Second (signature, time))
    | _ -> assert false
  in
  List.init 40 ~f:Fn.id
  |> Deferred.List.fold ~init:"" ~f:(fun sig_so_far _ ->
      let%bind options =
        List.init 16 ~f:Fn.id
        |> List.map ~f:(fun j ->
            let hex_char =
              Char.of_int_exn (if j < 10 then Char.to_int '0' + j else Char.to_int 'a' + j - 10)
            in
            sig_so_far ^ String.of_char hex_char ^ String.make (40 - String.length sig_so_far - 1) '0')
        |> Deferred.List.map ~how:`Parallel ~f:test
      in
      let successes, failures = List.partition_map options ~f:Fn.id in
      let best_sig =
        match successes with
        | [signature] -> signature
        | _ :: _ :: _ -> assert false
        | [] ->
          List.max_elt failures ~compare:(Comparable.lift Time_ns.Span.compare ~f:snd)
          |> Option.value_exn
          |> fst
      in
      return (String.sub ~pos:0 ~len:(String.length sig_so_far + 1) best_sig))

let test ~port ~file ~signature =
  let%bind rep, body =
    Cohttp_async.Client.get
      (Uri.of_string (sprintf "http://localhost:%d/test?file=%s&signature=%s" port file signature))
  in
  let%bind body = Cohttp_async.Body.to_string body in
  print_s [%message (rep : Cohttp_async.Response.t) (body : string) (String.length signature : int)];
  return ()

let%expect_test "test server" =
  let%bind server, key = start_server () in
  let port = Cohttp_async.Server.listening_on server in
  let test = test ~port in
  let%bind () = test ~file:"foo" ~signature:"bar" in
  let%bind () = test ~file:"foo" ~signature:(Hash.hmac_sha1 ~key "foo" |> String_util.raw_to_hex) in
  [%expect {|
    ((rep
      ((encoding (Fixed 5))
       (headers ((connection keep-alive) (content-length 5))) (version HTTP_1_1)
       (status Internal_server_error) (flush false)))
     (body "oh no") ("String.length signature" 3))
    ((rep
      ((encoding (Fixed 11))
       (headers ((connection keep-alive) (content-length 11))) (version HTTP_1_1)
       (status OK) (flush false)))
     (body "you did it!") ("String.length signature" 40)) |}];
  Cohttp_async.Server.close server

(* commented out because it takes too long *)
(*
let%expect_test "final" =
  let%bind server, key = start_server () in
  let port = Cohttp_async.Server.listening_on server in
  let file = "this-is-a-file.txt" in
  let%bind signature = solve port ~file in
  let%bind () = test ~port ~file ~signature in
  [%expect {|
    ((rep
      ((encoding (Fixed 11))
       (headers ((connection keep-alive) (content-length 11))) (version HTTP_1_1)
       (status OK) (flush false)))
     (body "you did it!") ("String.length signature" 40)) |}];
  Cohttp_async.Server.close server
*)
