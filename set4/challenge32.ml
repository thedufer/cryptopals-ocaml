open Core
open Async
open! Import

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
  let test_multiple_times ~n signature =
    Deferred.repeat_until_finished (n, []) (fun (n, times) ->
        if n = 0 then
          return (`Finished (Either.Second (signature
                                           , Time_ns.Span.(/)
                                               (List.sum (module Time_ns.Span) times ~f:Fn.id)
                                               (List.length times |> Float.of_int))))
        else
          let%bind result = test signature in
          match result, times with
          | First signature, [] -> return (`Finished (Either.First signature))
          | First _, _ :: _ -> assert false
          | Second (_, time), times -> return (`Repeat (n - 1, time :: times)))
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
        |> Deferred.List.map ~how:`Sequential ~f:(test_multiple_times ~n:5)
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

(* commented out because it takes too long *)
(*
let%expect_test "final" =
  let%bind server, _key = Challenge31.start_server ~delay:(Time_ns.Span.of_ms 5.) in
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
