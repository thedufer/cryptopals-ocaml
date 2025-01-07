open! Core

let sec_since_epoch () =
  Time_ns_unix.now ()
  |> Time_ns_unix.to_span_since_epoch
  |> Time_ns_unix.Span.to_int_sec
