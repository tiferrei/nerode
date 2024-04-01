open Core
open Nerode

let () =
  let sysargs = Sys.get_argv () in
  let arg = Array.get sysargs 1 in
  let c = Array.get sysargs 2 in
  let rx = (Parser.parse_string arg) in
  let alpha = StringAlphabet.from_int 2 in
  let sym_of_str c = int_of_string c in
  begin
    Printf.printf "Your rx: %s\n%!" (Rx.to_string alpha rx);
    Printf.printf "d_%s(rx): %s\n%!" c (Rx.to_string alpha (Rx.d (sym_of_str c) rx))
  end
