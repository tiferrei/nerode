module Symbol : Alphabet.Symbol with type t = string = struct
  type t = string

  let compare = String.compare

  let to_string = Fun.id

  let of_string = Fun.id

  let to_json i = `String i

  let of_json json = match json with
  | `String s -> s
  | `Int i -> string_of_int i
  | `Bool b -> string_of_bool b
  | _ -> failwith "String symbol can only be serialised from JSON bool, int, or string objects."

  let of_sexp = Core.Sexp.to_string
  
  let to_sexp = Core.Sexp.of_string
end

include Alphabet.Make(Symbol)

let from_int (n: int) : t = Seq.ints 0 |> Seq.take n |> Seq.map string_of_int |> of_seq;