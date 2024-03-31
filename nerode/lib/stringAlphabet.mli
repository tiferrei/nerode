module Symbol : Alphabet.Symbol with type t = string

include module type of Alphabet.Make(Symbol)

(** [from_int k] returns an alphabet containing the symbols \{"0", "1", ..., "k-1"\},
    with int representatives 0, 1, ... *)
val from_int : int -> t