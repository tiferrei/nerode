(** Represents a finite alphabet of symbols. *)

module type A = sig
  (** A single symbol in an alphabet. *)
  type s

  (** A (usually more memory-efficient) representative of a symbol in the alphabet. *)
  type r

  (** An alphabet: a finite collection of symbols. *)
  type t

  (** [size alpha] returns the size of [alpha] *)
  val size : t -> int

  (** [compare x y] returns a number greater than 0 if x is greater than y, 0 if
  x = y,  and less than 0 if x is less than y. *)
    val compare : r -> r -> int

  (** [to_list alpha] returns a list of the symbols in this alphabet. *)
  val to_list : t -> s list

  (** [of_list lst] returns an alphabet containing the symbols in the given list. *)
  val of_list : s list -> t

  (** [of_seq seq] returns an alphabet containing the symbols in the given sequence. *)
  val of_seq : s Seq.t -> t

  (** [toreps_list alpha] returns a list of the representatives in this alphabet. *)
  val reps : t -> r list

  (** [iter f alpha] performs [f x] for each representative [x] in [alpha]. *)
  val iter : (r -> unit) -> t -> unit

  (** [fold f init alpha] folds left the function [f] over [alpha], i.e.,
    returning [f ( f init x1 ) x2 ...] for all the representatives [x1, x2, ...] in [alpha]. *)
  val fold : ('a -> r -> 'a) -> 'a -> t -> 'a

  (** [map f alpha] returns a list [[f x1, f x2, ...]] for the representatives
    [x1, x2, ...] in [alpha]. *)
  val map : (r -> 'a) -> t -> 'a list

  (** Return the symbol being represented *)
  val sym_of_rep : t -> r -> s

  (** Return the representative of the symbol *)
  val sym_to_rep : t -> s -> r

  (** Return the string representation of a representative in the alphabet *)
  val rep_to_string : t -> r -> string

  (** Deserialize alphabet from JSON. *)
  val of_json : Yojson.Basic.t -> t

  (** Serialize alphabet to JSON. *)
  val to_json : t -> Yojson.Basic.t

  (** Convert an alphabet to its string representation. *)
  val to_string : t -> string

  type wildchar = Wildcard | Letter of r
  type wildstring = wildchar list

  val parse_wildstring : t -> wildstring -> r list list
end

module type Symbol = sig
  (** A single symbol. *)
  type t

  (** Standard [compare] operation *)
  val compare: t -> t -> int

  (** Converts a symbol to its string representation. *)
  val to_string: t -> string

  (** Parses a symbol from its string representation. *)
  val of_string: string -> t

  (** Deserializes a symbol from JSON. *)
  val of_json: Yojson.Basic.t -> t

  (** Serializes a symbol to JSON. *)
  val to_json: t -> Yojson.Basic.t

  (** Deserializes a symbol from an S-Expression. *)
  val of_sexp : Core.Sexp.t -> t

  (** Serializes a symbol to an S-Expression. *)
  val to_sexp : t -> Core.Sexp.t
end

(* Make a [Symbol] alphabet backed by [int] representatives. *)
module Make : functor (S : Symbol) -> A with type s = S.t and type r = int

(* Make a [Symbol] alphabet backed directly by [Symbol] representatives. *)
module MakeDirect : functor (S : Symbol) -> A with type s = S.t and type r = S.t