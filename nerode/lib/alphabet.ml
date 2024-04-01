module type A = sig
  type s
  type r
  type t
  val size : t -> int
  val compare : r -> r -> int
  val to_list : t -> s list
  val of_list : s list -> t
  val of_array : s array -> t
  val of_seq : s Seq.t -> t
  val reps : t -> r list
  val iter : (r -> unit) -> t -> unit
  val fold : ('a -> r -> 'a) -> 'a -> t -> 'a
  val map : (r -> 'a) -> t -> 'a list
  val sym_of_rep : t -> r -> s
  val sym_to_rep : t -> s -> r
  val rep_to_sexp : r -> Core.Sexp.t
  val rep_of_sexp : Core.Sexp.t -> r
  val of_json : Yojson.Basic.t -> t
  val to_json : t -> Yojson.Basic.t
  val to_string : t -> string

  type wildchar = Wildcard | Letter of r
  type wildstring = wildchar list

(** Form a list of words from a wildstring. A wildstring corresponds to a list
    of alphabet symbols or a wildcard. That is, for example, for the alphabet
    \{"0", "1"\}, [parse_wildstring alphabet [[0; Wildcard]] converts to the words
    [[0;0]] and [[0;1]]. *)
  val parse_wildstring : t -> wildstring -> r list list
end

module type Symbol = sig
  type t
  val compare: t -> t -> int
  val to_string: t -> string
  val of_string: string -> t
  val of_json: Yojson.Basic.t -> t
  val to_json: t -> Yojson.Basic.t
  val of_sexp : Core.Sexp.t -> t
  val to_sexp : t -> Core.Sexp.t
end

module Make (S : Symbol) : A with type s = S.t and type r = int = struct
  type s = S.t
  type r = int
  type t = s array
  
  let size = Array.length

  let compare = Int.compare

  let to_list = Array.to_list

  let of_list = Array.of_list
  
  let of_array = Fun.id

  let of_seq = Array.of_seq

  let reps t = Seq.ints 0 |> Seq.take (size t) |> List.of_seq

  let iter f t = List.iter f (reps t)

  let fold f a t = List.fold_left f a (reps t)

  let map f t = List.map f (reps t)

  let sym_of_rep t r = t.(r)

  let sym_to_rep t s =
    (* TODO: Memoize. *)
    let mtch = fun x -> S.compare s x == 0 in
    let search = Array.find_index mtch t in
    Option.get search

  let rep_to_sexp = Core.Int.sexp_of_t

  let rep_of_sexp = Core.Int.t_of_sexp

  let of_json = function
    | `List lst -> List.map S.of_json lst |> Array.of_list
    | _ -> failwith "Alphabet must be a JSON list"
  let to_json (a: t) = 
    let lst = to_list a |> List.map (fun s -> `String (S.to_string s)) in
    `List lst
  let to_string t =
    to_list t |> List.map S.to_string |> String.concat " "

  type wildchar = Wildcard | Letter of r
  type wildstring = wildchar list

  let parse_wildstring (alpha: t) (s: wildstring) =
    let rec consume rem acc =
      match rem with
      | [] -> acc
      | Letter l :: tl -> consume tl (List.map (fun w -> l :: w) acc)
      | Wildcard :: tl -> consume tl (List.fold_left (fun a2 w -> 
        List.fold_left (fun a3 x -> (x::w)::a3) a2 (reps alpha)) [] acc)
    in
    consume (List.rev s) [[]]
end

module MakeDirect (S : Symbol) : A with type s = S.t and type r = S.t = struct
  type s = S.t
  type r = S.t
  type t = s array
  
  let size = Array.length

  let compare = S.compare

  let to_list = Array.to_list

  let of_list = Array.of_list

  let of_array = Fun.id

  let of_seq = Array.of_seq

  let reps = to_list

  let iter f t = List.iter f (reps t)

  let fold f a t = List.fold_left f a (reps t)

  let map f t = List.map f (reps t)

  let sym_of_rep t r = r

  let sym_to_rep t s = s

  let rep_to_sexp = S.to_sexp
  
  let rep_of_sexp = S.of_sexp

  let rep_to_string t r = S.to_string r

  let of_json = function
    | `List lst -> List.map S.of_json lst |> Array.of_list
    | _ -> failwith "Alphabet must be a JSON list"
  let to_json (a: t) = 
    let lst = to_list a |> List.map (fun s -> `String (S.to_string s)) in
    `List lst
  let to_string t =
    to_list t |> List.map S.to_string |> String.concat " "

  type wildchar = Wildcard | Letter of r
  type wildstring = wildchar list

  let parse_wildstring (alpha: t) (s: wildstring) =
    let rec consume rem acc =
      match rem with
      | [] -> acc
      | Letter l :: tl -> consume tl (List.map (fun w -> l :: w) acc)
      | Wildcard :: tl -> consume tl (List.fold_left (fun a2 w -> 
        List.fold_left (fun a3 x -> (x::w)::a3) a2 (reps alpha)) [] acc)
    in
    consume (List.rev s) [[]]
end