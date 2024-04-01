(** A word [Word.t] is implemented as a representative list, with each element 
representing a letter in the word, with the same order in the list as in the word.*)
type t = StringAlphabet.r list
let compare = List.compare StringAlphabet.compare
let epsilon = []
let suffixes w = 
  List.fold_right (fun l acc -> (l::List.hd acc)::acc) w [epsilon]
let prefixes w = suffixes (List.rev w) |> List.map List.rev |> List.rev
let append_letter w (l : StringAlphabet.r) : t = w @ [l]
let concat w1 w2 = w1 @ w2  
let of_intlist lst = lst
let to_intlist (w: t) = w
let to_string (alpha: StringAlphabet.t) (w: t) = 
  match w with
  | [] -> "ε"
  | xs -> List.fold_left (fun acc l -> acc^(l |> StringAlphabet.sym_of_rep alpha)) "" xs
let to_symlist = Fun.id
let of_symlist = Fun.id

let rec prefix_of (s1: t) (s2: t) : bool =
  match s1, s2 with
  | [], _ -> true
  | a::s1_tail, b::s2_tail when a = b -> prefix_of s1_tail s2_tail
  | _, _ -> false

let rec resid (pre: t) (w: t) : t option =
  match pre, w with
  | [], _ -> Some w
  | a::s1_tail, b::s2_tail when a = b -> resid s1_tail s2_tail
  | _, _ -> None

let words_of_strings (alpha: StringAlphabet.t) s =
  let rec idx s i =
    if String.equal (StringAlphabet.sym_of_rep alpha i) s then
      i
    else
      idx s (i+1) in
  let index s = idx s 0 in
  let rec consume rem acc =
    match rem with
    | [] -> acc
    | "X" :: tl ->consume tl (List.fold_left (fun a2 w -> 
      List.fold_left (fun a3 x -> (x::w)::a3) a2 (StringAlphabet.reps alpha)) [] acc)
    | hd :: tl -> consume tl (List.map (fun w -> (index hd)::w) acc)
  in
  consume (List.rev s) [[]]
