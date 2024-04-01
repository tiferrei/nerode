module BA = Ba.EqualityBA(Int)

type state = int
type pred = BA.p
type symbol = BA.d

type word = Word.t
type valuation = int -> symbol option

module RegSet = Set.Make(Int)

type trans = {
  src: state;
  pred: pred;
  eRegs: RegSet.t;
  iRegs: RegSet.t;
  uRegs: RegSet.t;
  dst: state;
}

module TransOrdered = struct
  type t = trans

  (* FIXME: iffy! *)
  let compare = compare
end

module StateSet = Set.Make(Int)
module TransSet = Set.Make(TransOrdered)

type t = {
  alpha: StringAlphabet.t;
  start: state;
  trans: TransSet.t;
  final: StateSet.t
}

let mk_trans (src : state) (pred: pred) (eRegs: RegSet.t) (iRegs: RegSet.t) (uRegs: RegSet.t) (dst: state) : trans =
  {src = src; pred = pred; 
  eRegs = eRegs; iRegs = iRegs; uRegs = uRegs;
  dst = dst}

(* src -- pred / {reg} {} {} --> dst *)
let mk_check_trans (src : state) (pred: pred) (reg: int) (dst: state) : trans =
  {src = src; pred = pred; 
    eRegs = RegSet.singleton reg;
    iRegs = RegSet.empty;
    uRegs = RegSet.empty;
    dst = dst}

(* src -- pred / {} {} {reg} --> dst *)
let mk_store_trans (src : state) (pred: pred) (reg: int) (dst: state) : trans =
  {src = src; pred = pred; 
  eRegs = RegSet.empty;
  iRegs = RegSet.empty;
  uRegs = RegSet.singleton reg;
  dst = dst}
  
(* src -- pred / {} {0 ... regCount - 1} {reg} --> dst *)
let mk_fresh_trans (src : state) (pred: pred) (reg: int) (regCount: int) (dst: state) : trans =
  {src = src; pred = pred; 
  eRegs = RegSet.empty;
  iRegs = Seq.ints 0 |> Seq.take regCount |> RegSet.of_seq;
  uRegs = RegSet.singleton reg;
  dst = dst}

let mk_sra (alpha: StringAlphabet.t) (start: state) (trans: TransSet.t) (final: StateSet.t) : t =
  {alpha = alpha; start = start; trans = trans; final = final}

(* FIXME: Could be 0 reg SRA? *)
let mk_empty_sra (alpha: StringAlphabet.t) : t =
  let start = 0 in
  let trans = Array.make 2 (mk_check_trans 0 BA.top 0 0) in
  trans.(1) <- mk_fresh_trans 0 BA.bot 0 1 0;
  let trans = trans |> Array.to_list |> TransSet.of_list in
  let final = StateSet.empty in
  mk_sra alpha start trans final

let mk_full_sra (alpha: StringAlphabet.t) : t =
  let start = 0 in
  let trans = Array.make 2 (mk_check_trans 0 BA.top 0 0) in
  trans.(1) <- mk_fresh_trans 0 BA.bot 0 1 0;
  let trans = trans |> Array.to_list |> TransSet.of_list in
  let final = StateSet.singleton start in
  mk_sra alpha start trans final
  
let empty_val = (fun _ -> None)
  
let compare_states = Int.compare

let states (sra: t) : StateSet.t = 
  let trans = TransSet.to_list sra.trans in
  let statePairs = List.map (fun t -> (t.src, t.dst)) trans in
  let acc set (s1, s2) = set |> StateSet.add s1 |> StateSet.add s2 in
  List.fold_left acc StateSet.empty statePairs

let state_trans (sra: t) (state: state) : TransSet.t =
  TransSet.filter (fun t -> Int.equal t.src state) sra.trans

let size (sra: t) = sra |> states |> StateSet.cardinal

let get_alpha (sra: t) = sra.alpha
let get_start (sra: t) = sra.start

let step (sra: t) (sv: state * valuation) (a: symbol) : (state * valuation) option =
  let (s, v) = sv in
  let srcMatching = TransSet.filter (fun t -> t.src == s) sra.trans in
  let symMathcing = TransSet.filter (fun t -> BA.eval t.pred a) srcMatching in
  (* FIXME: Do not use structural eq. *)
  let eRegMatching = TransSet.filter (fun t -> RegSet.for_all (fun r -> v r == Some a) t.eRegs) symMathcing in
  let iRegMatching = TransSet.filter (fun t -> RegSet.for_all (fun r -> v r != Some a) t.iRegs) eRegMatching in
  if TransSet.cardinal iRegMatching > 1 then
    failwith "The SRA provided is not deterministic! Multiple valid transitions for (s, a) pair."
  else match TransSet.choose_opt iRegMatching with
    | None -> None
    | Some tr ->
        let v' = (fun r -> if RegSet.mem r tr.uRegs then Some a else v r) in
        Some (tr.dst, v')

let steps (sra: t) (sv: state * valuation) (w: word) =
  let opt_step sra sv a = match sv with
  | None -> None
  | Some qv -> step sra qv a in
  List.fold_left (opt_step sra) (Some sv) w

let accepting (sra: t) (s: state) =
  StateSet.mem s sra.final

let accept (sra: t) (w: word) =
  let final = steps sra (get_start sra, empty_val) w in
  match final with
  | None -> false
  | Some (s, v) -> accepting sra s

let validate (sra: t) (pos: word list) (neg: word list) : bool =
  not (List.exists (fun s -> not (accept sra s)) pos) &&
  not (List.exists (fun s -> accept sra s) neg)

let is_empty (sra: t) : bool =
  failwith "unimplemented!"

let rep (sra: t) : word =
  failwith "unimplemented!"

let complement (sra: t) : t =
  failwith "unimplemented!"

let print (sra: t) =
  StateSet.iter (fun q ->
    let acc = if accepting sra q then "+" else "-" in
    let start = if Int.equal q sra.start then "-->" else "" in
    Printf.printf ("%s(s%d, %s):\n") start q acc;
    TransSet.iter (fun t ->
      let regPrint (rs : RegSet.t) = 
        let body = rs |> RegSet.to_list |> List.map string_of_int |> String.concat ", " in
        "}" |> String.cat body |> String.cat "{" in
      Printf.printf ("  (s%2d) -- g / {%s} {%s} {%s} --> (s%2d)\n") t.src (regPrint t.eRegs) (regPrint t.iRegs) (regPrint t.uRegs) t.dst;
    ) (state_trans sra q);
  ) (states sra)

let minimize (sra: t) : t =
  failwith "unimplemented!"

let intersect (a1: t) (a2: t) : t =
  failwith "unimplemented!"

let union (a1: t) (a2: t) : t =
  failwith "unimplemented!"

let symdiff (a1: t) (a2: t) : t =
  failwith "unimplemented!"

let diff (a1: t) (a2: t) : t =
  failwith "unimplemented!"

let equiv (d0: t) (d1: t) : bool = symdiff d0 d1 |> is_empty
