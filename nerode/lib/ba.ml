module type BA = sig
  type d
  type p
  
  val eval : p -> d -> bool
  val atom : d -> p
  val not : p -> p
  val ba_and : p -> p -> p
  val ba_or : p -> p -> p
  val top : p
  val bot : p
end

module EqualityBA (D : Set.OrderedType) : BA with type d = D.t = struct 
  module DSet = Set.Make(D)  

  type d = DSet.elt
  type p = {
    mem: bool;
    set: DSet.t;
  }

  let eval p el = 
    let mem_flip = if p.mem then Fun.id else Bool.not in
    DSet.mem el p.set |> mem_flip
  let atom el = {mem = true; set = DSet.singleton el}
  let not p = {mem = Bool.not p.mem; set = p.set}
  let ba_and p1 p2 = match (p1.mem, p2.mem) with
  | (true, true) -> {mem = true; set = DSet.inter p1.set p2.set}
  | (false, false) -> {mem = false; set = DSet.union p1.set p2.set}
  | (true, false) -> {mem = true; set = DSet.diff p1.set p2.set}
  | (false, true) -> {mem = true; set = DSet.diff p2.set p1.set}
  let ba_or p1 p2 = match (p1.mem, p2.mem) with
  | (true, true) -> {mem = true; set = DSet.union p1.set p2.set}
  | (false, false) -> {mem = false; set = DSet.inter p1.set p2.set}
  | (true, false) -> {mem = false; set = DSet.diff p2.set p1.set}
  | (false, true) -> {mem = false; set = DSet.diff p1.set p2.set}
  let top = {mem = false; set = DSet.empty}
  let bot = {mem = true; set = DSet.empty}
end