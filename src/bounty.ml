let (let@) p f = f p

module A = struct
  type id = string [@@deriving encoding, jsoo]
  type amount = int [@number] [@@deriving encoding, jsoo]
  type tsp = float [@@deriving encoding, jsoo]
end

type amount = {
  cents: int;
  str: string;
} [@@deriving encoding, jsoo]

type sailor = {
  id: A.id;
  peer: A.id option; [@opt]
  name: string;
} [@@deriving encoding, jsoo]

type crew = sailor list [@@deriving encoding, jsoo]

type share = {
  sailor: A.id;
  amount: amount;
} [@@deriving encoding, jsoo]

type weight = {
  sailor: A.id;
  weight: int;
} [@@deriving encoding, jsoo]

type shares =
  | Equal of A.id list
  | Weighted of weight list
  | Custom of share list
[@@deriving encoding, jsoo {snake}]

type currency_change = {
  symbol: string;
  factor: float;
} [@@deriving encoding, jsoo]

type loot = {
  id: A.id;
  adventure: A.id;
  name: string;
  updated: A.tsp;
  date: A.tsp;
  captain: A.id;
  treasurer: A.id;
  amount: amount;
  shares: shares;
  currency: currency_change option; [@opt]
} [@@deriving encoding, jsoo {mut}]

type adventure = {
  id: A.id;
  name: string;
  updated: A.tsp;
  currency: string;
  crew: crew;
  loots: loot list;
  archived: bool;
} [@@deriving encoding, jsoo {mut}]

type modif_kind =
  | AdventureName of string
  | AdventureCurrency of string
  | Sailor of sailor
  | RemoveSailor of A.id
  | Loot of loot
  | RemoveLoot of A.id
[@@deriving encoding, jsoo]

(* type modif = { *)
(*   id: A.id; *)
(*   adventure: A.id; *)
(*   kind: modif_kind; *)
(*   origin: A.id; *)
(*   seq: int; *)
(*   tsp: A.tsp; *)
(* } [@@deriving encoding, jsoo] *)

(* type sync = (A.id * int) list [@assoc] [@@deriving encoding] *)
(* [@@@jsoo *)
(*   type sync_jsoo = int Ezjs_min.Table.ct *)
(*   let sync_to_jsoo (l: sync) : sync_jsoo Ezjs_min.t = Ezjs_min.Table.make l *)
(*   let sync_of_jsoo (js: sync_jsoo Ezjs_min.t) : sync = Ezjs_min.Table.items (Ezjs_min.Unsafe.coerce js) *)
(* ] *)

(* type message_kind = *)
(*   | Adventure of adventure *)
(*   | Modifs of (sync * modif list) *)
(*   | Sync of sync *)
(* [@@deriving encoding, jsoo] *)

(* type message = { *)
(*   id: A.id; *)
(*   adventure: A.id; *)
(*   kind: message_kind; *)
(* } [@@deriving encoding, jsoo] *)

let id () =
  Uuidm.(to_string @@ v4_gen (Random.State.make_self_init ()) ())

let zero = { cents=0; str="0.00" }

let parse_amount str =
  match Float.of_string_opt str with
  | Some f -> Some (Float.to_int (f *. 100.))
  | None -> None

let show_amount cents =
  let str = Format.sprintf "%.02f" (Float.of_int cents /. 100.) in
  { cents; str }

let compute_equal_amount ~treasurer ~id ~shares amount =
  let n = List.length shares in
  let amount, rest = amount / n, amount mod n in
  if not (List.mem id shares) then 0 else
  if id = treasurer then amount + rest else amount

let compute_weighted_amount ~treasurer ~id ~shares amount =
  let weight, n, _ = List.fold_left (fun (weight, n, i) (s: weight) ->
    let weight = if s.sailor = id then Some s.weight else weight in
    let n = n + s.weight in
    weight, n, i+1) (None, 0, 0) shares in
  let amount, rest = amount / n, amount mod n in
  match weight with
  | None -> 0
  | Some weight -> if id = treasurer then weight * amount + rest else weight * amount

let compute_custom_amount ~id ~shares =
  Option.value ~default:0 @@
  List.find_map (fun (s: share) -> if s.sailor = id then Some s.amount.cents else None) shares

let compute_amount ~treasurer ~id ~shares amount = match shares with
  | Equal shares -> compute_equal_amount ~treasurer ~id ~shares amount
  | Weighted shares -> compute_weighted_amount ~treasurer ~id ~shares amount
  | Custom shares -> compute_custom_amount ~id ~shares

let compute_currency_total ~factor ~treasurer ~amount = function
  | Equal shares ->
    List.fold_left (fun acc id ->
      let cents = compute_equal_amount ~treasurer ~id ~shares amount in
      acc + Float.to_int (Float.of_int cents *. factor)
    ) 0 shares
  | Weighted shares ->
    List.fold_left (fun acc (s: weight) ->
      let cents = compute_weighted_amount ~treasurer ~id:s.sailor ~shares amount in
      acc + Float.to_int (Float.of_int cents *. factor)
    ) 0 shares
  | Custom shares ->
    List.fold_left (fun acc (s: share) ->
      let cents = compute_custom_amount ~id:s.sailor ~shares in
      acc + Float.to_int (Float.of_int cents *. factor)
    ) 0 shares

let sailor_balance ~id l =
  let aux ~factor cents = Float.to_int (Float.of_int cents *. factor) in
  List.fold_left (fun acc (l: loot) ->
    let cents = compute_amount ~treasurer:l.treasurer ~id ~shares:l.shares l.amount.cents in
    let cents = match l.currency, id = l.treasurer with
      | None, false -> - cents
      | None, true -> l.amount.cents - cents
      | Some { factor; _ }, false -> - aux ~factor cents
      | Some { factor; _ }, true ->
        let cents = aux ~factor cents in
        let total_sailor = compute_currency_total ~factor ~treasurer:l.treasurer ~amount:l.amount.cents l.shares in
        total_sailor - cents in
    acc + cents
  ) 0 l

let total_balance l = List.fold_left (fun acc (l: loot) ->
  match l.currency with
  | None -> acc + l.amount.cents
  | Some { factor; _ } -> acc + Float.to_int (Float.of_int l.amount.cents *. factor)
) 0 l

let compare_adventure a1 a2 =
  let c = Float.compare a2.updated a1.updated in
  if c <> 0 then c else
  let c = String.compare a1.id a2.id in
  if c <> 0 then c else
  let s1 = EzEncoding.construct adventure_enc { a1 with loots=[] } in
  let s2 = EzEncoding.construct adventure_enc { a2 with loots=[] } in
  String.compare s1 s2

let compare_loot lt1 lt2 =
  let c = Float.compare lt2.date lt1.date in
  if c <> 0 then c else
  let c = Float.compare lt2.updated lt1.updated in
  if c <> 0 then c else
  let c = String.compare lt1.id lt2.id in
  if c <> 0 then c else
  let s1, s2 = EzEncoding.construct loot_enc lt1, EzEncoding.construct loot_enc lt2 in
  String.compare s1 s2

(* let compare_modif m1 m2 = *)
(*   let c = Float.compare m1.tsp m2.tsp in *)
(*   if c <> 0 then c else String.compare m1.id m2.id *)
