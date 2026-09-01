open Ezjs_min
open Bounty
open Ui

let%file _ = "./loot.html"

let%prop loot : adv_and_loot = {req}

let%data [@noconv] currency app : js_string t =
  match to_opt currency_change_of_jsoo app##.loot##.loot##.currency with
  | None -> app##.loot##.adv##.currency
  | Some {symbol; _} -> string symbol
and me app : A.id option = me (crew_of_jsoo app##.loot##.adv##.crew)

let%computed shares app : share list =
  let treasurer = A.id_of_jsoo app##.loot##.loot##.treasurer in
  match shares_of_jsoo app##.loot##.loot##.shares with
  | Custom l -> l
  | Equal shares ->
    let amount = app##.loot##.loot##.amount##.cents in
    List.map (fun id ->
      let cents = compute_equal_amount ~treasurer ~id ~shares amount in
      { sailor=id; amount = show_amount cents }) shares
  | Weighted shares ->
    let amount = app##.loot##.loot##.amount##.cents in
    List.map (fun {sailor; _ } ->
      let cents = compute_weighted_amount ~treasurer ~id:sailor ~shares amount in
      { sailor; amount = show_amount cents }) shares

let%meth sailor_name app (id: A.id) =
  let crew = crew_of_jsoo app##.loot##.adv##.crew in
  optdef string @@ List.find_map (fun (s: sailor) ->
    if s.id = id then Some s.name else None
  ) crew

and remove app =
  let al = adv_and_loot_of_jsoo app##.loot in
  match to_optdef A.id_of_jsoo app##.me with
  | Some me when me = al.loot.captain ->
    let () = Db.remove_loot al.loot.id in
    let tsp = now () in
    let adv = { al.adv with updated = tsp; loots=[] } in
    Db.update_adventure adv;
    let m, sync = Db.create_modif ~tsp ~adventure:al.adv.id (RemoveLoot al.loot.id) in
    Comm.send_modif ~adventure:al.adv ~sync m;
    nav app (mkr (Adventure adv))
  | _ -> alert app "Only the captain of a loot can modify it"

and edit app =
  let loot = adv_and_loot_of_jsoo app##.loot in
  match to_optdef A.id_of_jsoo app##.me with
  | Some me when me = loot.loot.captain -> nav app Ui.(mkr (EditLoot loot))
  | _ -> alert app "Only the captain of a loot can modify it"

and show_datetime _app (tsp: float) =
  let d = new%js date_fromTimeValue (number_of_float (tsp *. 1000.)) in
  (Unsafe.coerce d)##toLocaleString (string "fr-FR")

and adventure app =
  let adv = adventure_of_jsoo app##.loot##.adv in
  nav app (mkr (Adventure { adv with loots=[] }))

[%%comp {name="loot"; conv}]
