open Ezjs_min
open Bounty
open Ui

let%file _ = "./adventure.html"

let%prop adv: adventure = {req}

let%data show_balance = false
and me app : A.id option = me (crew_of_jsoo app##.adv##.crew)
and currency : string = !Db.currency

let%computed total app : amount =
  show_amount @@ total_balance (to_listf loot_of_jsoo app##.adv##.loots)

and [@noconv] balances app : amount_jsoo t Table.t =
  let adv = adventure_of_jsoo app##.adv in
  let l = List.map (fun (s: sailor) ->
    let cents = sailor_balance ~id:s.id adv.loots in
    s.id, show_amount cents
  ) adv.crew in
  Table.makef amount_to_jsoo l

let%meth share app =
  let id = to_string app##.adv##.id in
  Ui.share id

and edit app =
  let a = adventure_of_jsoo app##.adv in
  nav app (mkr (EditAdventure a))

and push app =
  let a = adventure_of_jsoo app##.adv in
  nav app (mkr (NewLoot { a with loots = [] }))

and refresh app =
  let adv = adventure_of_jsoo app##.adv in
  Back.sync adv;
  adventure app adv.id

and edit_loot app (loot: loot) =
  let adv = { (adventure_of_jsoo app##.adv) with loots = [] } in
  nav app (mkr (EditLoot {adv; loot}))

and view_loot app (loot: loot) =
  let adv = { (adventure_of_jsoo app##.adv) with loots = [] } in
  nav app (mkr (Loot {adv; loot}))

and remove_loot app (loot: loot) =
  let adv = adventure_of_jsoo app##.adv in
  Back.process ~tsp:(now ()) ~id:(id ()) adv (RemoveLoot loot.id) @@ fun () ->
  app##refresh

and sailor_name app (id: A.id) : string option =
  let crew = crew_of_jsoo app##.adv##.crew in
  List.find_map (fun (s: sailor) ->
    if s.id = id then Some s.name else None
  ) crew

and show_date _app (tsp: float) =
  let d = new%js date_fromTimeValue (number_of_float (tsp *. 1000.)) in
  let two = string "2-digit" in
  (Unsafe.coerce d)##toLocaleDateString (string "fr-FR")
    (object%js val day = two val month = two val year = two end)

[%%comp {name="adventure"; conv}]
