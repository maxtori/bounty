open Ezjs_min
open Bounty
open Ui

let%file _ = "./adventure.html"

let%prop adventure: adventure = {req}

let%data show_balance = false
and me app : A.id option = me (crew_of_jsoo app##.adventure##.crew)
and currency : string = !Db.currency

let%computed total app : amount =
  let loots = to_listf loot_of_jsoo app##.adventure##.loots in
  let cents = total_balance loots in
  show_amount cents

and [@noconv] balances app : amount_jsoo t Table.t =
  let loots = to_listf loot_of_jsoo app##.adventure##.loots in
  let crew = crew_of_jsoo app##.adventure##.crew in
  let l = List.map (fun (s: sailor) ->
    let cents = sailor_balance ~id:s.id loots in
    s.id, show_amount cents
  ) crew in
  Table.makef amount_to_jsoo l

let%meth share app =
  let id = to_string app##.adventure##.id in
  Ui.share id

and edit app =
  let a = adventure_of_jsoo app##.adventure in
  nav app (mkr (EditAdventure a))

and push app =
  let a = adventure_of_jsoo app##.adventure in
  nav app (mkr (NewLoot a))

and refresh app =
  Db.load_adventure (A.id_of_jsoo app##.adventure##.id) @@ function
  | None -> nav app (mkr (Adventures []))
  | Some adv -> nav app (mkr (Adventure adv))

and edit_loot app (loot: loot) =
  let adv = adventure_of_jsoo app##.adventure in
  nav app (mkr (EditLoot {adv; loot}))

and view_loot app (loot: loot) =
  let adv = adventure_of_jsoo app##.adventure in
  nav app (mkr (Loot {adv; loot}))

and remove_loot app (loot: loot) =
  let adv = adventure_of_jsoo app##.adventure in
  Db.remove_loot loot.id;
  let tsp = now () in
  Db.update_adventure { adv with updated = tsp };
  let m, sync = Db.create_modif ~tsp ~adventure:adv.id (RemoveLoot loot.id) in
  Comm.send_modif ~adventure:adv ~sync m;
  app##refresh

and sailor_name app (id: A.id) =
  let crew = crew_of_jsoo app##.adventure##.crew in
  optdef string @@ List.find_map (fun (s: sailor) ->
    if s.id = id then Some s.name else None
  ) crew

and show_date _app (tsp: float) =
  let d = new%js date_fromTimeValue (number_of_float (tsp *. 1000.)) in
  let two = string "2-digit" in
  (Unsafe.coerce d)##toLocaleDateString (string "fr-FR")
    (object%js val day = two val month = two val year = two end)

[%%comp {name="adventure"; conv}]
