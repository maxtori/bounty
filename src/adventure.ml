open Ezjs_min
open Bounty
open Ui

let%file _ = "./adventure.html"

let%prop id: A.id = {req}

let%data show_balance = false
and adv : adventure option = None
and me : A.id option = None
and currency : string = !Db.currency

let%computed total app : amount = match to_optdef adventure_of_jsoo app##.adv with
  | None -> zero
  | Some adv -> show_amount @@ total_balance adv.loots

and [@noconv] balances app : amount_jsoo t Table.t =
  match to_optdef adventure_of_jsoo app##.adv with
  | None -> Table.create ()
  | Some adv ->
    let l = List.map (fun (s: sailor) ->
      let cents = sailor_balance ~id:s.id adv.loots in
      s.id, show_amount cents
    ) adv.crew in
    Table.makef amount_to_jsoo l

let wrap app f = match Optdef.to_option app##.adv with
  | None -> alert app "Adventure not loaded"
  | Some adv -> f adv

let%meth share app = wrap app @@ fun adv ->
  let id = to_string adv##.id in
  Ui.share id

and edit app = wrap app @@ fun adv ->
  let a = adventure_of_jsoo adv in
  nav app (mkr (EditAdventure a))

and push app = wrap app @@ fun adv ->
  let a = adventure_of_jsoo adv in
  nav app (mkr (NewLoot { a with loots = [] }))

and refresh app = wrap app @@ fun adv ->
  let adv = adventure_of_jsoo adv in
  Comm.sync adv;
  nav app (mkr (Adventure adv.id))

and edit_loot app (loot: loot) = wrap app @@ fun adv ->
  let adv = { (adventure_of_jsoo adv) with loots = [] } in
  nav app (mkr (EditLoot {adv; loot}))

and view_loot app (loot: loot) = wrap app @@ fun adv ->
  let adv = { (adventure_of_jsoo adv) with loots = [] } in
  nav app (mkr (Loot {adv; loot}))

and remove_loot app (loot: loot) = wrap app @@ fun adv ->
  let adv = adventure_of_jsoo adv in
  let m = Db.create_modif ~tsp:(now ()) ~adventure:adv.id (RemoveLoot loot.id) in
  Db.apply_modif adv m @@ fun adv ->
  Comm.sync adv;
  app##refresh

and sailor_name app (id: A.id) = match Optdef.to_option app##.adv with
  | None -> undefined
  | Some adv ->
    let crew = crew_of_jsoo adv##.crew in
    optdef string @@ List.find_map (fun (s: sailor) ->
      if s.id = id then Some s.name else None
    ) crew

and show_date _app (tsp: float) =
  let d = new%js date_fromTimeValue (number_of_float (tsp *. 1000.)) in
  let two = string "2-digit" in
  (Unsafe.coerce d)##toLocaleDateString (string "fr-FR")
    (object%js val day = two val month = two val year = two end)

[%%mounted fun app ->
  Db.load_adventure (A.id_of_jsoo app##.id) @@ function
  | None -> nav app (mkr Adventures)
  | Some adv ->
    app##.adv := def (adventure_to_jsoo adv);
    app##.me := optdef A.id_to_jsoo (me adv.crew)]

[%%comp {name="adventure"; conv}]
