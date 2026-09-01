open Ezjs_min
open Bounty
open Ui

let%file _ = "./newAdventure.html"

let%prop adventure: adventure option = None

let%data adv app: adventure =
  match to_optdef adventure_of_jsoo app##.adventure with
  | Some adv -> adv
  | _ ->
    let crew = [{ id=id (); peer = Some !Db.peer; name = "" }] in
    { id=id (); name=""; updated=now (); crew; loots=[]; currency= !Db.currency; archived=false }
and update app : bool = Option.is_some (Optdef.to_option app##.adventure)

let%meth add app =
  let crew = crew_of_jsoo app##.adv##.crew @ [
    { id = id (); peer = None; name = "" }
  ] in
  app##.adv##.crew := crew_to_jsoo crew

and remove app (id: A.id) =
  let loots = Option.fold ~none:[] ~some:(fun (a: adventure) -> a.loots) @@
    to_optdef adventure_of_jsoo app##.adventure in
  let has_shares = List.exists (fun (s: loot) ->
    match s.shares with
    | Equal l -> List.exists (fun sid -> sid = id) l
    | Weighted l -> List.exists (fun (w: weight) -> w.sailor = id) l
    | Custom l -> List.exists (fun (s: share) -> s.sailor = id) l
  ) loots in
  if has_shares then alert app "The sailor has some shares" else
  let crew = List.filter (fun (s: sailor) -> s.id <> id) @@
    crew_of_jsoo app##.adv##.crew in
  app##.adv##.crew := crew_to_jsoo crew

and push app =
  let adv = adventure_of_jsoo app##.adv in
  match to_optdef adventure_of_jsoo app##.adventure with
  | None ->
    if adv.name = "" then alert app "The adventure needs a name"
    else if List.exists (fun (s: sailor) -> s.name = "") adv.crew then
      alert app "Every sailor needs a name"
    else (Db.register_adventure adv; nav app (mkr (Adventure adv)))
  | Some old ->
    if compare_adventure old adv = 0 then alert app "The adventure hasn't been changed" else
    let tsp = now () in
    let adv = { adv with updated=tsp; loots=[] } in
    let () = Db.update_adventure adv in
    let kind : modif_kind = Adventure adv in
    let m, sync = Db.create_modif ~tsp ~adventure:adv.id kind in
    Comm.send_modif ~adventure:adv ~sync m;
    nav app (mkr (Adventure adv))

[%%comp {name="new-adventure"; conv}]
