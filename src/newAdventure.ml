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
and modifs : modif_kind list = []

let%meth add app =
  let sailor = { id = id (); peer = None; name = "" } in
  let crew = crew_of_jsoo app##.adv##.crew @ [ sailor ] in
  app##.adv##.crew := crew_to_jsoo crew;
  let modifs = to_listf modif_kind_of_jsoo app##.modifs in
  app##.modifs := of_listf modif_kind_to_jsoo @@ modifs @ [ Sailor sailor ];
  [%next app (fun app ->
    Optdef.iter [%ref app (sailor.id ^ "-input")] @@ fun a ->
    Optdef.iter (array_get (Unsafe.coerce a) 0) @@ fun elt ->
    elt##focus)]

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
  app##.adv##.crew := crew_to_jsoo crew;
  let modifs = to_listf modif_kind_of_jsoo app##.modifs in
  let in_modif = function Sailor sailor when sailor.id = id -> true | _ -> false in
  if List.exists in_modif modifs then
    app##.modifs := of_listf modif_kind_to_jsoo @@ List.filter (fun x -> not (in_modif x)) modifs
  else app##.modifs := of_listf modif_kind_to_jsoo @@ modifs @ [ RemoveSailor id ]

and push app =
  let adv = adventure_of_jsoo app##.adv in
  match to_optdef adventure_of_jsoo app##.adventure with
  | None ->
    if adv.name = "" then alert app "The adventure needs a name"
    else if List.exists (fun (s: sailor) -> s.name = "") adv.crew then
      alert app "Every sailor needs a name"
    else (Db.register_adventure adv; adventure app adv.id)
  | Some old ->
    let modifs = to_listf modif_kind_of_jsoo app##.modifs in
    let modifs = if old.name <> adv.name then modifs @ [ AdventureName adv.name ] else modifs in
    let modifs = if old.currency <> adv.currency then modifs @ [ AdventureCurrency adv.currency ] else modifs in
    let modifs = List.fold_left (fun acc (s: sailor) ->
      if List.exists (fun (s_old: sailor) -> s_old.id = s.id && s_old.name <> s.name) old.crew then
        acc @ [ Sailor s ]
      else acc) modifs adv.crew in
    match modifs with
    | [] -> alert app "The adventure hasn't been changed"
    | _ ->
      let modifs, _ = List.fold_left (fun (acc, tsp) kind ->
        let kind = match kind with
          | Sailor (s: sailor) ->
            Sailor (Option.value ~default:s @@
                    List.find_opt (fun (sailor: sailor) -> sailor.id = s.id) adv.crew)
          | k -> k in
        acc @ [ Back.create ~id:(id ()) ~tsp ~entity:adv.id kind ], Float.succ tsp
      ) ([], now ()) modifs in
      Back.apply old modifs @@ fun adv ->
      Back.sync adv;
      adventure app adv.id

and refresh _app = ()

[%%comp {name="new-adventure"; conv}]
