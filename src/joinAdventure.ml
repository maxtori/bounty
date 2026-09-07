open Ezjs_min
open Bounty
open Ui

let%file _ = "./joinAdventure.html"

let%prop info: join_adventure = {req}

let%data adv : adventure option = None
and chosen : A.id option = None

let%meth push app =
  match to_optdef adventure_of_jsoo app##.adv with
  | None -> alert app "Adventure not loaded"
  | Some adv ->
    match List.find_opt (fun (s: sailor) -> Some s.id = to_optdef A.id_of_jsoo app##.chosen) adv.crew with
    | None -> alert app "Sailor not identified"
    | Some { peer = Some _; _ } -> alert app "Sailor is already identified by someone else"
    | Some s ->
      Back.process ~listen:true ~id:(id ()) ~tsp:(now ()) adv (Sailor { s with peer = Some !Db.peer }) @@ fun () ->
      let path = to_string Dom_html.window##.location##.pathname in
      adventure ~path app adv.id

[%%mounted fun app ->
  let info = join_adventure_of_jsoo app##.info in
  Back.join ~entity:info.adventure info.peer @@ fun adv ->
  match List.find_opt (fun (s: sailor) -> s.peer = Some !Db.peer) adv.crew with
  | Some _ -> adventure app adv.id
  | _ -> app##.adv := def (adventure_to_jsoo adv)
]

[%%comp {name="join-adventure"; conv}]
