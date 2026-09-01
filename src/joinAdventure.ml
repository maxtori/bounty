open Ezjs_min
open Bounty
open Ui
open Comm

let%file _ = "./joinAdventure.html"

let%prop info: join_adventure = {req}

let%data adv : adventure option = None
and chosen : A.id option = None

let%meth push app =
  match to_optdef adventure_of_jsoo app##.adv,
        to_optdef A.id_of_jsoo app##.chosen with
  | None, _ -> alert app "Adventure not loaded"
  | _, None -> alert app "Sailor not identified"
  | Some adv, Some sailor_id ->
    if List.exists (fun (s: sailor) -> s.id = sailor_id && Option.is_some s.peer) adv.crew then
      alert app "Sailor is already identified by someone else"
    else
    let crew = List.map (fun (s: sailor) -> if s.id = sailor_id then { s with peer = Some !Db.peer } else s) adv.crew in
    let tsp = now () in
    let adv = { adv with crew; updated=tsp; loots=[] } in
    Db.register_adventure adv;
    let m, sync = Db.create_modif ~tsp ~adventure:adv.id (Adventure adv) in
    Comm.send_modif ~listen:true ~adventure:adv ~sync m;
    let path = to_string Dom_html.window##.location##.pathname in
    nav app (mkr ~path (Adventure adv))

[%%mounted fun app ->
  let info = join_adventure_of_jsoo app##.info in
  connect ~metadata:(Join info.adventure) info.peer @@ fun conn ->
  PeerJS.data_once conn PeerJS.Data (fun m ->
    try
      let msg = message_of_jsoo (Unsafe.coerce m) in
      match msg.adventure = info.adventure, msg.kind with
      | true, Adventure adv ->
        app##.adv := def (adventure_to_jsoo adv);
        Option.iter (fun (s: sailor) -> app##.chosen := def (A.id_to_jsoo s.id)) @@
        List.find_opt (fun (s: sailor) -> s.peer = Some !Db.peer) adv.crew
      | _ -> ()
    with _ -> ());
  PeerJS.data_listen conn PeerJS.Error (fun e -> Js_of_ocaml.Console.console##warn (Unsafe.coerce e)##toString)
]

[%%comp {name="join-adventure"; conv}]
