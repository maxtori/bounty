open Ezjs_min
open Bounty
open Ui
open Comm

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
      let tsp = now () in
      let m = Db.create_modif ~tsp ~adventure:adv.id (Sailor { s with peer = Some !Db.peer }) in
      Db.apply_modif adv m @@ fun adv ->
      Comm.sync ~listen:true adv;
      let path = to_string Dom_html.window##.location##.pathname in
      nav app (mkr ~path (Adventure adv.id))

[%%mounted fun app ->
  let info = join_adventure_of_jsoo app##.info in
  connect ~metadata:(Join info.adventure) info.peer @@ fun conn ->
  PeerJS.data_once conn PeerJS.Data (fun m ->
    try
      let msg = message_of_jsoo (Unsafe.coerce m) in
      match msg.adventure = info.adventure, msg.kind with
      | true, Adventure adv ->
        (match List.find_opt (fun (s: sailor) -> s.peer = Some !Db.peer) adv.crew with
         | Some _ -> nav app (mkr (Adventure adv.id))
         | _ -> app##.adv := def (adventure_to_jsoo adv))
      | _ -> ()
    with _ -> ());
  PeerJS.data_listen conn PeerJS.Error (fun e -> Js_of_ocaml.Console.console##warn (Unsafe.coerce e)##toString)
]

[%%comp {name="join-adventure"; conv}]
