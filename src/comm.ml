open Ezjs_min
open Bounty
module SSet = Set.Make(String)

type connect_metadata =
  | Join of A.id
  | Sync of (string * sync) list [@assoc]
[@@deriving jsoo]

let peer : PeerJS.peer t option ref = ref None

let peer f =
  let p = match !peer with
    | Some p -> p
    | None ->
      let p = PeerJS.peer ~id:!Db.peer () in
      peer := Some p;
      PeerJS.peer_listen p PeerJS.Error (fun e -> Js_of_ocaml.Console.console##warn (Unsafe.coerce e)##toString);
      p in
  if to_bool p##.open_ then f p
  else PeerJS.peer_once p PeerJS.Open (fun id -> log "peer open: %s" id; f p)

let connect ?metadata ?peer:p dst f =
  log "connect to %s" dst;
  let aux p =
    let conns : (A.id * PeerJS.data_connection t js_array t) list = Table.itemsf Unsafe.coerce p##.connections in
    let conn = match List.assoc_opt dst conns, metadata with
      | _, Some metadata ->
        let metadata = Some (Unsafe.inject (connect_metadata_to_jsoo metadata)) in
        let options = { PeerJS.label = None; metadata; serialization=None; reliable=None } in
        PeerJS.connect ~options p dst
      | None, _ -> PeerJS.connect p dst
      | Some arr, _ ->
        match Optdef.to_option @@ array_get arr 0 with
        | None -> PeerJS.connect p dst
        | Some conn -> conn in
    if to_bool conn##.open_ then f conn
    else PeerJS.data_once conn PeerJS.Open (fun _ -> f conn) in
  match p with
  | None -> peer aux
  | Some p -> aux p

let send ?id ~dst ~adventure (kind: message_kind) =
  let id = match id with None -> Bounty.id () | Some id -> id in
  let aux conn =
    log "sending message to %s for %s" (to_string conn##.peer) adventure;
    let () = match kind with
      | Sync sync -> log "sync %a" Db.print_sync sync
      | Modifs (sync, l) -> log "modifs (%d) %a" (List.length l) Db.print_sync sync
      | _ -> () in
    let msg = {id; kind; adventure} in
    PeerJS.send conn (message_to_jsoo msg) in
  match dst with
  | `conn conn -> aux conn
  | `peer dst -> connect dst aux

let sync_adventure ~conn ~adventure sync =
  let modifs = Db.modifs_after_sync ~adventure sync in
  (match modifs with
   | [] -> ()
   | _ -> send ~dst:(`conn conn) ~adventure (Modifs (sync, modifs)));
  Option.iter (fun sync ->
    send ~dst:(`conn conn) ~adventure (Sync sync)
  ) @@ Db.outdated_sync ~adventure sync

let listener (conn: PeerJS.data_connection t) ({adventure; kind; _}: message) =
  let peer = to_string conn##.peer in
  log "receiving message from %s for %s" peer adventure;
  match kind with
  | Sync sync ->
    log "sync %a" Db.print_sync sync;
    sync_adventure ~conn ~adventure sync
  | Modifs (sync, modifs) ->
    log "modifs (%d) %a" (List.length modifs) Db.print_sync sync;
    Db.insert_modifs ~adventure (sync, modifs) @@ fun () ->
    let sync = Db.get_sync adventure in
    send ~dst:(`conn conn) ~adventure (Sync sync)
  | _ -> ()

let listeners = ref SSet.empty

let add_data_listener (conn: PeerJS.data_connection t) =
  let label = to_string conn##.label in
  if SSet.mem label !listeners then () else
  let () = listeners := SSet.add label !listeners in
  PeerJS.data_listen conn PeerJS.Data @@ fun m ->
  try
    let msg = message_of_jsoo (Unsafe.coerce m) in
    listener conn msg
  with _ -> ()

let add_peer_listener (peer: PeerJS.peer t) =
  PeerJS.peer_listen peer PeerJS.Connection @@ fun conn ->
  log "connection from %s" (to_string conn##.peer);
  add_data_listener conn;
  PeerJS.data_once conn PeerJS.Open @@ fun () ->
  match to_optdef (fun x -> connect_metadata_of_jsoo (Unsafe.coerce x)) conn##.metadata with
  | Some Join id ->
    log "joining %s" id;
    Db.get_adventure id @@
    Option.iter (fun (adv: adventure) -> send ~dst:(`conn conn) ~adventure:id (Adventure adv))
  | Some Sync l ->
    log "syncing %s" @@ String.concat ", " @@ List.map fst l;
    List.iter (fun (adventure, sync) -> sync_adventure ~conn ~adventure sync) l
  | _ -> ()

let send_modif ?(listen=false) ~(adventure: adventure) ~(sync: sync) m =
  List.iter (fun (s: sailor) ->
    Option.iter (fun dst ->
      if dst <> !Db.peer then
        connect dst (fun conn ->
          if listen then add_data_listener conn;
          send ~dst:(`conn conn) ~adventure:adventure.id (Modifs (sync, [ m ])))
    ) s.peer
  ) adventure.crew

let init adventures =
  peer @@ fun peer ->
  add_peer_listener peer;
  let conns = List.fold_left (fun acc (adv: adventure) ->
    List.fold_left (fun acc (s: sailor) ->
      match s.peer with
      | Some dst when dst <> !Db.peer ->
        (match List.assoc_opt dst acc with
         | None -> (dst, [ adv.id ]) :: acc
         | Some l -> (dst, (adv.id :: l)) :: List.remove_assoc dst acc)
      | _ -> acc
    ) acc adv.crew
  ) [] adventures in
  List.iter (fun (dst, adventures) ->
    let syncs = List.map (fun id -> id, Db.get_sync id) adventures in
    connect ~peer ~metadata:(Sync syncs) dst add_data_listener
  ) conns
