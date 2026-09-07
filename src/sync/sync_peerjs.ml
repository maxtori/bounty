open Ezjs_min

module type S = sig
  type t [@@deriving jsoo]
  type entity [@@deriving jsoo]
  val origin: unit -> Sync.Types.A.id
end

module Make(S: S) = struct
  type conn = PeerJS.data_connection t
  type entity = S.entity
  type content = S.t

  let peer : PeerJS.peer t option ref = ref None

  let peer f =
    let p = match !peer with
      | Some p -> p
      | None ->
        let p = PeerJS.peer ~id:(S.origin ()) () in
        peer := Some p;
        PeerJS.peer_listen p PeerJS.Error (fun e -> Js_of_ocaml.Console.console##warn (Unsafe.coerce e)##toString);
        p in
    if to_bool p##.open_ then f p else PeerJS.peer_once p PeerJS.Open (fun _ -> f p)

  let origin = S.origin
  let conn_id conn = Sync.Types.A.id_of_jsoo conn##.label

  let connect ?metadata id f =
    peer @@ fun p ->
    let conns : (Sync.Types.A.id * PeerJS.data_connection t js_array t) list = Table.itemsf Unsafe.coerce p##.connections in
    let conn = match List.assoc_opt id conns, metadata with
      | _, Some metadata ->
        let metadata = Some (Unsafe.inject (Sync.Types.A.id_to_jsoo metadata)) in
        let options = { PeerJS.label = None; metadata; serialization=None; reliable=None } in
        PeerJS.connect ~options p id
      | None, _ -> PeerJS.connect p id
      | Some arr, _ ->
        match Optdef.to_option @@ array_get arr 0 with
        | None -> PeerJS.connect p id
        | Some conn -> conn in
    if to_bool conn##.open_ then f conn
    else PeerJS.data_once conn PeerJS.Open (fun _ -> f conn)

  let send ~conn ~entity (kind: _ Sync.Types.message_kind) =
    let msg = Sync.Types.message_to_jsoo (S.entity_to_jsoo, S.entity_of_jsoo) (S.to_jsoo, S.of_jsoo) {Sync.Types.kind; entity} in
    PeerJS.send conn msg

  let data_listen ?(once=false) conn f =
    let aux = if once then PeerJS.data_once else PeerJS.data_listen in
    aux conn PeerJS.Data @@ fun m ->
    try
      let msg = Sync.Types.message_of_jsoo (S.entity_to_jsoo, S.entity_of_jsoo) (S.to_jsoo, S.of_jsoo) (Unsafe.coerce m) in
      f msg
    with _ -> ()

  let conn_listen ~on f =
    peer @@ fun p ->
    PeerJS.peer_listen p PeerJS.Connection @@ fun conn ->
    f conn;
    PeerJS.data_once conn PeerJS.Open @@ fun () ->
    match to_optdef (fun x -> Sync.Types.A.id_of_jsoo (Unsafe.coerce x)) conn##.metadata with
    | Some id -> on conn (Some id)
    | _ | exception _ -> on conn None
end
