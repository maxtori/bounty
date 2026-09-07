module SSet = Set.Make(String)
module Types = Types
open Types

module Make(Db: Types.Db)(Comm: Types.Comm with type content = Db.content)
    (App: Types.App with type content = Db.content and type entity = Comm.entity) = struct

  type nonrec modif = Db.content modif

  let cache : (string, (string * modif list) list) Hashtbl.t = Hashtbl.create 10

  let add_cache (m: modif) =
    match Hashtbl.find_opt cache m.entity with
    | None -> Hashtbl.add cache m.entity [ m.origin, [ m ] ]
    | Some l -> match List.assoc_opt m.origin l with
      | None | Some [] -> Hashtbl.replace cache m.entity ((m.origin, [ m ]) :: List.remove_assoc m.origin l)
      | Some (h :: _ as x) when m.seq > h.seq ->
        Hashtbl.replace cache m.entity ((m.origin, m :: x) :: List.remove_assoc m.origin l)
      | _ -> ()

  let init_cache indexes f =
    let rec aux = function
      | [] -> f ()
      | id :: tl ->
        Db.list id @@ fun l ->
        let m = List.fold_left (fun acc m ->
          match List.assoc_opt m.origin acc with
          | None -> (m.origin, [ m ]) :: acc
          | Some x -> (m.origin, m :: x) :: List.remove_assoc m.origin acc
        ) [] l in
        Hashtbl.add cache id m;
        aux tl in
    aux indexes

  let register m =
    Db.register m;
    add_cache m

  let get_sync index =
    let l = Option.value ~default:[] @@ Hashtbl.find_opt cache index in
    List.map (fun (peer, l) -> peer, match l with [] -> 0 | h :: _ -> h.seq) l

  let create ~id ~tsp ~entity content =
    let sync = get_sync entity in
    let origin = Comm.origin () in
    let seq = match List.assoc_opt origin sync with Some seq -> seq+1 | None -> 1 in
    let m = { id; entity; origin; seq; tsp; content } in
    register m;
    m

  let rec apply acc l f = match l with
    | [] -> App.finally acc f
    | m :: tl -> App.apply ~tsp:m.tsp acc m.content (fun acc -> apply acc tl f)

  let after ~entity x =
    let l = Option.value ~default:[] @@ Hashtbl.find_opt cache entity in
    let rec aux acc is_after = function
      | h :: tl when is_after h -> aux (h :: acc) is_after tl
      | _ -> acc in
    let acc = List.concat_map (match x with
      | `modif (m: modif) -> fun (_, l) -> aux [] (fun (h: modif) -> compare_modif m h < 0) l
      | `sync sync -> fun (peer, l) -> match List.assoc_opt peer sync with
        | None -> l
        | Some seq -> aux [] (fun (h:modif) -> h.seq > seq) l) l in
    List.sort compare_modif acc

  let insert ~entity l f = match l with
    | [] -> f None
    | m :: _ ->
      let base = after ~entity (`modif m) in
      let modifs = match base with [] -> l | _ -> List.sort_uniq compare_modif (base @ l) in
      App.load entity @@ function
      | None -> f None
      | Some acc -> List.iter register l; apply acc modifs (fun acc -> f (Some acc))

  let outdated ~entity sync =
    let s = get_sync entity in
    if List.exists (fun (peer, seq1) -> match List.assoc_opt peer s with
      | Some seq2 when seq1 <= seq2 -> false
      | _ -> true) sync then Some s
    else None

  let sync_entity ~conn ~entity sync =
    let modifs = after ~entity (`sync sync) in
    (match modifs with [] -> () | _ -> Comm.send ~conn ~entity (Modifs modifs));
    Option.iter (fun sync -> Comm.send ~conn ~entity (Sync sync)) @@ outdated ~entity sync

  let refresh : (A.id -> unit) ref = ref (fun _ -> ())

  let listener (conn: Comm.conn) ({entity; kind}: _ message) = match kind with
    | Sync sync -> sync_entity ~conn ~entity sync
    | Modifs l ->
      insert ~entity l @@ Option.iter (fun _ ->
        !refresh entity;
        let sync = get_sync entity in
        Comm.send ~conn ~entity (Sync sync))
    | _ -> ()

  let listeners = ref SSet.empty

  let add_data_listener (conn: Comm.conn) =
    let label = Comm.conn_id conn in
    if SSet.mem label !listeners then () else
    let () = listeners := SSet.add label !listeners in
    Comm.data_listen conn (listener conn)

  let add_peer_listener () =
    let on conn metadata =
      Option.iter (fun entity ->
        App.load entity @@ Option.iter (fun acc -> Comm.send ~conn ~entity (Init acc))
      ) metadata in
    Comm.conn_listen ~on add_data_listener

  let sync ?(listen=false) acc =
    let entity = App.id acc in
    let sync = get_sync entity in
    List.iter (fun id ->
      if id <> Comm.origin () then
        Comm.connect id (fun conn ->
          if listen then add_data_listener conn;
          Comm.send ~conn ~entity (Sync sync))
    ) (App.peers acc)

  let init_comm l =
    add_peer_listener ();
    List.iter (sync ~listen:true) l

  let join ~entity peer f =
    Comm.connect ~metadata:entity peer @@ fun conn ->
    Comm.data_listen ~once:true conn @@ function
    | { kind = Init x; _ } -> f x
    | _ -> ()

  let init l f = init_cache (List.map App.id l) @@ fun () -> init_comm l; f ()

  let process ?listen ~id ~tsp acc content f =
    let entity = App.id acc in
    let _ = create ~id ~tsp ~entity content in
    App.apply ~tsp acc content @@ fun acc ->
    App.finally acc @@ fun acc ->
    sync ?listen acc;
    f ()

end
