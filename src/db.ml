open Ezjs_min
open Ezjs_idb
open Bounty

module SettingStore = Store(Ezjs_idb.StringTr)(Ezjs_idb.StringTr)

module AdventureStore = Store(Ezjs_idb.StringTr)(struct
    type js = adventure_jsoo t
    type t = adventure
    let to_js = adventure_to_jsoo
    let of_js = adventure_of_jsoo
  end)

module LootStore = Store(Ezjs_idb.StringTr)(struct
    type js = loot_jsoo t
    type t = loot
    let to_js = loot_to_jsoo
    let of_js = loot_of_jsoo
  end)

module ModifStore = Store(Ezjs_idb.StringTr)(struct
    type js = modif_jsoo t
    type t = modif
    let to_js = modif_to_jsoo
    let of_js = modif_of_jsoo
  end)

let db : Types.iDBDatabase t ref = ref (Unsafe.obj [||])

let open_ f =
  AdventureStore.set_name "adventures";
  LootStore.set_name "loots";
  SettingStore.set_name "settings";
  ModifStore.set_name "modifs";
  let upgrade db e =
    if e.new_version = 1 && e.old_version = 0 then (
      ignore (AdventureStore.create db);
      ignore (SettingStore.create db);
      let st = LootStore.create db in
      ignore (LootStore.create_index ~name:"adventure" ~key_path:"adventure" st);
      let st = ModifStore.create db in
      ignore (ModifStore.create_index ~name:"adventure" ~key_path:"adventure" st))
    else if e.old_version = 1 && e.new_version = 0 then (
      db##deleteObjectStore (string "adventures");
      db##deleteObjectStore (string "loots");
      db##deleteObjectStore (string "settings");
      db##deleteObjectStore (string "modifs")) in
  openDB "bounty" ~upgrade ~version:1 (fun d -> db := d; f ())

let store_get (type k) (type kjs) (type d) (type djs)
    (module S: S with type K.t = k and type D.t = d and type K.js = kjs and type D.js = djs)
    (st: (kjs, djs) Types.iDBObjectStore t) (k: k) (f: d option -> unit): unit =
  S.get st f (S.K k)

let peer = ref ""
let theme = ref "dark"
let currency = ref "€"

let load_settings f =
  let st = SettingStore.store ~mode:READWRITE !db in
  store_get (module SettingStore) st "id" @@ function
  | None ->
    let id = id () in
    SettingStore.add ~key:"id" st id;
    peer := id;
    f ()
  | Some id ->
    peer := id;
    store_get (module SettingStore) st "theme" @@ fun th ->
    Option.iter (fun th -> theme := th) th;
    store_get (module SettingStore) st "currency" @@ fun cur ->
    Option.iter (fun cur -> currency := cur) cur;
    f ()

let set_currency c =
  currency := c;
  let st = SettingStore.store ~mode:READWRITE !db in
  SettingStore.put ~key:"currency" st c

let load_loots ~adventure f =
  let tst = LootStore.store ~mode:READONLY !db in
  let index = LootStore.get_index tst "adventure" in
  let iDBKeyRange : LootStore.K.js Types.iDBKeyRange t = Unsafe.global##._IDBKeyRange in
  let key = LootStore.KR (iDBKeyRange##only (string adventure)) in
  LootStore.get_all ~key (Unsafe.coerce index) @@ fun loots ->
  f (List.sort compare_loot loots)

let get_adventures f =
  let ast = AdventureStore.store ~mode:READONLY !db in
  AdventureStore.get_all ast @@ fun l ->
  f (List.sort compare_adventure l)

let adventures_count f =
  let ast = AdventureStore.store ~mode:READONLY !db in
  AdventureStore.count ast f

let get_adventure id f =
  let st = AdventureStore.store ~mode:READONLY !db in
  store_get (module AdventureStore) st id f

let load_adventure id f =
  get_adventure id @@ function
  | None -> f None
  | Some adv ->
    load_loots ~adventure:adv.id @@ fun loots ->
    f (Some { adv with loots })

let register_adventure (adventure: adventure) =
  let st = AdventureStore.store ~mode:READWRITE !db in
  AdventureStore.add ~key:adventure.id st {adventure with loots=[]}

let update_adventure ?cb (adventure: adventure) =
  let st = AdventureStore.store ~mode:READWRITE !db in
  AdventureStore.put ?callback:cb ~key:adventure.id st {adventure with loots=[]}

let remove_adventure ?cb id =
  let st = AdventureStore.store ~mode:READWRITE !db in
  AdventureStore.delete ?callback:cb st (AdventureStore.K id)

let register_loot ?cb (loot: loot) =
  let st = LootStore.store ~mode:READWRITE !db in
  LootStore.put ?callback:cb ~key:loot.id st loot

let remove_loot ?cb id =
  let st = LootStore.store ~mode:READWRITE !db in
  LootStore.delete ?callback:cb st (LootStore.K id)

let get_loot id f =
  let st = LootStore.store ~mode:READONLY !db in
  store_get (module LootStore) st id f

let modifs : (A.id, (A.id * modif list) list) Hashtbl.t = Hashtbl.create 10

let get_modifs adventure f =
  let tst = ModifStore.store ~mode:READONLY !db in
  let index = ModifStore.get_index tst "adventure" in
  let iDBKeyRange : ModifStore.K.js Types.iDBKeyRange t = Unsafe.global##._IDBKeyRange in
  let key = ModifStore.KR (iDBKeyRange##only (string adventure)) in
  ModifStore.get_all ~key (Unsafe.coerce index) @@ fun l ->
  let l = List.sort compare_modif l in
  f l

let add_modif_cache (m: modif) =
  match Hashtbl.find_opt modifs m.adventure with
  | None -> Hashtbl.add modifs m.adventure [ m.origin, [ m ] ]
  | Some l -> match List.assoc_opt m.origin l with
    | None | Some [] -> Hashtbl.replace modifs m.adventure ((m.origin, [ m ]) :: List.remove_assoc m.origin l)
    | Some (h :: _ as x) when m.seq > h.seq ->
      Hashtbl.replace modifs m.adventure ((m.origin, m :: x) :: List.remove_assoc m.origin l)
    | _ -> ()

let init adventures f =
  let rec aux = function
    | [] -> f ()
    | (adv: adventure) :: tl ->
      get_modifs adv.id @@ fun l ->
      let m = List.fold_left (fun acc m ->
        match List.assoc_opt m.origin acc with
        | None -> (m.origin, [ m ]) :: acc
        | Some x -> (m.origin, m :: x) :: List.remove_assoc m.origin acc
      ) [] l in
      Hashtbl.add modifs adv.id m;
      aux tl in
  aux adventures

let register_modif (m: modif) =
  let st = ModifStore.store ~mode:READWRITE !db in
  ModifStore.put ~key:m.id st m;
  add_modif_cache m

let get_sync adventure =
  let l = Option.value ~default:[] @@ Hashtbl.find_opt modifs adventure in
  List.map (fun (peer, l) -> peer, match l with [] -> 0 | h :: _ -> h.seq) l

let create_modif ?(id=id ()) ~tsp ~adventure kind =
  let sync = get_sync adventure in
  let origin = !peer in
  let seq = match List.assoc_opt origin sync with Some seq -> seq+1 | None -> 1 in
  let m = { id; adventure; kind; origin; seq; tsp } in
  register_modif m;
  m

let apply_modif adv (m: modif) f = match m.kind with
  | AdventureName name ->
    let a = { adv with name; updated=m.tsp } in
    update_adventure ~cb:(fun _ -> f a) a
  | AdventureCurrency currency ->
    let a = { adv with currency; updated=m.tsp } in
    update_adventure ~cb:(fun _ -> f a) a
  | Sailor s ->
    let crew, sailor = List.fold_left (fun (crew, s) (sailor: sailor) -> match s with
      | Some (s: sailor) when s.id = sailor.id -> crew @ [ s ], None
      | _ -> crew @ [ sailor ], s) ([], Some s) adv.crew in
    let crew = match sailor with None -> crew | Some s -> crew @ [ s ] in
    let a = { adv with crew; updated=m.tsp } in
    update_adventure ~cb:(fun _ -> f a) a
  | RemoveSailor id ->
    let crew = List.filter (fun (s: sailor) -> s.id <> id) adv.crew in
    let a = { adv with crew; updated=m.tsp } in
    update_adventure ~cb:(fun _ -> f a) a
  | RemoveLoot id ->
    let adv = { adv with updated=m.tsp } in
    remove_loot ~cb:(fun _ -> update_adventure ~cb:(fun _ -> f adv) adv) id
  | Loot lt ->
    let adv = { adv with updated=lt.updated } in
    register_loot ~cb:(fun _ -> update_adventure ~cb:(fun _ -> f adv) adv) lt

let rec apply_modifs adv l f = match l with
  | [] -> f adv
  | m :: tl -> apply_modif adv m (fun adv -> apply_modifs adv tl f)

let modifs_after_sync ~adventure sync =
  let l = Option.value ~default:[] @@ Hashtbl.find_opt modifs adventure in
  let rec aux acc seq = function
    | h :: tl when h.seq > seq -> aux (h :: acc) seq tl
    | _ -> acc in
  let acc = List.fold_left (fun acc (peer, l) -> match List.assoc_opt peer sync with
    | None -> acc @ l
    | Some seq -> aux acc seq l) [] l in
  List.sort compare_modif acc

let print_sync fmt sync =
  Format.fprintf fmt "%s" @@ String.concat ", " @@
  List.map (fun (peer, seq) -> Format.sprintf "%s: %d" peer seq) sync

let insert_modifs ~adventure (sync, l) f =
  let base = modifs_after_sync ~adventure sync in
  let after = match base with [] -> l | _ -> List.sort compare_modif (base @ l) in
  get_adventure adventure @@ function
  | None -> ()
  | Some adv ->
    List.iter register_modif l;
    apply_modifs adv after (fun _ -> f ())

let outdated_sync ~adventure sync =
  let s = get_sync adventure in
  if List.exists (fun (peer, seq1) -> match List.assoc_opt peer s with
    | Some seq2 when seq1 <= seq2 -> false
    | _ -> true) sync then Some s
  else None
