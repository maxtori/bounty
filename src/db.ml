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

let db : Types.iDBDatabase t ref = ref (Unsafe.obj [||])

module Sync = Sync_idb.Make(struct
    type t = modif_kind [@@deriving jsoo]
    type nonrec jsoo = jsoo Ezjs_min.t
    let db () = !db
    let table = "modifs"
  end)

let open_ f =
  AdventureStore.set_name "adventures";
  LootStore.set_name "loots";
  SettingStore.set_name "settings";
  let upgrade db e =
    if e.new_version = 1 && e.old_version = 0 then (
      ignore (AdventureStore.create db);
      ignore (SettingStore.create db);
      let st = LootStore.create db in
      ignore (LootStore.create_index ~name:"adventure" ~key_path:"adventure" st);
      Sync.create_table db)
    else if e.old_version = 1 && e.new_version = 0 then (
      db##deleteObjectStore (string "adventures");
      db##deleteObjectStore (string "loots");
      db##deleteObjectStore (string "settings");
      Sync.delete_table db) in
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
