open Ezjs_min
open Ezjs_idb

module type S = sig
  type t [@@deriving jsoo]
  val db: unit -> Types.iDBDatabase Ezjs_min.t
  val table: string
end

module Make(S: S) = struct
  type content = S.t

  module ModifTr = struct
    type t = S.t Sync.Types.modif
    type js = S.jsoo Sync.Types.modif_jsoo Ezjs_min.t
    let to_js = Sync.Types.modif_to_jsoo (S.to_jsoo, S.of_jsoo)
    let of_js = Sync.Types.modif_of_jsoo (S.to_jsoo, S.of_jsoo)
  end

  module St = Store(StringTr)(ModifTr)

  let () = St.set_name S.table
  let create_table db =
    let st = St.create db in
    ignore (St.create_index ~name:"entity" ~key_path:"entity" st)
  let delete_table db = db##deleteObjectStore (string S.table)

  let list id f =
    let tst = St.store ~mode:READONLY (S.db ()) in
    let index = St.get_index tst "entity" in
    let iDBKeyRange : St.K.js Types.iDBKeyRange t = Unsafe.global##._IDBKeyRange in
    let key = St.KR (iDBKeyRange##only (string id)) in
    St.get_all ~key (Unsafe.coerce index) @@ fun l ->
    let l = List.sort Sync.Types.compare_modif l in
    f l

  let register m =
    let st = St.store ~mode:READWRITE (S.db ()) in
    St.put ~key:m.Sync.Types.id st m
end
