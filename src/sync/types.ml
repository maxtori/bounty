open Ezjs_min

module A = struct
  type id = string [@@deriving jsoo]
  type tsp = float [@@deriving jsoo]
end

type 'a modif = {
  id: A.id;
  entity: A.id;
  origin: A.id;
  seq: int;
  tsp: A.tsp;
  content: 'a;
} [@@deriving jsoo]

let compare_modif x1 x2 =
  let c = Float.compare x1.tsp x2.tsp in
  if c <> 0 then c else String.compare x1.id x2.id

type sync = (A.id * int) list
type sync_jsoo = int Table.ct
let sync_to_jsoo (l: sync) : sync_jsoo t = Table.make l
let sync_of_jsoo (js: sync_jsoo t) : sync = Table.items (Unsafe.coerce js)

let print_sync fmt l = Format.fprintf fmt "%s" @@
  String.concat ", " @@ List.map (fun (id, seq) -> Format.sprintf "%s: %d" id seq) l

type ('a, 'content) message_kind =
  | Init of 'a
  | Modifs of 'content modif list
  | Sync of sync
[@@deriving jsoo]

type ('a, 'content) message = {
  entity: A.id;
  kind: ('a, 'content) message_kind;
} [@@deriving jsoo]

module type Db = sig
  type content
  val list: A.id -> (content modif list -> unit) -> unit
  val register: content modif -> unit
end

module type Comm = sig
  type conn
  type entity
  type content
  val origin: unit -> A.id
  val conn_id: conn -> A.id
  val connect: ?metadata:A.id -> A.id -> (conn -> unit) -> unit
  val send: conn:conn -> entity:A.id -> (entity, content) message_kind -> unit
  val data_listen: ?once:bool -> conn -> ((entity, content) message -> unit) -> unit
  val conn_listen: on:(conn -> A.id option -> unit) -> (conn -> unit) -> unit
end

module type App = sig
  type entity
  type content
  val apply: tsp:A.tsp -> entity -> content -> (entity -> unit) -> unit
  val finally: entity -> (entity -> unit) -> unit
  val load: A.id -> (entity option -> unit) -> unit
  val id: entity -> A.id
  val peers: entity -> A.id list
end
