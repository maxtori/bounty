open Ezjs_min
open Bounty

type adv_and_loot = {
  adv: adventure;
  loot: loot;
} [@@deriving jsoo]

type join_adventure = {
  adventure: A.id;
  peer: A.id;
} [@@deriving jsoo]

type settings = {
  id: A.id;
  theme: string;
  currency: string
} [@@deriving jsoo]

type page =
  | Loading
  | Adventures
  | Adventure of A.id
  | NewAdventure
  | EditAdventure of adventure
  | Loot of adv_and_loot
  | NewLoot of adventure
  | EditLoot of adv_and_loot
  | Settings of settings
  | JoinAdventure of join_adventure
[@@deriving jsoo {remove_undefined; snake}]

type sharing = [ `equal | `weighted | `custom ] [@@deriving jsoo]

type route = {
  page: page; [@mutable]
  loading: bool;
  prev: page option;
  path: string option;
  set_state: bool;
} [@@deriving jsoo]

let to_raw x = Unsafe.global##._Vue##toRaw x

let rec unproxy x =
  let x = to_raw x in
  if x = Unsafe.pure_js_expr "undefined" then
    Unsafe.pure_js_expr "undefined"
  else if x = Unsafe.pure_js_expr "null" then
    Unsafe.pure_js_expr "null"
  else if to_bool (Unsafe.global##._Array##isArray x) then
    Unsafe.coerce (array_map unproxy (Unsafe.coerce x))
  else if to_string (typeof x) = "object" then
    let a = Unsafe.global##._Object##entries x in
    let entries = Unsafe.coerce (array_map unproxy (Unsafe.coerce a)) in
    Unsafe.coerce (Unsafe.global##._Object##fromEntries entries)
  else x

let share id =
  let origin = to_string Dom_html.window##.location##.origin in
  let pathname = to_string Dom_html.window##.location##.pathname in
  let pathname = match List.rev @@ String.split_on_char '/' pathname with
    | "index.html" :: tl -> "/" ^ String.concat "/" (List.rev @@ List.filter (fun s -> s <> "") tl)
    | [""; ""] -> ""
    | _ -> pathname in
  let s = Format.sprintf "%s%s?adventure=%s&peer=%s" origin pathname id !Db.peer in
  let navigator = Unsafe.coerce Dom_html.window##.navigator in
  let p =
    try navigator##share (object%js val url = string s val title = string "bounty" end)
    with _ -> navigator##.clipboard##writeText (string s) in
  Promise.jthen p Fun.id

let mkr ?path ?(loading=true) ?prev ?(set_state=true) page =
  route_to_jsoo { page; path; loading; prev; set_state }

let mkrjs ?path ?(loading=true) ?prev ?(set_state=true) page =
  let r = route_to_jsoo { page=Loading; path; loading; prev; set_state } in
  r##.page := page;
  r

let now () =
  let date = new%js date_now in
  (float_of_number date##valueOf) /. 1000.

let alert app s = [%emit "alert" app (string s)]
let nav app (r: route_jsoo t) = [%emit "nav" app r]

let me crew =
  List.find_map (fun (s: sailor) -> if s.peer = Some !Db.peer then Some s.id else None) crew
