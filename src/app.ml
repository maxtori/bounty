open Ezjs_min
open Bounty
open Ui

let%data page: page = Loading
and tsp : int = Int32.to_int (to_int32 date##now) / 1000
and error: string option = None

let navigate app (r: route_jsoo t) =
  app##.tsp := Int32.to_int (to_int32 date##now) / 1000;
  let p0, p1 = unproxy app##.page, unproxy r##.page in
  if to_bool r##.loading then (app##.page := page_to_jsoo Loading);
  Firebug.console##log_3 p0 (string "-->") p1;
  let state p = some @@ Unsafe.coerce p  in
  let () = match page_of_jsoo p0 with
    | Loading -> ()
    | _ ->
      if to_bool r##.set_state_ then
        let p0 = match Optdef.to_option r##.prev with None -> p0 | Some p0 -> p0 in
        Dom_html.window##.history##replaceState (state p0) (string "") null in
  let finish app =
    app##.page := p1;
    if to_bool r##.set_state_ then
      let path = Opt.option (Optdef.to_option r##.path) in
      Dom_html.window##.history##pushState (state p1) (string "") path in
  finish app

let adventures app l = navigate app (mkr (Adventures l))
let new_adventure app = navigate app (mkr ~loading:false NewAdventure)

let init app =
  Db.get_adventures @@ function
  | [] -> new_adventure app
  | l -> adventures app l

let load app =
  Db.load_settings @@ fun () ->
  Db.get_adventures @@ fun adventures ->
  Db.init adventures @@ fun () ->
  Comm.init adventures;
  Dom_html.window##.onpopstate := Dom_html.handler (fun (e : Dom_html.popStateEvent t) ->
    (try navigate app (mkrjs ~set_state:false @@ Unsafe.coerce e##.state) with _exn -> init app); _false);
  (Unsafe.coerce Dom_html.window)##.onfocus := Dom_html.handler (fun (_e : Dom_html.popStateEvent t) ->
    let now = Int32.to_int (to_int32 date##now) / 1000 in
    if now > app##.tsp + 3600 then navigate app (mkrjs app##.page);
    _false);
  let join_param =
    let search = to_string Dom_html.window##.location##.search in
    if search = "" then None else
    let l = String.split_on_char '&' (String.sub search 1 (String.length search - 1)) in
    let l = List.map (fun s -> match String.split_on_char '=' s with
      | [] -> s, None | [ k ] -> k, None
      | k :: v -> k, Some (String.concat "=" v)
    ) l in
    match List.assoc_opt "peer" l, List.assoc_opt "adventure" l with
    | Some Some peer, Some Some adventure -> Some (peer, adventure)
    | _ -> None in
  match join_param, Opt.to_option (Unsafe.coerce Dom_html.window##.history)##.state with
  | Some (peer, adventure), _ ->
    navigate app (mkr (JoinAdventure { peer; adventure }))
  | _, Some p ->
    (match page_of_jsoo p with
     | Loading -> init app
     | _ -> navigate app (mkrjs ~set_state:false p)
     | exception _ -> init app)
  | _ | exception _ -> init app

let selected page =
  let f x = array_get x 1 <> undefined in
  to_string @@ Option.get @@ Optdef.to_option @@
  array_get ((Unsafe.global##._Object##entries page)##find (wrap_callback f)) 0

let%computed selected app : string = selected app##.page

let%meth [@noconv] nav app (r: route_jsoo t) = match route_of_jsoo r with
  | { page = Adventures []; _ } -> init app
  | { page = Adventure ({loots=[]; _ } as adv); _ } as r ->
    Db.load_loots ~adventure:adv.id @@ fun loots ->
    navigate app (route_to_jsoo { r with page = Adventure { adv with loots } })
  | { page = EditAdventure adv; _ } as r ->
    Db.get_adventure adv.id (function
      | None -> init app
      | Some adv -> navigate app (route_to_jsoo { r with page = EditAdventure adv }))
  | _ -> navigate app r

and [@noconv] alert app (s: js_string t) =
  app##.error := def s;
  let cs : _ constr = Unsafe.global##.bootstrap##._Modal in
  let md = new%js cs (string "#error-modal") in
  ignore md##show

and [@noconv] push app =
  let s = to_string app##.selected in
  match Optdef.to_option [%ref app s] with
  | Some comp -> (Unsafe.coerce comp)##push
  | _ -> ()

[%%mounted fun app ->
  Db.open_ @@ fun () ->
  load app;
  let error_elt = Dom_html.getElementById "error-modal" in
  ignore (Js_of_ocaml.Dom_events.listen error_elt (Js_of_ocaml.Dom_events.Typ.make "hide.bs.modal") @@ fun _ _ ->
          app##.error := undefined; true);
]

[%%app {
  conv; mount; unhide; export;
  components=[
    Navigation; Settings;
    Adventures; Adventure; NewAdventure; JoinAdventure;
    Loot; NewLoot ]
}]
