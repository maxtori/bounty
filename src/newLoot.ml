open Ezjs_min
open Bounty
open Ui

let%file _ = "./newLoot.html"

let%prop adv: adventure = {req}
and loot: loot option = None

let%data lt app : loot = match to_optdef loot_of_jsoo app##.loot with
  | Some lt -> lt
  | None ->
    let adv = adventure_of_jsoo app##.adv in
    let captain = Option.value ~default:"" @@ me adv.crew in
    let shares = Equal (List.map (fun (s: sailor) -> s.id) adv.crew) in
    let tsp = Ui.now () in
    let loot = {
      id = id (); adventure=(to_string app##.adv##.id);
      name=""; updated = tsp; date = tsp;
      captain; treasurer=captain; amount=zero; shares; currency=None } in
    loot

and sharing app : sharing = match to_optdef loot_of_jsoo app##.loot with
  | None -> `equal
  | Some lt -> match lt.shares with
    | Equal _ -> `equal
    | Weighted _ -> `weighted
    | Custom _ -> `custom
and update app : bool = Option.is_some (Optdef.to_option app##.loot)

let%computed [@noconv] currency app : js_string t =
  match to_opt currency_change_of_jsoo app##.lt##.currency with
  | None -> app##.adv##.currency
  | Some { symbol; _ } -> string symbol

let reset_amount ?amount ~crew sharing =
  match sharing with
  | `equal -> Equal (List.map (fun (s: sailor) -> s.id) crew), Option.value ~default:0 amount
  | `weighted -> Weighted (List.map (fun (s: sailor) -> { sailor=s.id; weight=1 }) crew), Option.value ~default:0 amount
  | `custom -> match amount with
    | None -> Custom (List.map (fun (s: sailor) -> { sailor=s.id; amount={cents=0; str="0.00" }}) crew), 0
    | Some am ->
      let n = List.length crew in
      let amount, rest = am / n, am mod n in
      Custom (List.mapi (fun i (s: sailor) ->
        let cents = if i = 0 then amount + rest else amount in
        { sailor=s.id; amount=show_amount cents }) crew), am

let%meth sailor_name app (id: A.id) : string =
  (List.find (fun (s: sailor) -> s.id = id) @@ crew_of_jsoo app##.adv##.crew).name

and [@noconv] tsp_to_datetime _app (tsp: float) =
  let d = new%js date_fromTimeValue (number_of_float (tsp *. 1000.)) in
  ignore (d##setMinutes (d##getMinutes - d##getTimezoneOffset));
  d##toISOString##slice 0 16

and [@noconv] change_date app (ev: Dom_html.inputElement Dom.event t) =
  match Opt.to_option ev##.target with
  | None -> ()
  | Some target ->
    let cs = Unsafe.global##._Date in
    let d : date t = new%js cs target##.value in
    app##.lt##.date := number_of_float ((float_of_number d##valueOf) /. 1000.)

and change_sharing app (sharing: sharing) =
  let crew = crew_of_jsoo app##.adv##.crew in
  let amount = parse_amount (to_string app##.lt##.amount##.str) in
  let shares, amount = reset_amount ?amount ~crew sharing in
  app##.lt##.amount := amount_to_jsoo (show_amount amount);
  app##.lt##.shares := shares_to_jsoo shares

and change_amount app (amount: string) =
  match parse_amount amount with
  | None ->
    alert app (Format.sprintf "cannot parse amount %s" amount);
    app##.lt##.amount := amount_to_jsoo zero
  | Some cents -> app##.lt##.amount := amount_to_jsoo (show_amount cents)

and show_equal_amount app (id: A.id) : amount =
  let treasurer = A.id_of_jsoo app##.lt##.treasurer in
  let amount = Option.value ~default:0 @@ parse_amount (to_string app##.lt##.amount##.str) in
  let shares = List.map (fun (s: sailor) -> s.id) @@ crew_of_jsoo app##.adv##.crew in
  let amount = compute_equal_amount ~treasurer ~id ~shares amount in
  show_amount amount

and show_weighted_amount app (id: A.id) (shares: weight list): amount =
  let treasurer = A.id_of_jsoo app##.lt##.treasurer in
  let amount = Option.value ~default:0 @@ parse_amount (to_string app##.lt##.amount##.str) in
  let amount = compute_weighted_amount ~treasurer ~id ~shares amount in
  show_amount amount

and change_custom_amount app (id: A.id) (am: string) (shares: share list) =
  match parse_amount am with
  | None -> ()
  | Some cents ->
    let total, shares = List.fold_left (fun (total, shares) (s: share) ->
      if s.sailor = id then total + cents, { sailor=id; amount = show_amount cents } :: shares
      else total + s.amount.cents, s :: shares) (0, []) shares in
    app##.lt##.shares := shares_to_jsoo (Custom (List.rev shares));
    app##.lt##.amount := amount_to_jsoo (show_amount total)

and [@noconv] change_currency app (ev: Dom_html.inputElement Dom.event t) =
  match Opt.to_option ev##.target with
  | None -> ()
  | Some target ->
    let adv_currency = to_string app##.adv##.currency in
    let symbol = to_string target##.value in
    if adv_currency = symbol then app##.lt##.currency := null
    else app##.lt##.currency := some (currency_change_to_jsoo { symbol; factor = 1. })

and push app =
  let adv = adventure_of_jsoo app##.adv in
  let loot = loot_of_jsoo app##.lt in
  if loot.name = "" then alert app "The loot needs a name" else
  if loot.amount.cents = 0 then alert app "The amount of a loot cannot be empty" else
  if Some loot.captain <> me adv.crew then alert app "Only the captain of a loot can modify it" else
  let old = to_optdef loot_of_jsoo app##.loot in
  if Option.compare compare_loot old (Some loot) = 0 then alert app "The loot hasn't been changed" else
  let tsp = now () in
  Back.process ~tsp ~id:(id ()) adv (Loot { loot with updated=tsp }) @@ fun () ->
  adventure app adv.id

and adventure app =
  let id = A.id_of_jsoo app##.adv##.id in
  adventure app id

[%%comp {name="new-loot"; conv}]
