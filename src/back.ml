open Bounty

module Comm = Sync_peerjs.Make(struct
    type t = modif_kind [@@deriving jsoo]
    type nonrec jsoo = jsoo Ezjs_min.t
    type entity = adventure [@@deriving jsoo]
    type nonrec entity_jsoo = entity_jsoo Ezjs_min.t
    let origin () : Sync.Types.A.id = !Db.peer
  end)

module App = struct
  type entity = adventure
  type content = modif_kind
  let apply ~tsp adv kind f =
    let adv = { adv with updated = tsp } in
    match kind with
    | AdventureName name -> f { adv with name }
    | AdventureCurrency currency -> f { adv with currency }
    | Sailor s ->
      let crew, sailor = List.fold_left (fun (crew, s) (sailor: sailor) -> match s with
        | Some (s: sailor) when s.id = sailor.id -> crew @ [ s ], None
        | _ -> crew @ [ sailor ], s) ([], Some s) adv.crew in
      let crew = match sailor with None -> crew | Some s -> crew @ [ s ] in
      f { adv with crew }
    | RemoveSailor id ->
      let crew = List.filter (fun (s: sailor) -> s.id <> id) adv.crew in
      f { adv with crew }
    | RemoveLoot id -> Db.remove_loot ~cb:(fun _ -> f adv) id
    | Loot lt -> Db.register_loot ~cb:(fun _ -> f adv) lt
  let finally a f = Db.update_adventure ~cb:(fun _ -> f a) a
  let load = Db.get_adventure
  let id (adv: adventure) = adv.id
  let peers (adv: adventure) = List.filter_map (fun (s: sailor) -> s.peer) adv.crew
end

include Sync.Make(Db.Sync)(Comm)(App)
