open Ezjs_min
open Bounty
open Ui

let%file _ = "./adventures.html"

let%data adventures: adventure list = []

let%meth adventure app (id: A.id) = nav app (mkr (Adventure id))
and share _app (id: A.id) = share id
and remove app (id: A.id) =
  Db.remove_adventure id;
  nav app (mkr Adventures)
and edit app (a: adventure) = nav app (mkr (EditAdventure a))
and push app = nav app (mkr NewAdventure)
and refresh app = nav app (mkr Adventures)

[%%mounted fun app ->
  Db.get_adventures @@ function
  | [] -> nav app (mkr NewAdventure)
  | l -> app##.adventures := of_listf adventure_to_jsoo l
]


[%%comp {name="adventures"; conv}]
