open Bounty
open Ui

let%file _ = "./adventures.html"

let%prop adventures: adventure list = []

let%meth adventure app (id: A.id) = adventure app id
and share _app (id: A.id) = share id
and remove app (id: A.id) =
  Db.remove_adventure id;
  adventures app
and edit app (a: adventure) = nav app (mkr (EditAdventure a))
and push app = nav app (mkr NewAdventure)

[%%comp {name="adventures"; conv}]
