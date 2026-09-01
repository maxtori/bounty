open Bounty
open Ui

let%file _ = "./adventures.html"

let%prop adventures: adventure list = {req}

let%meth adventure app (a: adventure) = nav app (mkr (Adventure a))
and share _app (id: A.id) = share id
and remove app (id: A.id) =
  Db.remove_adventure id;
  nav app (mkr (Adventures []))
and edit app (a: adventure) = nav app (mkr (EditAdventure a))
and push app = nav app (mkr NewAdventure)

[%%comp {name="adventures"; conv}]
