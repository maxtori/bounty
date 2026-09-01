open Ezjs_min
open Ui

let%file _ = "./settings.html"

let%prop settings : settings = {req}

let%meth push app =
  Db.set_currency (to_string app##.settings##.currency);
  nav app (mkr (Adventures []))

[%%comp {name="settings"; conv}]
