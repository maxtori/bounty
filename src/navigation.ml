open Ui

type push_content = {
  txt: string;
  cl: string
} [@@deriving jsoo]

let%file _ = "./navigation.html"

let%prop page : string = {req}

let%computed push_content app : push_content = match Ezjs_min.to_string app##.page with
  | "adventures" | "adventure" -> { txt="+"; cl="" }
  | "new_adventure" | "new_loot" -> { txt="Create"; cl="text-bg-secondary" }
  | "edit_adventure" | "edit_loot" -> { txt="Edit"; cl="text-bg-secondary" }
  | "join_adventure" -> { txt="Join"; cl="text-bg-secondary" }
  | "settings" -> { txt="Save"; cl="text-bg-secondary" }
  | _ -> { txt=""; cl="" }

let%meth settings app = nav app @@ mkr (Settings { id = !Db.peer; theme = !Db.theme; currency = !Db.currency })
and adventures app = nav app @@ mkr (Adventures [])
and push app = [%emit "push" app]

[%%comp {name="navigation"; conv}]
