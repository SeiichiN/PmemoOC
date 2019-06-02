
type pmemo = {
    mutable no : int;
    mutable name : string;
    mutable id   : string;
    mutable email : string;
    mutable password : string;
    mutable other : string;
    mutable created_at : string
}

let get_field_value n r =
    match n with
    0 -> ""
    | 1 -> r.name
    | 2 -> r.id
    | 3 -> r.email
    | 4 -> r.password
    | 5 -> r.other
    | _ -> ""

let field_list = [
    (1, "name"); (2, "id"); (3, "email"); (4, "password"); (5, "other")
]

let message_list = [
    (1, "name > "); (2, "id > "); (3, "email > "); (4, "password > "); (5, "other > ")
]

let remake_pmemo m fieldname newValue =
    match fieldname with
    "name" -> m.name <- newValue; m
    | "id" -> m.id <- newValue; m
    | "email" -> m.email <- newValue; m
    | "password" -> m.password <- newValue; m
    | "other" -> m.other <- newValue; m
    | "" ->  m
    | _ -> m
