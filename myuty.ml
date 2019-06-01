

let rec assoc s = function
    [] -> ""
    | (a, v) :: rest ->
            if a = s 
            then v
            else assoc s rest

