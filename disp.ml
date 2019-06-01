(*
 * disp.ml
 *)

let rec disp_list = function
    [] -> ()
        (* | (name,id,email,password,memo) :: rest ->  *)
        | (a : pmemo) :: rest -> 
                printf 
                "%-20s %-10s %-20s %-20s %-10s %-20s\n" 
                a.name a.id a.email a.password a.other a.created_at;
            disp_list rest    
