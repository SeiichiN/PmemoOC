(*
 * len.ml -- 文字列の長さを取得する
 *)
let s1 = "google"
and s2 = "itソリューション"
and s3 = "大阪市立図書館"
in
print_endline (s1 ^ "= " ^ string_of_int(String.length s1));
print_endline (s2 ^ "= " ^ string_of_int(String.length s2));
print_endline (s3 ^ "= " ^ string_of_int(String.length s3));

print_endline "全角文字の数"

(* あ -- \227 \129 \130
 * い -- \227 \129 \132
 * 　 -- \227 \128 \128 (全角空白)
 *)

let t = ref 0

let count x =
    let check () = 
        if (Char.code x) >= 227
        then
            t := !t + 1
    in


let mblength s =
    let n = ref 0 in
    String.iter (fun x -> n := !n + 1) s; !n
 


(*
        Exception: Invalid_argument "index out of bounds".
*)
