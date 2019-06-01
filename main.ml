#load "readfile.cmo"
#load "myuty.cmo"

open Readfile
open Myuty
open Printf
module P = Mysql.Prepared

let conf = read_conf "pmemo.conf" 

let rec assoc s = function
    [] -> ""
    | (a, v) :: rest ->
            if a = s 
            then v
            else assoc s rest

let hostname = assoc "hostname" conf
and username = assoc "username" conf
and password = assoc "password" conf
and dbname = assoc "dbname" conf
and tablename = assoc "tablename" conf


(* 接続 *)
let db = Mysql.quick_connect ~database:dbname
    ~user:username ~password:password ~host:hostname ()


let rec loop t =
  match P.fetch t with
  | Some arr -> Array.iter (function Some s -> printf "%s " s
                                        | None -> print_string "<NULL>") arr;
    print_endline "";
    loop t
  | None -> ()


