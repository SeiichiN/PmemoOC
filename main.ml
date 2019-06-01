#load "readfile.cmo"
#load "myuty.cmo"

open Readfile
open Myuty
open Printf
module P = Mysql.Prepared

type pmemo = {
    name : string;
    id   : string;
    email : string;
    password : string;
    memo : string
}

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
  | Some one ->
        (printf "name = %s\n id = %s\n email = %s\n password = %s\n memo = %s\n"
        one.name one.id one.email one.password one.memo);
        print_endline "";
        loop t;
  | None -> ()


let listAll () =
    let sql = "select * from " ^ tablename  in
    let select = P.create db sql in
    loop (P.execute select);
    P.close select;
    print_endline "done all.";
    ()


