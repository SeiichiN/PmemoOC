#load "readfile.cmo"
#load "myuty.cmo"
#load "mysql.cma"

open Readfile
open Myuty
open Printf
open Mysql
module P = Mysql.Prepared

type pmemo = {
    name : string;
    id   : string;
    email : string;
    password : string;
    other : string;
    created_at : string
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

(*
 * 全レコードを読む
 * @param: c -- dbdオブジェクト
 * @resutn: pmemo list
 *)
let read_table c =
    let sql = "select * from " ^ tablename in
    let r = exec c sql in
    let col = column r in
    let row x = { 
        name       = not_null str2ml (col ~key:"name" ~row:x);
        id         = not_null str2ml (col ~key:"id" ~row:x);
        email      = not_null str2ml (col ~key:"email" ~row:x);
        password   = not_null str2ml (col ~key:"password" ~row:x);
        other      = not_null str2ml (col ~key:"other" ~row:x);
        created_at = not_null str2ml (col ~key:"created_at" ~row:x)
    } in
    let rec loop = function
        | None -> []
        | Some x -> row x :: loop (fetch r)
    in
    loop (fetch r)


let rec disp_list = function
    [] -> ()
        (* | (name,id,email,password,memo) :: rest ->  *)
        | (a : pmemo) :: rest -> 
                printf 
                "%-20s %-10s %-20s %-20s %-10s %-20s\n" 
                a.name a.id a.email a.password a.other a.created_at;
            disp_list rest    

let listAll () =
    let l = read_table db in
    ignore(disp_list l); 
        disconnect db;
        ()

let _ = listAll ()

