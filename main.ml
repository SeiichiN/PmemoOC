(*
 * main.ml
 *)
#load "readfile.cmo"
#load "myuty.cmo"
#load "mysql.cma"
#load "type.cmo"
#load "disp.cmo"

open Readfile
open Myuty
open Printf
open Mysql
open Type
open Disp
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

let count = ref 0

(*
 * 全レコードを読む
 * @param: c -- dbdオブジェクト
 * @resutn: pmemo list
 *)
let read_table c =
    let sql = "select * from " ^ tablename in
    let r = exec c sql in
    let col = column r in
    let row n x = { 
        no         = n;
        name       = not_null str2ml (col ~key:"name" ~row:x);
        id         = not_null str2ml (col ~key:"id" ~row:x);
        email      = not_null str2ml (col ~key:"email" ~row:x);
        password   = not_null str2ml (col ~key:"password" ~row:x);
        other      = not_null str2ml (col ~key:"other" ~row:x);
        created_at = not_null str2ml (col ~key:"created_at" ~row:x)
    } in
    let rec loop n = function
        | None -> []
        | Some x -> 
                row n x :: loop (n+1) (fetch r)
    in
    loop 1 (fetch r)


let listAll () =
    let l = read_table db in
    ignore(disp_list l); 
    disconnect db;
    ()

(* 複数選択されたリストからひとつを選択する *)
let specify_data n l =
    let rec loop n = function
        [] -> []
        | (a : pmemo) :: rest ->
                if a.no = n 
                then a::[] 
                else loop n rest
    in
    loop n l

(* mysql から得た一つのデータは配列なので、それを pmemo タイプに換える *)        
let arr_to_rec r =
    count := !count + 1;
    let oneRecord = {
        no         = !count;
        name       = not_null str2ml (Array.get r 0);
        id         = not_null str2ml (Array.get r 1);
        email      = not_null str2ml (Array.get r 2);
        password   = not_null str2ml (Array.get r 3);
        other      = not_null str2ml (Array.get r 4);
        created_at = not_null str2ml (Array.get r 5)
    } in
    oneRecord
       
(* mysql で得たデータから pmemo list を作る *)
let rec select_loop t =
    match P.fetch t with
    | None -> []
    | Some r -> 
            (arr_to_rec r) :: (select_loop t)

(* データを検索する *)
let select_data c s =
    let ss = "%" ^ s ^ "%" in
    let sql = "select * from " ^ tablename ^ " where name like ?" in
    let select = P.create db sql in
    let dataList = select_loop (P.execute select [|ss|]) in
    if (List.length dataList) > 0
    then 
        disp_select_data dataList;
        if (List.length dataList) > 1
        then
            let num = disp_specify_data () in
            let dataList' = specify_data num dataList in
            disp_select_data dataList';
            ()
    else print_endline "データはありません。";
    P.close select;
    print_endline "done.";
    ()

(* 検索・訂正処理 *)
let edit_data () =
    let s = disp_edit_menu () in
    count := 0;
    select_data db s

let select_menu () =
    let n = choice 4 disp_menu in
    match n with
    1 -> ()
        | 2 -> edit_data ()
        | 3 -> listAll ()
        | 4 -> ()
        | 0 -> print_endline "BYE."
        | _ -> print_endline "?????"


let _ = select_menu ()

