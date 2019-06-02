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

(* データの修正
 * n -- int 修正する項目番号
 * l -- pmemo list 修正するデータ
 *)
let retouch n l =
    let one_record = List.hd l in    (* この時点ではリストは一つである *)
    let field_value = get_field_value n one_record in
    let field_message = assoc n message_list in
    let field_name = assoc n field_list in
    let message_str = "現在の設定： " ^ field_message ^ " = " ^ field_value in
    print_endline message_str;
    print_string "新しい値 > ";
    flush stdout;
    let newValue = input_line stdin in
    (one_record, field_name, newValue)
 

(* データを検索する *)
let select_data c s =
    let dataList = ref [] in
    let ss = "%" ^ s ^ "%" in
    let sql = "select * from " ^ tablename ^ " where name like ?" in
    let select = P.create db sql in
    let dataList' = select_loop (P.execute select [|ss|]) in
    P.close select;
    if (List.length dataList') > 0 (* データがあれば *)
    then 
        disp_select_data dataList';
        let data_max = List.length dataList' in
        let syori () =
            if data_max > 1  (* データが複数あれば *)
            then
                let num = choice data_max disp_specify_data in   (* 選択画面 *)
                dataList := specify_data num dataList'  (* 選択する *)
            else 
                dataList := dataList'
        in
        syori ();
        disp_select_data !dataList;  (* データを表示 *)
        let n = choice 5 disp_select_number in  (* 修正する項目を番号で選択 *)
        if n > 0 && n < 6
        then 
            let (pmemo', fieldname, newValue) = retouch n !dataList in
            let new_pmemo = remake_pmemo pmemo' fieldname newValue in
            ignore new_pmemo
    else 
        print_endline "データはありません。";
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

