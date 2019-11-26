(*
 * main.ml
 *
 * ocamlfind ocamlopt -o pmemo -linkpkg -package mysql,str type.ml myuty.ml len.ml readfile.ml disp.ml main.ml
 *)
(* utopの場合はこれをはずす *)
(*
#load "readfile.cmo"
#load "myuty.cmo"
#load "mysql.cma"
#load "type.cmo"
#load "disp.cmo"
#load "len.cmo"
*)

open Readfile
open Myuty
open Printf
open Mysql
open Type
open Disp
open Len
module P = Mysql.Prepared

exception Out_of_loop

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

(* Table ga nakereba tukuru *)
let table_check () =
  let sql = "CREATE TABLE IF NOT EXISTS " ^ tablename
            ^ " (no int NOT NULL AUTO_INCREMENT, "
            ^ "name varchar(255), "
            ^ "id varchar(255), "
            ^ "email varchar(255), "
            ^ "password varchar(255), "
            ^ "other varchar(255), "
            ^ "created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP, "
            ^ "PRIMARY KEY (no)) DEFAULT CHARSET=utf8"
  in
  Mysql.exec db sql
    
    
    
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
    let row nn x = { 
        no         = nn;
        recno      = not_null int2ml (col ~key:"no" ~row:x);
        name       = not_null str2ml (col ~key:"name" ~row:x);
        id         = not_null str2ml (col ~key:"id" ~row:x);
        email      = not_null str2ml (col ~key:"email" ~row:x);
        password   = not_null str2ml (col ~key:"password" ~row:x);
        other      = not_null str2ml (col ~key:"other" ~row:x);
        created_at = not_null str2ml (col ~key:"created_at" ~row:x)
    } in
    let rec loop nn = function
        | None -> []
        | Some x -> 
                row nn x :: loop (nn+1) (fetch r)
    in
    loop 1 (fetch r)


let listAll () =
    let l = read_table db in
    ignore(disp_list l); 
    (* disconnect db; *)
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
        recno      = not_null int2ml (Array.get r 0);
        name       = not_null str2ml (Array.get r 1);
        id         = not_null str2ml (Array.get r 2);
        email      = not_null str2ml (Array.get r 3);
        password   = not_null str2ml (Array.get r 4);
        other      = not_null str2ml (Array.get r 5);
        created_at = not_null str2ml (Array.get r 6)
    } in
    oneRecord
       
(* mysql で得たデータから pmemo list を作る *)
let rec select_loop t =
    match P.fetch t with
    None -> []
    | Some r -> 
            (arr_to_rec r) :: (select_loop t)

(*
 * MySQLでデータをインサートする処理
 *)
let insert_pmemo pm =
  let sql = "insert into " ^ tablename ^ " (name, email, id, password, other) values (?, ?, ?, ?, ?)" in
  let insert = P.create db sql in
  ignore (P.execute insert [|pm.name; pm.email; pm.id; pm.password; pm.other|]);
  P.close insert


(*
 * MySQLでデータをアップデートする処理
 * @param: pm : pmemo -- レコード型
 * @return: ()
 *)
let update_pmemo pm =
    let sql = "update " ^ tablename ^ 
    " set id = ?, email = ?, password = ?, other = ? where name = ?" in
    let update = P.create db sql in
    ignore (P.execute update [|pm.id; pm.email; pm.password; pm.other; pm.name|]);
    P.close update

(*
 * MySQLでデータを削除する処理
 * @param: name : string -- 削除したいデータの name
 * @return: ()
 *)
let delete_pmemo name =
  let sql = "delete from " ^ tablename ^ " where name = ?" in
  let delete = P.create db sql in
  ignore (P.execute delete [|name|]);
  P.close delete

(* データの修正
 * n -- int 修正する項目番号
 * l -- pmemo list 修正するデータ
 * @return: (pmemo, string, string)
 *)
let retouch n l =
    let one_record = List.hd l in    (* この時点ではリストは一つである *)
    let field_value = get_field_value n one_record in
    let field_message = assoc n message_list in
    let field_name = assoc n field_list in
    let message_str = "現在の設定： " ^ field_message ^ field_value in
    print_endline message_str;
    print_string "新しい値 > ";
    flush stdout;
    let newValue = input_line stdin in
    (one_record, field_name, newValue)

(* name だけをリストとして返す関数 *)
let list_name c =
    let sql = "select name from " ^ tablename in
    let r = exec c sql in
    let col = column r in
    let row x = (  
        not_null str2ml (col ~key:"name" ~row:x)
      ) in
    let rec loop = function
        None -> []
        | Some x -> 
                row x :: loop (fetch r)
    in
    loop (fetch r)

(*
 * MySQLで検索のsqlを実行する
 * @param: c : dbd -- データベース接続オブジェクト
 *         s : string 検索文字列 ここでは name で検索する
 * @return: dataList -- pmemo list (複数の可能性がある)
 *)
let search_data c s =
    let ss = "%" ^ s ^ "%" in
    let sql = "select * from " ^ tablename ^ " where name like ?" in
    let select = P.create db sql in
    let dataList = select_loop (P.execute select [|ss|]) in
    P.close select;
    dataList


(*
 * データを検索する
 * データリストが複数ある場合、ひとつに絞る
 *)
let select_data c s =
    let dataList' = search_data c s in
    let data_max = List.length dataList' in
    match data_max with
          0 ->
            print_endline "データはありません。"; []
        | 1 -> dataList'
        | _ -> 
            disp_select_data dataList';
            (* 「どのデータを選択しますか？ (NOで指定  0:もどる)」 *)
            let num = choice data_max disp_specify_data in   (* 選択画面 *)
            if num = 0
            then
                []
            else
                let dataList = specify_data num dataList' in  (* 選択する *)
                dataList  (* 戻り値のデータリスト *)
        
(* 指定された項目を修正する *)
let syusei dataList =
    (* 「どの項目を修正しますか？ (数字で指定  0:もどる)」 *)
    let n = choice 5 disp_select_number in  (* 修正する項目を番号で選択 *)
    if n > 0 && n < 6
    then 
        let (pmemo', fieldname, newValue) = retouch n dataList in
        let new_pmemo = remake_pmemo pmemo' fieldname newValue in
        disp_select_data [new_pmemo];
        let yesno = ask_yesno "これでよろしいですか？" in
        if yesno = "y"
        then
            (* new_pmemo をmysqlでアップデートする処理 *)
             update_pmemo new_pmemo
    else
        ()

(* 指定されたリストを削除する *)
let delete_data l =
  let yesno = ask_yesno "本当によろしいですか？" in
  if yesno = "y"
  then
    let m = List.hd l in
    delete_pmemo m.name;
    print_newline ();
    print_endline "削除しました。"
  else
    ()


(* 訂正か削除か処理を分岐 *)
let edit_or_delete l =
  let num = int_of_string (disp_edit_or_delete ()) in
  match num with
  0 -> ()
  | 1 -> syusei l  (* ()  *)
  | 2 -> delete_data l (*  ()  *)
  | _ -> ()

(* 検索処理 *)
let kensaku_data () =
    try
        let rec loop () =
            (* 「データの名前を指定してください(list:リスト表示  0:中止)」 *)
            let s = disp_search_menu () in
            match s with
            "0" -> raise Out_of_loop
            | "list" -> 
                    let datalist = list_name db in
                    ignore (disp_name_list 1 datalist);
                    loop ()  (* list のときの処理 *)
            | _ ->
                    count := 0;
                    let l = select_data db s in
                    disp_select_data l;
                    edit_or_delete l
        in
        loop ()
    with Out_of_loop -> ()

(* 追加処理 *)
let insert_data () =
  let new_pmemo = make_new_pmemo () in
  disp_select_data [new_pmemo];
  let insert_syori () =
    let yesno = ask_yesno "これでよろしいですか？" in
    if yesno = "y"
    then
      insert_pmemo new_pmemo
    else
      ()
  in
  insert_syori ()



let select_menu () =
    let n = choice 4 disp_menu in
    match n with
        0 -> print_endline "bye."; n
      | 1 -> insert_data (); n
      | 2 -> kensaku_data (); n
      | 3 -> listAll (); n
      | 4 -> kensaku_data (); n
      | _ -> print_endline "?????"; n


let _ =
  table_check ();
  let rec loop () =
    let n = select_menu () in
    if n = 0
    then ( disconnect db; ()) (* exit 0 *)
    else loop ()
  in
  loop ()


