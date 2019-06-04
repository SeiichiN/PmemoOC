(*
 * disp.ml
 *)

open Type
open Printf
open Myuty

(*
 * 与えられた文字列を繰り返し表示する関数
 * @param: s : string -- 文字列。たとえば "-"
 *         n : int -- 繰り返す回数
 * @return: n回繰り返された s文字列
 *)
let rec rep s n =
    if n = 0 then ""
    else s ^ (rep s (n-1))

let rec disp_list = function
    [] -> ()
    | (a : pmemo) :: rest -> 
            printf 
            "%4d %-20s %-10s %-20s %-20s %-10s %-20s\n" 
            a.no a.name a.id a.email a.password a.other a.created_at;
            disp_list rest    

let disp_menu () =
    print_newline ();
    print_endline "---------------- Pmemo -----------------";
    print_endline "|  1) データの追加                     |";
    print_endline "|  2) データの検索・訂正               |";
    print_endline "|  3) データの一覧                     |";
    print_endline "|  4) 削除処理                         |";
    print_endline "|  0) 終了                             |";
    print_endline "| -----------------------------------  |";
    print_endline "| Copyright (c) 2019 Seiichi Nukayama  |";
    print_endline "| http://www.billies-works.com/        |";
    print_endline "----------------------------------------";
    print_newline ();
    print_string "番号を入力してください > ";
    flush stdout;
    let bango = input_line stdin in
    bango

let disp_edit_menu () =
    print_newline ();
    print_endline ( (rep "-" 15) ^ " データの検索 " ^ (rep "-" 15) );
    print_newline ();
    print_string "データの名前を指定してください (list:リスト表示  0:中止) > ";
    flush stdout;
    let objName = input_line stdin in
    objName


let print_one_data m =
    print_newline ();
    print_endline ( (rep "-" 15) ^ " データ " ^ (rep "-" 15) );
    printf " NO: %-4d\n"     m.no;
    printf " 1)       name: %-50s\n" m.name;
    printf " 2)         id: %-50s\n" m.id;
    printf " 3)      email: %-50s\n" m.email;
    printf " 4)   password: %-50s\n" m.password;
    printf " 5)      other: %-50s\n" m.other;
    printf "    created_at: %-20s\n" m.created_at;
    print_endline (rep "-" 38);
    print_newline ()

let rec disp_select_data = function
    [] -> ()
  | m :: rest ->
          print_one_data m;
          disp_select_data rest


let disp_specify_data () =
    let bango = disp_mes_get_bango "どのデータを選択しますか？ (NO で指定  0:もどる) > " in
    bango

let disp_select_number () =
    let bango =  disp_mes_get_bango "どの項目を修正しますか？ (数字で指定  0:もどる) > " in
    bango

let disp_edit_or_delete () =
    let bango =  disp_mes_get_bango "1:訂正  2:削除  0:もどる > " in
    bango
    

let rec disp_name_list n = function
    [] -> ""
  | a :: rest ->
          if (n mod 4) = 0 then print_newline ();
          printf "%-20s " a;
          disp_name_list (n+1) rest

let ask_yesno s =
    print_newline ();
    print_string (s ^ " (y/n) > ");
    flush stdout;
    let yesno = String.lowercase_ascii (input_line stdin) in
    if yesno = "y"
    then "y"
    else "n"

let make_new_pmemo () =
  print_newline ();
  print_endline ( (rep "-" 15) ^ " 新規データ作成 " ^ (rep "-" 15) );
  print_newline ();
  let temp_name = get_user_input "name > "
  and temp_id = get_user_input "id > "
  and temp_email = get_user_input "email > "
  and temp_password = get_user_input "password > "
  and temp_other = get_user_input "other > " in
  let new_pmemo = {
    no = 0;
    name = temp_name;
    id = temp_id;
    email = temp_email;
    password = temp_password;
    other = temp_other;
    created_at = ""
  } in
  new_pmemo





