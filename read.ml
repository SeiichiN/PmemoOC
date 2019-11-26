(*
readfile.ml
ファイルから設定情報を読み込む
 *)
open Str

(* リストのリストを 連想リストに変換する *)
let rec mk_assoc_list = function
    [] -> []
    | (_::_::_::_)::_ -> []
    | (_::[])::_ -> []
    | []::_ -> []
    | [a; b] :: rest ->
            (a, b) :: mk_assoc_list rest


let rec input_conf ch =
  (* ファイルの終了は、-- をわたす *)
  let s = try input_line ch with End_of_file -> "--" in
  (* 先頭が ; の行はコメントなので、パス *)
  if string_match (regexp "^;.") s 0 then input_conf ch
  else
      (* 先頭が -- の行は、終了 *)
      if string_match (regexp "^--") s 0
      then []
      else
          (* 空白を区切りとしてリストにする *)
          let conf =
              split (regexp "[ \t]+") s in
          conf :: input_conf ch

let read_conf filename =
  let ch = open_in filename in
  let l = input_conf ch in
  close_in ch;
  mk_assoc_list l

let _ =
  let dir = Sys.getcwd() in
  let f = dir ^ "/pmemo.conf" in
  read_conf f
