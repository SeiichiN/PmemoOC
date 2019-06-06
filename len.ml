(*
 * len.ml -- 文字列の長さを取得する
 *)
let s1 = "google";;
let s2 = "itソリューション";;
let s3 = "大阪市立図書館";;

(* あ -- \227 \129 \130
 * い -- \227 \129 \132
 * 　 -- \227 \128 \128 (全角空白)
 *)

let t = ref 0
let n = ref 0
let zen = ref 0
let sw = ref false 

let count x =
  n := !n + 1; 
  let check1 () =
    if (Char.code x) >= 227
    then ( sw := true )
    else ()
  in
  check1 ();
  let check2 () =
    if !sw = true
    then (
      t := !t + 1;
      zen := !zen + 1 )
    else ()
  in
  check2 ();
  let check3 () =
    if !t = 3
    then (sw := false; t := 0 )
    else ()
  in
  check3 ()



let mblength s =
  sw := false;
  n := 0;
  zen := 0;
  t := 0;
  String.iter (fun x -> count x) s;
  Printf.printf "半角文字数 = %d\n" (!n - !zen);
  Printf.printf "全角文字数 = %d\n" (!zen / 3)
 
(*
 * 決められた文字数で文字列を表示する
 * @param: s : string -- 表示する文字列
 *         l : int    -- 表示の幅（文字数）
 * @return: string -- 決められたサイズの文字列
 *                    サイズの小さな文字列は空白で埋めることとする
 *)
let set_length s l =
  ""
