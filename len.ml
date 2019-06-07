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

let t = ref 0  (* swがtrue になって3バイトまでを数えるため *)
let n = ref 0
let zen = ref 0
let sw = ref false 

(*
 * 総バイト数と総全角文字バイト数を求める関数
 * n -- バイト数
 * zen -- 全角文字のバイト数
 *)
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


(*
 * 半角文字数 + 全角文字数 * 2 を求める関数
 *)
let mblength s =
  sw := false;
  n := 0;
  zen := 0;
  t := 0;
  String.iter (fun x -> count x) s;
  let han_len = !n - !zen in
  let zen_len = !zen / 3 in
  Printf.printf "半角文字バイト数 = %d\n" han_len;
  Printf.printf "全角文字バイト数 = %d\n" zen_len;
  han_len + zen_len * 2

(*
 * n 個の連続した s を求める関数
 *)
let rec rep s n =
    s ^ (rep s (n-1))

(*
 * char文字が1バイト文字か utf-8かをチェックし、
 * その結果を返す
 * @return:
 *    0 -- 1バイト文字
 *    1 -- utf-8 の 1バイトめ
 *    2 -- utf-8 の 2バイトめ
 *    3 -- utf-8 の 3バイトめ
 *)
let char_check c =
    sw := false;
    let kekka = ref 0 in
    let check1 () =
        if (Char.code c) >= 227 && !sw = false
        then sw := true
        else kekka := 0
    in
    let check2 () =
        if !sw = true
        then 
            t := !t + 1;
            kekka := !t
    in
    let check3 () =
        if !sw = true && !t = 3
        then
            sw := false;
            t := 0
    in
    check1 ();
    check2 ();
    check3 ();
    !kekka



(*
 * 文字列s を先頭から n文字分切り取った文字列を求める関数
 * 文字列s は、n文字分以上の長さがあるものとする
 *)
let mbsubstr s n =
    let ans = ref ""
    and len = ref 0
    and z_moji = ref "" in
    (*
     * 1char文字 c を受け取って、もし 1バイト文字なら 1 を
     * utf-8 の 3バイトめ なら 2 を返す
     * utf-8 の 1バイトめ、2バイトめなら、0 をかえす
     * つまり、画面上に占有する桁数を返す
     *)
    let str_count c =
        let m = char_check c in
        if m = 0 then 1
        else
            if m = 3 then 2
            else 0
    in
    (*
     * Char文字 c を受け取って、
     * str_count（桁数）が
     *   1 -- ans 文字列に c を追加
     *   2 -- ans 文字列に z_moji を追加
     *   0 -- z_moji に c を追加
     *)
    let mkstr c =
        let keta = str_count c in
        let moji = Char.escaped c in
        let check1 () =
            if keta = 1
            then ans := !ans ^ moji
            else
                if keta = 2
                then ans := !z_moji ^ moji
                else z_moji := moji
        in
        let keta_check () =
            if keta = 0 then let keta' = 2 in keta'
            else let keta' = keta in keta'
        in
        let keta' = keta_check() in
        len := !len + keta';
        if !len <= n then check1 ()
        else ()
    in
    String.iter (fun x -> mkstr x) s;
    !ans




(*
 * 決められた文字数で文字列を表示する
 * @param: s : string -- 表示する文字列
 *         l : int    -- 表示の幅（文字数）
 * @return: string -- 決められたサイズの文字列
 *                    サイズの小さな文字列は空白で埋めることとする
 *)
let show_length s l =
    let moji = ref "" in
    if (mblength s) < l
    then (moji := s ^ (rep " " (l - (mblength s))); !moji)
    else
        if (mblength s) = l
        then s
        else mbsubstr s l

