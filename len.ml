(*
 * len.ml -- 文字列の長さを取得する
 *)
let s1 = "google";;
let s2 = "itソリ";;
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
 * @return: !kekka : int
 *    0 -- 1バイト文字
 *    1 -- utf-8 の 1バイトめ
 *    2 -- utf-8 の 2バイトめ
 *    3 -- utf-8 の 3バイトめ
 *)
let tt = ref 0

let char_check c =
    let kekka = ref 0 in
    let check1 () =
        if (Char.code c) >= 227 && !sw = false
        then sw := true
        else kekka := 0
    in
    let check2 () =
        if !sw 
        then 
            (tt := !tt + 1;
            kekka := !tt)
        else ()
    in
    let check3 () =
        if !sw = true && !tt = 3
        then ( 
            sw := false;
            tt := 0 )
        else ()
    in
    check1 ();
    check2 ();
    check3 ();
    print_endline ("c= " ^ (Char.escaped c) ^ " " ^ (string_of_int !kekka));
    !kekka



(*
 * 文字列s を先頭から n文字分切り取った文字列を求める関数
 * 文字列s は、n文字分以上の長さがあるものとする
 *)
let mbsubstr s n =
    (* sw := false; *) (* char_check の sw  *)
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
     * 与えられた n という桁数を
     * 1バイト文字なら 1 
     * 3バイト文字なら 3　というふうに換算して
     * 新しい n2 というバイト文字数に変換する
     * String.sub s n2
     *)
    let get_new_n x =
        let i = ref 0 in
        let rec loop n2 x =
            let byte_nth = (str_count s.[!i]) in (* utf-8 なら何バイトめか *)(* その文字の画面表示に必要な桁数 *)
            print_endline ("byte_nth= " ^ (string_of_int byte_nth));
            let keta =
                if byte_nth = 0 then 1
                else
                    if byte_nth = 3 then 2 else 0
            in
            if x = 0 then (print_endline ("n2= " ^ (string_of_int n2)); n2)
            else (
                i := !i + 1;
                if byte_nth = 0
                then  loop (n2 + 1) (x - keta)
                else loop (n2 + byte_nth) (x - keta)  (* n2 = 必要なバイト数 *)
            )
        in
        loop 0 x
    in
    String.sub s 0 (get_new_n n)


(*
 * 決められた文字数で文字列を表示する
 * @param: s : string -- 表示する文字列
 *         l : int    -- 表示の幅（文字数）
 * @return: string -- 決められたサイズの文字列
 *                    サイズの小さな文字列は空白で埋めることとする
 *)
let show_length s l =
    let moji = ref "" in
    let s_length = mblength s in
    if s_length < l
    then (moji := s ^ (rep " " (l - s_length)); !moji)
    else
        if s_length = l
        then s
        else mbsubstr s l

