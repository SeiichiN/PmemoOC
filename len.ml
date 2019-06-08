(*
 * len.ml -- 文字列の長さを取得する
 *
 * han_length s -- string -> int 
 *                 文字列s 中の半角文字数を求める。
 * zen_length s -- string -> int
 *                 文字列s 中の全角文字数を求める。
 * mblength s   -- string -> int
 *                 文字列s 中の半角文字数 + 全角文字数 * 2 を求める。
 *                 つまり、画面に表示される桁数を求める。
 * show_length s n -- string -> int -> string
 *                 文字列s を n桁 で表示する。
 *                 もし、文字列s が、n桁よりも小さければ、空白で埋める。
 *                 もし、文字列s が、n桁よりも大きければ、その分を削除する。
 *)
let s1 = "google";;
let s2 = "itソリ";;
let s3 = "大阪市立図書館";;

(* あ -- \227 \129 \130
 * い -- \227 \129 \132
 * 　 -- \227 \128 \128 (全角空白)
 *)

(*
 * 半角文字数を求める関数
 *)
let han_length s =
    let rec loop n x sw =
        if n = (String.length s) then x
        else
            let c = s.[n] in
            if (Char.code c) >= 227
            then loop (n+1) x (sw+1)
            else
                match sw with
                    0 -> loop (n+1) (x+1) sw
                    | 3 -> loop (n+1) x 0
                    | _ -> loop (n+1) x (sw+1)
    in
    loop 0 0 0

(*
 * 全角文字数を求める関数
 *)
let zen_length s =
    let rec loop n x sw =
        if n = String.length s then x
        else
            let c = s.[n] in
            if (Char.code c) >= 227
            then loop (n+1) (x+1) (sw+1)
            else
                match sw with
                    0 -> loop (n+1) x sw
                    | 3 -> loop (n+1) (x+1) 0
                    | _ -> loop (n+1) x (sw+1)
    in
    loop 0 0 0


(*
 * 半角文字数 + 全角文字数 * 2 を求める関数
 *)
let mblength s =
    (han_length s) + (zen_length s) * 2


(*
 * 文字列の各文字が1バイト文字か utf-8かをチェックし、
 * その結果をリストで返す
 * @return: int list
 *    (例)
 *    "itコム" -- [0; 0; 1; 2; 3; 1; 2; 3]
 *    0 -- 1バイト文字
 *    1 -- utf-8 の 1バイトめ
 *    2 -- utf-8 の 2バイトめ
 *    3 -- utf-8 の 3バイトめ
 *)
exception Coming_3
let mbcheck s =
    try
        let rec loop n sw x =
            if n = String.length s then List.rev x
            else
                let c = (Char.code s.[n]) in
                if c >= 227 && sw = 0
                then loop (n+1) (sw+1) (1::x) 
                else
                    match sw with
                        0 -> loop (n+1) (sw) (0::x)
                        | 1 -> loop (n+1) (sw+1) (2::x)
                        | 2 -> loop (n+1) 0 (3::x)
                        | _ -> raise Coming_3
        in
        loop 0 0 []
    with Coming_3 -> print_endline "Coming_3"; []


(*
 * 文字列s を先頭v から n文字分切り取った文字列を求める関数
 * 文字列s は、n文字分以上の長さがあるものとする
 * @param:
 *    s : string -- 対象となる文字列
 *    v : int    -- スタート位置
 *                （0 を想定している。他はまだ考えていない）
 *    n : int    -- 表示したい桁数
 * @return: 文字列
 *    画面上では全角文字は、1文字につき、1バイト文字2桁分を必要とする。
 *    String.sub s v n の場合、画面上で全角文字を4桁で表示したいとする
 *    なら、n には、6文字分を与えなければならない。
 *    すなわち、String.sub s 0 5 となる。
 *)
let mbsubstr s v n =
    (*
     * 与えられた n という桁数を
     * 1バイト文字なら 1 
     * 3バイト文字なら 3　というふうに換算して
     * 新しい n2 というバイト文字数に変換する
     * String.sub s n2
     *)
    let get_new_n x =
        let ml = mbcheck s in
        let rec loop n2 x l = 
            if x = 0 then n2
            else
                match l with
                    [] -> n2
                    | a :: rest ->
                        match a with
                            0 -> loop (n2 + 1) (x-1) rest
                            | 3 -> loop (n2 + 1) (x-2) rest
                            | _ -> loop (n2 + 1) x rest
        in
        loop 0 x ml
    in
    String.sub s v (get_new_n n)


(*
 * n 個の連続した s を求める関数
 *)
let rec rep s n =
    if n = 0 then s
    else
        s ^ (rep s (n-1))

(*
 * 決められた文字数で文字列を表示する
 * @param: s : string -- 表示する文字列
 *         l : int    -- 表示の幅（文字数）
 * @return: string -- 決められたサイズの文字列
 *                    サイズの小さな文字列は空白で埋めることとする
 *)
let show_length s l =
    let s_length = mblength s in
    if s_length < l
    then s ^ (rep " " (l - s_length))
    else
        if s_length = l
        then s
        else mbsubstr s 0 l

