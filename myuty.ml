(*
 * myuty.ml
 * できるだけ汎用的な関数を集める
 *)

(*
 * assoc
 * @param: tupple list -- (key, value)という形の組のリスト
 *         s           -- 探したいkey
 * @return: v          -- 探したい key が見つかれば、その value を返す
 *)
let rec assoc s = function
    [] -> ""
    | (a, v) :: rest ->
            if a = s 
            then v
            else assoc s rest

(*
 * choice -- 選択肢の中から一つを選ぶ関数 (0 ... max)
 * @param: int max -- メニューの最大番号
 *         func f  -- 選択肢を表示し、その中から1つを選択させる関数
 *                    この関数は数字を文字列で返すようにする
 * @return: !num : int -- 数字
 *)
let choice max f =
    let num = ref (max + 1) in
    while ( !num > max || !num < 0 ) do
        try
            num := int_of_string (f ());  (* 文字列を数値に変換 *)
            if ( !num > max || !num < 0 )
            then print_endline ("0 〜 " ^ (string_of_int max) ^ " の数字です。")
            else ()
        with Failure a ->
            print_endline "番号ではありませんでした";
            num := (max + 1)
    done;
    !num

(*
 * ユーザーからの入力を受け取る処理
 * @param: s : string -- 画面に表示する文字列
 *)
let get_user_input s =
  print_string s;
  flush stdout;
  let str = input_line stdin in
  str


(*
 * メッセージを表示して、番号を得る関数
 * @param: s : string -- 画面に表示する文字列
 * @return: bango : string -- 番号
 *)
let disp_mes_get_bango s =
    print_newline ();
    print_string s;
    flush stdout;
    let bango = input_line stdin in
    bango

