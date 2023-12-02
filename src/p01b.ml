let first arr = Array.get arr 0
let last arr = Array.get arr ((Array.length arr) - 1)

let find_replace find_str rep_str char_seq =
  let rep_seq = String.to_seq rep_str in
  let n_find_chars = String.length find_str in
  let prefix = (Seq.take n_find_chars char_seq) |> String.of_seq in
  if prefix = find_str
    then Seq.append rep_seq (Seq.drop n_find_chars char_seq)
    else char_seq

let replace_digit_words str =
  let rec iterate left_seq right_seq =
    if Seq.is_empty right_seq
    then left_seq
    else
      let transformed_right_seq = right_seq
        |> find_replace "one"    "1"
        |> find_replace "two"    "2"
        |> find_replace "three"  "3"
        |> find_replace "four"   "4"
        |> find_replace "five"   "5"
        |> find_replace "six"    "6"
        |> find_replace "seven"  "7"
        |> find_replace "eight"  "8"
        |> find_replace "nine"   "9" in
    iterate (Seq.append left_seq (Seq.take 1 transformed_right_seq)) (Seq.drop 1 right_seq) in
  iterate (Seq.empty) (String.to_seq str)

let main filename =
  let ic = open_in filename in
  let rec agg handle score_accumulator =
    try
      let line = input_line handle in
      let score = (String.trim line)
        |> replace_digit_words
        |> Seq.filter (fun c -> c >= '0' && c <= '9')
        |> Array.of_seq
        |> fun arr -> (first arr) :: (last arr) :: []
        |> List.to_seq
        |> String.of_seq
        |> int_of_string in
      agg handle (score_accumulator + score)
    with End_of_file ->
      score_accumulator in
  let total = agg ic 0 in
    close_in ic;
  print_int total;
  print_endline ""

let () = main Sys.argv.(1)