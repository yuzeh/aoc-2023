module IntSet = Set.Make (Int)

let count_matches_in_card str =
  let [ str_winners; str_held ] =
    String.split_on_char ':' str
    |> List.tl |> List.hd |> String.split_on_char '|'
  in
  let parse numbers =
    String.split_on_char ' ' numbers
    |> List.filter (fun x -> String.length x > 0)
    |> List.map int_of_string |> IntSet.of_list
  in
  let winners = parse str_winners in
  let held = parse str_held in
  IntSet.inter winners held |> IntSet.cardinal
[@@warning "-8"]

let main filename =
  let ic = open_in filename in
  let rec agg handle score_accumulator =
    try
      let line = input_line handle in
      let n_matches = String.trim line |> count_matches_in_card in
      let score = if n_matches = 0 then 0 else 1 lsl pred n_matches in
      agg handle (score_accumulator + score)
    with End_of_file -> score_accumulator
  in

  let total = agg ic 0 in
  close_in ic;
  print_endline (string_of_int total)

let () = main Sys.argv.(1)
