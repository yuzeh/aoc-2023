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
  let rec agg handle scores =
    try
      let line = input_line handle in
      let n_matches = String.trim line |> count_matches_in_card in
      agg handle (n_matches :: scores)
    with End_of_file -> scores
  in

  let arr = agg ic [] |> List.rev |> Array.of_list in
  let len = Array.length arr in
  let cps = Array.make len 1 in
  for i = 0 to pred len do
    if arr.(i) > 0 then
      for j = 1 to arr.(i) do
        cps.(i + j) <- cps.(i + j) + cps.(i)
      done
  done;
  close_in ic;
  print_endline (string_of_int (Array.fold_left ( + ) 0 cps))

let () = main Sys.argv.(1)
