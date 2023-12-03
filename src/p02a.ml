let max_red = 12
let max_green = 13
let max_blue = 14

let is_hand_possible bag =
  max_red >= bag.(0) && max_green >= bag.(1) && max_blue >= bag.(2)

let parse_hand str =
  let elements = String.split_on_char ',' str in
  let num_red = ref 0 in
  let num_green = ref 0 in
  let num_blue = ref 0 in
  let update_counts hand_item =
    let str_pair = String.split_on_char ' ' (String.trim hand_item) in
    match str_pair with
    | [ count; "red" ] -> num_red := int_of_string count
    | [ count; "green" ] -> num_green := int_of_string count
    | [ count; "blue" ] -> num_blue := int_of_string count
    | _ -> raise (Failure "Ooops")
  in
  let _ = List.map update_counts elements in
  [| !num_red; !num_green; !num_blue |]

let main filename =
  let ic = open_in filename in
  let rec agg handle score_accumulator =
    try
      let line = input_line handle in
      let line_pair = String.split_on_char ':' (String.trim line) in
      let game_number =
        let game_desc = String.trim (List.nth line_pair 0) in
        let line_len = String.length game_desc in
        int_of_string (String.sub game_desc 5 (line_len - 5))
      in
      let is_game_possible =
        List.nth line_pair 1 |> String.trim |> String.split_on_char ';'
        |> List.map parse_hand |> List.map is_hand_possible
        |> List.for_all (fun x -> x)
      in
      if is_game_possible then agg handle (score_accumulator + game_number)
      else agg handle score_accumulator
    with End_of_file -> score_accumulator
  in
  let total = agg ic 0 in
  close_in ic;
  print_endline (string_of_int total)

let () = main Sys.argv.(1)
