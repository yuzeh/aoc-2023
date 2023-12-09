let rec take n lst =
  if n <= 0 then []
  else match lst with [] -> [] | x :: xs -> x :: take (pred n) xs

let rec drop n lst =
  if n <= 0 then lst
  else match lst with [] -> [] | x :: xs -> drop (pred n) xs

let read_until ic stop_str =
  let stop_chars = String.to_seq stop_str |> List.of_seq |> List.rev in
  let nc = List.length stop_chars in
  let rec loop chars =
    try
      let c = input_char ic in
      let next = c :: chars in
      if take nc next = stop_chars then drop nc next else loop next
    with
    | End_of_file -> chars
    | e ->
        close_in_noerr ic;
        raise e
  in
  loop [] |> List.rev |> List.to_seq |> String.of_seq

let read_to_colon ic = ignore (read_until ic ":")
let read_to_colon_nl ic = ignore (read_until ic ":\n")

let parse_ints str =
  String.trim str |> String.split_on_char ' '
  |> List.filter (( != ) String.empty)
  |> List.map int_of_string

let tup3_of_list lst = (List.nth lst 0, List.nth lst 1, List.nth lst 2)
let parse_tup3 str = parse_ints str |> tup3_of_list

let readline ic =
  let str = read_until ic "\n\n" in
  str |> String.split_on_char '\n' |> List.map parse_tup3

let se_of_sl (s, l) = (s, s + l)
let sl_of_se (s, e) = (s, e - s)
let ival_contains (b_s, b_e) (a_s, a_e) = b_s <= a_s && a_e <= b_e

let process_interval (dst_start, src_start, src_len) interval =
  let a_s, a_e = se_of_sl interval in
  let b_s, b_e = se_of_sl (src_start, src_len) in
  let splits =
    if a_e <= b_s then [ (a_s, a_e) ]
    else if b_e <= a_s then [ (a_s, a_e) ]
    else if b_s <= a_s && a_e <= b_e then [ (a_s, a_e) ]
    else if a_s <= b_s && b_s < a_e then [ (a_s, b_s); (b_s, a_e) ]
    else if b_s <= a_s && b_e < a_e then [ (a_s, b_e); (b_e, a_e) ]
    else [ (a_s, b_s); (b_s, b_e); (b_e, a_e) ]
  in

  let contained = List.filter (ival_contains (b_s, b_e)) splits in
  let non_contained =
    List.filter (fun v -> not (ival_contains (b_s, b_e) v)) splits
  in
  let diff = b_s - dst_start in
  let translate (s, e) = (s - diff, e - diff) in
  ( contained |> List.map translate |> List.map sl_of_se,
    non_contained |> List.map sl_of_se )

let apply_map m intervals =
  let rec inner map ivals acc =
    match map with
    | [] -> ivals @ acc
    | tup3 :: rest ->
        let results = List.map (process_interval tup3) ivals in
        let mapped_ival_lists, unmapped_ival_lists = List.split results in
        let mapped_ivals = List.concat mapped_ival_lists in
        let unmapped_ivals = List.concat unmapped_ival_lists in
        inner rest unmapped_ivals (mapped_ivals @ acc)
  in
  inner m intervals []

let main filename =
  let ic = open_in filename in
  read_to_colon ic;
  let seeds = read_until ic "\n\n" |> parse_ints in
  read_to_colon_nl ic;
  let seed_to_soil = readline ic in
  read_to_colon_nl ic;
  let soil_to_fertilizer = readline ic in
  read_to_colon_nl ic;
  let fertilizer_to_water = readline ic in
  read_to_colon_nl ic;
  let water_to_light = readline ic in
  read_to_colon_nl ic;
  let light_to_temperature = readline ic in
  read_to_colon_nl ic;
  let temperature_to_humidity = readline ic in
  read_to_colon_nl ic;
  let humidity_to_location = readline ic in
  read_to_colon_nl ic;
  close_in ic;

  let seed_ivals =
    let xs, ys =
      List.mapi
        (fun i v -> if i mod 2 = 0 then ([ v ], []) else ([], [ v ]))
        seeds
      |> List.split
    in
    List.map2 (fun a b -> (a, b)) (List.concat xs) (List.concat ys)
  in

  let locations =
    seed_ivals |> apply_map seed_to_soil
    |> apply_map soil_to_fertilizer
    |> apply_map fertilizer_to_water
    |> apply_map water_to_light
    |> apply_map light_to_temperature
    |> apply_map temperature_to_humidity
    |> apply_map humidity_to_location
  in

  (* not sure why i need to filter here... *)
  let non_empty_locations =
    List.filter (fun (a, b) -> b > 0 && a > 0) locations
  in
  let starts, lens = List.split non_empty_locations in

  print_endline (string_of_int (List.length starts));
  print_endline (string_of_int (List.fold_left min max_int lens));
  print_endline (string_of_int (List.fold_left min max_int starts))

let () = main Sys.argv.(1)
