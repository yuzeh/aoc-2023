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

let tup3_of_int lst = (List.nth lst 0, List.nth lst 1, List.nth lst 2)
let parse_tup3 str = parse_ints str |> tup3_of_int

let readline ic =
  let str = read_until ic "\n\n" in
  str |> String.split_on_char '\n' |> List.map parse_tup3

let rec apply_map map item =
  match map with
  | [] -> item
  | (dst_start, src_start, src_len) :: rest ->
      if src_start <= item && item < src_start + src_len then
        item - src_start + dst_start
      else apply_map rest item

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

  let locations =
    List.map
      (fun x ->
        x |> apply_map seed_to_soil
        |> apply_map soil_to_fertilizer
        |> apply_map fertilizer_to_water
        |> apply_map water_to_light
        |> apply_map light_to_temperature
        |> apply_map temperature_to_humidity
        |> apply_map humidity_to_location)
      seeds
  in

  print_endline (string_of_int (List.fold_left min max_int locations))

let () = main Sys.argv.(1)
