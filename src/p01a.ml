let first arr = Array.get arr 0
let last arr = Array.get arr ((Array.length arr) - 1)

let main filename =
  let ic = open_in filename in
  let rec agg handle score_accumulator =
    try
      let line = input_line handle in
      let score = (String.trim line)
        |> String.to_seq
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