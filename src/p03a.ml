let read_matrix h =
  let rec agg handle char_list_revlist_acc =
    try
      let line =
        input_line handle |> String.trim |> String.to_seq |> List.of_seq
      in
      agg handle (line :: char_list_revlist_acc)
    with End_of_file -> char_list_revlist_acc
  in
  let char_list_revlist = agg h [] in
  let char_list_list = List.rev char_list_revlist in
  char_list_list |> List.map Array.of_list |> Array.of_list

let is_num c = '0' <= c && c <= '9'

let scan_row row =
  let cur_num_rev = ref [] in
  let retval = ref [] in
  let new_number end_idx =
    let num =
      List.rev !cur_num_rev |> List.to_seq |> String.of_seq |> int_of_string
    in
    let len = List.length !cur_num_rev in
    let new_entry = (num, end_idx - len + 1, end_idx) in
    retval := new_entry :: !retval;
    cur_num_rev := []
  in

  Array.iteri
    (fun idx c ->
      if is_num c then cur_num_rev := c :: !cur_num_rev
      else if !cur_num_rev != [] then
        let end_idx = idx - 1 in
        new_number end_idx)
    row;

  (* In case the last character in the array is a numeral *)
  if !cur_num_rev != [] then new_number (Array.length row - 1);
  List.rev !retval

let scan_matrix mat =
  let num_rows = Array.length mat in
  let num_cols = Array.length mat.(0) in
  let retval = Array.make_matrix num_rows num_cols [] in

  Array.iteri
    (fun idx row ->
      let entries = scan_row row in
      List.iter
        (fun (num, start_idx, end_idx) ->
          for i = pred idx to succ idx do
            for j = pred start_idx to succ end_idx do
              if 0 <= i && i < num_rows && 0 <= j && j < num_cols then
                retval.(i).(j) <- num :: retval.(i).(j)
            done
          done)
        entries)
    mat;
  retval

let find_part_numbers char_mat num_adj_mat =
  Array.mapi
    (fun i row ->
      Array.mapi
        (fun j c ->
          if (not (is_num c)) && c != '.' then num_adj_mat.(i).(j) else [])
        row
      |> Array.to_list |> List.concat)
    char_mat
  |> Array.to_list |> List.concat

let main filename =
  let ic = open_in filename in
  let char_mat = read_matrix ic in
  let num_adj_mat = scan_matrix char_mat in
  let part_numbers = find_part_numbers char_mat num_adj_mat in
  let part_numbers_sum = List.fold_left ( + ) 0 part_numbers in

  close_in ic;
  print_endline (string_of_int part_numbers_sum)

let () = main Sys.argv.(1)
