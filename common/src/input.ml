type 'a t = (Pos.t * 'a) list

let is_empty input = input = []

let from_stdin () =
  let rec all_input acc pos =
    try
      match input_char stdin with
      | '\n' -> all_input ((pos, '\n') :: acc) (Pos.next_newline pos)
      | c -> all_input ((pos, c) :: acc) (Pos.next pos)
    with End_of_file -> List.rev ((pos, '\004') :: acc)
  in
  all_input [] Pos.start

let pos = function
  | [] -> raise @@ Invalid_argument "Empty input"
  | (pos, _) :: _ -> pos
