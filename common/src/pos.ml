type pos = {
  line : int;
  column : int;
}

let pos_add pos x = { pos with column = pos.column + x; }
let pos_next pos = pos_add pos 1
let pos_newline pos = { line = pos.line + 1; column = 0; }

let pos_start = {
  line = 1;
  column = 0;
}

let pos_nil = {
  line = -1;
  column = -1;
}

let string_of_pos pos =
  if pos = pos_nil
  then "<nil>"
  else Printf.sprintf "line %d, column %d" pos.line pos.column

type t = {
  first : pos;
  last : pos;
}

let start = { first = pos_start; last = pos_start; }
let nil = { first = pos_nil; last = pos_nil; }

let next t =
  let pos = pos_next t.last
  in
  { first = pos; last = pos; }

let next_newline t =
  let pos = pos_newline t.last
  in
  { first = pos; last = pos; }

let merge t1 t2 =
  if t1 = nil
  then t2
  else { first = t1.first; last = t2.last; }

let to_string t =
  if t.first = t.last
  then string_of_pos t.first
  else (string_of_pos t.first) ^ " - " ^ (string_of_pos t.last)

let to_dashes pos =
  if pos = nil
  then "+++"
  else if pos.first.line = pos.last.line
  then
    let s = String.make (pos.last.column + 1) ' '
    in
    assert (pos.first.column <= pos.last.column);
    for i = pos.first.column + 1 to pos.last.column - 1 do
      s.[i] <- '-';
    done;
    if pos.first.column = pos.last.column
    then s.[pos.first.column] <- '#'
    else begin
      s.[pos.first.column] <- '|';
      s.[pos.last.column] <- '|';
    end;
    s
  else
    let s1 = String.make (pos.first.column + 3) ' ' in
    let s2 = String.make (pos.last.column + 1) '='
    in
    s1.[pos.first.column] <- '|';
    s1.[pos.first.column + 1] <- '=';
    s1.[pos.first.column + 2] <- '>';
    s2.[pos.last.column] <- '|';
    s1 ^ "\n" ^ s2
