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

let merge t1 t2 = { first = t1.first; last = t2.last; }

let string_of_t t =
  if t.first = t.last
  then string_of_pos t.first
  else (string_of_pos t.first) ^ " - " ^ (string_of_pos t.last)
