let string_of_list printer seperator list =
  let (_, str) =
    List.fold_left
      (fun (sep, str) item -> (seperator, str ^ sep ^ (printer item)))
      ("", "")
      list
  in
  str
