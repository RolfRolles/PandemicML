let intersperse_string what strlist =
  List.fold_left (fun acc s -> match acc with | None -> Some(s) | Some(st) -> Some(st^what^s)) None strlist