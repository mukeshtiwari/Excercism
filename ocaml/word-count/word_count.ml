open Core.Std

let findandreplace mp key =
  match String.Map.find mp key with
  | None -> String.Map.add mp key 1
  | Some v -> String.Map.add mp key (v + 1)

let word_count (xs : string) : int String.Map.t =
  String.lowercase xs
  |> String.map
       ~f:(fun x -> if Char.is_alphanum x || Char.is_whitespace x then x
                    else ' ')
  |> String.split ~on:' '
  |> List.filter ~f:(fun t -> not (String.is_empty t))
  |> List.fold_left ~init:String.Map.empty
                    ~f:findandreplace
