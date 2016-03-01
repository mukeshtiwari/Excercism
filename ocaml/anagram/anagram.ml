open Core.Std

let alphabeticsort x =
  String.lowercase x
  |> String.to_list
  |> List.sort ~cmp:Char.compare
  |> List.fold_right ~f:(fun a b -> String.of_char a ^ b) ~init:""


let anagrams (x : string) (xs : string list) : string list =
  let y = alphabeticsort x in
  List.filter ~f:(fun t ->
                String.lowercase t <> String.lowercase x
                && alphabeticsort t = y) xs
