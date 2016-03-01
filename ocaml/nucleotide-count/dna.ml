open Core.Std

let count (xs : string) (x : char) : int =
  String.filter ~f:(fun y ->  y = x) xs
  |> String.length

let nucleotide_counts (xs : string) : int Char.Map.t =
  [('A', count xs 'A'); ('C', count xs 'C'); ('G', count xs 'G'); ('T', count xs 'T')]
  |> List.filter ~f:(fun (_, y) -> y <> 0)
  |>  Char.Map.of_alist_exn
