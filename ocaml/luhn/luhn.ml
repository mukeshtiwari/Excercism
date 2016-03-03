open Core.Std

let doublenumber n =
  if 2 * n >= 10 then 2 * n - 9
  else 2 * n


let doublelist xs =
  let rec doubleinner ys i =
    match ys with
    | [] -> []
    | y :: ys' ->
       if i mod 2 <> 0 then doublenumber y :: doubleinner ys' (i + 1)
       else y :: doubleinner ys' (i + 1)
  in doubleinner xs 0



let valid (s : string) : bool =
  String.to_list s
  |> List.rev
  |> List.map ~f:(fun v -> Char.to_int v - Char.to_int '0')
  |> doublelist
  |> List.fold ~init:0 ~f:(fun acc a -> acc + a)
  |> (fun v -> v mod 10 = 0)
