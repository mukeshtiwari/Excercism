open Core.Std

let verse = function
  | 0 -> "No more bottles of beer on the wall, no more bottles of beer.\nGo to "
         ^ "the store and buy some more, 99 bottles of beer on the wall.\n"
  | 1 -> "1 bottle of beer on the wall, 1 bottle of beer.\nTake it down and pass"
          ^ " it around, no more bottles of beer on the wall.\n"
  | 2 -> "2 bottles of beer on the wall, 2 bottles of beer.\nTake one down and "
         ^ "pass it around, 1 bottle of beer on the wall.\n"
  | n -> string_of_int n ^ " bottles of beer on the wall, " ^
         string_of_int n ^ " bottles of beer.\nTake one down and pass it around, "
         ^  string_of_int (n - 1) ^ " bottles of beer on the wall.\n"


let sing ~from  ~until : string =
  List.map ~f:verse (List.range ~stride:(-1) ~stop:`inclusive from until)
  |> List.fold_right ~f:(fun x y -> x ^ "\n" ^ y) ~init:""
