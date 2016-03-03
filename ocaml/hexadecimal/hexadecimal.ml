open Core.Std
let mp = [('0', 0); ('1', 1); ('2', 2);
          ('3', 3); ('4', 4); ('5', 5); ('6', 6);
          ('7', 7); ('8', 8); ('9', 9); ('a', 10);
          ('b', 11); ('c', 12); ('d', 13); ('e', 14);
          ('f', 15)]

let to_int (s : string) : int =
  let rec innerfun xs multi acc =
    match xs with
    | [] -> acc
    | x :: xs' ->
       match List.find mp (fun (y, _) -> y = x) with
       | Some (_, v) -> innerfun xs' (multi * 16) (v * multi + acc)
       | None -> innerfun [] 0 0
  in innerfun (List.rev (String.to_list s)) 1 0
