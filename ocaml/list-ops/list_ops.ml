let length (xs : 'a list) : int =
  let rec innerlength acc = function
    | [] -> acc
    | _ :: xs -> innerlength (acc + 1) xs
  in innerlength 0 xs

let reverse (xs : 'a list) : 'a list =
  let rec innerreverse acc = function
    | [] -> acc
    | x :: xs -> innerreverse (x :: acc) xs
  in innerreverse [] xs

let map ~f (xs : 'a list) : 'b list =
  let rec innermap acc = function
    | [] -> reverse acc
    | x :: xs -> innermap (f x :: acc) xs
  in innermap [] xs

let filter ~f (xs : 'a list) : 'a list =
  let rec innerfilter acc = function
    | [] -> reverse acc
    | x :: xs -> if f x then innerfilter (x :: acc) xs
                 else innerfilter acc xs
  in innerfilter [] xs

let rec fold ~init ~f = function
  | [] -> init
  | x :: xs -> fold ~init:(f init x) ~f xs

(*
let append' first second =
  let rec innerappend xs ys =
    match xs with
    | [] -> ys
    | x :: xs' -> innerappend xs' (x :: ys)
  in innerappend (reverse first) second
 *)

let append xs ys =
  fold ~init:ys ~f:(fun acc x -> x :: acc) (reverse xs)

let concat xs =
  fold ~init:[] ~f:(fun acc x -> append x acc) (reverse xs)

(*
let rec fold_right (f : 'a -> 'b -> 'b) (l : 'a list) (acc : 'b) : 'b =
  match l with
    [] -> acc
  | x :: xs -> f x (fold_right f xs acc)

let append xs ys =
  fold_right (fun x y -> x :: y) xs ys

let concat xs =
  fold_right append xs []
 *)
