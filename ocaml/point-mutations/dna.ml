open Core.Std

type nucleotide = A | C | G | T

let compare (x : nucleotide) (y : nucleotide) : bool =
  match x, y with
  | A, A | C, C | G, G | T, T -> true
  | _, _ -> false

let rec zip (xs : 'a list) (ys : 'b list) : ('a * 'b) list =
  match xs, ys with
  | [], _ -> []
  | _, [] -> []
  | x :: xs', y :: ys' -> (x, y) :: zip xs' ys'


(** Compute the hammning distance between the two lists. *)
let hamming_distance (xs : nucleotide list) (ys : nucleotide list) : int =
  zip xs ys
  |> List.filter ~f:(fun (x, y) -> not (compare x y))
  |> List.length
