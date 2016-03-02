(** Grade-school exercise *)
open Core.Std

type school = string list Int.Map.t


(** Create a new empty school *)
let create : unit -> school =
  fun () -> Int.Map.empty


(** Add a student to a school *)
let add (xs : string)  (n : int)  (mp : school) : school =
  let np = Int.Map.of_alist_exn [(n, [xs])] in
  Int.Map.merge mp np
                ~f:(fun ~key:_ -> function
                     | `Both (u, v) -> Some (u @ v)
                     | `Left u -> Some u
                     | `Right v -> Some v)

(** Get all the students from a grade *)
let grade (n : int)  (mp : school)  :  string list =
  match Int.Map.find mp n with
  | Some v -> v
  | None -> []

(** Sort the list of students in a grade, if necessary *)
let sort (mp : school) :  school =
  Int.Map.to_alist mp
  |> List.map ~f:(fun (a, b) ->
                (a, List.sort ~cmp:(fun x y -> String.compare x y) b))
  |> Int.Map.of_alist_exn



(** Return the list of grades and students as a map. This one
really got my nurves :) *)
let to_map (mp : school) : string list Int.Map.t = mp
