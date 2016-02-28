open Core.Std

type response =
  | Sure
  | Woah
  | Fine
  | What

let responseoption : response -> string =
  function
  | Sure -> "Sure."
  | Woah -> "Woah, chill out!"
  | Fine -> "Fine. Be that way!"
  | What -> "Whatever."

(* look for nested if else alternative in Ocaml *)

let responseList (s : char list) : response =
  if List.for_all ~f:Char.is_whitespace s then Fine
  else if (List.exists ~f:Char.is_alpha s) &&
          (List.filter ~f:Char.is_alpha s |>
             List.for_all ~f:Char.is_uppercase) then Woah
  else if List.last_exn s ='?' then Sure
  else What



let response_for : string -> string =
  fun s -> String.to_list s |> responseList |> responseoption
