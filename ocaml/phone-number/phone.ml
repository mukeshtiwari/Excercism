open Core.Std

let validnumber (xs : string) : string option =
  let len = String.length xs in
  match xs with
  |_ when (len < 10 || len > 11) -> None
  |_ when len = 10 -> Some xs
  |_ when len = 11 ->
    if String.get xs 0 = '1' then Some (String.suffix xs (len - 1))
    else None


let extractnum (xs : string) : string =
  String.filter ~f:(Char.is_digit) xs

let stringsplit (xs : string) (n : int) : string * string =
  (String.slice xs 0 n, String.suffix xs (String.length xs - n))


(** Extract the digits from a valid phone number. *)
let number (xs : string) : string option =
  extractnum xs
  |> validnumber

(** Extract the area code from a valid phone number. *)
let area_code (xs : string) : string option =
  match number xs with
  | Some xs -> Some (String.slice xs 0 3)
  | None -> None

(** Pretty print a valid phone number. *)
let pretty (xs : string) : string option =
  match number xs with
  | None -> None
  | Some xs ->
     let (areacode, rest) = stringsplit xs 3 in
     let (first, last) = stringsplit rest 3 in
     Some ("(" ^ areacode ^ ") " ^ first ^ "-" ^ last)
