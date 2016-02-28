open Core.Std

let leap_year : int -> bool = function
  | t when t mod 4 <> 0 -> false
  | t when t mod 100 <> 0 -> true
  | t when t mod 400 <> 0 -> false
  | _ -> true
