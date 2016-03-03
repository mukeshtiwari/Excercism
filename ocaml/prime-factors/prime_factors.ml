open Core.Std

let factors_of (n : int64) : int64 list =
  let rec innerfactor (n : int64) (i: int64) acc =
    let open Int64 in
    match n with
    |_ when n = 1L -> List.rev acc
    |_ when n <= i -> List.rev (i :: acc)
    |_ when n % i = 0L -> innerfactor (n / i) i (i :: acc)
    |_ -> innerfactor n (i + 1L) acc
  in innerfactor n 2L []
