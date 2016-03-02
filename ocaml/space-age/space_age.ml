open Core.Std

type planet = Mercury | Venus | Earth | Mars
            | Jupiter | Saturn | Neptune | Uranus

(** Convert seconds to years on the specified planet *)
let age_on (p : planet)  (n : int) : float =
  let earthage =  31557600.0 in
  match p with
  | Mercury -> (Float.of_int n) /. (0.2408467 *. earthage)
  | Venus -> (Float.of_int n) /. (0.61519726 *. earthage)
  | Earth -> (Float.of_int n) /. earthage
  | Mars -> (Float.of_int n) /. (1.8808158 *. earthage)
  | Jupiter -> (Float.of_int n) /. (11.862615 *. earthage)
  | Saturn -> (Float.of_int n) /. (29.447498 *. earthage)
  | Neptune -> (Float.of_int n) /. (164.79132 *. earthage)
  | Uranus -> (Float.of_int n) /. (84.016846 *. earthage)
