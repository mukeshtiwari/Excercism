open Core.Std

type dna = [ `A | `C | `G | `T ]
type rna = [ `A | `C | `G | `U ]

let rnafun : dna -> rna = function
  | `C -> `G
  | `G -> `C
  | `A -> `U
  | `T -> `A

let to_rna  = List.map ~f:rnafun
