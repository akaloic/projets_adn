type base = A | C | G | T | WC (* wildcard *)

type dna = base list

(*---------------------------------------------------------------------------*)
(*                               ECHAUFFEMENT                                *)
(*---------------------------------------------------------------------------*)


let string_of_base (b : base) : string =
  match b with
  | A -> "A"
  | C -> "C"
  | G -> "G"
  | T -> "T"
  | WC -> "."

(* explode a string into a char list *)
let rec explode (str : string) : char list =
  match str with
  | "" -> []
  | _ -> str.[0] :: explode (String.sub str 1 ((String.length str) - 1))

(* conversions *)
let base_of_char (c : char) : base =
  match c with
  | 'A' -> A
  | 'C' -> C
  | 'G' -> G
  | 'T' -> T
  | '.' -> WC
  | _ -> failwith "la base n'est pas valide"

let dna_of_string (s : string) : base list =
  List.map base_of_char (explode s)

let string_of_dna (seq : dna) : string =
  let rec aux seq str = 
    match seq with
    | [] -> str
    | b :: bs -> aux bs (str ^ (string_of_base b))
  in aux seq ""

(*---------------------------------------------------------------------------*)
(*                                   SLICES                                  *)
(*---------------------------------------------------------------------------*)
(*
   Une {\em tranche} de $l = \langle x_1,\dots x_n\rangle$ est une sous-liste
   de $l$ de la forme $\langle x_i, \dots x_{j}$, o\`u $1 \leq i \leq j\leq n$.
 *)


(* if list = pre@suf, return Some suf. otherwise, return None *)
let rec cut_prefix (slice : 'a list) (list : 'a list) : 'a list option =
  match (slice, list) with 
  | ([], []) -> Some []
  | (_, []) -> None
  | ([], res) -> Some res
  | (a :: aa, b :: bb) -> 
    if a=b then cut_prefix aa bb
    else None

(*
  cut_prefix [1; 2; 3] [1; 2; 3; 4] = Some [4]
  cut_prefix [1; 2; 3; 4] [1; 2; 3; 4] = Some []
  cut_prefix [1; 2; 0] [1; 2; 3; 4] = None
 *)


(* return the prefix and the suffix of the first occurrence of a slice,
   or None if this occurrence does not exist.
*)
let first_occ (slice : 'a list) (list : 'a list) : ('a list * 'a list) option =
  let rec aux_first_occ slice before after =
    let cut = cut_prefix slice after in 
    if List.length after < List.length slice then None
    else
      match cut with 
      | None -> aux_first_occ slice (before @ [List.hd after]) (List.tl after)
      | Some _ -> 
          let rec enleve_n lst n = 
            match lst with
            | [] -> []
            | _ :: ll -> if n = 0 then lst else enleve_n ll (n-1)
          in Some (before, enleve_n after (List.length slice))
  in aux_first_occ slice [] list

(*
  first_occ [1; 2] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([1; 1], [3; 4; 1; 2])
  first_occ [1; 1] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([], [1; 2; 3; 4; 1; 2])
  first_occ [1; 3] [1; 1; 1; 2; 3; 4; 1; 2] = None
 *)

let rec slices_between (start : 'a list) (stop : 'a list) (list : 'a list) : 'a list list =
  match first_occ start list with
  | None -> []
  | Some (_, after) -> 
      match first_occ stop after with
      | None -> []
      | Some (between, _) -> between :: slices_between start stop ((List.tl start) @ after)

(*
slices_between [1; 1] [1; 2] [1; 1; 1; 1; 2; 1; 3; 1; 2] = [[1]; []; [2; 1; 3]]
slices_between [1; 2] [4; 1] [1; 1; 2; 3; 2; 1; 4; 1; 2] = [[3; 2 ;1]] 
slices_between [A] [G] [A; C; T; G; G; A; C; T; A; T; G; A; G] = [[C; T]; [C; T; A; T]; [T]; []] 
*)
    
let cut_genes (dna : dna) : (dna list) =
  (* du comprendre par moi même en faisant des recherches que start : "ATG" et stop : "TAA" *)
  let start = dna_of_string "ATG" in
  let stop = dna_of_string "TAA" in
  slices_between start stop dna




(*
  "CCTGGGCATTGAGATCATTGGCACCCTGCA"; 
  "TGTGAC.TGTAGAGCTCTTCCTGACCATGCA"; 
  "CA";
  ".CCAATGGCACAGC.TGGTATC..TTTGCCA"; 
  "GCACAGC.TGGTATC..TTTGCCA";
  "GCTCCTGGTGGAGCTGATAGTCACT.TTCCA"; 
  "CATGGTGGTGGAGTTATTCTTGACTTTCCA";
  "GTGGTGGAGTTATTCTTGACTTTCCA"
   
  "CCTGGGCATTGAGATCATTGGCACCCTGCA";
  "TGTGACTGTAGAGCTCTTCCTGACCATGCA";
  ".CCAATGGCACAGCTGGTATC..TTTGCCA";
  "GCTCCTGGTGGAGCTGATAGTCACTTTCCA";
  "CATGGTGGTGGAGTTATTCTTGACTTTCCA" 
   *)

(*---------------------------------------------------------------------------*)
(*                          CONSENSUS SEQUENCES                              *)
(*---------------------------------------------------------------------------*)


type 'a consensus = Full of 'a | Partial of 'a * int | No_consensus

(* return (Full a) if all elements of the list are equal to a,
   (Partial (a, n)) if a is the only element of the list with the
   greatest number of occurrences and this number is equal to n,
   No_consensus otherwise. the list must be non-empty *)
let consensus (list : 'a list) : 'a consensus =
  failwith "À compléter"

(*
   consensus [1; 1; 1; 1] = Full 1
   consensus [1; 1; 1; 2] = Partial (1, 3)
   consensus [1; 1; 2; 2] = No_consensus
 *)

(* return the consensus sequence of a list of sequence : for each position
   in the elements of ll, compute the consensus  of the set of values at this
   position  in the sequences. the lists must be of same length. if all lists
   are empty, return the empty sequence.
 *)

let consensus_sequence (ll : 'a list list) : 'a consensus list =
  failwith "À compléter"

(*
 consensus_sequence [[1; 1; 1; 1];
                     [1; 1; 1; 2];
                     [1; 1; 2; 2];
                     [1; 2; 2; 2]]
 = [Full 1; Partial (1, 3); No_consensus; Partial (2, 3)]
 *)
