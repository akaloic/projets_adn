type base = A | C | G | T | WC (* wildcard *)

type dna = base list

(*---------------------------------------------------------------------------*)
(*                               ECHAUFFEMENT                                *)
(*---------------------------------------------------------------------------*)


let string_of_base (b : base) : string =    (*convertit une base b en string*)
  match b with
  | A -> "A"
  | C -> "C"
  | G -> "G"
  | T -> "T"
  | WC -> "."

(* explode a string into a char list *)
let rec explode (str : string) : char list =
  match str with
  | "" -> []    (* si str est vide on retourne []*)
  | _ -> str.[0] :: explode (String.sub str 1 ((String.length str) - 1)) (*sinon on ajoute le 1er caractere de str a la liste et on recommence avec le reste de str *)

(* conversions *)
let base_of_char (c : char) : base =  (*convertit un char c en base*)
  match c with
  | 'A' -> A
  | 'C' -> C
  | 'G' -> G
  | 'T' -> T
  |  _ -> WC

let dna_of_string (s : string) : base list =  (*convertit un string s en list de base*)
  List.map base_of_char (explode s)           (*on applique base_of_char à tout le resultat de explode s*)

let string_of_dna (seq : dna) : string =
  let rec aux seq str = 
    match seq with
    | [] -> str                               (*si seq est vide on retourne str*)
    | b :: bs -> aux bs (str ^ (string_of_base b))    (*sinon on ajoute le string de la base b a str et on recommence avec le reste*)
  in aux seq ""

(*---------------------------------------------------------------------------*)
(*                                   SLICES                                  *)
(*---------------------------------------------------------------------------*)
(*
   Une {\em tranche} de $l = \langle x_1,\dots x_n\rangle$ est une sous-liste
   de $l$ de la forme $\langle x_i, \dots x_{j}$, o\`u $1 \leq i \leq j\leq n$.
 *)


(* if list = pre@suf, return Some suf. otherwise, return None *)


let cut_prefix (slice : 'a list) (list : 'a list) : 'a list option =
  let rec aux sliceAcc listAcc sliceReste listReste =             (*utilisation d'une fonction auxiliaire pour pouvoir utiliser des accumulateurs*)
    match (sliceReste, listReste) with                             (*on compare sliceReste et listReste qui sont les restes de slice et list*)
    | ([],_) -> Some (List.rev sliceAcc)                          (*si sliceReste est vide, on retourne Some (List.rev sliceAcc) car sliceAcc est le prefixe de slice inversé*)
    | (a :: aa, b :: bb) ->
      if a = b then aux (a::sliceAcc) (b::listAcc) aa bb          (*si a=b on recommence avec a::sliceAcc et b::listAcc*)
      else None
    | (_, []) -> None                                               (*si listReste est vide et sliceReste non, on retourne None car cela veut dire que slice n'est pas un prefixe de list*)
  in aux [] [] slice list
(*
  cut_prefix [1; 2; 3] [1; 2; 3; 4] = Some [4]
  cut_prefix [1; 2; 3; 4] [1; 2; 3; 4] = Some []
  cut_prefix [1; 2; 0] [1; 2; 3; 4] = None
 *)


(* return the prefix and the suffix of the first occurrence of a slice,
   or None if this occurrence does not exist.
*)
let first_occ (slice : 'a list) (list : 'a list) : ('a list * 'a list) option =
  let rec aux_first_occ before after =
    let cut = cut_prefix slice after in (* on recupere le prefixe avec cut_prefix *)
    if List.length after < List.length slice then None  (* si la taille de after est plus petite que celle de slice, on retourne None *)
    else
      match cut with 
      | None -> aux_first_occ (before @ [List.hd after]) (List.tl after)  (* si cut est None, on ajoute le premier element de after a before et on recommence *)
      | Some _ -> 
          let rec enleve_n lst n =  (* on enleve n elements de lst *)
            match lst with
            | [] -> []
            | _ :: ll -> if n = 0 then lst else enleve_n ll (n-1)
          in Some (before, enleve_n after (List.length slice))  (* on retourne Some (before, after), after etant after sans les elements de slice *)
  in aux_first_occ [] list

(*
  first_occ [1; 2] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([1; 1], [3; 4; 1; 2])
  first_occ [1; 1] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([], [1; 2; 3; 4; 1; 2])
  first_occ [1; 3] [1; 1; 1; 2; 3; 4; 1; 2] = None
 *)

 let slices_between (start : 'a list) (stop : 'a list) (list : 'a list) : 'a list list =
  let rec find_stop acc lst =                               (*on cherche le premier stop dans lst*)
    match lst with
    | [] -> (List.rev acc, [])                              (*si lst est vide, on retourne (List.rev acc, []) car le stop n'a pas été trouvé*)
    | hd :: tl ->
      if List.length acc >= List.length stop && List.rev (List.tl (List.rev acc)) = stop then       (*si la taille de acc est plus grande ou egale a celle de stop et que le suffixe de acc est egal a stop, on retourne (List.rev (List.tl (List.rev acc)), tl) *)
        (List.rev (List.tl (List.rev acc)), tl)
      else find_stop (hd :: acc) tl                                                                   (*sinon on ajoute hd a acc et on recommence *)
  in
  let rec aux acc lst =                                                          (*on cherche le premier start dans lst*)
    match first_occ start lst with
    | None -> List.rev acc
    | Some (_, after_start) ->                                                (*si on trouve un start, on cherche le premier stop dans after_start*)
      match find_stop [] after_start with
      | between, after_stop ->
        aux (between :: acc) ((List.tl start) @ after_stop)                     (*on ajoute between a acc et on recommence avec le suffixe et le prefixe de start*)
  in
  aux [] list
(*
slices_between [1; 1] [1; 2] [1; 1; 1; 1; 2; 1; 3; 1; 2] = [[1]; []; [2; 1; 3]]
slices_between [1; 2] [4; 1] [1; 1; 2; 3; 2; 1; 4; 1; 2] = [[3; 2 ;1]] 
slices_between [A] [G] [A; C; T; G; G; A; C; T; A; T; G; A; G] = [[C; T]; [C; T; A; T]; [T]; []] 
*)

let cut_genes (dna : dna) : (dna list) =
  let start = dna_of_string "ATG" in    
  let stop = dna_of_string "TAA" in
  slices_between start stop dna

(*---------------------------------------------------------------------------*)
(*                          CONSENSUS SEQUENCES                              *)
(*---------------------------------------------------------------------------*)


type 'a consensus = Full of 'a | Partial of 'a * int | No_consensus

(* return (Full a) if all elements of the list are equal to a,
   (Partial (a, n)) if a is the only element of the list with the
   greatest number of occurrences and this number is equal to n,
   No_consensus otherwise. the list must be non-empty *)
(* let consensus (list : 'a list) : 'a consensus =
  let rec count_occurrences result list =   (* on compte le nombre d'occurences de chaque element de list afin de les trier par ordre decroissant *)
    match list with
    | [] -> result    (* si list est vide, on retourne result qui represente le nombre d'occurences de chaque element de list *)
    | x :: xs ->
        let rec find_and_increment list elem =    (* on cherche elem dans list et on incremente son nombre d'occurences *)
          match list with
          | [] -> (elem, 1) :: list
          | (b, n) :: res ->
              if b = elem then (b, n + 1) :: res  (* si on trouve elem, on incremente son nombre d'occurences *)
              else (b, n) :: find_and_increment res elem    (* sinon on continue de chercher elem *)
        in
        count_occurrences (find_and_increment result x) xs
  in

  let list_decroissant = List.sort (fun (_, count1) (_, count2) -> compare count2 count1) (count_occurrences [] list)   
  (* on trie les elements de list par ordre decroissant de leur nombre d'occurences grace a count_occurrences qui nous retourne une liste de duplets (element, nombre d'occurences) *)
  in

  match list_decroissant with
  | [] -> No_consensus  (* si list_decroissant est vide, on retourne No_consensus *)
  | (base, count) :: [] -> Full base  (* si list_decroissant contient un seul element, on retourne Full base *)
  | (b1, occ1) :: (b2, occ2) :: _ -> if occ1 = occ2 then No_consensus else Partial (b1, occ1) si list_decroissant contient au moins deux elements, on regarde si les deux premiers ont le meme nombre d'occurences, si oui on retourne No_consensus, sinon on retourne Partial (b1, occ1) *)

let consensus (list : 'a list) : 'a consensus =
  let rec count_occurrences acc list =   (* on compte le nombre d'occurences de chaque element de list afin de les trier par ordre decroissant *)
    match list with
    | [] -> acc    (* si list est vide, on retourne acc qui represente le nombre d'occurences de chaque element de list *)
    | x :: xs ->
        let rec find_and_increment acc elem =    (* on cherche elem dans acc et on incremente son nombre d'occurences *)
          match acc with
          | [] -> count_occurrences ((elem, 1) :: acc) xs
          | (b, n) :: res ->
              if b = elem then count_occurrences ((b, n + 1) :: res) xs  (* si on trouve elem, on incremente son nombre d'occurences *)
              else find_and_increment ((b, n) :: res) elem    (* sinon on continue de chercher elem *)
        in
        find_and_increment acc x
  in

  let rec sort_and_consensus acc remaining_list =
    match remaining_list with
    | [] ->  (* si la liste restante est vide *)
        (match acc with
         | [] -> No_consensus  (* si la liste triée est vide, on retourne No_consensus *)
         | [(base, count)] -> Full base  (* si la liste triée contient un seul élément, on retourne Full base *)
         | (b1, occ1):: (b2, occ2) ::_ -> if occ1 = occ2 then No_consensus else Partial (b1, occ1))  (* si la liste triée contient au moins deux éléments, on compare les deux premiers *)
    | _ :: _ ->
        let sorted_list = List.sort (fun (_, count1) (_, count2) -> compare count2 count1) (count_occurrences [] remaining_list) in     (*cela tri par ordre croissant du nompbre d'occurence*)
        sort_and_consensus sorted_list []
  in

  sort_and_consensus [] list
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

let rec consensus_sequence (ll : 'a list list) : 'a consensus list =
  match ll with 
  | [] -> []
  | x :: res -> consensus x :: consensus_sequence res   (* on applique la fonction consensus a chaque element de ll pour obtenir la liste de consensus *)

(*
 consensus_sequence [[1; 1; 1; 1];
                     [1; 1; 1; 2];
                     [1; 1; 2; 2];
                     [1; 2; 2; 2]]
 = [Full 1; Partial (1, 3); No_consensus; Partial (2, 3)]
 *)
