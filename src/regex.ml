open Regex_base

let rec repeat n l =
  if n = 0 then 
    []
  else 
    l @ repeat (n-1) l          (* on concatene la liste avec elle-même n fois*)

let rec expr_repeat n e =
  if n = 0 then 
    Eps
  else 
    Concat(e, expr_repeat (n-1) e)   (*on concatene l'expression avec elle meme n fois*)

    (*Pour is_empty, null et is_finite on verifie que e est respectivement vide, null ou fini avec les conditions sur eps,Base et Joker*)
let rec is_empty e =
  match e with 
  | Eps -> true
  | Base x -> false
  | Joker -> false
  | Concat (g, d) -> is_empty g && is_empty d
  | Alt (g, d) -> is_empty g && is_empty d
  | Star x -> is_empty x  (* car Eps puissance n reste Eps *)
  
let rec null e =
  match e with 
  | Eps -> true
  | Base x -> false
  | Joker -> false
  | Concat (g, d) -> null g && null d   
  | Alt (g, d) -> null g || null d
  | Star _ -> true

let rec is_finite e =
  match e with 
  | Eps -> true
  | Base x -> true
  | Joker -> true
  | Concat (g, d) -> is_finite g && is_finite d
  | Alt (g, d) -> is_finite g && is_finite d  
  | Star x -> is_empty x  (* car Eps puissance n reste Eps *)

let rec product l1 l2 =
  match l1 with 
  | [] -> []
  | x :: ll ->
      let rec aux list =
        match list with
        | [] -> product ll l2   (* on passe au prochain élément de l1 *)
        | y :: res -> [x @ y] @ aux res  (* on forme une liste avec le premier élément de l1 et chaque élément de l2 puis on concatène avec le reste de l2 *)
      in aux l2 

let enumerate alphabet e =
  if not (is_finite e) then None    (* si l'expression n'est pas finie, on ne peut pas l'énumérer donc on renvoie None *)
  else
    let rec eval exp =    (* fonction récursive qui renvoie la liste des mots de l'expression *)
      match exp with 
      | Eps -> [[]]   (* si l'expression est vide, on renvoie une liste contenant une liste vide *)
      | Base x -> [[x]]   (* si l'expression est un caractère, on renvoie une liste de liste contenant le caractère *)
      | Joker -> List.map (fun x -> [x]) alphabet   (* si l'expression est un Joker, on renvoie une liste de liste contenant chaque caractère de l'alphabet *)
      | Concat (g, d) -> product (eval g) (eval d)      (* fait le produit (product) sur les listes de mots de g et de d, afin de concaténer chaque mot de g avec chaque mot de d *)
      | Alt (g, d) -> eval g @ eval d   (* on concatène les listes de mots de g et de d *)
      | Star x -> [] (* car le seul cas où Star apparait alors que is_finite e = true c'est lorsque x = Eps *)
    in Some (eval e)

let alphabet_expr e =
  match enumerate [] e with   (* on énumère l'expression avec un alphabet vide *)
  | Some langage ->
    let alpha = List.flatten langage in     (* on transforme la liste de liste en une liste *)
    List.sort_uniq compare alpha    (* on trie et on supprime les doublons *)
  | None -> 
    let rec aux exp =
      match exp with 
      | Eps -> []   (* si l'expression est vide, on renvoie une liste vide *)
      | Base x -> [x]   (* si l'expression est un caractère, on renvoie une liste contenant le caractère *)
      | Joker -> []   (* si l'expression est un Joker, on renvoie une liste vide, parce que l'alphabet est vide *)
      | Concat (g, d) -> aux g @ aux d   (* on concatène les listes de mots de g et de d *)
      | Alt (g, d) -> aux g @ aux d   (* on concatène les listes de mots de g et de d *)
      | Star x -> aux x     
    in List.sort_uniq compare (aux e)   (* on trie et on supprime les doublons *)

type answer =
  Infinite | Accept | Reject

let accept_partial e w =
  let alphabet = alphabet_expr e in     (* on récupère l'alphabet de l'expression *)
  match (alphabet, enumerate alphabet e) with
  | (alpha, Some language_e) ->   (* si l'énumération est possible *)
    let resultat =
      if not (is_finite e) then Infinite          (*si is_finite renvoie false alors e est un langage infini donc on renvoie infinite*)
      else if List.for_all (fun c -> List.mem c alpha) w && List.mem w language_e then Accept   (*Si les 2 conditions sont respectées alors on Accepte*)
      else Reject   (* sinon on renvoie Reject *)
    in
    resultat
  | _ -> Infinite     (* si l'énumération n'est pas possible, on renvoie Infinite *)

  (*Pour la ligne 91 : on verifie que tout les caractere de w appartiennent à l'alphabet alpha. De l'autre cote, on verifie que w appartient bien au langage*)