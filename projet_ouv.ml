(*************************************************************)
(*                                                           *)
(*                                                           *)
(*                       I.Echauffement                      *)  
(*                                                           *)
(*                                                           *)
(*************************************************************)  


(*****************************************)
(*                                       *)
(* Q1 : Struture de données et Primitive *)
(*                                       *)
(*****************************************)

(* liste d'entiers a 64 bit *)
type grand_entier = int64 list;;


(* insert : a' list -> a' -> a' list *)
(* insert une valeur en la fin de liste x *)
let insert x valeur =
  x @ [valeur]
;;

(* remove_head : a' list -> a' list *)
(* enlève le premier element de la liste *)
let remove_head x  = 
  match x with
  |[] -> failwith "Il n'y a aucunn élément dans la liste"
  |t::s -> s
;;

(* get_head : a' list  -> a' *)
(* récupère le premier élèment de la liste si il existe, sinon erreur *)
let get_head x =
  match x with
  |[] -> failwith "Il n'y a aucunn élément dans la liste"
  |t::_ -> t
;;

(* print_grand_entier : int64 list -> unit *)
let print_grand_entier x =
  Printf.printf "Grand entier :\n";
  for i = 0 to List.length x - 1 do
    Printf.printf "Élément %d : %Ld\n" (i + 1) (List.nth x i)
  done
;;

let print_int64 n =
  Printf.printf "%Ld\n" n;
;;


(* print_bits : bool list -> unit *)
(* affiche la liste de bits (bool) *)
let print_bits bits =
  let rec print_elements lst =
    match lst with
    | [] -> ()
    | true :: s ->
      print_string "true ";
      print_elements s
    | false :: s ->
      print_string "false ";
      print_elements s
  in
   print_elements bits;
  print_newline ()  (* Pour passer à la ligne à la fin *)
;;


(*
let () = 
  (* Exemple d'utilisation *)
  let x = [] in
  let x = insert x 0L in
  let x = insert x (Int64.shift_left 1L 36) in
  Printf.printf "Test primitive\n";
  print_newline();
  Printf.printf "Liste initial\n";
  print_grand_entier x;
  print_newline();
  Printf.printf "get_head : %Ld\n" (get_head x);
  print_newline();
  let x = remove_head x in
  Printf.printf "après remove_head \n";
  print_grand_entier x;
  print_newline();
  print_newline();
;;
*)


(*****************************************)
(*                                       *)
(*          Q3 : completion              *)
(*                                       *)
(*****************************************)

(* create_false_list : int -> bool list *)
(* créer une liste de false de taille n *)
let rec create_false_list n =
  if n <= 0 then
    []
  else
    false :: create_false_list (n - 1)
;;


(* completion : bool list -> int -> bool list *)
let rec completion bits n =
  match bits,n with
  |[],_ -> create_false_list n
  |_,0 -> []
  |x::s,_ -> x::(completion s (n-1))
;;

let () = 
  let bits = [false;true;true;false;false;true] in 
  let bits_c1 = completion bits 4 in
  let bits_c2 = completion bits 8 in
  (* test completion *)
  assert (bits_c1 = [false;true;true;false]);
  assert (bits_c2 = [false;true;true;false;false;true;false;false]);

  (*
  print_bits bits_c1;
  print_bits bits_c2;
  *)
;;


(*****************************************)
(*                                       *)
(*        Q2 : decomposition             *)
(*                                       *)
(*****************************************)


(* int64_to_bits : int64 -> bool list *)
(* convertie un nombre int64 en bits false : 0 et true : 1 *)
let rec int64_to_bits x  =
  match x with
  |0L -> []
  |_ -> (Int64.rem x 2L = 1L) :: int64_to_bits (Int64.div x 2L)
;;


(* decomposition : grand_entier -> bool list *)
let rec decomposition x =
  match x with
  |[] -> []
            (* concat une liste de taille 64 bits remplit de false a une decomposition de la suite de la liste*)
  |0L::s -> (create_false_list 64) @ (decomposition s) 
  |t::s -> 
    if s=[] then
      (* convertie x (le dernier élément de la liste ) : int64 en binaire booléene *)
      (int64_to_bits t) @ (decomposition s)
    else
      (* convertie x en binaire de 64bits *)
      (completion (int64_to_bits t) 64) @ (decomposition s)
;;




let () =
  let bits1 = decomposition [38L] in
  let bits2 = decomposition[Int64.shift_left 1L 36] in
  let bits3 = decomposition [0L; Int64.shift_left 1L 36] in
  let bits4 = decomposition[1L;Int64.shift_left 1L 36] in
  let bits5 = decomposition[0L;1L;Int64.shift_left 1L 36] in
  
  (* test sur decomposition*)
  assert (bits1 = [false;true;true;false;false;true]);
  assert(List.length bits2 = 37);
  assert (List.length bits3 = 101);
  assert(List.hd bits4 = true);
  assert(List.length bits4 = 101);
  assert(List.length bits5 = 165);
  assert(List.nth bits5 64 = true);
  assert(List.nth bits5 164 = true);
  
  (*
  print_string "Décomposition de la liste [38] : ";
  print_bits bits1;

  Printf.printf "Longueur de [2^36] : %d" (List.length bits2);
  print_newline();
  Printf.printf "Decomposition [2^36] : ";
  print_bits bits2;

  Printf.printf "Longueur de [0; 2^36] : %d" (List.length bits3);
  print_newline();
  Printf.printf "Decomposition [0; 2^36] : ";
  print_bits bits3;

  Printf.printf "Longueur de [1; 2^36] : %d" (List.length bits4);
  print_newline();
  Printf.printf "Decomposition [1; 2^36] : ";
  print_bits bits4;

  Printf.printf "Longueur de [0; 0; 2^36] : %d" (List.length bits5);
  print_newline();
  Printf.printf "Decomposition [0; 0; 2^36] : ";
  print_bits bits5;
  *)
;;




(*****************************************)
(*                                       *)
(*         Q4 : composition              *)
(*                                       *)
(*****************************************)

(* int64_of_binary_list : bool list -> int64 *)
let int64_of_binary_list bits =
  (* binary_list_to_int64 : bool list -> grand_entier -> puissance *)
  let rec binary_list_to_int64 bits acc puissance =
    match bits with
    | [] -> acc
    | true :: s -> binary_list_to_int64 s (Int64.add acc (Int64.shift_left 1L puissance)) (puissance+1)
    | false :: s -> binary_list_to_int64 s acc (puissance+1)
  in
  binary_list_to_int64 bits 0L 0
;;


(* remove_nb : bool list -> int -> bool list *)
(* supprime n éléments de la liste a partie le la tête  *)
let rec remove_nb bits n =
  match bits,n with
  |[],_ -> failwith "Aucun éléments dans la liste"
  |_,0 -> bits
  |_::s,_ -> remove_nb s (n-1)
;;

(* composition : bool list -> grand_entier *)
let rec composition bits =
  match bits with
  | [] -> [0L]  (* Si la liste est vide, renvoie [0L] *)
  | _ -> 
    (* Si la liste dépasse 64 bits *)
    if List.length bits > 64 then
      (* prend les 64 premier bits de la liste *)
      let head = completion bits 64 in
      (* supprime les 64 premier bits de la liste *)
      let suite = remove_nb bits 64 in
      (* Si les 64 premiers bits sont tous faux, renvoyer [0L] suivi de la composition du reste de la liste *)
      if List.for_all (fun x -> x = false) head then
        0L :: (composition suite)
      else
        (* Sinon, convertir les bits en int64 et les ajouter à la composition du reste de la liste *)
        (int64_of_binary_list head) :: (composition suite)
    else
      [int64_of_binary_list bits]
;;

let () =
  let x1 = composition [false;true;true;false;false;true] in
  let x2 = composition [] in
  let bits = create_false_list 100 in
  let bits = insert bits true in
  let x3 = composition bits in
  (* test composition *)
  assert ((get_head x1) = 38L);
  assert ((get_head x2) = 0L);
  assert (( List.nth x3 1) = (Int64.shift_left 1L 36)); 

  (*
  print_grand_entier x1;
  print_grand_entier x2;
  print_grand_entier x3;
  *)
;;

(*****************************************)
(*                                       *)
(*             Q5 : table                *)
(*                                       *)
(*****************************************)

(* table : grand_entier -> int -> bool list *)
let table x n =
  let bits = decomposition x in
  completion bits n
;;

(*****************************************)
(*                                       *)
(*            Q6 : GenAlea               *)
(*                                       *)
(*****************************************)

(* genere_aleatoire : int -> grand_entier *)
let rec genere_aleatoire n =
  Random.self_init ();
  if n > 64 then begin
    let head = Random.int64 Int64.max_int in
    head :: (genere_aleatoire (n - 64))
  end else
    let x = Random.int64 (Int64.sub (Int64.shift_left 1L n) 1L) in
    [x]
;;



let () = 
  let x1 = (genere_aleatoire 100) in
  let bits1 = decomposition x1 in
  let x2 = (genere_aleatoire 100) in
  let bits2 = decomposition x1 in
  let x3 = (genere_aleatoire 100) in
  let bits3 = decomposition x1 in

  Printf.printf "grand entier générer aléatoire sur 100bits\n";
  print_grand_entier x1;
  print_bits bits1;
  print_grand_entier x2;
  print_bits bits2;
  print_grand_entier x3;
  print_bits bits3;
  
;;










(*************************************************************)
(*                                                           *)
(*                                                           *)
(*                  II.Arbre de décision                     *)  
(*                                                           *)
(*                                                           *)
(*************************************************************)


(*****************************************)
(*                                       *)
(*    Q7 : Structure de données          *)
(*                                       *)
(*****************************************)

type arbre_decision =
  | Feuille of bool
  | Noeud of int * arbre_decision * arbre_decision
;;



(*****************************************)
(*                                       *)
(*          Q8 : cons_arbre              *)
(*                                       *)
(*****************************************)



(*****************************************)
(*                                       *)
(*         Q9 : liste_feuilles           *)
(*                                       *)
(*****************************************)











(*************************************************************)
(*                                                           *)
(*                                                           *)
(*      III.Compression de l'arbre de décision et ZDD        *)
(*                                                           *)
(*                                                           *)
(*************************************************************)





(*****************************************)
(*                                       *)
(*        Q10 : ListeDejaVus             *)
(*                                       *)
(*****************************************)

(*****************************************)
(*                                       *)
(*      Q11 : CompressionParListe        *)
(*                                       *)
(*****************************************)

(*****************************************)
(*                                       *)
(*              Q12 : dot                *)
(*                                       *)
(*****************************************)



















(*************************************************************)
(*                                                           *)
(*                                                           *)
(*           IV.Compression avec historique stoké            *)
(*              dans une structure arborescente              *)  
(*                                                           *)
(*                                                           *)
(*************************************************************)





(*****************************************)
(*                                       *)
(*        Q15 : ArbreDejaVus             *)
(*                                       *)
(*****************************************)

(*****************************************)
(*                                       *)
(*                Q16                    *)
(*                                       *)
(*****************************************)

(*****************************************)
(*                                       *)
(*       Q17 : CompressionParArbre       *)
(*                                       *)
(*****************************************)

(*****************************************)
(*                                       *)
(*                Q18                    *)
(*                                       *)
(*****************************************)