(*****************************************)
(*                                       *)
(*                                       *)
(*            I.Echauffement             *)  
(*                                       *)
(*                                       *)
(*****************************************)  


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

(* remove : a' list -> a' list *)
(* enlève le premier element de la liste *)
let remove x  = 
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


let print_grand_entier x =
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

(* Exemple d'utilisation *)
(*
let () = 
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
  let x = remove x in
  Printf.printf "après remove \n";
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
  let bits = decomposition [38L] in
  let bits2 = decomposition [0L; Int64.shift_left 1L 36] in
  let bits3 = decomposition[1L;Int64.shift_left 1L 36] in
  (* test sur decomposition*)
  assert (bits = [false;true;true;false;false;true]);
  assert (List.length bits2 = 101);
  assert(List.hd bits3 = true);
  assert(List.length bits3 = 101);
  
  (*
  print_string "Décomposition de la liste [38] : ";
  print_bits bits;
  
  let len = List.length bits2 in
  Printf.printf "longueur de [0; 2^36] : %d" len;
  print_newline();
  Printf.printf "longueur du decomposition [0; 2^36] : ";
  print_bits bits2;
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


(* remove : bool list -> int -> bool list *)
(* supprime n éléments de la liste a partie le la tête  *)
let rec remove bits n =
  match bits,n with
  |[],_ -> failwith "Aucun éléments dans la liste"
  |_,0 -> bits
  |_::s,_ -> remove s (n-1)
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
      let suite = remove bits 64 in
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
  print_string "x1 : ";
  print_grand_entier x1;
  print_string "x2 : ";
  print_grand_entier x2;
  print_string "x3 : ";
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















(*****************************************)
(*                                       *)
(*                                       *)
(*        II.Arbre de décision           *)  
(*                                       *)
(*                                       *)
(*****************************************) 


(*****************************************)
(*                                       *)
(*    Q7 : Structure de données          *)
(*                                       *)
(*****************************************)



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











(*****************************************)
(*                                       *)
(*                                       *)
(*      III.Compression de l'arbre       *)
(*          de décision et ZDD           *)  
(*                                       *)
(*                                       *)
(*****************************************) 





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



















(*****************************************)
(*                                       *)
(*                                       *)
(* IV.Compression avec historique stoké  *)
(*   dans une structure arborescente     *)  
(*                                       *)
(*                                       *)
(*****************************************) 





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