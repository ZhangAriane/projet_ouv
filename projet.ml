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

(* liste d'entiers a 64 bits *)
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
  |[] -> failwith "Il n'y a aucun élément dans la liste"
  |t::s -> s
;;

(* get_head : a' list  -> a' *)
(* récupère le premier élèment de la liste si il existe, sinon erreur *)
let get_head x =
  match x with
  |[] -> failwith "Il n'y a aucun élément dans la liste"
  |t::_ -> t
;;

(* print_grand_entier : int64 list -> unit *)
let print_grand_entier x =
  Printf.printf "Grand entier : [";
  for i = 0 to List.length x - 1 do
    Printf.printf "%Ld" (List.nth x i);
    if i != (List.length x - 1) then 
      Printf.printf ";";
  done;
  Printf.printf "]\n";
;;



(* print_bits : bool list -> unit *)
(* affiche la liste de bits (bool) *)
let print_bits bits =
  let rec print_elements lst =
    match lst with
    | [] -> ()
    | true :: s ->
      if s = [] then
        print_string "true"
      else 
        print_string "true; ";
      print_elements s
    | false :: s ->
      if s = [] then
        print_string "false"
      else
        print_string "false; ";
      print_elements s
  in
  print_string "Bits : [";
  print_elements bits; 
  print_string "]\n";
;;




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
(* si la liste bits contient au moins n élement alors retourne *)
(* les n 1er éléments sinon complète les bits manquantes par des false *)
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
(*  convertie un grand entier en bits *)
let rec decomposition x =
  match x with
  |[] -> []
            (* concat une liste de taille 64 bits remplit de false a une decomposition de la suite de la liste*)
  |0L::s -> (create_false_list 64) @ (decomposition s) 
  |t::s -> 
    if s = [] then
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
(* converti bits en int64 *)
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
(* supprime n 1er éléments de la liste s'il exixte*)
let rec remove_nb bits n =
  match bits,n with
  |[],_ -> failwith "Aucun élément dans la liste"
  |_,0 -> bits
  |_::s,_ -> remove_nb s (n-1)
;;

(* composition : bool list -> grand_entier *)
(* convertie bits en grand entier *)
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

(* is_power_of_two : int -> bool *)
(* vérifier si n est une puissance de 2 *)
let is_power_of_two n =
  n > 0 && (n land (n - 1) = 0)
;;

(* table : grand_entier -> int -> bool list *)
(* convertie un grand entier en table de vérité, c-à-d en bits d'une taille n de puissance de 2 *)
let table x n =
  if not (is_power_of_two n) then failwith (Printf.sprintf "%d n'est pas une puissance de 2" n)
  else
    let bits = decomposition x in
    completion bits n
;;

(*****************************************)
(*                                       *)
(*            Q6 : GenAlea               *)
(*                                       *)
(*****************************************)
(* random_int64 : unit -> int64 *)
(* génére un int64 aléatoirement *)
let random_int64 () =
  let high = Int64.of_int (Random.bits ()) in
  let mid = Int64.of_int (Random.bits ()) in
  let low = Int64.of_int (Random.bits ()) in
  Int64.(logor (shift_left high 42) (logor (shift_left mid 21) low))
;;

(* genere_aleatoire : int -> grand_entier *)
(* génère aléatoirement un grand entier de au moins n bits *)
let rec genere_aleatoire n =
  Random.self_init ();
  if n > 64 then begin
    let head = random_int64 () in
    head :: (genere_aleatoire (n - 64))
  end else
    let x = Random.int64 (Int64.shift_left 1L n) in
    [x]
;;



let () = 
  for i=0 to 1000 do
    let x = (genere_aleatoire 100) in
    let bits = decomposition x in
    assert (List.length bits <= 100); 
    (*
    Printf.printf "grand entier générer aléatoire sur 100bits\n";
    Printf.printf "Bits : %d\n" (List.length bits);
    print_grand_entier x;
    print_newline();
    *)
  done;
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

(* set_fils_gauche_arbre : arbre_decision -> arbre_decision -> arbre_decision *)
(* si l'arbre possède un fils gauche alors renvoie l'arbre modifier sinon sinon l'arbre initial*)
let set_fils_gauche_arbre arbre nouveau_fils_gauche = 
  match arbre with
  | Noeud(n, gauche, droit) ->
    (*retourne l'arbre modifier*)
      Noeud(n,nouveau_fils_gauche,droit)
  | _ -> arbre
;;

(* set_fils_droit_arbre : arbre_decision -> arbre_decision -> arbre_decision *)
(* si l'arbre possède un fils droit alors renvoie l'arbre modifier sinon l'arbre initial *)
let set_fils_droit_arbre arbre nouveau_fils_droit = 
  match arbre with
  | Noeud(n, gauche, droit) ->
    (*retourne l'arbre modifier*)
     Noeud(n,gauche,nouveau_fils_droit)
  | _ -> arbre
;;

(* set_fils_arbre : arbre_decision -> arbre_decision -> rbre_decision -> arbre_decision *)
let set_fils_arbre arbre nouveau_fils_gauche nouveau_fils_droit = 
  match arbre with
  | Noeud(n, _, _) ->
    (*retourne l'arbre modifier*)
      Noeud(n,nouveau_fils_gauche,nouveau_fils_droit)
  | _ -> arbre
;;


(* cons_arbre : bool list -> arbre *)
(* construit d'un arbre a partir d'une table de vérité *)
let cons_arbre table = 
  (* aux : bool list -> int -> arbre *)
  let rec aux table profondeur = 
    match table with
    | [] -> failwith "La table de vérité est vide.";
    | [x] ->  Feuille x  (* Si la table contient un seul élément, on crée une feuille *) 
    | _ -> 
      (* construction en préfixe : créer d'abord la racine puis enfant gauche et enfant droit *)
      let noeud = Noeud(profondeur, (Feuille false),(Feuille false)) in
      let mid = (List.length table) / 2 in
      let gauche = (aux (completion table mid) (profondeur + 1)) in  (* Récupérer la première moitié *)
      let droit = (aux (remove_nb table mid) (profondeur + 1)) in  (* Récupérer la seconde moitié *)
      let noeud = set_fils_gauche_arbre noeud  gauche in
      let noeud = set_fils_droit_arbre noeud  droit in
      noeud
  in
  let arbre = aux table 1 in
  arbre
;;




(* afficher l'arbre de décision *)
let rec print_arbre_decision = function 
  | Feuille x -> Printf.printf "%b " x
  | Noeud (v, g, d) -> 
      Printf.printf "Noeud(%d, " v;
      print_arbre_decision g;
      Printf.printf ", ";
      print_arbre_decision d;
      Printf.printf ")"
;;


let () = 
  let t1 = table [25899L] 16 in
  let arbre1 = cons_arbre t1  in
  
  (*
  print_bits t1;
  print_arbre_decision arbre1;
  *)

  let arbre2 = Noeud(1,
                     (Noeud(2,
                            (Noeud(3,
                                   (Noeud(4,  (Feuille true),  (Feuille true))),
                                   (Noeud(4,  (Feuille false), (Feuille true))))),
                            (Noeud(3,
                                   (Noeud(4,  (Feuille false),  (Feuille true))),
                                   (Noeud(4,  (Feuille false),  (Feuille false))))))),
                    
                     (Noeud(2,
                            (Noeud(3,
                                   (Noeud(4, (Feuille true), (Feuille false))),
                                   (Noeud(4, (Feuille true), (Feuille false))))),
                            (Noeud(3,
                                   (Noeud(4,( Feuille false), (Feuille true))),
                                   (Noeud(4, (Feuille true), (Feuille false))))))))
  in
  assert(arbre1 = arbre2);
;;


(*****************************************)
(*                                       *)
(*         Q9 : liste_feuilles           *)
(*                                       *)
(*****************************************)

(* liste_feuilles : arbre -> bool list *)
(* convertie un arbre en table de vérité *)
let rec liste_feuilles arbre =
  match arbre with
  | Feuille x -> [x]
  | Noeud (_, g, d) -> (liste_feuilles g) @ (liste_feuilles d)
;;




let () = 
  let t = table [25899L] 16 in
  let arbre = cons_arbre t  in
  let l = liste_feuilles arbre in
  assert(t = l);
  (*
  print_bits t;
  print_bits l;
  *)
;;




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

type liste_deja_vus = (grand_entier * arbre_decision) list


(* contient : liste_deja_vus -> grand_entier -> bool *)
(* test si x est la 1er composant d'un coupe de la liste_deja_vue *)
let rec contient liste_deja_vue x = 
  match liste_deja_vue with
  |[] -> false
  |(n,pointeur)::suite -> (n = x) || contient suite x
;;


(* get_pointeur_liste_deja_vue : liste_deja_vus -> grand_entier -> arbre_decision *)
(* cherche le 2eme composant d'un couple de la liste_deja_vue avec x *)
let rec get_pointeur_liste_deja_vue liste_deja_vue x =
  match liste_deja_vue with
  |[] -> failwith "le grand_entier n'est pas dans la liste"
  |(n,pointeur)::suite -> 
      if x = n then
        pointeur
      else
        get_pointeur_liste_deja_vue suite x
;;


(*****************************************)
(*                                       *)
(*      Q11 : CompressionParListe        *)
(*                                       *)
(*****************************************)

(* regle_M : arbre_decision -> liste_deja_vus ref -> arbre_decision *)
let rec regle_M noeud liste_deja_vue =
  match noeud with 
  | Feuille x -> noeud
  | Noeud (x, gauche, droit) -> 
      let nouveau_gauche = regle_M gauche liste_deja_vue in
      let nouveau_droit = regle_M droit liste_deja_vue in
      
      (* Grand entier correspondant aux fils gauche et droit *)
      let n_gauche = composition (liste_feuilles nouveau_gauche) in
      let n_droit = composition (liste_feuilles nouveau_droit) in
      
      (* vérifie si fils gauche et droit sont dans la liste déjà vus *)
      let b1 = contient !liste_deja_vue n_gauche in
      let b2 = contient !liste_deja_vue n_droit in
      
      if n_gauche = n_droit then
        if b1 then begin
          (* fils gauche = fils droit, fils gauche et fils droit dans la liste *)
          let nouveau_pointeur = get_pointeur_liste_deja_vue !liste_deja_vue n_gauche in
          if nouveau_gauche = nouveau_pointeur then
            Noeud(x, nouveau_pointeur, nouveau_pointeur)
          else
            Noeud(x, nouveau_gauche, nouveau_gauche)
        end
        else begin
          (* fils gauche = fils droit, flis gauche et fils droit pas dans la liste *)
          liste_deja_vue := (n_gauche, nouveau_gauche)::!liste_deja_vue;
          Noeud(x, nouveau_gauche, nouveau_gauche)
        end
      else begin
        if b1 && b2 then begin
          (*fils gauche != fils droit, fils gauche et fils droit dans la liste*)
          let nouveau_pointeur_gauche = get_pointeur_liste_deja_vue !liste_deja_vue n_gauche in
          let nouveau_pointeur_droit = get_pointeur_liste_deja_vue !liste_deja_vue n_droit in
          if nouveau_gauche = nouveau_pointeur_gauche && nouveau_droit = nouveau_pointeur_droit then
            Noeud(x, nouveau_pointeur_gauche, nouveau_pointeur_droit)
          else if nouveau_gauche = nouveau_pointeur_gauche then
            Noeud(x, nouveau_pointeur_gauche, nouveau_droit)
          else
            Noeud(x, nouveau_gauche, nouveau_pointeur_droit)
        end
        else if b1 then begin
          (* fils gauche != fils droit, fils gauche dans la liste, fils droit pas dans la liste *)
          liste_deja_vue := (n_droit, nouveau_droit)::!liste_deja_vue; 
          let nouveau_pointeur = get_pointeur_liste_deja_vue !liste_deja_vue n_gauche in
          if nouveau_gauche = nouveau_pointeur then
            Noeud(x, nouveau_pointeur, nouveau_droit)
          else
            Noeud(x, nouveau_gauche, nouveau_droit)
        end
        else if b2 then begin   
          (*fils gauche != fils droit, fils gauche pas dans la liste, fils droit dans la liste*)
          liste_deja_vue := (n_gauche, nouveau_gauche)::!liste_deja_vue; 
          let nouveau_pointeur = get_pointeur_liste_deja_vue !liste_deja_vue n_droit in
          if nouveau_droit = nouveau_pointeur then
            Noeud(x, nouveau_gauche, nouveau_pointeur)
          else
            Noeud(x, nouveau_gauche, nouveau_droit)
        end
        else begin
          (* fils gauche != fils droit, fils gauche et fils droit pas dans la liste *)
          liste_deja_vue := (n_droit, nouveau_droit)::(n_gauche, nouveau_gauche)::!liste_deja_vue;
          Noeud(x, nouveau_gauche, nouveau_droit)
        end
      end
;;


(* regle_Z : arbre_decision -> arbre_decision *)
let rec regle_Z noeud =
  match noeud with
  |Feuille x -> noeud
  |Noeud(x,gauche,droit) -> 
      let nouveau_gauche = regle_Z gauche in
      let nouveau_droit = regle_Z droit in

      if nouveau_droit = Feuille false then 
         nouveau_gauche
      else
        Noeud(x,nouveau_gauche,nouveau_droit)
;;


(* compression_par_liste : arbre_decision -> (int64 list * arbre_decision) list ref -> arbre_decision*)
let compression_par_liste arbre liste_deja_vue =
  let nouveau_arbre = regle_Z arbre in
  let nouveau_arbre = regle_M nouveau_arbre liste_deja_vue in
  nouveau_arbre
;;

(*****************************************)
(*                                       *)
(*              Q12 : dot                *)
(*                                       *)
(*****************************************)


let rec calcul_nb_noeud arbre =
  match arbre with
  |Feuille x -> 1
  |Noeud(x,gauche,droit)-> 
    1 + calcul_nb_noeud(gauche) + calcul_nb_noeud(droit)
;;

let arrondi_puissance2_superieur n =
  let puissance2 = ref 1 in
  while n>(!puissance2) do
    puissance2:= !puissance2*2
  done;
  !puissance2
;;

(* add_tableau : (arbre_decision * int) array -> arbre_decision -> int -> unit *)
(* t est un array composé de de couple de arbre et un id *)
(* index est l'index du tableau ou l'on souhaite mettre a jour *)
let mise_a_jour_tableau t nouveau_arbre index =
  (* Cherche si nouveau_arbre est le 1er composant de l'un des couples de l'array et prend son index *)
  (* c-à-d la valeurs et pointeur deux arbres sont égale *)
  let i = Array.fold_left (fun acc (p, id) -> if p == nouveau_arbre then Some id else acc) None t in

  match i with
  |None-> 
    (* si le nouveau arbre n'existe pas dans l'array alors met a jour nouveau couple avec nouveau_arbre et id nouveau*)
    Array.set t index ( nouveau_arbre,index);
   (** Printf.printf "existe pas  : indice : %d -> valeur : %d\n" index index;*)
  |Some(x) -> 
    let pointeur,id = t.(x) in
    (* si le nouveau arbre existe pas dans l'array alors met a jour le couple avec l'ancien id*)
    Array.set t index ( pointeur,id);
    (*Printf.printf "existe déjà : indice : %d -> valeur : %d\n" index id;*)
;;


(* tableau : arbre_decision -> (arbre_decision * int) array *)
(* un tableau qui contient des couples (pointeur vers un arbre, id) de tout les noeuds de l'arbre*)
let tableau arbre =
  (* un tableau initialisé a la taille puissance de 2 supérieur au nombre de noeud de l'arbre (noeud interne et feuille)*)
  (* c-à-d un arbre de 5 noeud -> 8, un arbre de 7 -> 8, un arbre de 31 -> 32 *)
  let len = arrondi_puissance2_superieur (calcul_nb_noeud arbre) in
  let tab = Array.make len ( arbre, 0) in
  (* la valeur de index 1 de array correspond a la racine *)
  tab.(1) <- (arbre, 1);

  let rec aux arbre index t = 
    match arbre with
    | Feuille b ->
        ()
    | Noeud (profondeur, gauche, droit) ->  
        (* les index correspond au numéro des noeuds, c-à-d *)
        (* index =1 -> racine, index = 2 -> fils gauche de la racine, index = 3 fils droit de la racine*)
        (* formule numéro du fils gauche : index+2*)
        (* formule numéro du fils gauche : index+2+1*)
        mise_a_jour_tableau t ( gauche) (index*2); 
        mise_a_jour_tableau t ( droit) (index*2+1);
        aux gauche (index * 2) t;
        aux droit (index * 2 + 1) t;
  in
  aux arbre 1 tab;
  tab
;;

(* generate_dot_arbre : out_channel -> arbre_decision -> int -> (arbre_decision * int) array -> unit *)
(* génére l'arbre en langage dot dans le fichier file *)
let rec generate_dot_arbre file arbre index tableau =
  match arbre with
  | Feuille b ->
      let pointeur, n =  tableau.(index) in
      Printf.fprintf file "  Noeud%d [label=\"%b\", shape=\"none\"];\n" n b
  | Noeud (profondeur, gauche, droit) ->
      let pointeur, n =  tableau.(index) in
      Printf.fprintf file "  Noeud%d [label=\"%d\", shape=\"none\"];\n" n profondeur;
      generate_dot_arbre file gauche (index*2) tableau;
      generate_dot_arbre file droit (index*2+1) tableau;

      let pointeur_gauche, n_gauche = tableau.(index*2) in
      let pointeur_droit, n_droit = tableau.(index*2+1) in
      Printf.fprintf file "  Noeud%d -> Noeud%d [style=dotted, shape=\"none\"];\n" n n_gauche;
      Printf.fprintf file "  Noeud%d -> Noeud%d [style=solid, shape=\"none\"];\n" n n_droit
;;



(* Supprime les lignes redondantes d'un fichier *)
let remove_duplicate_lines input_file output_file =
  (* Ouvrez le fichier d'entrée en lecture *)
  let ic = open_in input_file in
  (* Ouvrez le fichier de sortie en écriture *)
  let oc = open_out output_file in
  (* Initialisez un ensemble pour garder une trace des lignes déjà vues *)
  let seen_lines = Hashtbl.create 1000 in
  try
    while true do
      let line = input_line ic in

      (* Vérifiez si la ligne a déjà été vue *)
      if not (Hashtbl.mem seen_lines line) then begin
        (* Écrivez la ligne dans le fichier de sortie *)
        output_string oc (line ^ "\n");
        (* Ajoutez la ligne à l'ensemble des lignes vues *)
        Hashtbl.add seen_lines line true;
      end
    done
  with
  | End_of_file ->
    close_in ic;
    close_out oc
  | Sys_error err ->
    Printf.eprintf "Erreur : %s\n" err
;;


(* dot : string -> arbre_decision *)
let dot filename arbre =
  let file_intermediaire = "file_intermediaire.dot" in
  let file = open_out file_intermediaire in
  Printf.fprintf file "digraph ArbreDecision {\n";
  let t = tableau arbre in

  generate_dot_arbre file arbre 1 t ;
  Printf.fprintf file "}\n";
  close_out file;

  (* enlève les lignes redondant *)
  remove_duplicate_lines file_intermediaire filename;
;;

(*****************************************)
(*                                       *)
(*                 Q13                   *)
(*                                       *)
(*****************************************)


let () =
 (** Printf.printf "test arbre\n ";*)
  let t = table [25899L] 16 in
  let arbre = cons_arbre t  in 
  dot "arbre_decision.dot" arbre;
;;


(*****************************************)
(*                                       *)
(*                 Q14                   *)
(*                                       *)
(*****************************************)


let () = 
  let t = table [25899L] 16 in
  let arbre = cons_arbre t  in
  let zdd = compression_par_liste arbre (ref []) in
  dot "zdd.dot" zdd;
  (*print_arbre_decision zdd;
  print_newline();
  let n = calcul_nb_noeud zdd in
  Printf.printf "nb noeud : %d\n" n;*)

  let arbre2 = Noeud(1, 
                     Noeud(2,
                           Noeud(3,
                                 Noeud(4,Feuille true, Feuille true),
                                 Noeud(4,Feuille false, Feuille true)
                                ),
                           Noeud(4,Feuille false, Feuille true)
                          ),
                     Noeud(2,
                           Noeud(3,Feuille true, Feuille true),
                           Noeud(3,
                                 Noeud(4,Feuille false, Feuille true),
                                 Feuille true)
                          )
                    )
  in  
  assert(zdd=arbre2);
;;



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

type arbre_deja_vus =
  | Leaf
  | Node of  arbre_decision option * arbre_deja_vus  * arbre_deja_vus
;;

let rec add_noeud_arbre_deja_vus arbre pointeur chemin =
  match arbre, chemin with
  | Leaf, [] -> Node (Some pointeur,Leaf, Leaf)
  | Leaf, b::bs -> if b then Node ( None,Leaf, add_noeud_arbre_deja_vus Leaf pointeur bs)
                     else Node ( None,add_noeud_arbre_deja_vus Leaf pointeur bs, Leaf)
  | Node ( e,g, d), [] -> Node ( Some pointeur,g, d)
  | Node ( e,g, d), b::bs -> if b then Node ( e,g, add_noeud_arbre_deja_vus d pointeur bs)
                           else Node ( e,add_noeud_arbre_deja_vus g pointeur bs, d)
;;

let rec print_arbre_deja_vus arbre =
  let rec aux = function
    | Leaf -> "Leaf"
    | Node (decision, gauche, droit) ->
      let decision_str =
        match decision with
        | Some x -> "arbre_decision"
        | None -> "vide"
      in
      "Node (" ^ decision_str ^ ", " ^ aux gauche ^ ", " ^ aux droit ^ ")"
  in
  let res = aux arbre in 
  Printf.printf "%s\n" res;
;;


let rec get_element_arbre_deja_vus arbre chemin =
  let rec aux node = function
    | [] -> (
        match node with
        | Leaf -> failwith"L'element n'existe pas"
        | Node(e,_,_) -> 
          match e with
          |None -> failwith"L'element n'existe pas"
          |Some x -> x
      )
    | true :: bs -> (
        match node with
        | Node (_, _, droit) -> aux droit bs
        | _ -> failwith"L'element n'existe pas"
      )
    | false :: bs -> (
        match node with
        | Node (_, gauche, _) -> aux gauche bs
        | _ -> failwith"L'element n'existe pas"
      )
  in
  aux arbre chemin
;;




let rec contient_arbre_deja_vus arbre chemin =
  let rec aux node = function
    | [] -> (
        match node with
        | Leaf -> false
        | Node(e,_,_) -> 
          match e with
          |None -> false
          |Some x -> true
      )
    | true :: bs -> (
        match node with
        | Node (_, _, droit) -> aux droit bs
        | _ -> false
      )
    | false :: bs -> (
        match node with
        | Node (_, gauche, _) -> aux gauche bs
        | _ -> false
      )
  in
  aux arbre chemin
;;


(*****************************************)
(*                                       *)
(*                Q16                    *)
(*                                       *)
(*****************************************)

(* regle_M : arbre_decision -> arbre_daja_vus ref -> arbre_decision *)
let rec regle_M_bis noeud arbre_deja_vue =
  match noeud with 
  | Feuille x -> noeud
  | Noeud (x, gauche, droit) -> 
      let nouveau_gauche = regle_M_bis gauche arbre_deja_vue in
      let nouveau_droit = regle_M_bis droit arbre_deja_vue in
      
      (* Bit correspondant aux fils gauche et droit *)
      let n_gauche = liste_feuilles nouveau_gauche in
      let n_droit = liste_feuilles nouveau_droit in
      
      (* vérifie si fils gauche et droit sont dans l'arbre déjà vus *)
      let b1 = contient_arbre_deja_vus !arbre_deja_vue n_gauche in
      let b2 = contient_arbre_deja_vus !arbre_deja_vue n_droit in
      
      if n_gauche = n_droit then
        if b1 then begin
          (* fils gauche = fils droit, fils gauche et fils droit dans la liste *)
          let nouveau_pointeur = get_element_arbre_deja_vus !arbre_deja_vue n_gauche in
          if nouveau_gauche = nouveau_pointeur then
            Noeud(x, nouveau_pointeur, nouveau_pointeur)
          else
            Noeud(x, nouveau_gauche, nouveau_gauche)
        end
        else begin
          (* fils gauche = fils droit, flis gauche et fils droit pas dans la liste *)
          arbre_deja_vue := add_noeud_arbre_deja_vus !arbre_deja_vue nouveau_gauche n_gauche;
          Noeud(x, nouveau_gauche, nouveau_gauche)
        end
      else begin
        if b1 && b2 then begin
          (*fils gauche != fils droit, fils gauche et fils droit dans la liste*)
          let nouveau_pointeur_gauche = get_element_arbre_deja_vus !arbre_deja_vue n_gauche in
          let nouveau_pointeur_droit = get_element_arbre_deja_vus !arbre_deja_vue n_droit in
          if nouveau_gauche = nouveau_pointeur_gauche && nouveau_droit = nouveau_pointeur_droit then
            Noeud(x, nouveau_pointeur_gauche, nouveau_pointeur_droit)
          else if nouveau_gauche = nouveau_pointeur_gauche then
            Noeud(x, nouveau_pointeur_gauche, nouveau_droit)
          else
            Noeud(x, nouveau_gauche, nouveau_pointeur_droit)
        end
        else if b1 then begin
          (* fils gauche != fils droit, fils gauche dans la liste, fils droit pas dans la liste *)
          arbre_deja_vue := add_noeud_arbre_deja_vus !arbre_deja_vue nouveau_droit n_droit; 
          let nouveau_pointeur = get_element_arbre_deja_vus !arbre_deja_vue n_gauche in
          if nouveau_gauche = nouveau_pointeur then
            Noeud(x, nouveau_pointeur, nouveau_droit)
          else
            Noeud(x, nouveau_gauche, nouveau_droit)
        end
        else if b2 then begin   
          (*fils gauche != fils droit, fils gauche pas dans la liste, fils droit dans la liste*)
          arbre_deja_vue := add_noeud_arbre_deja_vus !arbre_deja_vue nouveau_gauche n_gauche; 
          let nouveau_pointeur = get_element_arbre_deja_vus !arbre_deja_vue n_droit in
          if nouveau_droit = nouveau_pointeur then
            Noeud(x, nouveau_gauche, nouveau_pointeur)
          else
            Noeud(x, nouveau_gauche, nouveau_droit)
        end
        else begin
          (* fils gauche != fils droit, flis gauche et fils droit pas dans la liste *)
          arbre_deja_vue := add_noeud_arbre_deja_vus !arbre_deja_vue nouveau_gauche n_gauche;
          arbre_deja_vue := add_noeud_arbre_deja_vus !arbre_deja_vue nouveau_droit n_droit;
          Noeud(x, nouveau_gauche, nouveau_droit)
        end
      end
;;


(*****************************************)
(*                                       *)
(*       Q17 : CompressionParArbre       *)
(*                                       *)
(*****************************************)

(* compression_par_arbre : arbre_decision -> arbre_deja_vus ref -> arbre_decision *)
let compression_par_arbre arbre arbre_deja_vue =
  let nouveau_arbre = regle_Z arbre in
  let nouveau_arbre = regle_M_bis nouveau_arbre arbre_deja_vue in
  nouveau_arbre
;;


(*****************************************)
(*                                       *)
(*                Q18                    *)
(*                                       *)
(*****************************************)

let () = 
  let t = table [25899L] 16 in
  let arbre = cons_arbre t  in
  let arbre_deja_vue = Leaf in
  let zdd = compression_par_arbre arbre (ref arbre_deja_vue) in
  dot "zdd_bis.dot" zdd;
  (*
  print_arbre_decision zdd;
  print_newline();
  let n = calcul_nb_noeud zdd in
  Printf.printf "nb noeud : %d\n" n;
  *)

  let arbre2 = Noeud(1, 
                     Noeud(2,
                           Noeud(3,
                                 Noeud(4,Feuille true, Feuille true),
                                 Noeud(4,Feuille false, Feuille true)
                                ),
                           Noeud(4,Feuille false, Feuille true)
                          ),
                     Noeud(2,
                           Noeud(3,Feuille true, Feuille true),
                           Noeud(3,
                                 Noeud(4,Feuille false, Feuille true),
                                 Feuille true)
                          )
                    )
  in 
  assert(zdd=arbre2);

;;

(*************************************************************)
(*                                                           *)
(*                                                           *)
(*                V.Analyses de complexité                   *)  
(*                 VI.Etude expérimentale                    *)
(*                                                           *)
(*                                                           *)
(*************************************************************)

(*****************************************)
(*                                       *)
(*               Q19,20,21               *)
(*                                       *)
(*****************************************)


(* Measure the execution time of a function f applied to x *)
let time f x =
  let start = Unix.gettimeofday () in
  let fx = f x in
  (Unix.gettimeofday () -. start), fx

(* Calculate the size of a tree to simulate space complexity *)
let rec taille_arbre (arbre: arbre_decision): int = match arbre with 
  | Feuille _ -> 1
  | Noeud (_,g, d) -> 1 + taille_arbre g + taille_arbre d

(*
let rec taille_compression_liste liste_deja_vus =
  match liste_deja_vus with
  | [] -> 0
  | _::reste -> 1 + taille_compression_liste reste
;;



let rec taille_compression_arbre arbre_deja_vus =
  match arbre_deja_vus with
  | Leaf -> 0
  | Node (_, gauche, droit) -> 1 + taille_compression_arbre gauche + taille_compression_arbre droit
;;
*)
let taille_arbre_unique (arbre: arbre_decision) : int =
  let visited = Hashtbl.create 1024 in
  let rec aux arbre =
    match arbre with
    | Feuille _ -> 1
    | Noeud (_, g, d) as node ->
        let node_id = Hashtbl.hash node in
        if Hashtbl.mem visited node_id then 0
        else begin
          Hashtbl.add visited node_id true;
          1 + aux g + aux d
        end
  in aux arbre
;;
(*
let genere_et_compresse_arbre_record bits =
  let grand_entier_aleatoire = genere_aleatoire bits in
  let bits_list = table grand_entier_aleatoire bits in
  let construction_time, arbre_genere = time cons_arbre bits_list in
  let tree_size = taille_arbre arbre_genere in
  let space_complexity_construction = tree_size in (* 假设生成树的大小就是其空间复杂度 *)
  let arbre_deja_vue_liste = ref [] in
  let compression_by_list_time, compressed_list = time (compression_par_liste arbre_genere) arbre_deja_vue_liste in
  let space_complexity_by_list = taille_compression_liste compressed_list in  (* Assuming list is converted to tree for measurement *)
  let arbre_deja_vue_arbre = ref Leaf in
  let compression_by_tree_time, compressed_tree = time (compression_par_arbre arbre_genere) arbre_deja_vue_arbre in
  let space_complexity_by_tree = taille_compression_arbre compressed_tree in
  (bits, tree_size, construction_time, compression_by_list_time, compression_by_tree_time, space_complexity_by_list, space_complexity_by_tree, space_complexity_construction)
*)

let genere_et_compresse_arbre_record bits =
  let grand_entier_aleatoire = genere_aleatoire bits in
  let bits_list = table grand_entier_aleatoire bits in
  let construction_time, arbre_genere = time cons_arbre bits_list in
  let tree_size = taille_arbre arbre_genere in
  let space_complexity_construction = tree_size in (* Assuming the size of the generated tree is its space complexity *)

  let arbre_deja_vue_liste = ref [] in
  let compression_by_list_time, compressed_list = time (compression_par_liste arbre_genere) arbre_deja_vue_liste in
  let space_complexity_by_list = taille_arbre_unique compressed_list in  (* Assuming list is converted to tree for measurement *)
  let arbre_deja_vue_arbre = ref Leaf in
  let compression_by_tree_time, compressed_tree = time (compression_par_arbre arbre_genere) arbre_deja_vue_arbre in
  let space_complexity_by_tree = taille_arbre_unique compressed_tree in  

  (bits, tree_size, construction_time, compression_by_list_time, compression_by_tree_time, space_complexity_by_list, space_complexity_by_tree, space_complexity_construction)



let generate_data_points () =
  let rec aux acc bits =
    if bits > 1024 then acc
    else
      let record = genere_et_compresse_arbre_record bits in
      aux (record :: acc) (bits*2) 
  in
  aux [] 1 

let write_to_csv file_path data =
  let oc = open_out file_path in
  Printf.fprintf oc "bits, tree_size, construction_time, compression_by_list_time, compression_by_tree_time, space_complexity_by_list, space_complexity_by_tree, space_complexity_construction\n";
  List.iter (fun (bits, tree_size, construction_time, compression_by_list_time, compression_by_tree_time, space_complexity_by_list, space_complexity_by_tree, space_complexity_construction) ->
    Printf.fprintf oc "%d, %d, %f, %f, %f, %d, %d, %d\n" bits tree_size construction_time compression_by_list_time compression_by_tree_time space_complexity_by_list space_complexity_by_tree space_complexity_construction

  ) data;
  close_out oc

let () =
  Printexc.record_backtrace true; (* Enable stack trace *)
  try
    Random.self_init (); (* Initialize random generator *)
    let file_path = "arbre_times.csv" in
    let data_points = generate_data_points () in
    let data_to_write = List.rev data_points in (* Reverse the list for ascending order *)
    write_to_csv file_path data_to_write;
    Printf.printf "Data collection complete. Data written to %s\n" file_path;
  with e ->
    Printf.printf "An exception occurred: %s\n" (Printexc.to_string e);
    Printexc.print_backtrace stdout; (* Print the stack trace *)
    exit 1
