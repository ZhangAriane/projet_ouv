(***************************************)
(*                                     *)
(*                                     *)
(*            Echauffement             *)  
(*                                     *)
(*                                     *)
(***************************************)  


(***************************************)
(*                                     *)
(*         Q1  : Primmitive            *)
(*                                     *)
(***************************************)

(* liste d'entiers a 64 bit *)
type grand_entier = int64 list


(* insert : grand_entier -> int64 -> grand_entier *)
(* insert une valeur en la fin de liste ge *)
let insert ge valeur =
  ge @ [valeur]


(* remove : grand_entier -> grand_entier *)
(* enlève le premier element de la liste *)
let remove ge  = 
  match ge with
  |[] -> []
  |t::s -> s


(* get_head : grand_entier ->int64 option *)
(* récupère le premier élèment de la liste si il existe, sinon retourne None *)
let get_head ge =
  match ge with
  |[] -> None
  |t::_ -> Some t