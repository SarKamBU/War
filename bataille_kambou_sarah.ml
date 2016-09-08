
(* Sarah KAMBOU *)

open List
open Random

(* QUESTION 1 : on crée les types couleur, valeur, carte et paquet *)

(* on définit un type couleur pour la couleur d'une carte *)
type couleur = Pique | Coeur | Carreau | Trefle

(* on définit un type valeur pour la valeur d'une carte *)
type valeur = As | Roi | Dame | Valet | Petite

(* on définit le type carte : il a pour caractéristiques la valeur et la couleur de la carte *)
type carte = As of couleur
            |Roi of couleur
            |Dame of couleur
            |Valet of couleur
            |Petite of couleur * int

(* on définit le type paquet sous forme d'une liste de cartes *)
type paquet = carte list 

(* voici des cartes qui serviront pour tester les fonctions :

let as_coeur = As Coeur
let roi_pique = Roi Pique
let dix_carreau = Petite(Carreau,10)
let dix_trefle = Petite(Trefle,10)

résultats de la déclaration des variables :

val as_coeur : carte = As Coeur
val roi_pique : carte = Roi Pique
val dix_carreau : carte = Petite (Carreau, 10)
val dix_trefle : carte = Petite (Trefle, 10)

*)

(* QUESTION 2 : voici une fonction qui compare la valeur de deux cartes :
si la première gagne, la fonction renvoie 1 point;
si elle perd, la fonction renvoie -1;
s'il y a égalité la fonction renvoie 0. *)

let compare carte1 carte2 = 
  match (carte1,carte2) with
    |(As _,As _) -> 0 (* As = As *)
    |(As _,Roi _) -> 1 (* As > Roi *)
    |(As _,Dame _) -> 1 (* As > Dame *)
    |(As _,Valet _) -> 1 (* As > Valet *)
    |(As _ ,Petite(_,_)) -> 1 (* As > n'importe quelle Petite *)
    |(Roi _,As _) -> -1 (* Roi < As *)
    |(Roi _,Roi _) -> 0 (* Roi = Roi *)
    |(Roi _,Dame _) -> 1 (* Roi > Dame *)
    |(Roi _,Valet _) -> 1 (* Roi > Valet *)
    |(Roi _,Petite(_,_)) -> 1 (* Roi > n'importe quelle Petite *)
    |(Dame _,As _) -> -1 (* Dame < As *)
    |(Dame _,Roi _) -> -1 (* Dame < Roi *)
    |(Dame _,Dame _) -> 0 (* Dame = Dame *)
    |(Dame _,Valet _) -> 1 (* Dame > Valet *)
    |(Dame _,Petite(_,_)) -> 1 (* Dame > n'importe quelle Petite *)
    |(Valet _,As _) -> -1 (* Valet < As *)
    |(Valet _,Roi _) -> -1 (* Valet < Roi *)
    |(Valet _,Dame _) -> -1 (* Valet < Dame *)
    |(Valet _,Valet _) -> 0 (* Valet = Valet *)
    |(Valet _,Petite(_,_)) -> 1 (* Valet > n'importe quelle Petite *)
    |(Petite(_,_),As _) -> -1 (* N'importe quelle Petite < As *)
    |(Petite(_,_),Roi _) -> -1 (* N'importe quelle Petite < Roi *)
    |(Petite(_,_),Dame _) -> -1 (* N'importe quelle Petite < Dame *)
    |(Petite(_,_),Valet _) -> -1 (* N'importe quelle Petite < Valet *)
    |(Petite(_,x),Petite(_,y)) when (x<2||x>10)||(y<2||y>10)-> failwith "cas impossible" (* exception quand une petite est inf. à 2 ou sup. à 10 *)
    |(Petite(_,x),Petite(_,y)) when x=y -> 0 
(* prévient l'exception "division par zéro" et traite les cas d'égalités *)
    |(Petite(_,x),Petite(_,y)) -> (x - y) / (abs (x - y));; 
    (* Exemple : (9-8)/abs(9-8) = 1; (7-8)/abs(7-8) = -1 *)
		
(* résultat de la déclaration de la fonction : 

val compare : carte -> carte -> int = <fun>

*)

(* on teste avec les cartes données plus haut :

compare dix_carreau dix_trefle;; 
compare dix_carreau as_coeur;;
compare dix_trefle roi_pique;;
compare roi_pique as_coeur;;
compare as_coeur roi_pique;;

voici les résultats de la fonction compare :

- : int = 0
- : int = -1
- : int = -1
- : int = -1
- : int = 1

*)

(* QUESTION 3 : voici une fonction qui renvoie une chaîne de caractères
donnant la valeur de la carte prise en argument *)		

let string_of_carte une_carte = 
  let string_of_couleur coul =
    match coul with
      |Pique -> "pique"
      |Trefle -> "trefle"
      |Carreau -> "carreau"
      |Coeur -> "coeur"
  in
  match une_carte with
    |As colour -> "As de " ^(string_of_couleur colour)
    |Roi colour -> "Roi de " ^(string_of_couleur colour)
    |Dame colour -> "Dame de " ^(string_of_couleur colour)
    |Valet colour -> "Valet de " ^(string_of_couleur colour)
    |Petite(colour,number) -> (string_of_int number)^ " de " ^(string_of_couleur colour);;
	   
(* résultats de la déclaration de la fonction string_of_carte :

val string_of_carte : carte -> string = <fun> 

*)

(* on teste avec les cartes définies au début du code

string_of_carte as_coeur;;
string_of_carte roi_pique;;
string_of_carte dix_trefle;;
string_of_carte dix_carreau;;

voici les résultats de l'exécution de string_of_carte :

- : string = "As de coeur"
- : string = "Roi de pique"
- : string = "10 de trefle"
- : string = "10 de carreau"

*)

(* QUESTION 4 : un paquet de 52 cartes *)

let (p : paquet) =
As Coeur :: As Pique :: As Trefle :: As Carreau ::
Roi Coeur :: Roi Pique :: Roi Trefle :: Roi Carreau ::
Dame Coeur :: Dame Pique :: Dame Trefle :: Dame Carreau ::
Valet Coeur :: Valet Pique :: Valet Trefle :: Valet Carreau ::
Petite(Coeur,10) :: Petite(Pique,10) :: Petite(Trefle,10) ::
Petite(Carreau,10) ::Petite(Coeur,9) :: Petite(Pique,9) :: 
Petite(Trefle,9) :: Petite(Carreau,9) :: Petite(Coeur,8) :: Petite(Pique,8) ::
Petite(Trefle,8) :: Petite(Carreau,8) :: Petite(Coeur,7) :: Petite(Pique,7) :: 
Petite(Trefle,7) :: Petite(Carreau,7) :: Petite(Coeur,6) :: Petite(Pique,6) :: 
Petite(Trefle,6) :: Petite(Carreau,6) :: Petite(Coeur,5) :: Petite(Pique,5) :: 
Petite(Trefle,5) :: Petite(Carreau,5) :: Petite(Coeur,4) :: Petite(Pique,4) :: 
Petite(Trefle,4) :: Petite(Carreau,4) :: Petite(Coeur,3) :: Petite(Pique,3) :: 
Petite(Trefle,3) :: Petite(Carreau,3) :: Petite(Coeur,2) :: Petite(Pique,2) :: 
Petite(Trefle,2) :: Petite(Carreau,2) :: []

(* résultat de la declaration :

val p : paquet =
  [As Coeur; As Pique; As Trefle; As Carreau; Roi Coeur; Roi Pique;
   Roi Trefle; Roi Carreau; Dame Coeur; Dame Pique; Dame Trefle;
   Dame Carreau; Valet Coeur; Valet Pique; Valet Trefle; Valet Carreau;
   Petite (Coeur, 10); Petite (Pique, 10); Petite (Trefle, 10);
   Petite (Carreau, 10); Petite (Coeur, 9); Petite (Pique, 9);
   Petite (Trefle, 9); Petite (Carreau, 9); Petite (Coeur, 8);
   Petite (Pique, 8); Petite (Trefle, 8); Petite (Carreau, 8);
   Petite (Coeur, 7); Petite (Pique, 7); Petite (Trefle, 7);
   Petite (Carreau, 7); Petite (Coeur, 6); Petite (Pique, 6);
   Petite (Trefle, 6); Petite (Carreau, 6); Petite (Coeur, 5);
   Petite (Pique, 5); Petite (Trefle, 5); Petite (Carreau, 5);
   Petite (Coeur, 4); Petite (Pique, 4); Petite (Trefle, 4);
   Petite (Carreau, 4); Petite (Coeur, 3); Petite (Pique, 3);
   Petite (Trefle, 3); Petite (Carreau, 3); Petite (Coeur, 2);
   Petite (Pique, 2); Petite (Trefle, 2); Petite (Carreau, 2)]

 *)

(* QUESTION 5 : fonction permettant de mélanger aléatoirement un paquet de cartes *)

let melange (l : paquet) : paquet = 
  Random.self_init();
  let len = List.length l in
  let rec aux i acc l =
    let suppr_nth n l =  (* fonction servant à supprimer le n-ième élément d'une liste *)
      let rec aux acc i = function
	| [] -> List.rev acc
	| _::tl when i = n -> List.rev_append acc tl
	| x::tl -> aux (x::acc) (i+1) tl
      in 
      aux [] 0 l
    in
    if i < len then (* si i est plus petit que la taille de la liste *)
      let r = Random.int (len - i) in
 (* on choisit aléatoirement un élément compris dans l'intervalle [i,len] *)
      aux (i+1) (List.nth l r ::acc) (suppr_nth r l) 
(* on échange l'élément à la place i avec l'élément à la place r puis on supprime l'élément choisi de la liste pour éviter les doublons. Puis on fait de même avec tout le reste *)
    else acc (* sinon on renvoie la liste acc *)
  in
  aux 0 [] l;;
  
(*  résultat de la declaration :

val melange : paquet -> paquet = <fun>

 *)

(* on teste la fonction melange sur le paquet p

melange p;;

voici un résultat de melange :

- : paquet =
[Petite (Pique, 3); Petite (Carreau, 2); Petite (Pique, 7);
 Petite (Pique, 6); Petite (Trefle, 6); Petite (Pique, 4); Dame Coeur;
 Petite (Carreau, 10); Petite (Trefle, 9); Petite (Coeur, 4);
 Petite (Pique, 9); Roi Coeur; Petite (Trefle, 2); Petite (Coeur, 5);
 Petite (Pique, 2); Petite (Carreau, 7); Petite (Coeur, 8);
 Petite (Carreau, 4); Petite (Coeur, 2); Petite (Trefle, 10);
 Petite (Trefle, 5); Valet Pique; Petite (Coeur, 6); Valet Trefle;
 Petite (Pique, 8); Petite (Carreau, 3); Petite (Trefle, 3); Roi Carreau;
 As Trefle; Dame Trefle; Petite (Trefle, 8); As Coeur; Dame Carreau;
 Petite (Coeur, 3); As Carreau; Petite (Trefle, 7); Valet Coeur;
 Petite (Coeur, 10); Dame Pique; Petite (Carreau, 9); Petite (Carreau, 8);
 Petite (Coeur, 9); Petite (Pique, 5); Petite (Pique, 10);
 Petite (Carreau, 6); Roi Trefle; Roi Pique; Petite (Carreau, 5);
 Petite (Trefle, 4); As Pique; Petite (Coeur, 7); Valet Carreau]

 *)

(* QUESTION 6 : fonction permettant de partager un paquet de cartes en deux paquets distincts *)

let rec partage (pack : paquet) : paquet*paquet =
  match pack with
    |[] -> ([],[]) (* le paquet vide est partagé en deux paquets vides *)
    |[h] -> ([h],[]) 
    |h :: t :: t2 -> let (l1,l2) = partage t2 in ((h::l1),(t::l2));; 
(* ici, chaque carte du paquet de depart est envoyée dans un des deux sous paquets.
Exemple : [As Coeur, Dame Pique, Roi Trefle, Valet Carreau, ...] donne
((As Coeur::l),(Dame Pique::li)) et on applique la fonction récursivement sur
le reste du paquet de depart.
On obtiendra (As Coeur::(Roi Trefle::l),Dame Pique::(Valet Carreau::li)) 
et ainsi de suite jusqu'à ce qu'il n'y est plus rien dans le paquet de depart *)
																				
(* résultat de la déclaration :

val partage : paquet -> paquet * paquet = <fun>

*)			
 
(* on teste sur le paquet p

partage p;;

résultat :

- : paquet * paquet =
([As Coeur; As Trefle; Roi Coeur; Roi Trefle; Dame Coeur; Dame Trefle;
  Valet Coeur; Valet Trefle; Petite (Coeur, 10); Petite (Trefle, 10);
  Petite (Coeur, 9); Petite (Trefle, 9); Petite (Coeur, 8);
  Petite (Trefle, 8); Petite (Coeur, 7); Petite (Trefle, 7);
  Petite (Coeur, 6); Petite (Trefle, 6); Petite (Coeur, 5);
  Petite (Trefle, 5); Petite (Coeur, 4); Petite (Trefle, 4);
  Petite (Coeur, 3); Petite (Trefle, 3); Petite (Coeur, 2);
  Petite (Trefle, 2)],
 [As Pique; As Carreau; Roi Pique; Roi Carreau; Dame Pique; Dame Carreau;
  Valet Pique; Valet Carreau; Petite (Pique, 10); Petite (Carreau, 10);
  Petite (Pique, 9); Petite (Carreau, 9); Petite (Pique, 8);
  Petite (Carreau, 8); Petite (Pique, 7); Petite (Carreau, 7);
  Petite (Pique, 6); Petite (Carreau, 6); Petite (Pique, 5);
  Petite (Carreau, 5); Petite (Pique, 4); Petite (Carreau, 4);
  Petite (Pique, 3); Petite (Carreau, 3); Petite (Pique, 2);
  Petite (Carreau, 2)])

 *)

(* QUESTION 7 : fonction face à face qui renvoie le score selon les cartes utilisées pour l'affrontement *)

let face_a_face (a,b) c1 c2 =
  let f_a_f = compare c1 c2 in (* on compare les deux cartes *)
  match f_a_f with
    |0 -> (a,b) (* si il y a egalite le score reste le meme *)
    |1 -> (a+1,b)(* si la premiere carte gagne +1 point *)
    |_ -> (a,b+1);; (* si la premiere carte perd -1 point *)
	
(* résultat de la declaration
 
val face_a_face : int * int -> carte -> carte -> int * int = <fun>

*)
	
(* on teste la fonction avec les cartes de test définies au début du code

face_a_face (0,0) as_coeur dix_trefle;;
face_a_face (0,0) roi_pique as_coeur;;
face_a_face (0,0) dix_carreau dix_trefle;;

Le résultat produit est : 

- : int * int = (1, 0)
- : int * int = (0, 1)
- : int * int = (0, 0)

*)

(* QUESTION 8 : même fonction que celle de la question 7 mais avec du texte cette fois-ci *)

let face_a_face_texte (a,b) c1 c2 =
  let s1 = string_of_carte c1 in
  let s2 = string_of_carte c2 in
  let affichage_score () =
	print_string ("\nScore : "^(string_of_int a)^" a "^(string_of_int b)^
	("\n\n" ^s1 ^ " vs " ^ s2^"\n"))
  in
  affichage_score ();
  face_a_face (a,b) c1 c2;;
	
(* résultat de la déclaration :

val face_a_face_texte : int * int -> carte -> carte -> int * int = <fun>

 *)

(* on teste la fonction 
 
face_a_face_texte (0,0) as_coeur dix_trefle;;
face_a_face_texte (0,0) roi_pique as_coeur;;
face_a_face_texte (0,0) dix_carreau dix_trefle;;

résultats du test :

Score : 0 a 0

As de coeur vs 10 de trefle
- : int * int = (1, 0)

Score : 0 a 0

Roi de pique vs As de coeur
- : int * int = (0, 1)

Score : 0 a 0

10 de carreau vs 10 de trefle
- : int * int = (0, 0)

*)
 
(* QUESTION 9 : fonction bataille qui renvoie le score final à la fin de la partie *)

let bataille () = 

  let introduction () =
	print_endline ("\n\n-------------------"^"\nNOUVELLE BATAILLE\n"^"-------------------\n")
  in
  introduction ();
  
  let pqt = melange p in (* on melange le paquet de carte *)
  let (p1,p2) = partage pqt in (* on partage le paquet en deux *)
  let n = (List.length p1) - 1 in (* n = la taille d'un des sous-paquets *)
  let score = (0,0) in 
(* ci-dessus : on initialise le score pour la partie : on le met en reference pour pouvoir modifier la valeur plus tard *)

  let rec aux sc x l1 l2 =
   let versus = face_a_face_texte sc (List.nth l1 x) (List.nth l2 x) in
   let v1 = fst versus in
   let v2 = snd versus in
   let sco = (v1,v2) in
   match x with 
	|0 -> sco
	|_ -> aux sco (x-1) l1 l2
  in (* cette fonction auxilliaire permet de garder le score en mémoire *)
  
  let score_final = (aux score n p1 p2) in
  
   (* ce qui suit ne sert qu'à bien mettre en forme le résultat renvoyé *)
  let affichage_score () = 
	print_string ("\nScore : "^(string_of_int (fst score_final))^" a "^
	(string_of_int (snd score_final))^"\n")
  in
  affichage_score ();
  score_final;; (* on renvoie le score *)
	 
(*  résultat de la déclaration :

val bataille : unit -> int * int = <fun>

*)

(* test de bataille : cf dernier commentaire du code -> l'exécution de bataille correspond à la declaration stat 1 donc une partie
let main () =
  bataille();; *)
	
(*  QUESTION 10 : fonction qui prend en argument le nombre de parties et les exécute. Elle affiche aussi le nombre de victoires,
le score cumulé, le score moyen et le taux de victoires *)

let stat n =

 (* tout d'abord on initialise le nombre de victoires et le score cumulé de chaque joueur a 0 *)
  let wins_j1 = ref 0 in (* j'utilise ici des références car je n'ai pas trouvé d'autres moyens de faire sans *)
  let wins_j2 = ref 0 in
  let cumul_j1 = ref 0 in
  let cumul_j2 = ref 0 in
  let egalites = ref 0 in
  
  (* on exécute n parties grâce à une fonction récursive *)
  let rec aux y c1 c2 =
     let res = bataille () in
     let aux_wins w1 w2 eq =
        if (fst res > snd res)
          then begin
            w1 := !w1 + 1; (* on met à jour son nombre de victoires *)
			print_string "\n\nJOUEUR 1 gagne !\n"
          end
       else if fst res < snd res (* si le joueur 2 gagne la partie *)
       	  then begin 
	     w2 := !w2 + 1; (* on met à jour son nombre de victoires *) 
	     print_string "\n\nJOUEUR 2 gagne !\n"
          end
       else 
           begin
			eq := !eq + 1;
			print_string "\nEGALITE !"
          end
      in
      aux_wins wins_j1 wins_j2 egalites;
      c1 := !c1 + (fst res);
      c2 := !c2 + (snd res);
      match y with
        |1 -> (c1,c2)
        |_ -> aux (y-1) c1 c2
  in	
  let cumul = (aux n cumul_j1 cumul_j2) in
  
 (* maintenant on calcule le score moyen et le taux de victoires de chaque joueur *)
  let sc_moyen_j1 = (float_of_int !(fst cumul)) /. (float_of_int n) in 
  let sc_moyen_j2 = (float_of_int !(snd cumul)) /. (float_of_int n) in 
  let taux_victoires_j1 = ((float_of_int !wins_j1) /. (float_of_int n)) *. 100.0 in 
  let taux_victoires_j2 = ((float_of_int !wins_j2) /. (float_of_int n)) *. 100.0 in 
  
  let affichage_statistiques () = 
	(* on affiche les statistiques du joueur 1 *)
	print_endline ("\n\n\n------------------------"^"\nSTATISTIQUES JOUEUR 1 : "^"\nNombre de victoires : "^(string_of_int !wins_j1)^
	"\nScore cumule : "^(string_of_int !(fst cumul))^"\nScore moyen : "^(string_of_float sc_moyen_j1)^"\nTaux de victoires : "^
	(string_of_float taux_victoires_j1)^"%\n\n");
	(* on affiche les statistiques du joueur 2 *)
	print_endline ("\n------------------------"^"\nSTATISTIQUES JOUEUR 2 : "^"\nNombre de victoires : "^(string_of_int !wins_j2)^
	"\nScore cumule : "^(string_of_int !(snd cumul))^"\nScore moyen : "^(string_of_float sc_moyen_j2)^"\nTaux de victoires : "^
	(string_of_float taux_victoires_j2)^"%\n\n");
	print_string ("\nNombre d'egalites : "^(string_of_int !egalites)^"\n\n")
  in
  affichage_statistiques ();;
		
(* résultat de la déclaration :

val stat : int -> unit = <fun>

 *)

let main () =
  stat 4;;
	
main ();;

(* un résultat :

-------------------
NOUVELLE BATAILLE
-------------------

Score : 0 a 0

5 de carreau vs 10 de carreau

Score : 0 a 1

2 de coeur vs Valet de trefle

Score : 0 a 2

9 de pique vs Valet de pique

Score : 0 a 3

3 de carreau vs As de coeur

Score : 0 a 4

2 de trefle vs As de pique
let aux sc n l1 l2 =
   let versus = face_a_face_texte !sc (List.nth l1 n) (List.nth l2 n) in
   let v1 = fst versus in
   let v2 = snd versus in
   sc := (v1,v2);
   !sc;;
Score : 0 a 5

8 de coeur vs Dame de coeur

Score : 0 a 6

3 de pique vs 10 de pique

Score : 0 a 7

3 de coeur vs Roi de pique

Score : 0 a 8

Roi de carreau vs 4 de pique

Score : 1 a 8

Dame de carreau vs As de carreau

Score : 1 a 9

8 de trefle vs 2 de carreau

Score : 2 a 9

4 de carreau vs 4 de trefle

Score : 2 a 9

Dame de pique vs Roi de trefle

Score : 2 a 10

8 de carreau vs 7 de coeur

Score : 3 a 10

Valet de carreau vs 5 de trefle

Score : 4 a 10

9 de coeur vs 9 de carreau

Score : 4 a 10

6 de coeur vs 6 de pique

Score : 4 a 10

5 de coeur vs Valet de coeur

Score : 4 a 11

10 de coeur vs 9 de trefle

Score : 5 a 11

5 de pique vs Roi de coeur

Score : 5 a 12

7 de carreau vs 7 de trefle

Score : 5 a 12

2 de pique vs 4 de coeur

Score : 5 a 13

6 de trefle vs 7 de pique

Score : 5 a 14

Dame de trefle vs 10 de trefle

Score : 6 a 14

8 de pique vs As de trefle

Score : 6 a 15

6 de carreau vs 3 de trefle

Score : 7 a 15


JOUEUR 2 gagne !


-------------------
NOUVELLE BATAILLE
-------------------

Score : 0 a 0

4 de pique vs 4 de carreau

Score : 0 a 0

3 de carreau vs 9 de pique

Score : 0 a 1

6 de coeur vs 9 de coeur

Score : 0 a 2

Dame de pique vs Roi de coeur

Score : 0 a 3

9 de trefle vs 2 de coeur

Score : 1 a 3

8 de trefle vs 10 de pique

Score : 1 a 4

6 de trefle vs Roi de trefle

Score : 1 a 5

Roi de pique vs 2 de trefle

Score : 2 a 5

Valet de trefle vs Valet de coeur

Score : 2 a 5

Dame de carreau vs 10 de trefle

Score : 3 a 5

Valet de carreau vs 3 de pique

Score : 4 a 5

7 de pique vs 5 de coeur

Score : 5 a 5

Roi de carreau vs 10 de coeur

Score : 6 a 5

7 de trefle vs 8 de coeur

Score : 6 a 6

As de trefle vs As de coeur

Score : 6 a 6

8 de pique vs 2 de carreau

Score : 7 a 6

6 de pique vs 8 de carreau

Score : 7 a 7

6 de carreau vs 4 de coeur

Score : 8 a 7

5 de carreau vs 5 de pique

Score : 8 a 7

3 de coeur vs 2 de pique

Score : 9 a 7

5 de trefle vs Dame de coeur

Score : 9 a 8

Valet de pique vs As de pique

Score : 9 a 9

Dame de trefle vs 10 de carreau

Score : 10 a 9

As de carreau vs 3 de trefle

Score : 11 a 9

7 de coeur vs 7 de carreau

Score : 11 a 9

4 de trefle vs 9 de carreau

Score : 11 a 10


JOUEUR 1 gagne !


-------------------
NOUVELLE BATAILLE
-------------------

Score : 0 a 0

Valet de trefle vs Dame de carreau

Score : 0 a 1

7 de carreau vs 8 de coeur

Score : 0 a 2

4 de coeur vs 6 de trefle

Score : 0 a 3

9 de coeur vs 6 de carreau

Score : 1 a 3

2 de carreau vs 5 de trefle

Score : 1 a 4

Valet de pique vs 5 de coeur

Score : 2 a 4

As de trefle vs 10 de pique

Score : 3 a 4

3 de pique vs 7 de trefle

Score : 3 a 5

2 de coeur vs 9 de pique

Score : 3 a 6

7 de pique vs 4 de trefle

Score : 4 a 6

Valet de carreau vs 3 de carreau

Score : 5 a 6

9 de trefle vs 3 de coeur

Score : 6 a 6

4 de pique vs 5 de pique

Score : 6 a 7

Roi de carreau vs 10 de carreau

Score : 7 a 7

10 de coeur vs 4 de carreau

Score : 8 a 7

2 de trefle vs 9 de carreau

Score : 8 a 8

2 de pique vs 3 de trefle

Score : 8 a 9

As de coeur vs 10 de trefle

Score : 9 a 9

8 de carreau vs Dame de trefle

Score : 9 a 10

5 de carreau vs Valet de coeur

Score : 9 a 11

7 de coeur vs Roi de trefle

Score : 9 a 12

Roi de pique vs Dame de coeur

Score : 10 a 12

As de pique vs 6 de coeur

Score : 11 a 12

Dame de pique vs Roi de coeur

Score : 11 a 13

8 de trefle vs 6 de pique

Score : 12 a 13

As de carreau vs 8 de pique

Score : 13 a 13

EGALITE !

-------------------
NOUVELLE BATAILLE
-------------------

Score : 0 a 0

Dame de trefle vs As de pique

Score : 0 a 1

Roi de pique vs 2 de trefle

Score : 1 a 1

3 de coeur vs Valet de pique

Score : 1 a 2

6 de carreau vs Dame de carreau

Score : 1 a 3

8 de carreau vs 6 de trefle

Score : 2 a 3

Valet de trefle vs 4 de coeur

Score : 3 a 3

8 de coeur vs 4 de pique

Score : 4 a 3

Roi de trefle vs 8 de pique

Score : 5 a 3

5 de pique vs 7 de trefle

Score : 5 a 4

Roi de coeur vs 9 de carreau

Score : 6 a 4

As de carreau vs As de trefle

Score : 6 a 4

3 de pique vs Valet de carreau

Score : 6 a 5

9 de trefle vs 5 de carreau

Score : 7 a 5

Dame de pique vs 5 de trefle

Score : 8 a 5

10 de coeur vs 6 de pique

Score : 9 a 5

As de coeur vs 4 de carreau

Score : 10 a 5

Dame de coeur vs 7 de carreau

Score : 11 a 5

8 de trefle vs 6 de coeur

Score : 12 a 5

9 de coeur vs 9 de pique

Score : 12 a 5

2 de carreau vs 7 de pique

Score : 12 a 6

2 de coeur vs 3 de carreau

Score : 12 a 7

Roi de carreau vs 10 de pique

Score : 13 a 7

4 de trefle vs 5 de coeur

Score : 13 a 8

2 de pique vs 7 de coeur

Score : 13 a 9

10 de carreau vs Valet de coeur

Score : 13 a 10

3 de trefle vs 10 de trefle

Score : 13 a 11


JOUEUR 1 gagne !



------------------------
STATISTIQUES JOUEUR 1 : 
Nombre de victoires : 2
Score cumule : 44
Score moyen : 11.
Taux de victoires : 50.%


------------------------
STATISTIQUES JOUEUR 2 : 
Nombre de victoires : 1
Score cumule : 49
Score moyen : 12.25
Taux de victoires : 25.%


Nombre d'egalites : 1 *)
