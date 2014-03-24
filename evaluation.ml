(**
   Evaluateur de phrases pour mini CAML
   Evaluation par valeurs. Liaison dynamique.

   Necessite les modules [Lexical] et [Syntaxe].

   Compilation
   - [ocamlc -c lexical.cmo syntaxe.cmo evaluation.ml{i}]

   @author Yaker Mahieddine et Mickael Flament
*)

(* directive pour ne pas prefixer toutes les fonctions du module 
   Syntaxe
*)
open Syntaxe
open Lexical

type valeur = 
    Valeur of Syntaxe.expression 
  | Liaison of (string * valeur) 
  | Aucune

and environnement = (string * valeur) list

exception Variable_non_liee of string
exception Condition_incorrecte
exception Application_impossible
exception Typage_incorrect

(* avant changement, le parametre est generique, ce qui est normal dans le cas ou on a pas utilisé ce parametre dans le corps de la fonction 
*)
let rec eval_expr = fun env -> function
  |Expr_int n ->  Valeur (Expr_int n) 
  |Expr_bool b -> Valeur (Expr_bool b)
  |Expr_var v ->
     begin
       try
	 List.assoc v env   
       with |_ ->
	 raise (Variable_non_liee v)
     end
  |Expr_fonc (s,e) -> Valeur (Expr_fonc (s,e)) 
  |Expr_binaire (op,e1,e2) -> 
     let v2 = (eval_expr env e2) 
     in
     let v1 = (eval_expr env e1) 
     in
       begin
	 
	   match op,v1,v2 with
	     |Add,Valeur (Expr_int n1), Valeur(Expr_int n2)  -> Valeur (Expr_int(n1 + n2))
	     |Sub,Valeur (Expr_int n1), Valeur(Expr_int n2)  -> Valeur (Expr_int(n1 - n2))
	     |Mult,Valeur (Expr_int n1), Valeur(Expr_int n2)  -> Valeur (Expr_int(n1 * n2))
	     |Div,Valeur (Expr_int n1), Valeur(Expr_int n2)  -> Valeur (Expr_int(n1 / n2))
	     |And,Valeur (Expr_bool n1), Valeur(Expr_bool n2)  -> Valeur (Expr_bool(n1 && n2))
	     |Or,Valeur (Expr_bool n1), Valeur(Expr_bool n2)  -> Valeur (Expr_bool(n1 || n2))
	     |Eq,Valeur (Expr_int n1), Valeur(Expr_int n2)  -> Valeur (Expr_bool(n1 == n2))
	     |Inf,Valeur (Expr_int n1), Valeur(Expr_int n2)  -> Valeur (Expr_bool(n1 <= n2))  	     
	     | _ -> raise(Typage_incorrect)
       end
  |Expr_unaire (op,e1) ->
     begin
       
	 let v = (eval_expr env e1) in
	     match op,v with
	       |Moins,Valeur (Expr_int n) -> Valeur (Expr_int (-n))
	       |Not,Valeur (Expr_bool n) -> Valeur (Expr_bool (not n))
	       | _ ->raise(Typage_incorrect)
     end     
  |Expr_cond (e1,e2,e3) ->
    let v1 = (eval_expr env e1)
    in
    begin
      match v1 with
      |Valeur (Expr_bool true) -> eval_expr env e2
      |Valeur (Expr_bool false) -> eval_expr env e3
      |_ -> raise(Condition_incorrecte)
    end
  |Expr_appl (e1,e2) ->
    let v2 = eval_expr env e2 in
    let v1 = eval_expr env e1 in
    begin
      match v1 with
      |Valeur(Expr_fonc(x,e)) ->
  	begin
	  eval_expr ((x,v2)::env) e
  	end
      |_ -> raise(Application_impossible)
    end
  |_ -> Aucune 
     

let eval = fun env p ->
  match p with
    | Decl (Decla (x,e)) -> 
	let v = eval_expr env e in
	  ((x, v)::env, Liaison (x, v))
    | Expr e -> 
      let v = eval_expr env e in
   
      (env,v)
    | Vide -> (env, Aucune)
