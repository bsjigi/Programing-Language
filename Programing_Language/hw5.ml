(* 
 * hw5.ml
 * Feel free to define any helper functions.
 * You don't need to define a recursive function if possible.
 *)

exception NotImplemented

(**********************************)
(*            Problem 1           *)
(**********************************)
module Problem1 = struct
  open Lambda
    
  let rec check : exp -> bool =
    fun e ->
	let rec chk e scp =
		match e with
		|Var x ->
		begin
			try 
				(fun _ -> true) @@ List.find (fun y -> if x =y then true else false) scp
			with
				_ -> false
		end
		|Lambda(x,l) -> chk l (x::scp)
		|App (l1,l2) -> 
			chk l1 scp && chk l2 scp
	in
	chk e [] 

end  
						
		
	
	


(**********************************)
(*            Problem 2           *)
(**********************************)
module Problem2 = struct
  open Kml
  exception Error
  
 let rec eval : exp -> env -> value
=fun exp env ->
match exp with
|NUM n -> Int n
|VAR x -> apply_env env x
|ADD(e1,e2) ->
let v1 = eval e1 env in let v2 = eval e2 env in (match v1,v2 with
| Int n1, Int n2 -> Int (n1 + n2)
|_ -> raise (Error))
|SUB (e1,e2) ->
let v1 = eval e1 env in let v2 = eval e2 env in (match v1,v2 with
| Int n1, Int n2 -> Int (n1 - n2)
|_ -> raise (Error))
|MUL (e1,e2) ->
let v1 = eval e1 env in let v2 = eval e2 env in (match v1,v2 with
| Int n1, Int n2 -> Int(n1 * n2)
|_ -> raise(Error))
|DIV (e1,e2) ->
let v1 = eval e1 env in let v2 = eval e2 env in (match v1,v2 with
| Int n1, Int n2 -> Int(n1 / n2)
|_ -> raise(Error))
|UMINUS e -> (match eval e env with
|Int n -> Int (-n)
|_ -> raise(Error))
|EQ (e1,e2) -> Bool (e1 = e2)
|LE (e1,e2) -> Bool (e1 <= e2)
|LT (e1,e2) -> Bool (e1 < e2)
|GE (e1,e2) -> Bool (e1 >= e2)
|GT (e1,e2) -> Bool (e1 > e2) 
|NOT e -> (match eval e env with
|Bool p -> Bool (not p)
|_ -> raise(Error)) 
|OR _ -> raise(Error) 
|AND _ -> raise(Error)
|ISZERO e -> (match eval e env with 
|Int n when n = 0 -> Bool true 
| _ -> Bool false)
|IF (e1,e2,e3) -> (match eval e1 env with
|Bool true -> eval e2 env
|Bool false -> eval e3 env
|_ -> raise (Error))
|LET(x,e1,e2) -> let v1 = eval e1 env in eval e2 (extend_env (x,v1) env)
|LETREC(y,x,e1,e2) -> eval e2 (extend_env(y, RecClosure(y,x,e1,env)) env)
|FUN (x,e) -> Closure (x,e,env)
|APP (e1,e2) -> match eval e1 env with
	|Closure (x,e,env2) ->(match eval e2 env with
				|v2 -> eval e (extend_env(x,v2) env2)) 
	|RecClosure (y,x,e,env2) -> (match eval e2 env with
				|v2 -> eval e (extend_env(x,v2) (extend_env (y,RecClosure(y,x,e1,env2)) env2)))
	|_ -> raise(Error)

let run : program -> value
	= fun pgm -> eval pgm empty_env
	


end

(***********************************)
(*            Problem 3            *)
(***********************************)
module Problem3 = struct
  open Nameless
  exception CannotTranslate

  let rec compare v l idx = match l with
   [] -> raise(CannotTranslate) 
   |hd::tl -> if(hd=v) then idx else compare v tl idx+1

	
  let rec translate2 : program -> string list -> nl_program =
    fun e l -> match e with
	NUM n -> NL_NUM n
	|VAR v -> NL_VAR(compare v l 0)
	|ADD (e1,e2) -> NL_ADD (translate2 e1 l, translate2 e2 l)
	|SUB (e1,e2) -> NL_SUB (translate2 e1 l , translate2 e2 l)
	|MUL (e1,e2) -> NL_MUL (translate2 e1 l , translate2 e2 l)
	|DIV (e1,e2) -> NL_DIV (translate2 e1 l , translate2 e2 l)
	|UMINUS e -> NL_UMINUS (translate2 e l)
	|EQ (e1,e2) -> NL_EQ (translate2 e1 l , translate2 e2 l)
	|LE (e1,e2) -> NL_LE (translate2 e1 l , translate2 e2 l)
	|LT (e1,e2) -> NL_LT (translate2 e1 l, translate2 e2 l)
	|GE (e1,e2) -> NL_GE (translate2 e1 l, translate2 e2 l)
	|GT (e1,e2) -> NL_GT (translate2 e1 l, translate2 e2 l)
	|NOT e -> NL_NOT (translate2 e l)
	|OR (e1,e2) -> NL_OR (translate2 e1 l, translate2 e2 l)
	|AND (e1,e2) -> NL_AND (translate2 e1 l, translate2 e2 l)
	|ISZERO e -> NL_ISZERO (translate2 e l)
	|IF (e1,e2,e3) -> NL_IF (translate2 e1 l, translate2 e2 l, translate2 e3 l)
	|LET (x,e2,e3) -> let p = x::l in
				NL_LET(translate2 e2 l, translate2 e3 p)
	|FUN (x,e) -> let p = x::l in
			NL_FUN (translate2 e p)
	|APP (e1,e2) -> NL_APP (translate2 e1 l, translate2 e2 l)

let rec translate : program -> nl_program =
 fun pgm -> translate2 pgm []
end

(**********************************)
(*            Problem 4           *)
(**********************************)
module Problem4 = struct
  open Nameless
  exception Error

    

  let rec nl_run : nl_program -> nl_value =
    fun pgm -> NL_Int 0
end
