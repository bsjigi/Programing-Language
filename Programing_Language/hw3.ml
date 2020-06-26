(* hw3.ml *)

exception NotImplemented

let rec sum n = 
  if n > 0 then n + sum(n - 1) else 0;;
sum 10;;

let rec fac n =
  if n <= 1 then 1 else n * fac (n - 1);;
fac 5;;

let rec fib n = 
  if n < 3 then 1 else fib (n-1) + fib(n-2);;


let rec gcd m n = 
  if n = 0 then m else gcd n (m mod n);;
gcd 15 20;;


let rec max l = 
  match l with
    | [] -> 0
    | x :: [] -> x
    | x :: xm ->
        let v = max xm in
          if x > v then x else v;;

max [5; 3; 6; 7; 4];;

type tree = Leaf of int | Node of int * tree * tree

let rec sum_tree t = 
  match t with
    | Leaf n -> n
    | Node (v, l, r) ->
        v + (sum_tree l) + (sum_tree r);;

sum_tree( Node (7, Node (3, Leaf 1, Leaf 2), Leaf 4));;


let max_num x y = 
  if x > y then x else y;;

let rec depth t = 
  match t with
    | Leaf n -> 0
    | Node (_, l, r) ->
        1 + max_num (depth l) (depth r);;

depth (Node (7, Node (3, Leaf 1, Leaf 2), Leaf 4));;




let rec bin_search t x = 
  match t with
      Leaf n -> x=n
    | Node (n,l,r) -> if x=n then true 
        else if x<n then bin_search l x
        else bin_search r x;;

bin_search (Node(4, Node(2, Leaf 1, Leaf 3), Leaf 7)) 2;;
bin_search (Node(4, Node(2, Leaf 1, Leaf 3), Leaf 7)) 5;;


type exp =
      INT of int
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | MOD of exp * exp

let rec interp e = 
  match e with
      INT n -> n
    | ADD (x,y) -> (interp x) + (interp y)
    | SUB (x,y) -> (interp x) - (interp y)
    | MUL (x,y) -> (interp x) * (interp y)
    | DIV (x,y) -> (interp x) / (interp y)
    | MOD (x,y) -> (interp x) mod (interp y);;


interp (ADD(SUB (INT 100, INT 10), MUL (INT 2, INT 8)));;




type formula =
      True
    | False
    | Neg of formula
    | Or of formula * formula
    | And of formula * formula
    | Imply of formula * formula

let rec eval f = 
  match f with
      True -> True
    | False -> False
    | Neg (x) -> if (eval x) = True then False else True
    | Or (x, y) -> if (eval x) = False && (eval y) = False then False else True
    | And (x, y) -> if (eval x) = True && (eval y) = True then True else False
    | Imply (x, y) -> if (eval x) = True then eval y else True;;

eval (Imply (And (True, Or (True, False)), False));;



