exception NotImplemented

let rec list_add l1 l2 = 
  match (l1,l2) with
      ([],l2) -> l2
    | (l1,[]) -> l1
    | (i1::l1',i2::l2') -> (i1+i2) :: (list_add l1' l2');;

list_add [1; 2] [3; 4; 5];;

let rec insert m l = 
  match l with 
      [] -> [m]
    | x::xs -> if m < x then m :: x :: xs 
        else x :: (insert m xs);;

insert 3 [1; 2; 4; 5];;

let rec insort l = 
  match l with
      [] -> []
    | x::xs -> insert x (insort xs);;

insort [3; 7; 5; 1; 2];;

let rec ltake l n = 
  if n = 0 then [] else
    match l with
        h::t -> h :: ltake t (n - 1) ;;

ltake [3; 7; 5; 1; 2] 3;;
ltake ["h"; "y"; "e"; "o"; "n"; "s"; "e"; "u"; "n"; "g"] 5;;

let rec lall f l = 
  match l with 
      [] -> true
    | x::xs -> f x && lall f xs;;

lall (fun x -> x > 0) [];;
lall (fun x -> x > 0) [1; 2; 3];;
lall (fun x -> x > 0) [1; -2; 3];;


let rec lmap f l = 
  match l with
      [] -> []
    | x::xs -> f x :: lmap f xs;;
lmap (fun x -> x + 1) [1; 2; 3];;

let rec lfilter p l = 
  let rec lfilter_fuc p l gru = match l with 
      [] -> gru
    | x::xs -> if p x then lfilter_fuc p xs (gru @ [x]) 
        else lfilter_fuc p xs gru
  in lfilter_fuc p l [];;

lfilter (fun x -> x > 2) [0; 1; 2; 3; 4; 5];;

let rec ltabulate n f = 
  let rec ltabulate_fuc n f gru = match n with
      0 -> (f 0) :: gru
    | _ -> ltabulate_fuc (n - 1) f ((f n) :: gru)
  in ltabulate_fuc (n - 1) f [];;

ltabulate 4 (fun x -> x * x);;

let rec lrev l = 
  let rec lrev' out l = match out with 
      [] -> l
    | h::t -> lrev' t (h::l)
  in lrev' l [];;

lrev [1; 2; 3; 4];;

let rec lconcat l = 
  match l with
      [] -> []
    | x :: xs -> x @ lconcat xs;;

lconcat [[1; 2; 3]; [6; 5; 4]; [9]];;

let rec lfoldl f e l = 
  match l with
      [] -> e 
    | x :: xs -> lfoldl f (f (x, e)) xs;;

lfoldl (fun (x, y) -> x - y) 0 [1; 2; 3];;

let rec lzip l1 l2 = 
  match (l1,l2) with
      (x::xlist,y::ylist) -> (x,y) :: lzip xlist ylist
    | _ -> [];;

lzip ["A"; "B"; "C"; "D"] [1; 2; 3; 4; 5; 6];;

let rec split l =
  match l with
      [] -> ([],[])
    | [x] -> ([x],[])
    | x::y::t -> 
        let (t1,t2) = split t
        in (x::t1,y::t2);;

split [1; 3; 5; 7; 9; 11];;

let rec cartprod s t = 
  match s with
      [] -> []
    | x::xs -> 
        let x_sprod = cartprod xs t in 
        let rec pair_x s2 = match s2 with
            [] -> x_sprod
          | y::yt -> (x,y) :: (pair_x yt)
        in pair_x t;;

cartprod [1; 2] [3; 4; 5];;



