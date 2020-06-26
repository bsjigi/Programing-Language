let sum n =
  if n > 0 then n*(n+1)/2 else 0;;
sum;;
sum(-10);;
sum(100);;

let circle r =

  if r > 0.0 then (r*.r*.3.14) else  0.0;;

circle;;
circle (-10.1);;
circle 4.2;;

let concat s =  "Hello " ^ s;;
concat;;
concat "Bob!";;
concat "Alice!";;

let xor x y = 
  if x = y then false else true;;
xor;;
xor true true ;;
xor true false;;
xor false true;;
xor false false;;

let triangle x y z = 
  if (x > 0 && y > 0 && z > 0) && (x+y > z && y+z > x && z+x > y) then true else false;;
triangle;;
triangle (-3) 3 1;;
triangle 3 4 5;;
triangle 100 1 2;;

let int_if_then_else b x y = 
  if b = true then x + y else x - y;;
int_if_then_else true 2 100;;
int_if_then_else (100 < 2) 2 (-2);;


let sum_of_fun_val a b c d e = 
  ((a*(d*d)) + (b*d) + c) + ((a*(e*e)) + (b*e) + c);;
sum_of_fun_val;;
sum_of_fun_val 1 2 1 3 4;;
sum_of_fun_val 1 (-3) (-1) 200 123;;

let square x = x * x;;
let comp3 a b c d = 
  a*square(a*square(a*(square d) + b*d + c) + b*(a*(square d) + b*d + c) +c) + b*(a*square(a*(square d) + b*d + c) + b*(a*(square d) + b*d + c) +c) + c;;


comp3;;
comp3 1 1 1 1;;
comp3 1 (-2) 1 3;;

let string2 s = s ^ s;;
string2;;
string2 "hi";;
string2 "abcde";;


let string256 s = s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s;;
string256;;
string256 "a";;
string256 "ab";;
