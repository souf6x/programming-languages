(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)


(* string list -> string list *)
(* produce a string list that contain only the words that start with upercase *)

val only_capitals  =
    List.filter(fn s => (Char.isUpper o String.sub)(s, 0))

(* string list  -> string *)
(* produce the longest string in the list if the list is empty return "" when its tie return, the closest to the begining *)

val longest_string1 =
    List.foldl(fn (k ,s) => if String.size (s) < String.size (k)
			    then k
			     else s) ""

(* string list  -> string *)
(* produce the longest string in the list if the list is empty return "" when its tie return, the closest to the end *)

val longest_string2 =
    List.foldl(fn (k ,s) => if String.size (s) <= String.size (k)
      		            then k
			    else s) ""


(* (int * int -> bool) -> string list -> string *)
(* like longest string1 and 2 but more general *)
fun longest_string_helper f =
    List.foldl (fn (s, acc) => if f(String.size s, String.size acc)
			       then s
			       else acc) ""
    
(* string list  -> string *)
(* like longest_string1 but using longest_string_helper *)
val longest_string3 =
    longest_string_helper(fn (k, s) => s < k)



(* string list  -> string *)
(* like longest_string2 but using longest_string_helper *)
val longest_string4 =
    longest_string_helper(fn (k, s) => s <= k)


(* string list -> string *)
(* produce the longest word that start with capital if the is no word start with capital letter fproduce "" *)			 
val longest_capitalized  =
    longest_string1 o only_capitals


(* string -> string *)
(* produce a reverse of given string *)
			  
val rev_string =
    String.implode o List.rev o String.explode



fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | s :: xs' => case (f s) of
			SOME v => v
		     | _  => first_answer f xs'



fun all_answers f lst =
    let
	fun all_answers_helper remaining acc = 
	    case (remaining, acc) of
		([], _) => acc
	      | (x :: xs, SOME v) => (case f x of
					  NONE => NONE
					| SOME xv => all_answers_helper xs (SOME (xv @ v)))
	      | _ => NONE
    in
	all_answers_helper lst (SOME [])
    end




val count_wildcards =
    g (fn _ => 1) (fn _ => 0)


val count_wild_and_variable_lengths =
    g (fn _ => 1) (fn x => String.size x)


fun count_some_var (s, p) =
    g (fn _ => 0) (fn x => if x = s
		           then 1
			   else 0) p


fun check_pat p =
    let
			 
	fun all_pat p acc =
	    case p of
		Variable s => s :: acc
	      | TupleP xp => List.foldl (fn (p, x) => (all_pat p acc) @ x) acc xp
	      | ConstructorP (_, p) => all_pat p acc 
	      | _  => acc
      
        fun is_all_pat xs =
	    case xs of
		[] => true
	      | s :: xs' => if (List.exists (fn x => x = s) xs')
			    then false
			    else is_all_pat xs'			   

    in
	is_all_pat (all_pat p [])
    end
	


fun match (v, p) =
    case p of
	Wildcard => SOME []
     |  Variable s => SOME [(s, v)]
     | UnitP  => if v = Unit
		 then SOME []
	         else NONE
     | ConstP i => (case v of
		       Const c => if i = c
				  then SOME []
				  else NONE
		     | _  => NONE )
     | TupleP ps =>( case v of
			Tuple vs => if List.length(ps) = List.length(vs)
				    then all_answers match (ListPair.zip (vs, ps))
				    else NONE
		      | _  => NONE)
     | ConstructorP (s1, p) => (case v of
				   Constructor (s2, v) => if s1 = s2
							  then match (v, p)
							  else NONE
				 | _ => NONE)


fun first_match v plst =
    SOME (first_answer (fn p => match (v, p)) plst)
    handle NoAnswer => NONE
