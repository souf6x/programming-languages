(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* string * string list -> (string list) option *)
(* consumes string and string list and produce NONE if the string not in the list otherwise produce SOME list of string without the given string*)

fun all_except_option (s, xs) =
    let fun except_option (xs, acc) =
	    case xs of
		[] => NONE
	      | x :: xs' => if same_string(s, x) then SOME(acc @ xs') else except_option(xs', x :: acc)
    in
	except_option(xs, [])
    end
			 

(* (string list) list * string -> string list *)
(*produce a list of string that containe the substitution of given string if not exist than return empty list *)

fun get_substitutions1 (xxs, s) =
    case xxs of
        [] => [] 
      | xs :: xxs' => let val x = all_except_option(s, xs)
			  val ans_tl = get_substitutions1(xxs', s)
		      in
			  case x of
			      NONE => [] @ ans_tl
			   |  SOME t => t @ ans_tl
		      end
					  
(* (string list) list * string -> string list *)
(* like subtitutions1 but use tail recursion and local helper funtion*)

fun get_substitutions2 (xxs, s) =
    let
	fun option_to_list (x, xxs') =
	    case x of
		NONE => [] @ get_substitutions2(xxs', s)
	     |  SOME t => t @ get_substitutions2(xxs', s)
    in
	case xxs of
	    [] => []
	 |  xs :: xxs' => option_to_list(all_except_option(s, xs), xxs')
    end


(* (string list) list * {first: string, middle: string, last: string} -> {first: string, middle: string, last: string} *)
(* consumes a list of list of string and record contain 3 names first middle and last and prduce  a list of records that contain 3 names first middle and last of substitutions *)

fun similar_names (xxs, {first= f, middle= m, last= l}) =
    let
	val x = get_substitutions2(xxs, f)
	fun names (xs) =
	    case xs of
		[] => []
	     |  x :: xs' => {first= x, middle= m, last= l} :: names(xs')
								  
    in
	case x of
	    [] => [{first= f, middle= m, last= l}]
         |  _  => names(f :: x)
    end
	

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* card -> color *)
(* produce the color of give card *)

fun card_color (x, s) =
    case x of
	(Clubs |  Spades) => Black
     |  _  => Red

(* card -> int *)
(* produce the number of card aces should be 11 and everything else 10 *)

fun card_value (x, s) =
    case s of
	Ace => 11
    |  Num t => t
    |  _ => 10 
					   
     
(* card list * card * exn -> card list *)
(* produce a card list of cards and remove the given card from the card list if the card accure multipke times remove the first one if not exist in list rise an exception *)

fun remove_card (cs, c, e) =
    let
	(*acc : card list *)
	fun removing_cards (cs,c, acc) =
	    case cs of
		[] => raise e
	      | s :: cs' => if s = c
			    then acc @ cs'
			    else removing_cards(cs',c, s :: acc)



    in
	removing_cards(cs,c, []) 
    end


(* card list -> bool *)
(*produce true if all the cards in the list have same color false otherwise *)

fun all_same_color (cs) =
    let
	fun same_color (cs, c) =
	    case cs of
		[] => true
	      | s :: cs' => if card_color(s) = card_color(c)
			    then same_color(cs', c)
			    else false

    in
	case cs of
	    [] => true
	 |  c :: cs' => same_color(cs', c)
    end

(* card list -> int *)
(* produce the sum of values of the given cards *)

fun sum_cards (cs) =
    let
	 
	fun sum_cards1 (cs, acc) =
	    case cs of
		[] => acc
	     |  c :: cs' => sum_cards1(cs', acc + card_value(c))
    in
	sum_cards1(cs,0)
    end

	
(* card list * int -> int *)
(* produce the score while the score works as follow :
   	       (* let sum is the sum of values in the cards
	       	      	     (* if sum > goal
			     	then score = 3 * (sum - goal)
				else preliminary_score = goal - sum
			        (* if (*the color of all cards arnt the same*)
				   the  score = preliminary_score
				   else score = preliminary_score div 2*)*)*)*)

fun score (cs, goal) =
    let
	val sum = sum_cards(cs)
	val preliminary_score = goal - sum
	val score2 = preliminary_score div 2
	 
	
    in
	if sum > goal
	then 3 * (sum - goal)
	else if all_same_color(cs)
	     then score2
	     else preliminary_score
    end

(* card list * move list * int -> int *)
(* produce the score at the end it works as follow:
   	       (*the player has a list of held-cards initially empty.
	       	     (*the player make a move acording the given move list 
		     	   (*drawing: which mean removing the first card from given card list and adding it to held-cards *)
			   (*discrading card: mean choosing a card to remove and adding it to held-card*)
		     *)
		     the game end either when ther is no more moves or when the sum of values of held-card > goal*)*)
  
	
fun officiate (cs, ms, goal) =
    let
	fun game (cs, ms, acc) =
	    case ms of
		[] => score(acc, goal)
	     |  m :: ms' => case cs of
				[] => score(acc, goal)
			      | c :: _ =>  if sum_cards(acc)> goal
					     then score(acc, goal)
					   else  case m of
						       Discard t =>  game(remove_card(cs,t , IllegalMove), ms', t :: acc)
					              | _  =>   game(remove_card(cs, c, IllegalMove), ms', c :: acc)
						  


    in
	game(cs, ms, [])
    end




	
		

