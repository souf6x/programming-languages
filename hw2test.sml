(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw2provided.sml";



val test1_aeo = all_except_option ("string", ["string"]) = SOME []
val test2_aeo = all_except_option ("hi", []) = NONE
val test3_aeo = all_except_option ("hello", ["string", "hello", "hi"]) = SOME ["string", "hi"]
val test4_aeo = all_except_option ("hello", ["string", "hi"]) = NONE

val test1_s1 = get_substitutions1([[]], "hey") = []
val test2_s1 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test3_s1 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") = ["Fredrick","Freddie","F"]
val test4_s1 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff") = ["Jeffrey","Geoff","Jeffrey"]

val test1_s2 = get_substitutions2([[]], "hey") = []
val test2_s2 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test3_s2 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") = ["Fredrick","Freddie","F"]
val test4_s2 = get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff") = ["Jeffrey","Geoff","Jeffrey"]

val test1_sS = similar_names ([[]], {first="hey", middle="k", last="yeah"}) = [{first="hey", middle="k", last="yeah"}] 
val test2_sS = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]


val test1_cr = card_color (Clubs, Num 2) = Black
val test2_cr = card_color (Spades, Jack) = Black
val test3_cr = card_color (Diamonds, Queen) = Red
val test4_cr = card_color (Hearts, King) = Red 

val test1_ce = card_value (Clubs, Num 2) = 2
val test2_ce = card_value (Spades, Jack) = 10
val test3_ce = card_value (Diamonds, Queen) = 10
val test4_ce = card_value (Hearts, King) = 10
val test5_ce = card_value (Clubs, Ace) = 11

val test1_rc = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

val test2_rc = remove_card ([(Hearts, Ace), (Spades, Jack), (Diamonds, Queen), (Clubs, Num 4)], (Diamonds, Queen), IllegalMove) = [(Spades, Jack), (Hearts, Ace), (Clubs, Num 4)]
val test3_rc = remove_card ([(Hearts, Ace), (Spades, Jack), (Diamonds, Queen), (Diamonds, Queen)], (Diamonds, Queen), IllegalMove) =[(Spades, Jack), (Hearts, Ace),(Diamonds, Queen)]


val test1_asc = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test2_asc = all_same_color [] = true
val test3_asc = all_same_color [(Hearts, Ace), (Clubs, Jack)] = false
val test4_asc = all_same_color [(Clubs, Jack), (Spades, Queen)] = true

val test1_sc = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val test2_sc = sum_cards [(Hearts, Ace), (Spades, Jack), (Diamonds, Queen), (Clubs, Num 4)] = 35
val test3_sc = sum_cards [] = 0

val test1_s = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test2_s = score ([(Hearts, Num 8),(Clubs, Num 4)],10) = 6
val test3_s = score ([(Spades, Num 2),(Clubs, Num 4)], 10) = 2

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 6

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true) 
             
