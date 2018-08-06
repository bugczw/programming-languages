(*
1. This problem involves using first-name substitutions to come up with alternate names. For example,
Fredrick William Smith could also be Fred William Smith or Freddie William Smith. Only part (d) is
specifically about this, but the other problems are helpful.
*)





(*
(a) Write a function all_except_option, which takes a string and a string list. Return NONE if the
string is not in the list, else return SOME lst where lst is identical to the argument list except the string
is not in it. You may assume the string is in the list at most once. Use same_string, provided to you,
to compare strings. Sample solution is around 8 lines.
*)
fun same_string(s1 : string, s2 : string) =
    s1 = s2	
	
fun exist_in_list (str : string, strlist : string list) =
    case strlist of
	[] => false
	| head::tail =>  
	    if same_string(str, head)
	    then true
        else exist_in_list(str, tail)
	
fun elements (str : string, strlist : string list) =
    case strlist of
	   [] => []
	   | head::tail => 
	      let val temp= elements(str, tail) in
	        if(same_string(head,str))
		    then temp
		    else head::temp
		  end

fun all_except_option (str : string, strlist : string list) =
    if(exist_in_list(str, strlist))
	then SOME (elements(str,strlist))
	else NONE
	  
	  
	  
	  
  
(*
(b) Write a function get_substitutions1, which takes a string list list (a list of list of strings, the
substitutions) and a string s and returns a string list. The result has all the strings that are in
some list in substitutions that also has s, but s itself should not be in the result. Example:
get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
"Fred")
(* answer: ["Fredrick","Freddie","F"] *)
Assume each list in substitutions has no repeats. The result will have repeats if s and another string are
both in more than one list in substitutions. Example:
get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
"Jeff")
(* answer: ["Jeffrey","Geoff","Jeffrey"] *)
Use part (a) and ML’s list-append (@) but no other helper functions. Sample solution is around 6 lines.
*)

fun get_substitutions1 (strlist2 : string list list, s : string) =
    case strlist2 of
	[] => []
    | (head::tail) => 
	   (let val temp = get_substitutions1(tail, s) 
		in
	        case all_except_option(s,head) of
		    SOME v => v @ temp
            | NONE => temp
		end)


(*
(c) Write a function get_substitutions2, which is like get_substitutions1 except it uses a tail-recursive
local helper function.
*)
fun get_substitutions2 (strlist2 : string list list, s : string) =
    let fun re_get_sub(strlist2,s,ans) =
	        case strlist2 of
	        [] => ans
	        |(head::tail) =>
				     (case all_except_option(s,head) of
					  SOME v => re_get_sub(tail,s,ans @ v)
                      | NONE => re_get_sub(tail,s,ans))
    in  re_get_sub(strlist2,s,[])
	end


(*
(d) Write a function similar_names, which takes a string list list of substitutions (as in parts (b) and
(c)) and a full name of type {first:string,middle:string,last:string} and returns a list of full
names (type {first:string,middle:string,last:string} list). The result is all the full names you
can produce by substituting for the first name (and only the first name) using substitutions and parts (b)
or (c). The answer should begin with the original name (then have 0 or more other names). Example:
similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
{first="Fred", middle="W", last="Smith"})
(* answer: [{first="Fred", last="Smith", middle="W"},
{first="Fredrick", last="Smith", middle="W"},
{first="Freddie", last="Smith", middle="W"},
{first="F", last="Smith", middle="W"}] *)
Do not eliminate duplicates from the answer. Hint: Use a local helper function. Sample solution is
around 10 lines.	
*)			
type full_name = { first : string,  middle : string,  last : string }

fun similar_names (strlist2 : string list list, fname : full_name) =
   let
       val {first=f, middle=m, last=l} = fname
   in 
       (let fun map({first=nf, middle=nm, last=nl}, strlist : string list ) =
	           case strlist of
			   [] => []
			   | head::tail => {first=head, middle=nm, last=nl} :: map({first=nf, middle=nm, last=nl},tail)
	   in
	       fname :: map(fname, get_substitutions1(strlist2, f))
       end) 
   end


   
   
   
(*
2. This problem involves a solitaire card game invented just for this question. You will write a program that
tracks the progress of a game; writing a game player is a challenge problem. You can do parts (a)–(e) before
understanding the game if you wish.
A game is played with a card-list and a goal. The player has a list of held-cards, initially empty. The player
makes a move by either drawing, which means removing the first card in the card-list from the card-list and
adding it to the held-cards, or discarding, which means choosing one of the held-cards to remove. The game
ends either when the player chooses to make no more moves or when the sum of the values of the held-cards
is greater than the goal.
The objective is to end the game with a low score (0 is best). Scoring works as follows: Let sum be the sum
of the values of the held-cards. If sum is greater than goal, the preliminary score is three times (sum−goal),
else the preliminary score is (goal − sum). The score is the preliminary score unless all the held-cards are
the same color, in which case the score is the preliminary score divided by 2 (and rounded down as usual
with integer division; use ML’s div operator).
*)  	
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank
datatype color = Red | Black
datatype move = Discard of card | Draw 			 

				 
				 
				 
(*
(a) Write a function card_color, which takes a card and returns its color (spades and clubs are black,
diamonds and hearts are red). Note: One case-expression is enough
*)
fun card_color (s : suit , _ :rank) =
    case s of
	(Clubs | Spades) => Black
      | _  => Red 

	  
	  
(*
(b) Write a function card_value, which takes a card and returns its value (numbered cards have their
number as the value, aces are 11, everything else is 10). Note: One case-expression is enough.
*)
fun card_value(_ :suit, r:rank) =
    case r of
	Num(v) => v
	| Ace => 11
	| _ => 10

	
(*	
(c) Write a function remove_card, which takes a list of cards cs, a card c, and an exception e. It returns a
list that has all the elements of cs except c. If c is in the list more than once, remove only the first one.
If c is not in the list, raise the exception e. You can compare cards with =.
*)
exception IllegalMove
fun remove_card (cs : card list, c : card, e) = 
    case cs of
    [] => raise e
    | head::tail => if (head = c ) 
		      then tail
		      else  head :: remove_card(tail, c, e)

			  
(*
(d) Write a function all_same_color, which takes a list of cards and returns true if all the cards in the
list are the same color. Hint: An elegant solution is very similar to one of the functions using nested
pattern-matching in the lectures.
*)
fun all_same_color (cs : card list ) =
    case cs of
	[] => true
	| _::[] => true
	| head::(neck::rest) => (card_color(head) = card_color(neck)) andalso all_same_color(neck::rest)

	
(*
(e) Write a function sum_cards, which takes a list of cards and returns the sum of their values. Use a locally
defined helper function that is tail recursive. (Take “calls use a constant amount of stack space” as a
requirement for this problem.)
*)
fun sum_cards(cs : card list) =
   let fun temp_sum_cards(newcs : card list, num : int) =
           case newcs of
		   [] => num
		   | head::rest => temp_sum_cards(rest , num + card_value(head))
   in
       temp_sum_cards(cs, 0)
   end

   
(*
(f) Write a function score, which takes a card list (the held-cards) and an int (the goal) and computes
the score as described above.
*)
fun score(cs : card list, goal : int) =
    let val sum = sum_cards(cs)
	in
	    let val tempgoal = if (sum > goal) then 3 * (sum - goal) else (goal - sum)
		in
		    if (all_same_color cs) then tempgoal div 2 else tempgoal
		end
	end
	
	
	
(*
(g) Write a function officiate, which “runs a game.” It takes a card list (the card-list) a move list
(what the player “does” at each point), and an int (the goal) and returns the score at the end of the
game after processing (some or all of) the moves in the move list in order. Use a locally defined recursive
helper function that takes several arguments that together represent the current state of the game. As
described above:
• The game starts with the held-cards being the empty list.
• The game ends if there are no more moves. (The player chose to stop since the move list is empty.)
• If the player discards some card c, play continues (i.e., make a recursive call) with the held-cards
not having c and the card-list unchanged. If c is not in the held-cards, raise the IllegalMove
exception.
• If the player draws and the card-list is (already) empty, the game is over. Else if drawing causes
the sum of the held-cards to exceed the goal, the game is over (after drawing). Else play continues
with a larger held-cards and a smaller card-list.
Sample solution for (g) is under 20 lines.
*)
fun officiate (cards : card list, moves : move list, goal : int) = 
    let
	fun temp_officiate (cards : card list, held : card list, moves : move list, goal : int) =
	    case moves of
		[] => score (held, goal)
		| (Discard d)::ms =>
		    temp_officiate (cards, remove_card (held, d, IllegalMove), ms, goal)
	    | Draw::ms =>
		    (case cards of
			[] => score (held, goal)
		    |c::cs =>
			        (let val held' = c::held
				     in
		                if (sum_cards (held') > goal)
						then score (held', goal)
						else temp_officiate (cs, held', ms, goal)
					 end))
    in
	    temp_officiate (cards, [] , moves, goal)
    end
	
	
	

	
	
(*
3. Challenge Problems:
*)


(*
(a) Write score_challenge and officiate_challenge to be like their non-challenge counterparts except
each ace can have a value of 1 or 11 and score_challenge should always return the least (i.e., best)
possible score. (Note the game-ends-if-sum-exceeds-goal rule should apply only if there is no sum that
is less than or equal to the goal.) Hint: This is easier than you might think.
*)
fun new_card_value(_ :suit, r:rank) =
    case r of
	Num(v) => v
	| Ace => 1
	| _ => 10

fun new_sum_cards(cs : card list) =
   let fun temp_new_sum_cards(newcs : card list, num : int) =
           case newcs of
		   [] => num
		   | head::rest => temp_new_sum_cards(rest , num + new_card_value(head))
   in
       temp_new_sum_cards(cs, 0)
   end
	
	
fun new_score(cs : card list, goal : int) =
    let val sum = new_sum_cards(cs)
	in
	    let val tempgoal = if (sum > goal) then 3 * (sum - goal) else (goal - sum)
		in
		    if (all_same_color cs) then tempgoal div 2 else tempgoal
		end
	end
	
fun score_challenge (cs : card list, goal : int) =
    let val score1 = score(cs, goal) 
     	val score2 =new_score(cs, goal)
	in
	     if(score1 < score2) then score1 else score2
	end
	
fun officiate_challenge (cards : card list, moves : move list, goal : int) = 
    let
	fun temp_officiate_challenge (cards : card list, held : card list, moves : move list, goal : int) =
        case moves of
		[] => score_challenge (held, goal)
		| (Discard d)::ms =>
		    temp_officiate_challenge (cards, remove_card (held, d, IllegalMove), ms, goal)
	    | Draw::ms =>
		    (case cards of
			[] => score_challenge (held, goal)
		    |c::cs =>
			        (let val held' = c::held
				     in
		                if ((sum_cards (held') > goal) andalso (new_sum_cards (held') > goal))
						then score_challenge (held', goal)
						else temp_officiate_challenge (cs, held', ms, goal)
					 end))
    in
	    temp_officiate_challenge (cards, [] , moves, goal)
    end

(*
(b) Write careful_player, which takes a card-list and a goal and returns a move-list such that calling
officiate with the card-list, the goal, and the move-list has this behavior:
• The value of the held cards never exceeds the goal.
• A card is drawn whenever the goal is more than 10 greater than the value of the held cards. As a
detail, you should (attempt to) draw, even if no cards remain in the card-list.
• If a score of 0 is reached, there must be no more moves.
• If it is possible to reach a score of 0 by discarding a card followed by drawing a card, then this
must be done. Note careful_player will have to look ahead to the next card, which in many card
games is considered “cheating.” Also note that the previous requirement takes precedence: There
must be no more moves after a score of 0 is reached even if there is another way to get back to 0.
*)
