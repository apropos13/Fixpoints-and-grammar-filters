let subset a b = List.for_all (fun c -> List.mem c b) a;;

let equal_sets a b= (subset a b ) && (subset b a);;

let compare x y =
  if x>y then 1 else
  if x<y then -1 else
  0
  ;;

let set_union a b = List.merge compare a b;;


let rec set_intersection a b = match a with
  |[] -> []
  |hd::tl ->
            if List.mem hd b 
            then hd::(set_intersection tl b) 
            else set_intersection tl b
  ;;


let rec set_diff a b = match a with
  |[] -> [] 
  |hd::tl -> if List.mem hd b
             then set_diff tl b 
             else hd :: (set_diff tl b) 
  ;;



let rec computed_fixed_point eq f x = if eq x (f x) then x else computed_fixed_point eq f (f x) ;;

let rec computed_periodic_point eq f p x =
    if p = 0
    then x else if eq x (f (computed_periodic_point eq f (p-1) (f x))) 
    then x else computed_periodic_point eq f p (f x);;  


(*FILTER BLIND RULES *)

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(*Decides if a symbol is terminal or non terminal*)
let t_or_nt s = match s with 
  | T  t_var -> true
  | N  nt_var-> false
  |_->false 
;;

(*Decides if a list is exclusively made out of terminal symbols*)

let is_terminal_list li = 
  if List.for_all ( fun c-> t_or_nt c ) li then true else false ;;

(* determines if a rule has only terminal sumbols*)
let  is_terminal_rule rule = match rule with 
    |_,li -> if is_terminal_list li then true else false ;;


(*In a list of rules, find those consisting of terminal strings and add them to them to list b *)

(*First did the mistake to have hd::b; add_rules tl b ; *)
let rec add_rules rules b= match rules with
  |[]->[]
  |hd::tl -> (match hd with 
    |(l,r)->if (is_terminal_rule hd) then hd::add_rules tl b else add_rules tl b) ;;

(*takes the first elements of the list of unions and returns another list with just these elements*)
let rec take_first li_of_uni newlist = match li_of_uni with 
  |[]->[]
  |hd::tl -> (match hd with 
    |(l,r)-> l::take_first tl newlist ) ;;



(*Extracts the symbol N (and discards T symbols) from  the list i.e. [N Expr; N Lvalue; T "j"] gives [Expr; Lvalue] *)

let rec take_second li newlist = match li with 
  |[]->[]
  |hd::tl -> (match hd with 
    |N nt_var -> nt_var :: take_second tl newlist
    |T t_var -> take_second tl newlist )
;;

(*Adds the rhs of a to b iff the rhs of a exists in the lhs of b*)
(*For each element of r check if it belongs already in b, then add the left side l to b*)
(*ASSUMES THAT add_rule has already been called on b*)
let rec old_to_new a b= match a with 
  |[]->[]
  |hd::tl -> (match hd with 
    |(l,r) ->  if  subset (take_second r []) (take_first b [])  then hd::old_to_new tl b else old_to_new tl b )  ;;





(*Need to check if old_to_new added a new rule to b , if that is the case run the old_to_new again*)
(*THIS IS ASSUMING THE add_rules has been called previously*)
let rec filter g b= match g with 
    |(start, rules)-> computed_fixed_point (=) (fun x-> old_to_new rules x) b  ;;


(*Calls add rules before calling filter*)
let filter_blind_alleys_allias g b= match g with 
  |(start, rules)-> filter g (add_rules rules b ) ;;

(*Dont forget the start symbol!*)
let filter_blind_alleys g = match g with 
  |(start,rules)-> (start, filter_blind_alleys_allias g [] );;

