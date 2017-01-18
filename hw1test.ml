let subset_test0 = subset [] [5;6;7];; (*Tests if the empty set is always a subset*)
let subset_test1 = subset [2;4;5] [4;5;2];; (*Does order matter?*)
let subset_test2 = not (subset [13] [4;1]);; (*Not a subset*)

let equal_sets_test0 = equal_sets [2] [2;2;2;2;2;2;2;2;2];;
let equal_sets_test1 = not (equal_sets [1;2;3] [3;2;2;2;1]);;

let set_union_test0 = equal_sets (set_union [5;6] [10;1;0]) [5;6;10;1;0];;
let set_union_test2 = equal_sets (set_union [] [1;5;6]) [1;5;6];; (*Empty set works too!*)

let set_intersection_test0 =
  equal_sets (set_intersection [4;5;6] [0;100;200]) [];; (*No common elements*)
let set_intersection_test1 =
  equal_sets (set_intersection [1;2;3;4] [3;4;4;5;6]) [3;4];;


let set_diff_test0 = equal_sets (set_diff [8;10] [8;10;9;0]) [];;
let set_diff_test1 = equal_sets (set_diff [5;6;7] []) [5;6;7];;
let set_diff_test2 = equal_sets (set_diff [3] [2;3;4;5]) [];;


let computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x / 10) 100 = 0
let computed_fixed_point_test1 =
  computed_fixed_point (=) (fun x -> x *.x *.x *. x) 4. = infinity
let computed_fixed_point_test0 = 
  computed_fixed_point (=) (fun x -> x *. 10.) 0. = 0.


let computed_periodic_point_test0 =
  computed_periodic_point (=) (fun x -> x *x - 3*x) 0 (8) = 8

let computed_periodic_point_test1 =
  computed_periodic_point (=) (fun x -> x *. x -. 2. *. x +. 1.) 2 1. = 1.

type awksub_nonterminals =
  | Fox | Wolf | Bear | Dolphin | Shark

 
(*Decides if a symbol is terminal or non terminal*)
let t_or_nt0= 
  not (t_or_nt (N Fox)) ;;
let t_or_nt1=
  t_or_nt (T "5") ;;
let t_or_nt2=
 not  (t_or_nt (N Bear) );;
let t_or_nt3=
  t_or_nt (T ")");;


let li_0=[T"("; N Fox; T")"];;
let li_1=[T"("; T "5"; T"++"];;
let li_2=[T"panos"; N Wolf ; N Dolphin];;
let li_3=[N Bear; N Shark; N Fox];;

(*Decides if a list is exclusively made out of terminal symbols*)  
let is_terminal_list0= 
  not (is_terminal_list li_0) ;;
let is_terminal_list1=
  is_terminal_list li_1 ;;
let is_terminal_list2=
 not (is_terminal_list li_2);;
let is_terminal_list3=
 not (is_terminal_list li_3);;


let rule_0=Fox, [T"("; N Fox; T")"];;
let rule_1=Wolf, [T"("; T "4"; T")"];;

(* determines if a rule has only terminal sumbols*) 
let is_terminal_rule0=
 not (is_terminal_rule rule_0);;
let is_terminal_rule1=
 is_terminal_rule rule_1;;


let rules_0 =
   [
    Fox, [T"("; N Fox; T")"];
    Fox, [N Shark];
    Fox, [T "panos"];
    Fox, [N Fox; N Dolphin; N Fox];
    Fox, [T"++";T"-";T"2"];
    Fox, [T"5"; T"--";T"1"];
    Wolf,[T"valuez"]
   ] 

;;

(*In a list of rules, find those consisting of terminal strings and add them to them to list b *)
let add_rules0=
  add_rules rules_0 [] =[ (Fox, [T "panos"]); (Fox, [T "++"; T "-"; T "2"]);
   (Fox, [T "5"; T "--"; T "1"]); (Wolf, [T "valuez"]) ];;


let animal_rules =
   [Fox, [N Fox; T "cs" ; T")"];
    Fox, [N Shark];
    Fox, [N Fox; N Dolphin; N Fox];
    Fox, [N Wolf];
    Fox, [N Bear; N Wolf];
    Fox, [N Wolf; N Bear];
    Wolf, [T"$"; N Fox];
    Bear, [T"++"];
    Bear, [T"--"];
    Dolphin, [T"+"];
    Dolphin, [T"-"];
    Shark, [T"99"]
   ]
    

let add_rules1=
  add_rules animal_rules []= [(Bear, [T "++"]); (Bear, [T "--"]); (Dolphin, [T "+"]);
   (Dolphin, [T "-"]); (Shark, [T "99"])];;

(*takes the first elements of the list of unions and returns
 another list with just these elements*)  
let take_first0=
  take_first animal_rules []= [Fox; Fox; Fox; Fox; Fox; Fox; Wolf; Bear; Bear; Dolphin; Dolphin; Shark];;

let second_part = [T "pao"; N Fox; N Dolphin];;

(*Extracts the symbol N (T gets discarded) from the list 
i.e. [N Expr; N Lvalue; T "j"] gives [Expr; Lvalue] *)
let take_second0= 
  take_second second_part []= [Fox; Dolphin];;


let simple_rules= 
[
Fox, [T"5"; N Wolf; T "j"] ;
Dolphin, [N Wolf];
Bear, [N Shark];
Fox, [N Dolphin; N Shark];
Shark, [T "6"];

];;




let simple_grammar= Fox, simple_rules;;
let animal_grammar = Fox, animal_rules;;

(*Filters out the blind alley rules*)
let filter0 = 
  filter simple_grammar simple_rules= [(Bear, [N Shark]); (Shark, [T "6"])] ;;

(* All it does is call filter with simple_grammar and []*)
let filter_blind_alleys_allias0 =
 filter_blind_alleys_allias simple_grammar [] = [(Bear, [N Shark]); (Shark, [T "6"])] ;;

let filter_blind_alleys0 = 
  filter_blind_alleys simple_grammar=(Fox, [(Bear, [N Shark]); (Shark, [T "6"])]) ;;


