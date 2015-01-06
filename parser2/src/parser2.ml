(* GADT enabled recursive parser generator
 * Copyright (C) 2015 Goswin von Brederlow <goswin-v-b@web.de>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

module type Grammar = sig
  (* from ParserTypes *)
  type 'a n
  type 'a token
  type 'a attrib
  type lexeme
  val string_of_n : 'a n -> string
  val string_of_token : 'a token -> string
  val string_of_attrib : 'a attrib -> string
  val string_of_lexeme : lexeme -> string
  val string_of_lexemes : lexeme Input.t -> string
  val attrib_opt : 'a token -> lexeme -> 'a attrib option
  val value_of_attrib : 'a attrib -> 'a
  val n_equal : 'a n -> 'b n -> ('a, 'b) Eq.t option
  type terminal
  val terminal : 'a token -> terminal
  val string_of_terminal : terminal -> string
  val terminal_of_lexeme : lexeme -> terminal
  type nonterminal
  val nonterminal : 'a n -> nonterminal
  val string_of_nonterminal : nonterminal -> string
  type 'a symbol = NT of 'a n | T of 'a token
  val string_of_symbol : 'a symbol -> string
  type (_, _) symbols =
  | Ret : ('a, 'a) symbols
  | Arg : 'a symbol * ('b, 'c) symbols -> ('b, 'a -> 'c) symbols
  val string_of_symbols : ('a, 'b) symbols -> string
  val ret : ('a, 'a) symbols
  val ( ^^^ ) : 'a symbol -> ('b, 'c) symbols -> ('b, 'a -> 'c) symbols
  type 'a symbol_attribution =
  | SA : ('a, 'b) symbols * 'b -> 'a symbol_attribution
  val string_of_symbol_attribution : 'a symbol_attribution -> string
  type rule
  val rule : 'a n -> 'a symbol_attribution -> rule
  type ('a, 'b) rule_conv = {
    key : 'c . 'c n -> 'a;
    value : 'c . 'c symbol_attribution -> 'b;
  }
  val rule_conv : ('a, 'b) rule_conv -> rule -> ('a * 'b)
  val symbol_attribution_opt : 'a n -> rule -> 'a symbol_attribution option
  val rules_with_lhs : nonterminal -> rule list -> rule list
  val rule_has_n : 'a n -> rule -> bool
  val rule_length : rule -> int
  type usymbol = UNT of nonterminal | UT of terminal
  type urule = nonterminal * usymbol list
  val urule : rule -> urule
  val nonterminal_of_rule : rule -> nonterminal
  val string_of_rule : rule -> string
  type rules = rule list
  val string_of_rules : string -> rules -> string

  (* type of the start non terminal *)
  type start

  (* Grammar
   * There must be a single rule Start := Non-Terminal!
   *)
  val k : int (* lookahead size *)
  val nonterminals : nonterminal list
  val terminals : terminal list
  val rules : rules
  val start : start n
  type expr
  val expr : 'a n -> 'a -> expr
  val value_of_expr_opt : 'a n -> expr -> 'a option
  type stack_item =
  | Lexeme of lexeme
  | Expr of expr
  val rule_apply : rule -> stack_item list -> stack_item
end

(* List utilities *)

let rec truncate k l =
  match l with
    | [] -> []
    | x::xs ->
      if k = 0
      then []
      else x::(truncate (k - 1) xs)

let rec append_truncate k l1 l2 =
  if k = 0
  then []
  else
    match l1 with
      | [] -> truncate k l2
      | x::xs -> x :: (append_truncate (k - 1) xs l2)

let filter pred l =
  let rec f l r =
    match l with
      | [] -> List.rev r
      | x::xs ->
        if pred x 
        then f xs (x::r)
        else f xs r
  in f l []

let pair_map f l1 l2 =
  let rec loop_1 l1 =
    match l1 with
      | [] -> []
      | x::xs ->
        let rec loop_2 l2 =
          match l2 with
            | [] -> loop_1 xs
            | y::ys -> (f x y) :: loop_2 ys
        in loop_2 l2
  in loop_1 l1

let union l1 l2 =
  let rec loop add result =
    match add with
      | [] -> result
      | x::xs ->
        loop
          xs
          (if List.mem x result then result else x::result)
  in loop l1 l2

let union_list l = List.fold_left union [] l

let uniq l =
  let rec loop l r =
    match l with
      | [] -> List.rev r
      | x::xs -> loop xs (if List.mem x r then r else x::r)
  in loop l []
  
let first_equal first_1 first_2 =
  (List.length first_1 = List.length first_2) &&
    let rec loop first_1 =
      match first_1 with
      | [] -> true
      | x::xs -> (List.mem x first_2) && (loop xs)
    in
    loop first_1

let map_equal map_1 map_2 =
  let rec loop map_1 =
    match map_1 with
    | [] -> true
    | (nonterminal_1, first_1)::rest ->
      let first_2 = List.assoc nonterminal_1 map_2
      in
      (first_equal first_1 first_2) && (loop rest)
  in
  loop map_1

module MAKE(G : Grammar) = struct
  open G

  (* Map for first and follow *)
  module Map = struct
    module W = struct
      type 'a key = 'a n
      type 'a value = terminal list
      let string_of_key = string_of_n
      let string_of_value terminals =
        let (_, str) =
          List.fold_left
            (fun (sep, str) term -> (" ", str ^ sep ^ (string_of_terminal term)))
            ("", "")
            terminals
        in
        str
    end
    module Boxed = Univ.Witnessed.MAKE(W)
    type t = Boxed.t list
    let empty = []
    let add t n terminals = (Boxed.box n terminals) :: t
    let lookup n t = Boxed.lookup n t
  end
  
  let urules = List.map urule rules

  let urule_rhs (_, rhs) = rhs
  let urule_lhs (lhs, _) = lhs
  let urules_with_lhs n =
    filter (function rule -> urule_lhs rule = n) urules

  let lookup n map =
    try
      List.assoc n map
    with Not_found -> []

  (* first sets *)
  let initial_first_map = List.map (function n -> (n, [])) nonterminals

  let lookup_first map l =
    let rec loop = function
      | [] -> [[]]
      | symbol :: rest ->
        let rest_first = loop rest in
        match symbol with
        | UT t -> uniq (List.map (append_truncate k [t]) rest_first)
        | UNT n ->
          union_list
            (List.map
               (function first_rest ->
                 List.map
                   (function first_first ->
                     (append_truncate k first_first first_rest))
                   (lookup n map))
               rest_first)
    in
    loop l

  let lhs_next_first lhs old_map =
    let rec loop rules first =
      match rules with
        [] -> first
      | rule::rest ->
        let rhs_first = lookup_first old_map (urule_rhs rule) in
        loop rest (union first rhs_first)
    in loop (urules_with_lhs lhs) []

  let next_first_map old_map =
    List.map
      (function (nonterminal, _) ->
        (nonterminal, lhs_next_first nonterminal old_map))
      old_map

  let first_map =
    let rec loop map =
      let new_map = next_first_map map
      in
      if map_equal map new_map
      then new_map
      else loop new_map
    in
    loop initial_first_map
  
  let first = lookup_first first_map

  let () =
    Printf.printf "######################### first map ##################################\n";
    List.iter
      (fun (n, list) ->
        let n = string_of_nonterminal n
        in
        List.iter
          (fun terms ->
            let (_, str) =
              List.fold_left
                (fun (sep, str) term ->
                  (" ", str ^ sep ^ (string_of_terminal term)))
                (" -> ", n)
                terms
            in
            Printf.printf "%s\n" str)
          list)
      first_map;
    Printf.printf "######################################################################\n"
    
  (* follow sets *)
  let initial_follow_map =
    List.map
      (function n ->
        if n = nonterminal start then (n, [[]]) else (n, []))
      nonterminals

  let update_follow_map map nonterminal follow_symbol =
    let rec loop map =
      match map with
      | ((nonterminal', follow) as entry)::rest ->
        if nonterminal = nonterminal'
        then (nonterminal', union follow_symbol follow)::rest
        else entry::(loop rest)
      | [] -> assert false
    in
    loop map

  let next_follow_map first_g_k old_map =
    let rec loop rules old_map =
      match rules with
      | [] -> old_map
      |        rule::rest ->
        let rec rhs_loop rhs_rest old_map =
          match rhs_rest with
          | [] -> loop rest old_map
          | symbol::rhs_rest ->
            match symbol with
            | UT t -> rhs_loop rhs_rest old_map
            | UNT n ->
              let first_rest = first_g_k rhs_rest in
              let follow_lhs = List.assoc (urule_lhs rule) old_map in
              let follow_symbol =
                uniq (pair_map (append_truncate k) first_rest follow_lhs)
              in
              rhs_loop
                rhs_rest
                (update_follow_map old_map n follow_symbol)
        in
        rhs_loop (urule_rhs rule) old_map
    in
    loop urules old_map

  let follow_map =
    let rec loop map =
      let new_map = next_follow_map first map
      in
      if map_equal map new_map
      then new_map
      else loop new_map
    in
    loop initial_follow_map

  let follow n = List.assoc n follow_map

  let () =
    Printf.printf "######################### follow map #################################\n";
    List.iter
      (fun (n, list) ->
        let n = string_of_nonterminal n
        in
        List.iter
          (fun terms ->
            let (_, str) =
              List.fold_left
                (fun (sep, str) term ->
                  (" ", str ^ sep ^ (string_of_terminal term)))
                (" -> ", n)
                terms
            in
            Printf.printf "%s\n" str)
          list)
      follow_map;
    Printf.printf "######################################################################\n"

(* List functions *)

let filter pred l =
  let rec f l r =
    match l with
      [] -> List.rev r
    | x::xs ->
        if pred x 
        then f xs (x::r)
        else f xs r
  in f l []
let rec drop n l =
  if n = 0 then l else drop (n - 1) (List.tl l)
let rec take n l =
  match l with
    [] -> []
  | x::xs ->
      if n = 0
      then []
      else x::(take (n - 1) xs)

(* LR items *)
type item = Item of rule * int * terminal list

(* LR states are lists of items *)
type state = item list

let item_lookahead item = match item with Item(_, _, la) -> la

let item_rule item = match item with Item(rule, _, _) -> rule
(*
let item_lhs item = Grammar.rule_lhs (item_rule item)
let item_rhs item = Grammar.rule_rhs (item_rule item)
*)
  
let item_rhs_rest item =
  match item with
    Item(rule, pos, _) ->
      let rule = urule rule in
      drop pos (urule_rhs rule)

let item_shift item =
  match item with
    Item(rule, pos, la) -> Item(rule, pos+1, la)

let rec items_merge items_1 items_2 =
  match items_1 with
    [] ->items_2
  | item::items_1 ->
      if List.mem item items_2
      then items_merge items_1 items_2
      else items_merge items_1 (item::items_2)

let predict_equal items_1 items_2 =
  (List.length items_1) = (List.length items_2)
    &&
  let rec loop items_1 =
    match items_1 with
      [] -> true
    | item::items_1 ->
        (List.mem item items_2) && (loop items_1)
  in loop items_1

(* LR state closure *)
let compute_closure state =
  let initial_items : nonterminal -> usymbol list -> state = fun n la_suffix ->
    List.flatten
      (List.map
         (function rule ->
           List.map
             (function la -> Item(rule, 0, la))
             (first la_suffix))
         (rules_with_lhs n rules))
  in

  let next_predict item_set =
    let rec loop item_set predict_set =
      match item_set with
      | [] -> predict_set
      |        item::item_set ->
          match item_rhs_rest item with
          | [] -> loop item_set predict_set
          | lhs::rhs_rest ->
              match lhs with
              | UT t -> loop item_set predict_set
              |        UNT n ->
                let new_items =
                  initial_items
                    n
                    (rhs_rest @
                       (List.map
                          (function t -> UT t)
                          (item_lookahead item)))
                  in
                  loop
                    item_set
                    (items_merge new_items predict_set)
    in loop item_set item_set

  in let rec loop predict_set =
    let new_predict_set = next_predict predict_set in
    if predict_equal predict_set new_predict_set
    then new_predict_set
    else loop new_predict_set
  in loop state

let goto closure symbol =
  List.map
    item_shift
    (filter
       (function item ->
         let rest = item_rhs_rest item in
         (rest != [])
           &&
         (symbol = List.hd rest))
       closure)

let nactive state =
  let rec loop item_set m =
    match item_set with
      [] -> m
    | Item(_, pos, _)::item_set ->
              loop item_set (max pos m)
  in loop state 0

(* Which symbols may come next? *)

let next_symbols closure =
  let rec loop item_set symbols =
    match item_set with
      [] -> symbols
    | item::item_set ->
        let rhs_rest = item_rhs_rest item in
        loop
          item_set
          (if (rhs_rest != [] &&
               not (List.mem (List.hd rhs_rest) symbols))
          then (List.hd rhs_rest)::symbols
          else symbols)
  in loop closure []

let is_terminal symbol =
  match symbol with
  | T _ -> true
  | NT _ -> false

let next_terminals closure =
  let rec loop acc = function
    | [] -> acc
    | (UT t) :: rest -> loop (t :: acc) rest
    | (UNT _) :: rest -> loop acc rest
  in
  loop [] (next_symbols closure)

let accept_items closure =
  filter
    (function item -> (item_rhs_rest item) = [])
    closure

(* Could the input end in this state? *)

let final state =
  let rec loop item_set =
  match item_set with
  | [] -> None
  | Item(rule, pos, la)::item_set ->
    if (rule_has_n start rule) && (pos = rule_length rule)
    then Some rule
    else loop item_set
  in loop state

let start_item =
  Item(List.hd (rules_with_lhs (nonterminal start) rules),
       0,
       [])

(* Whose lookahead fits? *)

let select_lookahead_item item_set l =
  let prefix = truncate k l in
  let matches =
    filter
      (function Item(_, _, la) -> la = prefix)
      item_set
  in
  match matches with
    [] -> None
  | item::_ -> Some item

(* parser with attribute evaluation *)
exception Parse_error

let string_of_item (Item (rule, pos, la)) =
  let string_of_doted_symbol_attribution : type a . a symbol_attribution -> string = function SA (symbols, _) ->
    let rec loop : type a b . int -> string -> string -> (a, b) symbols -> string = fun pos sep str symbols ->
      if pos = 0
      then loop (pos - 1) " " (str ^ sep ^ ".") symbols
      else
        match symbols with
        | Ret -> str
        | Arg (sym, syms) ->
          loop (pos - 1) " " (str ^ sep ^ (string_of_symbol sym)) syms
    in
    loop pos "" "" symbols
  in
  let conv = {
    key = string_of_n;
    value =  string_of_doted_symbol_attribution;
  }
  in
  let (n, syms) = rule_conv conv rule
  in
  Printf.sprintf "%s := %s  << \"%s\""
    n
    syms
    (Util.string_of_list string_of_terminal " " la)

let rec print_state indent = function
  | [] -> ()
  | item::items ->
    Printf.printf "%s%s\n" indent (string_of_item item);
    print_state indent items

let indent x = String.make x ' '
  
let parse (stream : lexeme Input.t) =
  let rec parse depth state continuations attribute_instances stream =
    let indent = indent depth
    and indent2 = indent (depth + 1)
    in
    Printf.printf "%s### parse << %s\n%!" indent (string_of_lexemes stream);
    print_state indent2 state;
    match (stream, final state) with
    | ([], Some rule) ->
      let length_rhs = rule_length rule
      in
      begin
        match rule_apply rule (List.rev (take length_rhs attribute_instances)) with
        | Lexeme _ -> assert false
        | Expr e ->
          match value_of_expr_opt start e with
          | Some v -> v
          | None -> assert false
      end
    | _ ->
      let closure = compute_closure  state in
      Printf.printf "%sClosure:\n%!" indent;
      print_state indent2 closure;
      
      let rec c0 (symbol : usymbol) (v0 : stack_item) (stream : lexeme Input.t) =
        let next_state = goto closure symbol in
        let n_active = nactive next_state
        in
        parse
          (depth + 2)
          next_state
          (c0 :: (take (n_active - 1) continuations))
          (v0 :: (take (n_active - 1) attribute_instances))
          stream
      in
      match stream with
      | (pos, lexeme) :: rest when
          List.mem (terminal_of_lexeme lexeme) (next_terminals closure) ->
        Printf.printf "%spushing lexeme %s\n" indent (string_of_lexeme lexeme);
        c0 (UT (terminal_of_lexeme lexeme)) (Lexeme lexeme) rest
      | _ ->
        match select_lookahead_item 
          (accept_items closure) 
          (List.map
             (fun (_, lexeme) -> terminal_of_lexeme lexeme)
             (truncate k stream))
        with
        | None -> Printf.printf "%slookahead failed\n" indent; raise Parse_error
        | Some item ->
          Printf.printf "%slookahead item = %s\n" indent (string_of_item item);
          let Item (rule, _, _) = item in
          let length_rhs = rule_length rule
          in
          (List.nth (c0::continuations) length_rhs)
            (UNT (nonterminal_of_rule rule))
            (rule_apply rule (List.rev (take length_rhs attribute_instances)))
            stream
  in
  parse 0 [start_item] [] [] stream
end
