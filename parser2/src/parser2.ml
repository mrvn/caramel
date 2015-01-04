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
  val string_of_lexemes : lexeme Input.t -> string
  val attrib_opt : 'a token -> lexeme -> 'a attrib option
  val value_of_attrib : 'a attrib -> 'a
  val n_equal : 'a n -> 'b n -> ('a, 'b) Eq.t option
  type terminal
  val terminal : 'a token -> terminal
  val string_of_terminal : terminal -> string
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
  val symbol_attribution_opt : 'a n -> rule -> 'a symbol_attribution option
  type usymbol = UNT of nonterminal | UT of terminal
  type urule = nonterminal * usymbol list
  val urule : rule -> urule
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

  let urules = List.map urule rules

  let urule_rhs (_, rhs) = rhs
  let urule_lhs (lhs, _) = lhs
  let urules_with_lhs n =
    filter (function rule -> urule_lhs rule = n) urules

  (* first sets *)
  let initial_first_map =
    List.map (function n -> (n, [])) nonterminals

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
		   (List.assoc n map))
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
      |	rule::rest ->
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
end
