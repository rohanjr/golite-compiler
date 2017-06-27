(*
 * This code originated as part of the McGill COMP520 course requirements.
 * Any student examining the code or using any part of the code for their coursework must clearly
 * document what code they examined and give clear credit to any code that they used.
 *
 * Original authors:
 * Rohan Jacob-Rao (rohanjr)
 * Steven Thephsourinthone (stheph)
 * Shawn Otis 
 *)

open Ast

(* Map over an option *)
let omap (f : 'a -> 'b) (default : 'b) : 'a option -> 'b = function
  | None -> default
  | Some x -> f x

(* Unwrap an option *)
let oget (default : 'a) : 'a option -> 'a = omap (fun x -> x) default

(* Either type *)
type ('a, 'b) either = Left of 'a | Right of 'b

(* Turn a list into a string *)
let concatmap (f : 'a -> string) (l : 'a list) : string =
  String.concat "" (List.map f l)

let id_in_vss (i : id) (vss : varspecsimp) : bool =
  let (il, t) = vss in List.fold_left (fun x y -> x && y) true (List.map (fun x -> x = i) il)

let id_in_vsl (i : id) (vsl : varspecsimp list) : bool =
  List.fold_left (fun x y -> x && y) true (List.map (id_in_vss i) vsl)
