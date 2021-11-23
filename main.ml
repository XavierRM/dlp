
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

(*
Section 1.1 of the Assigment:
  We read a line and check to see if it contains a semicolon if it does we return the result
  of concatenating all the lines if it doesn't we keep on checking
*)
(*let read_line_semicolon () =
  let rec aux command =
    let line = read_line () in
      match (index_opt (read_line ()) ";") with
          Some pos -> (command ^ " " ^ (sub line 0 (pos-1)))
        | None -> aux (command ^ " " ^ line)
  in aux "";;*)

let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop ctx =
    print_string ">> ";
    flush stdout;
    try
      let tm = s token (from_string (read_line ())) in
      let tyTm = typeof ctx tm in
      print_endline (string_of_term (eval tm) ^ " : " ^ string_of_ty tyTm);
      loop ctx
    with
       Lexical_error ->
         print_endline "lexical error";
         loop ctx
     | Parse_error ->
         print_endline "syntax error";
         loop ctx
     | Type_error e ->
         print_endline ("type error: " ^ e);
         loop ctx
     | End_of_file ->
         print_endline "...bye!!!"
  in
    loop emptyctx
  ;;

top_level_loop ()
;;

