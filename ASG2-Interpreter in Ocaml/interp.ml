(* $Id: interp.ml,v 1.6 2019-01-24 19:14:14-08 - - $ *)

(* Jessica Pan - jeypan *)

open Absyn

exception Unimplemented of string
let unimpl reason = raise (Unimplemented reason)


let want_dump = ref false


let rec eval_expr (expr : Absyn.expr) : float = match expr with
    | Number number -> number
    | Memref memref -> (match memref with
		| Arrayref (ident, expr) -> 
			(if (int_of_float ((eval_expr expr) -. 1.0)) < Array.length(Hashtbl.find Tables.array_table ident)
				then (Hashtbl.find Tables.array_table ident).(int_of_float ((eval_expr expr) -. 1.0))
				else 0.)
			
		| Variable (ident) -> (Hashtbl.find Tables.variable_table ident))
	| Unary (oper, expr) -> ((Hashtbl.find Tables.unary_fn_table oper) (eval_expr expr))
    | Binary (oper, expr1, expr2) -> 
		if (Hashtbl.mem Tables.relop_fn_table oper)
			then (if ((Hashtbl.find Tables.relop_fn_table oper) (eval_expr expr1) (eval_expr expr2))
					then 1.
					else 0.)
			else ((Hashtbl.find Tables.binary_fn_table oper) (eval_expr expr1) (eval_expr expr2))
		

let interp_print (print_list : Absyn.printable list) =
    let print_item item =
        (print_string " ";
         match item with
         | String string ->
           let regex = Str.regexp "\"\\(.*\\)\""
           in print_string (Str.replace_first regex "\\1" string)
         | Printexpr expr ->
           print_float (eval_expr expr))
    in (List.iter print_item print_list; print_newline ())


let interp_input (memref_list : Absyn.memref list) =
    let input_number memref = match memref with
		| Arrayref (ident, expr) -> print_newline()
		| Variable (ident) -> 
			try  let number = Etc.read_number ()
				 in ((Hashtbl.add Tables.variable_table ident number))
			with End_of_file -> 
				 (Hashtbl.add Tables.variable_table "eof" 1.0)
    in List.iter input_number memref_list


let interp_let (memref : Absyn.memref) (exprr : Absyn.expr) = match memref with
	| Arrayref (ident, expr) -> 
		(Array.set (Hashtbl.find Tables.array_table ident) (int_of_float ((eval_expr expr) -. 1.0)) (eval_expr exprr))
	| Variable (ident) -> 
		(Hashtbl.add Tables.variable_table ident (eval_expr exprr))
	

let interp_dim (ident : Absyn.ident) (expr : Absyn.expr) = 
	(Hashtbl.add Tables.array_table ident (Array.make (int_of_float (eval_expr expr)) 0.0))
	
	
let interp_if (expr : Absyn.expr) (label : Absyn.label) : Absyn.program option = 
	if (eval_expr expr) = 1.
		then Some (Hashtbl.find Tables.label_table label)
		else None
	
	
let interp_goto (label : Absyn.label) : Absyn.program option = Some (Hashtbl.find Tables.label_table label)


let interp_stmt (stmt : Absyn.stmt) : Absyn.program option = match stmt with
    | Dim (ident, expr) -> interp_dim ident expr; None
    | Let (memref, expr) -> interp_let memref expr; None
    | Goto label -> interp_goto label
    | If (expr, label) -> interp_if expr label
    | Print print_list -> interp_print print_list; None
    | Input memref_list -> interp_input memref_list; None


let rec interpret (program : Absyn.program) = match program with
    | [] -> ()
    | firstline::otherlines -> match firstline with
      | _, _, None -> interpret otherlines
      | _, _, Some stmt -> 
		(let next_line = interp_stmt stmt
		in (match next_line with
			| None -> interpret otherlines
			| Some line -> interpret line))

	  

let interpret_program program =
    (Tables.init_label_table program; 
     if !want_dump then Tables.dump_label_table ();
     if !want_dump then Dumper.dump_program program;
     interpret program)

