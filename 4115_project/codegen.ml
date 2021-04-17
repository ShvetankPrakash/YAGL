(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Sast 
open Ast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate functions =
  let context    = L.global_context () in
  
  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "YAGL" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and float_t    = L.double_type context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and i64_t      = L.i64_type    context
  and void_t     = L.void_type   context
  in

  (* Graph Types *)
  let node_t     = L.struct_type context 
                   [| i32_t; 
                      L.pointer_type i1_t |]
  and edge_t     = L.struct_type context 
                   [| L.pointer_type (L.named_struct_type context "node_t");
                      L.pointer_type (L.named_struct_type context "node_t"); 
                      i32_t  |]
  (*and edge_list_t = L.struct_type context 
                    [| L.pointer_type (L.named_struct_type context "edge_t"); 
                       L.pointer_type (L.named_struct_type context "edge_list_t") |]*)
  and graph_t = L.struct_type context 
                [| i32_t; i32_t; i32_t; 
                   L.pointer_type (L.pointer_type (L.named_struct_type context "node_t")); 
                   L.pointer_type (L.pointer_type (L.named_struct_type context "edge_list_t"))  |]
  in

  (* Return the LLVM type for a YAGL type *)
  let rec ltype_of_typ = function
      A.Int          -> i32_t
    | A.Float        -> float_t  
    | A.String       -> L.pointer_type i8_t
    | A.Char         -> i8_t
    | A.Void         -> void_t
    | A.Bool         -> i1_t 
    | A.Node         -> L.pointer_type node_t
    | A.Graph        -> L.pointer_type graph_t
    | A.Edge         -> L.pointer_type edge_t
    | A.Array (t, e) -> let num =(match e with
                           Literal(l) -> l
                         | Binop(_, _, _) -> raise(Failure("TODO"))
                         | Id _  -> raise(Failure("TODO"))
                         | _ -> raise(Failure("TODO"))
                        )
                        in L.array_type (ltype_of_typ t)  num
  in

  (* Declare built-in functions *)
  let printf_t : L.lltype = 
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
      L.declare_function "printf" printf_t the_module in
  let make_graph_t : L.lltype =
          L.function_type (L.pointer_type graph_t)
          [| i32_t |] in
  let insert_edge_t : L.lltype = 
          L.function_type (L.pointer_type graph_t)
           [| (L.pointer_type graph_t); (L.pointer_type node_t); i32_t; (L.pointer_type node_t) |] in
  let remove_node_t : L.lltype = 
          L.function_type (L.pointer_type graph_t)
           [| (L.pointer_type graph_t); (L.pointer_type node_t) |] in
  let insert_node_t : L.lltype =
          L.function_type (L.pointer_type graph_t)
          [| L.pointer_type graph_t; L.pointer_type node_t |] in
  let print_graph_t : L.lltype =
          L.function_type i32_t [| (L.pointer_type graph_t)|] in 
  let sconcat_t : L.lltype =
          L.function_type (L.pointer_type i8_t) 
          [| L.pointer_type i8_t; L.pointer_type i8_t |] in
  let make_graph_func : L.llvalue =
      L.declare_function "make_graph" make_graph_t the_module in
  let insert_edge_func : L.llvalue =
      L.declare_function "insert_edge" insert_edge_t the_module in
  let remove_node_func : L.llvalue =
      L.declare_function "remove_node" remove_node_t the_module in
  let insert_node_func : L.llvalue =
      L.declare_function "insert_node" insert_node_t the_module in
  let print_graph_func : L.llvalue =
      L.declare_function "print_graph" print_graph_t the_module in
  let sconcat_func : L.llvalue =
      L.declare_function "sconcat" sconcat_t the_module in
  let strlen_t : L.lltype = 
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let strlen_func : L.llvalue = 
      L.declare_function "strlen" strlen_t the_module in    

  (* Graph related calls *)
  let make_node_t : L.lltype = 
      L.var_arg_function_type (L.pointer_type node_t) [| L.pointer_type i8_t |] in
  let make_node_func : L.llvalue = 
      L.declare_function "make_node" make_node_t the_module in   
  let print_node_t : L.lltype = 
      L.var_arg_function_type i32_t [| L.pointer_type node_t |] in
  let print_node_func : L.llvalue = 
      L.declare_function "print_node" print_node_t the_module in   


  (* Define each function (arguments and return type) so we can 
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types = 
	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in       
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder 
    and string_format_str = L.build_global_stringptr "%s\n" "fmt" builder 
    and char_format_str = L.build_global_stringptr "%c\n" "fmt" builder in

    
    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = 
        L.set_value_name n p;
  let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
  StringMap.add n local m 

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (t, n) =
  let local_var = L.build_alloca (ltype_of_typ t) n builder
  in StringMap.add n local_var m 
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals (List.fold_left 

      (fun bind_list stmt -> 
        match stmt with
          SBinding b -> b :: bind_list
        | SBinding_Assign (b, _) -> b :: bind_list
        | _ -> bind_list 
      ) [] fdecl.sbody)

    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let outermost_func_scope  = [local_vars]
    in

    let rec lookup n symbol_tables_list = match symbol_tables_list with
       last_map :: []        -> (try StringMap.find n last_map with Not_found -> raise (Failure ("Should have been caught in semant.")))
     | head_map :: tail_maps -> (try StringMap.find n head_map with Not_found -> lookup n tail_maps)
     | []                    -> raise(Failure("Internal Error: Symbol table not built."))
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder s_table ((_, e) : sexpr) = match e with
	SLiteral i  -> L.const_int i32_t i
      | SFLit f -> L.const_float_of_string float_t f
      | SEdgeList (e1, e2) ->
                      let _ = List.map (fun ele ->
                        expr builder s_table ele) (List.rev e2) in
                      expr builder s_table e1
      | SEdgeOp (e1, e2, op, e3, e4) ->
          let e1' = expr builder s_table e1
          and e2' = expr builder s_table e2
          and e3' = expr builder s_table e3
          and e4' = expr builder s_table e4 in
          (match op with
            A.Link -> L.build_call
                      insert_edge_func [| e1'; e2'; e3'; e4' |] "insert_edge" builder
          | A.Add  -> L.build_call
                      insert_node_func [| e1'; e4' |] "insert_node" builder
          | _ -> raise (Failure "This edge op is not implemented.")
          )
      | SId s   -> L.build_load (lookup s s_table) s builder
      | SAttr ((String, sId), "length") -> 
            L.build_call strlen_func [| (expr builder s_table (String, sId)) |] "strlen" builder
   (* | SAttr ((Node, nId), "name") -> expr builder (SNodeLit, nId) THIS IS BROKEN *)           
      | SAttr (_, _) -> 
            raise (Failure "unsupported attribute type") 
      | SNodeLit (_, nodeName) -> 
            L.build_call make_node_func [| (expr builder s_table nodeName) |]
            "make_node" builder
      | SBinop ((A.Graph, _ ) as e1, op, e2) ->
	  let e1' = expr builder s_table e1
	  and e2' = expr builder s_table e2 in
          (match e2 with
                (A.Node, _) ->
	                (match op with 
                                  A.Add -> L.build_call insert_node_func [| e1'; e2' |] "insert_node" builder
                                | A.Sub -> L.build_call remove_node_func [| e1'; e2' |] "remove_node" builder
                                | _ -> raise (Failure "Internal error: Semant should've caught")
                        )
                | _ -> raise (Failure "Internal error: Semant should've caught")
          )
      | SBinop ((A.Float,_ ) as e1, op, e2) ->
	  let e1' = expr builder s_table e1
	  and e2' = expr builder s_table e2 in
	  (match op with 
	    A.Add     -> L.build_fadd
	  | A.Sub     -> L.build_fsub
	  | A.Mult    -> L.build_fmul
	  | A.Div     -> L.build_fdiv 
	  | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
	  | A.Less    -> L.build_fcmp L.Fcmp.Olt
	  | A.Greater -> L.build_fcmp L.Fcmp.Ogt
	  | A.And | A.Or ->
	      raise (Failure "internal error: semant should have rejected and/or on float")
          | _ -> raise (Failure "This float binop is not implemented")
	  ) e1' e2' "tmp" builder
      | SBinop (((A.String,_ )) as e, op, e2) ->
          if op == A.Add then
                L.build_call sconcat_func [| (expr builder s_table e); (expr builder s_table e2)  |]
	        "sconcat" builder
          else
                raise (Failure "internal error: can only concatenate (+) strings")
      | SBinop (e1, op, e2) ->
	  let e1' = expr builder s_table e1
	  and e2' = expr builder s_table e2 in
	  (match op with
	    A.Add     -> L.build_add
	  | A.Sub     -> L.build_sub
	  | A.Mult    -> L.build_mul
          | A.Div     -> L.build_sdiv
	  | A.And     -> L.build_and
	  | A.Or      -> L.build_or
	  | A.Equal   -> L.build_icmp L.Icmp.Eq
	  | A.Less    -> L.build_icmp L.Icmp.Slt
	  | A.Greater -> L.build_icmp L.Icmp.Sgt
          | _         -> raise (Failure "This binop is not implemented")
	  ) e1' e2' "tmp" builder
      | SStrLit  s  -> L.build_global_stringptr s "fmt" builder
      | SChrLit  c  -> L.const_int i8_t (Char.code c)
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SGraphLit _ -> 
                  L.build_call make_graph_func [| L.const_int i32_t 1 |]
                  "make_graph" builder
      | SAssign (s, e1, e2) -> (match e2 with 
                               (_, SNoexpr) -> (let e' = expr builder s_table e1 in 
                                                ignore(L.build_store e' (lookup s s_table) builder); e')
                               | _ -> let e' = expr builder s_table e2 in 
                                      let index = (match e1 with (* expr builder e in *)
                                         (Int, _)          -> expr builder s_table e1
                                     (*| (Int, SLiteral l) -> L.const_int i64_t l May want to keep? *)
                                       | _                 -> raise(Failure("Semant.ml should have caught."))
                                      ) in
                                      let indices = 
                                        (Array.of_list [L.const_int i64_t 0; index]) in 
                                      let ptr =  
                                        L.build_in_bounds_gep (lookup s s_table) indices (s^"_ptr_") builder
                                      in L.build_store e' ptr builder
                               )
      | SCall ("printNode", [n]) ->
    L.build_call print_node_func [| expr builder s_table n |] "print_node" builder
      | SCall ("printGraph", [g]) ->
	  L.build_call print_graph_func [| expr builder s_table g |] "print_graph" builder
      | SCall ("printInt", [e]) | SCall ("printBool", [e]) ->
	  L.build_call printf_func [| int_format_str ; (expr builder s_table e) |]
	    "printf" builder
      | SCall ("printChar", [e])  ->
	  L.build_call printf_func [| char_format_str ; (expr builder s_table e) |]
	    "printf" builder
      | SCall ("printFloat", [e]) ->
    L.build_call printf_func [| float_format_str ; (expr builder s_table e) |]
      "printf" builder
      | SCall ("printString", [e]) ->
	  L.build_call printf_func [| string_format_str ; (expr builder s_table e) |]
	    "printf" builder
      | SCall ("printf", [e]) -> 
    L.build_call printf_func [| float_format_str ; (expr builder s_table e) |]
      "printf" builder
      | SCall (f, args) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
	 let llargs = List.rev (List.map (expr builder s_table) (List.rev args)) in
	 let result = (match fdecl.styp with 
                        A.Void -> ""
                      | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list llargs) result builder
      | SAccess (s, e) -> let index = (match e with (* expr builder e in *)
                             (Int, _)          -> expr builder s_table e
                         (*| (Int, SLiteral l) -> L.const_int i64_t l  We might want to check this? *)
                           | _                 -> raise(Failure("This should have been caught by semant.ml"))
                          ) in
                          let indices = 
                            (Array.of_list [L.const_int i64_t 0; index]) in 
                          let ptr =  
                            L.build_in_bounds_gep (lookup s s_table) indices (s^"_ptr_") builder
                          in L.build_load ptr (s^"_elem_") builder
      | _ -> raise (Failure("Only support Call and Integer Expressions currently.")) 
    in
    
    (* LLVM insists each basic block end with exactly one "terminator" 
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
	Some _ -> ()
      | None -> ignore (instr builder) in
	
    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)

    let rec stmt s_table builder s1 = match s1 with
	SBlock sl -> let add_local m (t, n) = 
                       let local_var = L.build_alloca (ltype_of_typ t) n builder 
                       in StringMap.add n local_var m 
                     in
                     let updated_table =  (List.fold_left add_local StringMap.empty (List.fold_left
                                            (fun bind_list stmt ->
                                              match stmt with
                                                SBinding b -> b :: bind_list
                                              | SBinding_Assign (b, _) -> b :: bind_list
                                              | _ -> bind_list
                                            ) [] sl)
                                          ) :: s_table (*TODO: FIX LEAKING ON THE STACK?*)
                                          in List.fold_left (stmt updated_table) builder sl
      | SExpr e -> ignore(expr builder s_table e); builder 
      | SBinding (_, _) -> builder;
      | SBinding_Assign ((_, _), e) -> ignore (expr builder s_table e); builder
      | SReturn e -> ignore(match fdecl.styp with
                (* Special "return nothing" instr *)
                A.Void -> L.build_ret_void builder
                (* Build return statement *)
                | _ -> L.build_ret (expr builder s_table e) builder );
            builder
      | SIf (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder s_table predicate in
	 let merge_bb = L.append_block context "merge" the_function in
         let build_br_merge = L.build_br merge_bb in (* partial function *)

	 let then_bb = L.append_block context "then" the_function in
	 add_terminal (stmt s_table (L.builder_at_end context then_bb) then_stmt)
	   build_br_merge;

	 let else_bb = L.append_block context "else" the_function in
	 add_terminal (stmt s_table (L.builder_at_end context else_bb) else_stmt)
	   build_br_merge;

	 ignore(L.build_cond_br bool_val then_bb else_bb builder);
	 L.builder_at_end context merge_bb
      | SWhile (predicate, body) ->
	  let pred_bb = L.append_block context "while" the_function in
	  ignore(L.build_br pred_bb builder);

	  let body_bb = L.append_block context "while_body" the_function in
	  add_terminal (stmt s_table (L.builder_at_end context body_bb) body)
	    (L.build_br pred_bb);

	  let pred_builder = L.builder_at_end context pred_bb in
	  let bool_val = expr pred_builder s_table predicate in

	  let merge_bb = L.append_block context "merge" the_function in
	  ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
	  L.builder_at_end context merge_bb


      | _ -> raise (Failure("Only support expression statements currently."))

    in

    (* Build the code for each statement in the function *)
    let builder = stmt outermost_func_scope builder (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.styp with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
