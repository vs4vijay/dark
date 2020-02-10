open Core_kernel
open Analysis_types
open Types
open Types.RuntimeT
module TL = Toplevel
module PReq = Parsed_request

(* -------------------- *)
(* Input_vars *)
(* -------------------- *)
let input_vars_for_user_fn (ufn : user_fn) : dval_map =
  let param_to_dval (p : param) : dval = DIncomplete SourceNone in
  ufn.metadata.parameters
  |> List.filter_map ~f:ufn_param_to_param
  |> List.map ~f:(fun f -> (f.name, param_to_dval f))
  |> Analysis_types.Symtable.from_list_exn


let dbs_as_input_vars (dbs : DbT.db list) : (string * dval) list =
  List.filter_map dbs ~f:(fun db ->
      match db.name with
      | Filled (_, name) ->
          Some (name, DDB name)
      | Partial _ | Blank _ ->
          None)


let http_route_input_vars (h : HandlerT.handler) (request_path : string) :
    input_vars =
  let route = Handler.event_name_for_exn h in
  Http.bind_route_variables_exn ~route request_path


(* -------------------- *)
(* Sample input vars *)
(* -------------------- *)
let sample_request_input_vars = [("request", PReq.to_dval PReq.sample_request)]

let sample_event_input_vars = [("event", DIncomplete SourceNone)]

let sample_unknown_handler_input_vars =
  sample_request_input_vars @ sample_event_input_vars


let sample_module_input_vars h : input_vars =
  match Handler.module_type h with
  | `Http ->
      sample_request_input_vars
  | `Cron ->
      []
  | `Repl ->
      []
  | `Worker ->
      sample_event_input_vars
  | `Unknown ->
      sample_unknown_handler_input_vars


let sample_route_input_vars (h : HandlerT.handler) : input_vars =
  match Handler.event_name_for h with
  | Some n ->
      n
      |> Http.route_variables
      |> List.map ~f:(fun k -> (k, DIncomplete SourceNone))
  | None ->
      []


let sample_input_vars h = sample_module_input_vars h @ sample_route_input_vars h

let sample_function_input_vars f =
  f |> input_vars_for_user_fn |> DvalMap.to_list


(* -------------------- *)
(* For exec_state *)
(* -------------------- *)
let load_no_results _ _ = None

let store_no_results _ _ _ = ()

let load_no_arguments _ = []

let store_no_arguments _ _ = ()

(* -------------------- *)
(* Execution *)
(* -------------------- *)
let execute_handler
    ~tlid
    ~execution_id
    ~input_vars
    ~dbs
    ~user_fns
    ~user_tipes
    ~account_id
    ~canvas_id
    ?(load_fn_result = load_no_results)
    ?(load_fn_arguments = load_no_arguments)
    ?(store_fn_result = store_no_results)
    ?(store_fn_arguments = store_no_arguments)
    (h : HandlerT.handler) : dval * tlid list =
  let input_vars = dbs_as_input_vars dbs @ input_vars in
  let tlid_store = TLIDTable.create () in
  let trace_tlid tlid = Hashtbl.set tlid_store ~key:tlid ~data:true in
  let state : exec_state =
    { tlid
    ; account_id
    ; canvas_id
    ; user_fns
    ; user_tipes
    ; dbs
    ; trace = (fun _ _ -> ())
    ; trace_tlid
    ; exec =
        (fun ~state _ _ -> Exception.internal "invalid state.exec function")
    ; context = Real
    ; execution_id
    ; fail_fn = None
    ; load_fn_result
    ; load_fn_arguments
    ; store_fn_result
    ; store_fn_arguments }
  in
  let result = Ast.execute_ast ~state ~input_vars h.ast in
  let tlids = TLIDTable.keys tlid_store in
  match result with
  | DErrorRail (DOption OptNothing) | DErrorRail (DResult (ResError _)) ->
      (DResp (Response (404, []), Dval.dstr_of_string_exn "Not found"), tlids)
  | DErrorRail _ ->
      ( DResp
          ( Response (500, [])
          , Dval.dstr_of_string_exn "Invalid conversion from errorrail" )
      , tlids )
  | dv ->
      (dv, tlids)


let execute_function
    ~tlid
    ~execution_id
    ~trace_id
    ~dbs
    ~user_fns
    ~user_tipes
    ~account_id
    ~canvas_id
    ~caller_id
    ~args
    ?(store_fn_result = store_no_results)
    ?(store_fn_arguments = store_no_arguments)
    fnname =
  let tlid_store = TLIDTable.create () in
  let trace_tlid tlid = Hashtbl.set tlid_store ~key:tlid ~data:true in
  let state : exec_state =
    { tlid
    ; account_id
    ; canvas_id
    ; user_fns
    ; user_tipes
    ; dbs
    ; trace = (fun _ _ -> ())
    ; trace_tlid
    ; exec =
        (fun ~state _ _ -> Exception.internal "invalid state.exec function")
    ; context = Real
    ; execution_id
    ; fail_fn = None
    ; load_fn_result = load_no_results
    ; load_fn_arguments = load_no_arguments
    ; store_fn_result
    ; store_fn_arguments }
  in
  (Ast.execute_fn state fnname caller_id args, TLIDTable.keys tlid_store)


(* -------------------- *)
(* Execution *)
(* -------------------- *)
let analyse_ast
    ~tlid
    ~execution_id
    ~input_vars
    ~dbs
    ~user_fns
    ~user_tipes
    ~account_id
    ~canvas_id
    ?(load_fn_result = load_no_results)
    ?(load_fn_arguments = load_no_arguments)
    (ast : expr) : analysis =
  let value_store = IDTable.create () in
  let trace id dval = Hashtbl.set value_store ~key:id ~data:dval in
  let input_vars = dbs_as_input_vars dbs @ input_vars in
  let state : exec_state =
    { tlid
    ; account_id
    ; canvas_id
    ; user_fns
    ; user_tipes
    ; dbs
    ; trace
    ; trace_tlid = (fun _ -> ())
    ; exec =
        (fun ~state _ _ -> Exception.internal "invalid state.exec function")
    ; context = Preview
    ; execution_id
    ; fail_fn = None
    ; load_fn_result
    ; load_fn_arguments
    ; store_fn_result = store_no_results
    ; store_fn_arguments = store_no_arguments }
  in
  let _ = Ast.execute_ast ~state ~input_vars ast in
  value_store
