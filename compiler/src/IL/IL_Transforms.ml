(* * Apply transformations to IL *)
 

(* ** Imports *)
open Core_kernel.Std
open Util
open IL_Lang
open IL_Utils
open IL_Compile
open Arith

(* ** Apply transformations in sequence.
 * ------------------------------------------------------------------------ *)

(* wrapper for liveness analysis that puts live sets into comments *)
let transform_register_liveness _efunc =
  failwith "undefined"
  (*
  let bis =
    List.concat_map (register_liveness efunc)
      ~f:(fun {li_bi = i; li_read_after_rhs = read} ->
        [Comment (fsprintf "live: %a" (pp_list "," pp_preg) (Set.to_list read)); i])
  in
  { efunc with
    ef_body = base_instrs_to_stmt bis }
  *)

let strip_comments bis =
  List.filter ~f:(function Comment(_) -> false | _ -> true) bis

type asm_lang = X64

type transform =
  | MacroExpand of string * u64 String.Map.t
  | ArrayAssignExpand of string
  | ArrayExpand of string
  | Type
  | Print of string * string option
  | Save of string * string option
  | RegisterAlloc of int
  | InlineCalls of string
  | RegisterLiveness
  | StripComments
  | Asm of asm_lang
  | Interp of string * u64 String.Map.t * u64 U64.Map.t * value list
    (* Interp(pmap,mmap,alist,fun):
         interpret call of function fun() with parameters pmap, memory mmap,
         argument list alist *)

let ptrafo =
  let open MP in
  let ident = many1 (MParser.none_of "]=") >>= fun l -> return (String.of_char_list l) in
  let asm_lang = choice [ string "x86-64" >>$ X64 ] in
  let enclosed p pstart pend = pstart >> p >>= fun x -> pend >>$ x in
  let bracketed p = enclosed p (char '[') (char ']') in
  (* let braced p = enclosed p (char '{') (char '}') in *)
  let u64 = many1 digit >>= fun s -> return (U64.of_string (String.of_char_list s)) in
  let int = many1 digit >>= fun s -> return (int_of_string (String.of_char_list s)) in
  let value () =
    choice
      [ (u64 >>= fun u -> return (Vu64 u))
      ; (char '[' >>= fun _ ->
        (sep_by u64 (char ',')) >>= fun vs ->
        char ']' >>= fun _ ->
        let vs = U64.Map.of_alist_exn (List.mapi vs ~f:(fun i v -> (U64.of_int i, v))) in
        return (Varr(vs))) ]
  in
  let pmapping =
    ident >>= fun s -> char '=' >> u64 >>= fun u -> return (s,u)
  in
  let mmapping =
    u64 >>= fun s -> char '=' >> u64 >>= fun u -> return (s,u)
  in
  let inline_args = bracketed ident in
  let register_num = bracketed int in
  let mappings p mc =
    bracketed (sep_by p (char ',') >>= fun l -> return (mc l))
  in
  let fname = bracketed ident in
  let args = bracketed (sep_by (value ()) (char ',')) in
  let pmap = mappings pmapping String.Map.of_alist_exn in
  let mmap = mappings mmapping U64.Map.of_alist_exn in
  let interp_args =
    pmap  >>= fun mparam ->
    mmap  >>= fun mmem ->
    fname >>= fun fn ->
    args  >>= fun args ->
    return (fn,mparam,mmem,args)
  in
  choice
    [ (string "typecheck" >>$ Type)
    ; (string "array_assign_expand" >> (bracketed ident) >>= fun fn ->
       return (ArrayAssignExpand fn))
    ; (string "array_expand" >> (bracketed ident) >>= fun fn ->
       return (ArrayExpand fn))
    ; (string "print" >> (bracketed ident) >>= fun name ->
       option (bracketed ident) >>= fun fname -> return (Print(name,fname)))
    ; (string "save"  >> (bracketed ident) >>= fun name ->
       option (bracketed ident) >>= fun fname -> return (Save(name,fname)))
    ; string "register_liveness" >>$ RegisterLiveness
    ; string "strip_comments" >>$  StripComments
    ; (string "register_allocate" >> register_num >>= fun l ->
       return (RegisterAlloc(l)))
    ; string "asm" >> char '[' >> asm_lang >>= (fun l -> char ']' >>$ (Asm(l)))
    ; (string "expand" >> bracketed ident >>= fun fname ->
       pmap >>= fun m -> return (MacroExpand(fname,m)))
    ; (string "inline" >> inline_args >>= fun fname -> return (InlineCalls(fname)))
    ; string "interp" >> interp_args >>=
        fun (fn,mp,mm,args) -> return (Interp(fn,mp,mm,args)) ]

let parse_trafo s =
  let open MP in
  match parse_string (sep_by ptrafo (char ',') >>= fun x -> eof >>$ x) s () with
  | Success t ->
    begin match List.rev t with
    | Asm(l)::rest ->
      if List.exists ~f:(function Asm(_) -> true | _ -> false) rest then (
        eprintf "asm[_] transformation must be last transformation";
        exit 1
      );
      (List.rev rest,Some l)
    | _ -> (t,None)
    end
  | Failed(s,_) ->
    eprintf "parsing transformation string failed: %s.\n%!" s;
    exit 1

let apply_transform trafo (modul0 : modul) =
  let _conv_trans _f _func =
    assert false (*
    let fdef = match func.f_def with
      | Def d -> d
      | Undef | Py _ -> failwith "cannot transform undefined function"
    in
    { func with
      f_def = Def
          { fdef with
            fd_body = stmt_to_base_instrs fdef.fd_body |> f |> base_instrs_to_stmt}
    }
    *)
  in
  let filter_fn modul ofname =
    match ofname with
    | Some fn -> { modul with
                   m_funcs =
                     List.filter modul.m_funcs ~f:(fun f -> f.f_name = fn) }
    | None -> modul
  in
  let app_trafo modul t =
    let notify s fname =
      F.printf "%s in function %a\n%!" s pp_string fname
    in
    match t with
    | InlineCalls(fname) ->
      notify "inlining all calls" fname;
      inline_calls_modul modul fname
    | ArrayExpand(fname) ->
      notify "expanding register arrays" fname;
      array_expand_modul modul fname
    | ArrayAssignExpand(fname) ->
      notify "expanding array assignments" fname;
      array_assign_expand_modul modul fname
    | StripComments -> assert false
      (* conv_trans strip_comments efun *)
    | Print(name,ofname) ->
      let modul_ = filter_fn modul ofname in
      F.printf ">> %s:@\n%a@\n@\n" name pp_modul modul_; modul
    | Save(fname,ofname) ->
      let modul_ = filter_fn modul ofname in
      Out_channel.write_all fname ~data:(fsprintf "%a" pp_modul modul_); modul
    | RegisterAlloc(_n) -> assert false
      (* register_allocate (min 15 n) efun *)
    | RegisterLiveness -> assert false
      (* transform_register_liveness efun *)
    | MacroExpand(fname,m) ->
      notify "expanding macros" fname;
      macro_expand_modul m modul fname
    | Asm(_) -> assert false
    | Type ->
      F.printf "type checking module\n%!" ;
      IL_Typing.typecheck_modul modul;
      modul
    | Interp(fname,pmap,mmap,args) ->
      notify "interpreting" fname;
      IL_Interp.interp_modul modul pmap mmap args fname
  in
  List.fold_left trafo ~init:modul0 ~f:app_trafo

let apply_transform_asm strafo modul =
  let (trafo,mlang) = parse_trafo strafo in
  let modul = apply_transform trafo modul in
  match mlang with
  | None     -> `IL modul
  | Some X64 -> assert false (* `Asm_X64 (List.map ~f:to_asm_x64 modul) *)
