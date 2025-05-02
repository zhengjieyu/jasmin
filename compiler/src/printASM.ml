open Prog
open Utils
open PrintCommon
open Arch_decl

type asm_element =
| Header of string * string list
| Label of string
| Dwarf of string (* Debug info in std dwarf format*)
| Instr of string * string list
| Comment of string
| Byte of string

let iwidth = 4

type asm = asm_element list

let pp_header fmt name params =
  match params with
  | [] -> Format.fprintf fmt "\t%s" name
  | _ ->  Format.fprintf fmt "\t%-*s\t%s" iwidth name (String.concat ", " params)

let pp_label fmt name =
  Format.fprintf fmt "%s:" name


let pp_instr fmt name params =
  (* Post-process the joined parameter string:
      remove any spaces or commas before an opening brace *)
  let post_process s =
    let re = Str.regexp ", {" in
    Str.global_replace re "{" s
  in
  match params with
  | [] -> Format.fprintf fmt "\t%s" name
  | _ ->
      let raw = String.concat ", " params in
      let cleaned = post_process raw in
      Format.fprintf fmt "\t%-*s\t%s" iwidth name cleaned
  
  

let pp_comment fmt comment =
  Format.fprintf fmt "// %s" comment

let pp_byte fmt byte =
  Format.fprintf fmt "\t.byte\t%s" byte

let pp_dwarf fmt (dwarf: string) =
  Format.fprintf fmt "\t%s" dwarf

let pp_asm_element fmt asm_element =
  match asm_element with
  | Header (name, params) ->
    pp_header fmt name params
  | Label name ->
    pp_label fmt name
  | Dwarf locs ->
    pp_dwarf fmt locs
  | Instr (name, params) ->
    pp_instr fmt name params
  | Comment content ->
    pp_comment fmt content
  | Byte data ->
    pp_byte fmt data

let pp_asm_line fmt =
  Format.fprintf fmt "%a\n%!" pp_asm_element

let pp_asm fmt asm =
  List.iter (pp_asm_line fmt) asm
