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

(* let pp_instr fmt name params =
  match params with
  | [] -> Format.fprintf fmt "\t%s" name (* In case there is no params, we do not print a tab*)
  | _ ->  Format.fprintf fmt "\t%-*s\t%s" iwidth name (String.concat ", " params) *)

  let pp_instr fmt name params =
    (* 1. 用逗号空格拼接所有参数 *)
    let s = String.concat ", " params in
  
    (* 2. 把 s 按逗号拆分并去掉空条目、首尾空白 *)
    let toks =
      s
      |> String.split_on_char ','
      |> List.map String.trim
      |> List.filter ((<>) "")
    in
  
    (* 3. 判断并处理向量+mask 的情况 *)
    let s' =
      let n = List.length toks in
      if n >= 2 then
        let last = List.nth toks (n - 1) in
        let prev = List.nth toks (n - 2) in
        let is_mask r =
          String.length r >= 2 && String.sub r 0 2 = "%k"
        in
        let is_vec r =
          String.length r >= 4
          && (String.sub r 0 4 = "%zmm"
              || String.sub r 0 4 = "%ymm"
              || String.sub r 0 4 = "%xmm")
        in
        if is_mask last && is_vec prev then
          (* 把前面 n-2 个寄存器取出来 *)
          let rec take m = function
            | [] -> []
            | _ when m <= 0 -> []
            | x :: xs -> x :: take (m - 1) xs
          in
          let rest = take (n - 2) toks in
          let combined = prev ^ "{" ^ last ^ "}" in
          String.concat ", " (rest @ [combined])
        else
          (* 不满足条件就不改 *)
          s
      else
        s
    in
  
    (* 4. 最终输出，和原来一样处理 name 和格式 *)
    match params with
    | [] ->
        (* 没有任何参数时，只输出 name *)
        Format.fprintf fmt "\t%s" name
    | _ ->
        (* 有参数时对齐 name，再输出处理过的 s' *)
        Format.fprintf fmt "\t%-*s\t%s" iwidth name s'
  

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
