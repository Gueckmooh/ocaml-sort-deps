let parse_string : string -> (string * string list) list = fun s ->
  Parser.main Lexer.token (Lexing.from_string s);;

let parse_file : in_channel -> (string * string list) list = fun s ->
  Parser.main Lexer.token (Lexing.from_channel s);;

let swap = fun x ->
  let is_cmo = fun x -> let r = Str.regexp ".*\\.cmo" in
    Str.string_match r x 0 in
  let r = Str.regexp "\\(.*\\.cm\\)\\(o\\|i\\)" in
  if is_cmo x then
    Str.replace_first r "\\1i" x
  else
    Str.replace_first r "\\1o" x;;

let find_first = fun l ->
  let firsts = List.map (function (a, _) -> a) l in
  let seconds = List.map (function (_, a) -> a) l in
  let find = fun x ->
    List.hd @@ List.filter (function (a, _) -> a = x) l in
  let rec iter = function
    | e :: s -> let v = List.map (fun x -> List.mem e x || List.mem (swap e) x)
                    seconds in
      if List.fold_left (||) false v then
        iter s
      else (find e)
    | [] -> assert false in
  iter firsts;;

let to_cmo = fun x ->
    let is_cmo = fun x -> let r = Str.regexp ".*\\.cmo" in
      Str.string_match r x 0 in
    let r = Str.regexp "\\(.*\\.cm\\)\\(o\\|i\\)" in
    if not @@ is_cmo x then
      Str.replace_first r "\\1o" x
    else x;;

let ttest = fun x ->
  let max = fun x y -> if x > y then x else y in
  let first = function (a, _) -> a in
  let o = find_first x in
  let h = Hashtbl.create 50 in
  let _ = Hashtbl.add h (first o) 1 in
  let set_value = fun n1 n2 ->
    let (v1, _), (v2, _) = n1, n2 in
    try
      let x = Hashtbl.find h v2 in
      let y = Hashtbl.find h v1 in
      Hashtbl.replace h v2 (max x (y + 1))
    with Not_found ->
      let x = Hashtbl.find h v1 in
      Hashtbl.add h v2 (x+1) in
  let rec iter : string * string list -> unit = fun l ->
    match l with
    | (y, z) ->
      let ll = (List.filter (function (a, _) -> List.mem a z) x) in
      let _ = List.iter (set_value l) ll in
      List.iter iter ll in
  let _ = iter o in
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) h [];;

let main = fun args ->
  if Array.length args = 1 then
    Printf.printf "You should give files as parametter\n"
  else
    let files = let i = Array.to_list Sys.argv in List.tl i in
    let pff = fun x -> let c = open_in x in parse_file c in
    let ll = List.map (fun x -> pff x) files in
    let l = List.fold_left (@) [] ll in
    let l = List.filter (fun x -> let r = Str.regexp ".*\\.cm\\(o\\)" in
                          let (a, _) = x in Str.string_match r a 0) l in
    let l = List.map (fun (x, l) ->
        (x, List.map to_cmo l |>
            List.filter (fun y -> String.compare x y != 0))) l in
    let ll = ttest l in
    List.sort (fun (_, x) (_, y) -> compare x y) ll |>
    List.map (fun (a, _) -> a) |>
    List.rev |>
    List.iter print_endline;;

let _ = main Sys.argv;;

(* TESTS *)

(* let files = ["../deps/sexp_parser.d";
 *              "../deps/lexer.d";
 *              "../deps/xml_interface.d";
 *              "../deps/grafcet.d";
 *              "../deps/logic.d";
 *              "../deps/main.d";
 *              "../deps/instruction.d";
 *              "../deps/instruction_parsing.d";
 *              "../deps/logic_parsing.d";
 *              "../deps/parser.d";
 *              "../deps/check_prop.d";
 *              "../deps/sexp_lexer.d";
 *              "../deps/color.d"];;
 * let pff = fun x -> let c = open_in x in parse_file c;;
 *
 * let ll = List.map (fun x -> let _ = print_endline x in
 *                     pff x) files;;
 *
 *
 * let l = List.fold_left (@) [] ll;;
 * let l = List.filter (fun x -> let r = Str.regexp ".*\\.cm\\(o\\)" in
 *                       let (a, _) = x in Str.string_match r a 0) l;;
 * let l = List.map (fun (x, l) -> (x, List.map to_cmo l |> List.filter (fun y -> String.compare x y != 0))) l;; *)
