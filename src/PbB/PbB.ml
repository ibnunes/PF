(* Programacão Funcional 2020/2021
 * Problema B
 * 
 * 41358 - Beatriz Tavares da Costa
 * 41381 - Igor Cordeiro Bordalo Nunes
 * 
 * Changelog:
 *    31/03/2021, 1.0 -> Primeira versão
 *    31/03/2021, 1.1 -> Cálculo dos dados da árvore durante a construção
 *    07/04/2021, 1.2 -> Optimização da leitura do input
 *    12/04/2021, 1.3 -> Correcção do cálculo da cor de um nodo da thumbnail
 * *)

(* Tipos de dados segundo definido pelo enunciado. *)
type color = W | B
type image =
  | L of color                            (* Leaf *)
  | N of image * image * image * image    (* Node *)

(* Define uma imagem no formato PBM: n é a dimensão (n * n) e pic é o vector que representa a imagem. *)
type pbm = {n : int; pic : int array}

(* Mapeia um 4-uplo com uma função f. *)
let map_quad f (a, b, c, d) = (f a, f b, f c, f d)

(* Devolve o quadrado de um inteiro. *)
let sqr x = x * x

(* Realiza o output formatado de uma imagem PBM. *)
let print_pbm pbm =
  Array.iteri (fun i x -> Printf.printf "%d%c" x (if (i + 1) mod pbm.n = 0 then '\n' else ' ')) pbm.pic


(* ===== For DEBUG purposes only! ===== *)
let string_of_color = function
  | W -> "W"
  | B -> "B"

let rec string_of_image = function
  | L c -> "L " ^ string_of_color c
  | N (a, b, c, d) -> let (a', b', c', d') = map_quad string_of_image (a, b, c, d) in Printf.sprintf "N (%s, %s, %s, %s)" a' b' c' d'
(* ==================================== *)


(* Dado um bit, devolve a respectiva cor no tipo de dados 'color'. *)
let color_of_bit = function
  | 0 -> W
  | 1 -> B
  | _ -> failwith "color_of_bit: Invalid bit."

(* Dado uma cor no tipo de dados 'color', devolve o respectivo bit. *)
let bit_of_color = function
  | W -> 0
  | B -> 1

(* Verifica se todos os elementos de um array são iguais. *)
let all_equal a = Array.for_all ((=) a.(0)) a


(* Constrói a árvore do tipo 'image' a partir da imagem de formato PBM.
 * Devolve um 3-uplo com a árvor, o nível com a folha mais alta e o número total de folhas. *)
let image_of_pbm pbm =
  let hleaf, nleaf = ref (-1), ref 0 in
  let sub_pbm pbm =
    let (n, pbm) = pbm.n, pbm.pic in
    let a = n * n in
    let x = n / 2 in
    let extract f =
      List.init x f |> List.map (fun i -> Array.sub pbm i x) |> Array.concat
    in
    let nw = {n = x; pic = extract (fun i -> i * (x * 2)) } in
    let ne = {n = x; pic = extract (fun i -> i * (x * 2) + x) } in
    let sw = {n = x; pic = extract (fun i -> i * (x * 2) + (a / 2)) } in
    let se = {n = x; pic = extract (fun i -> i * (x * 2) + (a / 2) + x) } in
      (nw, ne, se, sw)
  in
  let rec run i pbm =
    if all_equal pbm.pic then
      let () = nleaf := !nleaf + 1 in
      let () = if !hleaf <> (-1) then hleaf := min !hleaf i else hleaf := i in
        L (color_of_bit pbm.pic.(0))
    else
      let (nw, ne, se, sw) = sub_pbm pbm |> map_quad (run (i + 1))
      in N (nw, ne, se, sw)
  in let img = run 0 pbm in (img, !hleaf, !nleaf)


(* Constrói um vector no formato PBM a partir de uma árvore do tipo 'image'. *)
let pbm_of_image p img =
  let result = Array.make (p * p) 0 in
  let extract f x a =
    List.init x f |> List.map (fun i -> Array.sub a i x) |> Array.concat
  in
  let nw = fun x a i -> i * (x * 2) in
  let ne = fun x a i -> i * (x * 2) + x in
  let sw = fun x a i -> i * (x * 2) + (a / 2) in
  let se = fun x a i -> i * (x * 2) + (a / 2) + x in
  let fill ind =
    Array.iter (fun i -> result.(i) <- 1) ind
  in
  let rec build n node ind =
    let x = n / 2 in
    let a = n * n in
    match node with
    | L B            -> fill ind
    | N (b, c, d, e) -> List.iter (fun (ind', node') -> build x node' ind') (List.map (fun (f, nd) -> (extract (f x a) x ind, nd)) [(nw, b); (ne, c); (se, d); (sw, e)])
    | _              -> ()
  in
  build p img (Array.init (p * p) (fun i -> i)); {n = p; pic = result}


(* Calcula a thumbnail de dimensão p * p de uma imagem 'img' de tamanho original n * n. *)
let rec thumbnail n p img =
  let rec color_strength_of_node i node =
    match node with
    | L W -> 0
    | L B -> sqr i
    | N (a, b, c, d) -> let (a, b, c, d) = map_quad (color_strength_of_node (i / 2)) (a, b, c, d) in a + b + c + d
  in
  let color_from_strength n x =
    if x < (sqr n) / 2 then L W else L B
  in
  if p = 1 then
    match img with
    | L c -> L c
    | _   -> color_strength_of_node n img |> color_from_strength n
  else
    match img with
    | L c            -> L c
    | N (a, b, c, d) -> let (a, b, c, d) = map_quad (thumbnail (n / 2) (p / 2)) (a, b, c, d) in N (a, b, c, d)


(* Lê do input uma imagem no formato PBM, assumindo que esta é quadrada. *)
let scan_pbm () =
  let _ = read_line () in
  let x = Scanf.sscanf (read_line ()) "%d %d" (fun x _ -> x) in
  let n = x * x in
  let a = Array.make n 0 in
  let rec get_next k =
    if k = 0 then ()
    else
      let line = read_line () |> String.split_on_char ' ' in
      let () = List.iteri (fun i s -> a.(x * (x - k) + i) <- int_of_string s) line in
        get_next (k - 1)
  in let () = get_next x in {n = x; pic = a}


(* Bloco principal:
 * 1. Lê a imagem no formato PBM;
 * 2. Lê a dimensão da thumbnail pretendida;
 * 3. Constrói a árvore quaternária correspondente à imagem;
 * 4. Calcula a thumbnail na dimensão pretendida;
 * 5. Constrói o vector a partir da árvore quaternária;
 * 6. Realiza o output conforme formatado no enunciado.
 *  *)
let () =
  let pbm = scan_pbm () in
  let p   = read_int () in
  let (img, hl, nl) = image_of_pbm pbm in
  let thumb = thumbnail pbm.n p img in
  let thumb_pbm = pbm_of_image p thumb in
  let () = Printf.printf "%d\n%d\n" hl nl in
    print_pbm thumb_pbm
