(* Programacão Funcional 2020/2021
 * Problema D
 * 
 * 41381 - Igor Cordeiro Bordalo Nunes
 * 
 * Changelog:
 *    14/06/2021, 1.0 -> Primeira versão.
 *    19/06/2021, 1.1 -> Adiciona documentação. Simplifica identificadores.
 *                       Remove código para debugging.
 * *)

type edge = int * int             (*  edge = (Planeta "filho", custo)  *)

type planet = {
  mutable nodes : edge list;      (*  Planetas "filho"              *)
  mutable child : int;            (*  Custo maior enquanto "filho"  *)
  mutable root  : int             (*  Custo maior enquanto "raiz"   *)
}

type empire = planet array        (*  Império = conjunto de planetas  *)


(* Processamento dos custos (função descrita na documentação)
 *    p -> Planeta
 *    c -> Custo C (child cost === custo até à terra)
 *    w -> Custo de viagem local (peso da aresta)
 *    r -> Custo R (root cost)
 * *)
let process_costs emp =
  let rec run c (p, w) =
    let c = c + w in
    let () = emp.(p).child <- c in
    let r = List.map (run c) emp.(p).nodes |> List.fold_left max 0 in
    let () = emp.(p).root <- r in
      r + w
  in let _ = run 0 (0, 0) in Array.map (fun p -> max p.child p.root) emp


(* Leitura do input *)
let get_empire () =
  let n = read_int () and m = read_int () in
  let emp = Array.init n (fun _ -> {nodes = []; child = 0; root = 0}) in
  let rec get_wormholes i =
    if i <= 0 then ()
    else
      let (a, b, c) = Scanf.sscanf (read_line ()) "%d %d %d" (fun a b c -> (a, b, c)) in
      let () = emp.(a - 1).nodes <- (b - 1, c) :: emp.(a - 1).nodes in
        get_wormholes (i - 1)
  in let () = get_wormholes m in emp


(* Função "main" *)
let () = get_empire () |> process_costs |> Array.iter (Printf.printf "%d\n")


(* ALGORITMO
 * ---------
 * O algoritmo baseia-se no Depth-first search (DFS), adaptado ao problema D.
 * 
 * Dadas as 2 condições que definem para onde uma colónia pode viajar, podemos
 * obter dois corolários importantes para optimização:
 *    C1. A Terra tem como maior custo a colónia que lhe fica mais longe;
 *    C2. As colónias "folha" têm como maior custo a viagem até à Terra.
 * 
 * Ora, as colónias "intermédias" podem:
 *    1. Viajar até à Terra, passando pelas colónias que lhe ficam a caminho;
 *    2. Viajar (recursivamente) às colónias que lhe são "filhas".
 * 
 * Isto permite obter uma nova optimização para o cálculo do maior custo das
 * colónias "intermédias":
 *    C3. O maior custo das colónias intermédias é ou o custo até à Terra ou o
 *        custo até à colónia "filha" que lhe fica mais distante, sendo esta uma
 *        "folha".
 * 
 * Tendo em conta C3, podemos definir dois custos associados a cada colónia:
 *    1. Child cost: maior custo enquanto "filha" das colónias que lhe são "raiz";
 *    2. Root cost: maior custo enquanto "raiz" de uma sub-árvore de colónias.
 * 
 * Considere-se então o império dado no enunciado, em particular o seguinte "ramo".
 * Fique ainda associada a cada colónia os custos C (child cost) e R (root cost):
 * 
 *  R1 = 0          R5 = 0          R7 = 0
 *  C1 = 0          C5 = 0          C7 = 0
 *  (1) ---- 7 ---- (5) ---- 6 ---- (7)
 *   ^               |
 *   |               |
 * [...]             +----- 15 ---- (6)
 *                                  R6 = 0
 *                                  C6 = 0
 * 
 * Partindo da Terra (colónia 1), acumulamos o custo da viagem até ela, da esquerda
 * para a direita, considerando W (weight, ou peso da aresta):
 * 
 *  R1 = 0          R5 = 0          R7 = 0
 *  C1 = 0          C5 = 0 + 7      C7 = 7 + 6
 *  (1) ---- 7 ---> (5) ---- 6 ---> (7)
 *   ^               |
 *   |               |
 * [...]             +----- 15 ---> (6)
 *                                  R6 = 0
 *                                  C6 = 7 + 15
 * 
 * Ficamos, portanto, com os seguintes custos C (child cost):
 * 
 *  R1 = 0          R5 = 0          R7 = 0
 *  C1 = 0          C5 = 7          C7 = 13
 *  (1) ---- 7 ---- (5) ---- 6 ---- (7)
 *   ^               |
 *   |               |
 * [...]             +----- 15 ---- (6)
 *                                  R6 = 0
 *                                  C6 = 22
 * 
 * Façamos então a viagem de "regresso" a partir de cada folha, acumulando o
 * custo R (root cost). O custo R associado a cada colónia é o MÁXIMO de todos
 * os que "recebe" das colónias "filhas":
 * 
 *  R1 = 28         R5 = 15         R7 = 0
 *  C1 = 0          C5 = 7          C7 = 13
 *  (1) <--- 7 ---- (5) <--- 6 ---- (7)
 *   ^               ^
 *   |               |
 * [...]             +----- 15 ---- (6)
 *                                  R6 = 0
 *                                  C6 = 22
 * 
 * Onde:
 *    -> R5 = max(0 + 6, 0 + 15) = max(6, 15) = 15;
 *    -> R1 = max(15 + 7, ...) = max(22, ...) = 28.
 * 
 * Note-se que a Terra recebe valores para R de 2 outras colónias, nomeadamente:
 *    -> Colónia 2: R = 0 + 12 = 12;
 *    -> Colónia 3: R = 18 + 10 = 28.
 * Deste modo, R1 = max(22, 12, 28) = 28.
 * O valor para R3 advém da mesma lógica implementada na colónia 5.
 * 
 * O maior custo, L (largest cost), para cada colónia é dada por:
 *    L = max(C, R)
 * 
 *  L1 = 28         L5 = 15         L7 = 13
 *  (1) <--- 7 ---- (5) <--- 6 ---- (7)
 *   ^               ^
 *   |               |
 * [...]             +----- 15 ---- (6)
 *                                  L6 = 22
 * 
 * 
 * IMPLEMENTAÇÃO  (função 'process_costs')
 * ---------------------------------------
 * Podemos tirar partido das funções recursivas não-terminais para realizar as
 * "viagens de ida e volta", de tal forma que:
 *    1. O custo C é acumulado por argumento na chamada recursiva;
 *    2. O custo R é acumulado por valor de retorno (backtracking).
 * 
 * A função interna 'run' executa as chamadas recursivas e todos os cálculos,
 * começando no Planeta 1 (p = 0; índice 0 do array de impérios, 'emp') com um
 * custo para a Terra de 0 (c = 0) e um custo local de viagem de 0
 * (w = 0):    run 0 (0, 0)
 * 
 * O custo para a Terra é actualizado, sendo redefinido o identificador:
 *    let c = c + w
 * 
 * O custo C do Planeta 'planet' é definido:
 *    let () = emp.(p).child <- c
 * 
 * Os filhos da colónia actual são mapeados com a função 'run', o que irá produzir
 * uma lista de custos R. O maior destes custos é então obtido:
 *    let r = List.map (run c) emp.(p).nodes |> List.fold_left max 0
 * 
 * 'colony_cost' é, então, o custo R da colónia actual:
 *    let () = emp.(p).root <- r
 * 
 * Por fim, devolve-se para o backtracking o valor R da colónia raiz a partir da
 * colónia actual:
 *    r + w
 * 
 * O array de impérios 'emp' fica assim com os custos R e C actualizados para
 * todas as colónias. O resultado final é o máximo destes dois para cada colónia,
 * pelo que se pode mapear o array e gerar um array novo com os resultados finais:
 *    Array.map (fun p -> max p.child p.root) emp
 * 
 * Este array de inteiros tem, por ordem, o maior custo, L, para cada colónia.
 * 
 * 
 * COMPLEXIDADE
 * ------------
 * A chamada recursiva é feita para cada filho, o que corresponde a uma aresta.
 * Sabendo que existem M arestas, então são feitas M chamadas.
 * Se consierarmos o backtracing, são feitos mais M cálculos.
 * Uma vez que percorre o grafo todo, de N vértices, e sendo baseado num DFS,
 * concluímos que a complexidade é:
 * 
 *        O(N + M)
 * 
 * A complexidade é, portanto, LINEAR face ao tamanho do grafo.
 *)