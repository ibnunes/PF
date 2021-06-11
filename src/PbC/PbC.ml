(* Programacão Funcional 2020/2021
 * Problema C
 * 
 * 41358 - Beatriz Tavares da Costa
 * 41381 - Igor Cordeiro Bordalo Nunes
 * 
 * Changelog:
 *    31/03/2021, 1.0 -> Primeira versão.
 *    07/04/2021, 1.1 -> Optimização geral e redução de uso de memória.
 *    12/04/2021, 1.2 -> Correcção de bug severo na comparação dos algoritmos.
 *    25/04/2021, 1.3 -> Remoção de código excessivo. Melhoria da documentação.
 * *)

(* Devolve um 3-uplo com (moeda mais baixa, moeda mais alta, lista de moedas). *)
let info_of_coins coins = (List.hd coins, List.rev coins |> List.hd, coins)

(* Verifica se o último elemento da lista é maior que o valor mínimo da restante lista.
 * Caso seja maior, então o sistema monetário não é canónico.
 * Caso especial: 0 indica que o troco é impossível com tal moeda, sendo portanto uma condição de paragem imediata. *)
let check lst =
  let rec run m = function
    | 0 :: _       -> m
    | x :: []      -> if m < x then (-1) else x
    | x :: y :: ys -> if y = 0 then (if m < x then (-1) else x) else run (min m x) (y :: ys)
    | _            -> (-1)
  in run (List.hd lst) lst

(* Recebe um sistema monetário e determina se é canónico.
 * Caso a função retorne None, o sistema é canónico.
 * Caso não seja, devolve um option com o valor que falhou associado (Some k).
 * É determinada a menor (low) e a maior (high) moeda do sistema monetário.
 * É feito o teste até 2 * high.
 * O algoritmo recorre a programação dinâmica de complexidade linear, em particular O(n * k), onde:
 *   -> k: número de moedas do sistema monetário;
 *   -> n: limite do teste (valor máximo testado).
 * É utilizada uma hash table com high elementos uma vez que haverá, no máximo, high passos para fazer um troco.
 * Contudo, uma vez que a hash table é de tamanho dinâmico, é incluído um "garbage collector" que vai removendo
 * os elementos memorizados já não necessários a cada passo do problema. *)
let solve coins =
  let (low, high, coins) = info_of_coins coins in
  let lim = 2 * high in
  let table = Hashtbl.create high in
  let rec run k =
    let garbage_collect () =
      Hashtbl.remove table (k - high - 1)
    in
      if k > lim then None
      else (
        let lst = List.map (fun c -> match k - c with 0 -> 1 | z when z >= low -> Hashtbl.find table (k - c) + 1 | _ -> 0) coins in
        let steps = check lst in
        if steps = (-1) then Some k
        else (
          let () = Hashtbl.add table k steps in
          let () = garbage_collect () in
          run (k + 1)
        )
      )
  in if match coins with [] | _ :: [] -> true | _ -> false then None else run low

(* Função que obtém o input *)
let get_input () =
  let n = read_int () in
  let rec next = function
    | 0 -> []
    | i -> let k = read_int () in k :: next (i - 1)
  in next n

(* Bloco principal *)
let () =
  match get_input () |> solve with
  | None   -> print_endline "YES"
  | Some k -> Printf.printf "%d\n" k


(* ALGORITMO / EXEMPLO DE EXECUÇÃO
 * -------------------------------
 * Foi aplicado um algoritmo que permite testar simultaneamente os algoritmos guloso e optimal em tempo linear.
 * Dado um sistema monetário com 'n' moedas, valor de moeda mais baixo 'l', e valor mais alto 'h', a complexidade será O(n * (h - l + 1)).
 * 
 * Consideremos o exemplo do enunciado, no qual se testa o sistema monetário {1, 3, 4}.
 * Seja o sistema Euros, €, a fim de ajudar à leitura do algoritmo e do exemplo de execução.
 * Caso o algoritmo guloso e o optimal produzam caminhos pelo menos igualmente eficientes para fazer o troco até 8€,
 * então este sistema será canónico.
 * Construa-se a seguinte tabela:
 * 
 * +-------+-----+-----+-----+---------+
 * | Valor |  1€ |  3€ |  4€ |  Steps  |
 * +-------+-----+-----+-----+---------+
 * |   1€  |     |     |     |         |
 * |   2€  |     |     |     |         |
 * |   3€  |     |     |     |         |
 * |   4€  |     |     |     |         |
 * |   5€  |     |     |     |         |
 * |   6€  |     |     |     |         |
 * |   7€  |     |     |     |         |
 * |   8€  |     |     |     |         |
 * +-------+-----+-----+-----+---------+
 * 
 * Repare-se nas seguintes propriedades:
 *    1) O caso trivial é aquele em que o valor é igual à moeda, sendo o número de passos do troco 1;
 *    2) Quando a moeda é maior que o valor, o troco é impossível;
 *  	3) O algoritmo guloso, por definição, corresponde sempre à coluna mais à direita possível antes das colunas impossíveis;
 *    4) O algoritmo óptimo é melhor que o guloso quando o número de passos mínimo das células à esquerda da do algoritmo
 *       guloso é menor que esta.
 *    5) O sistema é canónico quando todas as linhas da tabela negam a propriedade 4;
 *    6) O sistema não é canónico assim que falha uma linha uma vez que esta tabela se baseia numa função monótona crescente.
 * 
 * +-------+-----+-----+-----+---------+
 * | Valor |  1€ |  3€ |  4€ |  Steps  |
 * +-------+-----+-----+-----+---------+
 * |   1€  | (1) | --- | --- |    1    |    <-- caso trivial
 * |   2€  | ( ) | --- | --- |         |
 * |   3€  |     | (1) | --- |    1    |    <-- caso trivial
 * |   4€  |     |     | (1) |    1    |    <-- caso trivial
 * |   5€  |     |     | ( ) |         |
 * |   6€  |     |     | ( ) |         |
 * |   7€  |     |     | ( ) |         |
 * |   8€  |     |     | ( ) |         |
 * +-------+-----+-----+-----+---------+
 *                        ^
 *                        |
 *                        +-- ( ) Algoritmo guloso
 * 
 * O valor irá de 1 a 8. Nas três colunas seguintes estão os passos necessários dar para fazer o troco com cada uma das
 * moedas do sistema monetário. O número de passos do melhor caminho é memorizado na coluna "Steps".
 * 
 * Iniciemos então com o valor da moeda mais baixa, 1€, que é o valor mais baixo do qual conseguimos fazer troco.
 *    -> Com uma moeda de 1€, temos o caso trivial de ser a própria moeda, pelo que precisamos de 1 passo;
 *    -> Com as moedas 3€ e 4€ é impossível (em código iremos assumir este caso como 0 passos);
 *    -> O melhor caminho tem 1 passo, assinalado com parêntesis ().
 * 
 * +-------+-----+-----+-----+---------+
 * | Valor |  1€ |  3€ |  4€ |  Steps  |
 * +-------+-----+-----+-----+---------+
 * |   1€  | (1) | --- | --- |    1    |  <-- caso trivial
 * +-------+-----+-----+-----+---------+
 * 
 * Para o valor 2€:
 *    -> Com uma moeda de 1, obtemos um valor de 1€:
 *        --> Por memoização, sabemos que é preciso 1 passo para obter o troco de 1€.
 *        --> Com o passo actual, são precisos 1 + 1 = 2 passos.
 *    -> Com as moedas de 3€ e 4€ é impossível.
 *    -> O melhor caminho tem 2 passos.
 * 
 * +-------+-----+-----+-----+---------+
 * | Valor |  1€ |  3€ |  4€ |  Steps  |
 * +-------+-----+-----+-----+---------+
 * |   1€  |  1  | --- | --- |    1    |  <-- memoização para 2€ (valor) - 1€ (moeda) = 1€ (sub-troco, ou "troco recursivo") (1 passo)
 * |   2€  | (2) | --- | --- |    2    |
 * +-------+-----+-----+-----+---------+
 * 
 * Para o valor 3€ ficamos com:
 * 
 * +-------+-----+-----+-----+---------+
 * | Valor |  1€ |  3€ |  4€ |  Steps  |
 * +-------+-----+-----+-----+---------+
 * |   1€  |  1  | --- | --- |    1    |  
 * |   2€  |  2  | --- | --- |    2    |  <-- memoização para 3€ - 1€ = 2€ (2 passos)
 * |   3€  |  3  | (1) | --- |    1    |  <-- caso trivial
 * +-------+-----+-----+-----+---------+
 * 
 * O algoritmo guloso continua a ser o optimal até ao momento uma vez que o menor número de passos
 * está o mais à direita.
 * Prossiguemos para o valor 4€:
 * 
 * +-------+-----+-----+-----+---------+
 * | Valor |  1€ |  3€ |  4€ |  Steps  |
 * +-------+-----+-----+-----+---------+
 * |   1€  |  1  | --- | --- |    1    |  <-- memoização para 4€ - 3€ = 1€ (1 passo)
 * |   2€  |  2  | --- | --- |    2    |  
 * |   3€  |  3  |  1  | --- |    1    |  <-- memoização para 4€ - 1€ = 3€ (1 passo)
 * |   4€  |  2  |  2  | (1) |    1    |  <-- caso trivial
 * +-------+-----+-----+-----+---------+
 * 
 * +-------+-----+-----+-----+---------+
 * | Valor |  1€ |  3€ |  4€ |  Steps  |
 * +-------+-----+-----+-----+---------+
 * |   1€  |  1  | --- | --- |    1    |  <-- memoização para 5€ - 4€ = 1€ (1 passo)
 * |   2€  |  2  | --- | --- |    2    |  <-- memoização para 5€ - 3€ = 2€ (2 passos)
 * |   3€  |  3  |  1  | --- |    1    |  
 * |   4€  |  2  |  2  |  1  |    1    |  <-- memoização para 5€ - 1€ = 4€ (1 passo)
 * |   5€  |  2  |  3  | (2) |    2    |  
 * +-------+-----+-----+-----+---------+
 * 
 * Neste caso, apesar de haver um caminho alternativo com 2 passos que não o algoritmo guloso,
 * não é melhor. O guloso continua a ser optimal, pelo que é considerado.
 * 
 * Passemos ao valor 6€:
 * 
 * +-------+-----+-----+-----+---------+
 * | Valor |  1€ |  3€ |  4€ |  Steps  |
 * +-------+-----+-----+-----+---------+
 * |   1€  |  1  | --- | --- |    1    |
 * |   2€  |  2  | --- | --- |    2    |  <-- memoização para 6€ - 4€ = 2€ (2 passos)
 * |   3€  |  3  |  1  | --- |    1    |  <-- memoização para 6€ - 3€ = 3€ (1 passo)
 * |   4€  |  2  |  2  |  1  |    1    |
 * |   5€  |  2  |  3  |  2  |    2    |  <-- memoização para 6€ - 1€ = 5€ (2 passos)
 * |   6€  |  3  | (2) |  3  |    2    |  <-- FALHA !!!
 * +-------+-----+-----+-----+---------+
 * 
 * Neste caso encontramos uma falha do teste. Pelas propriedades 3 e 4 temos que:
 *    a) O algoritmo guloso leva 3 passos (coluna mais à direita);
 *    b) O algoritmo optimal leva 2 passos (mínimo das colunas à esquerda da anterior).
 * 
 * Ou seja, como min({3, 2}) < 3, então o algoritmo guloso não é o algoritmo óptimo.
 * 
 * Por consequência, o sistema monetário {1, 3, 4} NÃO é canónico, e o valor mais baixo
 * em que tal ocorre é com o valor 6€.
 * 
 * 
 * DETALHES DE IMPLEMENTAÇÃO RELEVANTES
 * ------------------------------------
 * Por exemplo, com um sistema monetário {8, 10, 12, 15}, o algoritmo testa de 8€ até 30€, mas como a moeda mais alta
 * é a de 15€, não vale a pena manter em memória os passos para trocos de valores abaixo do valor actual em teste
 * subtraído a 16€.
 * Neste sentido, optou-se por uma hash table uma vez que esta tem operações de manipulação O(1) para adicionar e remover
 * elementos, permitindo reduzir o consumo de recursos da memoização sem afetar significativamente a performance.
 * A função interna garbage_collect () realiza esta operação, removendo a cada novo teste (valor k) os passos memorizados
 * para o valor dado por k - high - 1.
 * A memoização é feita pela coluna "Steps", senda esta, portanto, uma representação dos conteúdos da hash table utilizada
 * no código.
 *)
