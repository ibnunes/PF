(* Programacão Funcional 2020/2021
 * Problema A
 * 
 * 41358 - Beatriz Tavares da Costa
 * 41381 - Igor Cordeiro Bordalo Nunes
 * 
 * Changelog:
 *    24/02/2021, 1.0 -> Primeira versão.
 *    24/02/2021, 1.1 -> Utilizacão de árvores ternárias.
 *    25/02/2021, 1.2 -> Breve optimização (baseado nas árvores, mas sem as usar explicitamente).
 *    28/02/2021, 2.0 -> Utilização de uma tabela hash.
 * *)

exception BadLuck

(* Verifica se m é um múltiplo de d. *)
let (%) m d = m mod d = 0

(* Calcula o produto dos últimos dois dígitos de m. *)
let prod_of_last m = (m mod 10) * ((m / 10) mod 10)

(* FUNÇÃO 'solve':
 * Dado um valor inicial 'm', tenta encontrar quantos passos são estritamente
 * necessários para alcançar o valor 42 por aplicação das regras do enunciado.
 * Levanta uma excepção caso se determine que não há solução.
 * Algoritmo descrito no final do source code. *)
let solve m =
  let hashy : (int, unit) Hashtbl.t = Hashtbl.create 20 in
  let apply_rules x =
    let rule (d, f) =
      if List.exists ((%) x) d then
        let y = x - f x in
        if y >= 42 && x <> y then if not (Hashtbl.mem hashy y) then Hashtbl.add hashy y ()
    in
      let rules = [([2], fun z -> z / 2); ([3; 4], prod_of_last); ([5], fun _ -> 42)]
      in (List.iter rule rules; Hashtbl.remove hashy x)
  in
  let rec run s =
    if Hashtbl.mem hashy 42 then s else
      let keys = List.of_seq (Hashtbl.to_seq_keys hashy) in
        if keys = [] then raise BadLuck
        else (List.iter apply_rules keys; run (s + 1))
  in (Hashtbl.add hashy m (); run 0)

(* Bloco principal *)
let () =
  try Printf.printf "%d\n" (solve (read_int ()))
  with BadLuck -> print_endline "BAD LUCK"

(* ALGORITMO
 * ---------
 * Para maior eficiência de tempo e de espaço em memória, recorremos a uma tabela hash.
 * Alcunhámos a tabela de "hashy".
 * 
 * As chaves de hashy são os valores possíveis a cada passo para serem aplicadas as regras.
 * Como só nos interessam as chaves propriamente ditas, o binding será um unit.
 * 
 * O uso da hashy permite aceder aos elementos com tempo O(1), sendo apenas O(n) o acesso
 * à lista de chaves e consequente aplicação das regras.
 * 
 * A hashy começa com uma só chave: o valor inicial M.
 * A M são aplicadas as 3 regras e, caso os valores obtidos sejam válidos, são adicionados
 * como novas chaves da hashy.
 * Este processo é repetido recursivamente às chaves (x) actualizadas da hashy, sendo os novos
 * valores (y) adicionados se e só se forem válidos (x <> y e y >= 42).
 * 
 * A cada iteração verifica-se se 42 é uma chave de hashy. Se for, encontrámos a solução mais
 * curta do problema, sendo o processo automaticamente interrompido.
 * Contudo, caso não haja chaves na hashy, chegámos a uma situação em que não é possível
 * ganhar o jogo com o valor M fornecido.
 * *)

(* TÉCNICAS UTILIZADAS
 * -------------------
 * Tabelas de Hash
 * Funções lambda
 * Funções parciais
 * *)