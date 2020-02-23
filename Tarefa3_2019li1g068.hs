{- |
Module: Tarefa3_2019li1g068
Description: Módulo Haskell contendo as funções necessárias, relativas à Tarefa 3 do Projeto da Unidade Curricular de LI1 
Copyright: Fytex;
           Arkimedez

Um módulo contendo definições Haskell para executar com sucesso a Tarefa 3:
A Tarefa 3 consiste no ato de desconstruir um mapa num conjunto de instruções. Para tal temos de definir funções que nos possam ajudar nessa tarefa.
Os bulldozers (um por pista) que avançarão desde da partida, e usarão o conjunto de instruções para construir o mapa em questão.

-}


{- |

=Introdução

Na Tarefa 3 tivemos de arranjar uma forma de desconstruir mapas, de forma a que  dado um mapa cujo comprimento de cada pista é maior que 1, 
este fosse convertido numa sequência de instruções. Estas instruções eram dadas a um grupo de bulldozers (um por pista) que avançavam da partida para construir 
o mapa em questão.


= Objetivos

A questão chave desta Tarefa era a de identificar padrões, que podiam ser /horizontais/,/verticais/ ou /verticais desfasados/.
O nosso primeiro objetivo foi o de garantir que a nossa função 'desconstroi' fazia o básico, algo que foi garantido pelas funções auxiliares 'convertePeca' e
'converteMapa'.
Seguidamente, tentamos tratar os padrões /horizontais/, oficio que não nos pareceu muito complexo. Sucedemos, com a função 'loopPistas', que gerou as várias 
permutações de 'Repete's .Porém nesta fase da Tarefa já tinhamos em mente os padrões /verticais/ e, portanto, optámos por não usar nunca a opção de criar um padrão 
quando só se repete 2 vezes. Deixamos essa decisão em forma de /Nota/:
É preferivel [Anda Terra 0, Anda Terra 0] a [Repete 2 Anda Terra 0] pois podemos agrupar com a linha de baixo caso seja possivel!
Depois, era a vez de tentar os padrões /verticais/, que já constituiam uma fase bastante complexa da Tarefa. Conseguimos.
Há que deixar anotado que o nosso objetivo principal, principalmente depois de termos, todos os restantes objetivos desta Tarefa e as Tarefas 1 e 2 totalmente 
otimizadas, foi tentar tratar os padrões /verticais desfasados/. Tentamos, com bastante esforço, implementar a /Teleporta/ no nosso projeto. Conseguimos criar os 
padrões e a função até era funcional, mas tinha alguns /bugs/ e, como o site não esteve funcional durante um período de tempo considerável, o que aliado a uma 
grande quantidade de grupos que depois tiveram de testar as suas Tarefas, não conseguimos resolver os bugs da /Teleporta/.


= Discussão e Conclusão

Em suma, acreditamos que fizemos um bom trabalho e ficamos, grosso modo, satisfeitos com o resultado. Acabamos por ficar em 3º. lugar sem o tratamento dos padrões 
/verticais desfasados/ , como era o nosso objetivo. Além disso, não deixa de ser caricato constatar que mesmo com os /bugs/ referidos em cima, conseguimos ficar em
4º.lugar. 
No final optamos por deixar a versão sem os /bugs/.  

-}

--Este módulo define funções comuns da Tarefa 3 do trabalho prático.
module Tarefa3_2019li1g068 where

import LI11920
import Tarefa1_2019li1g068

-- * Testes

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Mapa'.
testesT3 :: [Mapa]
testesT3 = [[[Recta Terra 0, Recta Terra 0, Recta Terra 0]],[[Recta Terra 0, Recta Lama 0], [Recta Terra 0, Recta Boost 0], [Recta Terra 0, Recta Lama 0]], [[Recta Terra 0, Recta Terra 0, Recta Terra 0, Recta Terra 0, Recta Lama 0, Recta Lama 0, Recta Lama 0], [Recta Terra 0, Recta Terra 0, Recta Lama 0, Recta Terra 0, Recta Lama 0, Recta Terra 0, Recta Lama 0]], [[Recta Terra 0, Recta Terra 0, Recta Cola 0, Recta Lama 0, Recta Terra 0, Recta Cola 0, Recta Lama 0], [Recta Terra 0, Recta Terra 0, Recta Lama 0, Recta Terra 0, Recta Lama 0, Recta Boost 0, Recta Lama 0]],gera 5 5 5, gera 7 5 2, gera 2 4 5, gera 1 2 5, gera 9 10 3, gera 2 3 5, gera 12 2 3, gera 4 5 7, gera 4 8 1, gera 9 8 4,gera 4 19 9, gera 50 20 9]

-- * Funções principais da Tarefa 3.

-- | Desconstrói um 'Mapa' numa sequência de 'Instrucoes'.
--
-- __NB:__ Uma solução correcta deve retornar uma sequência de 'Instrucoes' tal que, para qualquer mapa válido (/m/), executar as instruções '(desconstroi m)' produza o mesmo mapa (/m/).
--
-- __NB:__ Uma boa solução deve representar o 'Mapa' dado no mínimo número de 'Instrucoes', de acordo com a função 'tamanhoInstrucoes'.
desconstroi :: Mapa -> Instrucoes
desconstroi m = verticalInstrucoes (loopPistas (converteMapa m 0))

--transpostaMatriz :: [[a]] -> [[a]]
--transpostaMatriz [] = []
--transpostaMatriz l = (map head l):transpostaMatriz (map tail l)

-- * Funções auxiliares da Tarefa 3.

  
-- | Função que dada uma Peca e um inteiro(relativo à pista da Peca), dá uma Instrucao, que será utilizada pelo bulldozer

convertePeca :: Int -> Peca -> Instrucao 
convertePeca n (Recta piso _) = (Anda [n] piso)
convertePeca n (Rampa piso y y') | y' > y = (Sobe [n] piso diff)
                                 | otherwise = (Desce [n] piso diff)
    where diff = abs (y-y')


-- | Função que converte o Mapa numa lista de 'Instrucoes'. Esta função é a mais simples, uma vez que não tem em conta os padrões horizontais, verticais e verticais desfasados. 

converteMapa :: Mapa -> Int -> [Instrucoes]
converteMapa [] _ = []
converteMapa (h:t) n = (map (convertePeca n) (tail h)):(converteMapa t (n+1))


-- | Função que será usada para gerar as várias permutações de 'Repete's

loopPistas :: [Instrucoes] -> [Instrucoes]
loopPistas [] = []
loopPistas (h:t) = (loopRepeteInstrucoes h (1, (div (length h) 2))):loopPistas t

-- | Função que será usada pela 'loopPistas' para fazer o /looping/ das várias permutações

loopRepeteInstrucoes :: Instrucoes -> (Int,Int) -> Instrucoes
loopRepeteInstrucoes l (start, end) | start > end = l
                                    | otherwise = loopRepeteInstrucoes (repeteInstrucoes l start) ((start + 1), end)

{- | 
Função que recebe uma permutação de cada vez e tenta gerar 'Instrucoes', otimizando essa lista com os 'Repete's 

==NOTA:
É preferivel [Anda Terra 0, Anda Terra 0] a [Repete 2 Anda Terra 0] pois podemos agrupar com a linha de baixo caso seja possivel!
-}

-- preferivel [Anda Terra 0, Anda Terra 0] a [Repete 2 Anda Terra 0] pois podemos agrupar com a linha de baixo caso seja possivel!
repeteInstrucoes :: Instrucoes -> Int -> Instrucoes
repeteInstrucoes [] _ = []
repeteInstrucoes l@(h:t) step | n > 0 && (n /= 1 || step /= 1) = (Repete (n+1) l1):repeteInstrucoes (drop (n*step) l2) step
                              | otherwise = h:repeteInstrucoes t step
    where
        (l1, l2) = splitAt step l
        l' = chunksOf step l2
        n = length (takeWhile (==l1) l')


-- | Função que vê as 'Instrucoes' de duas pistas e cria um padrão vertcal, se ele fôr possível.

verticalInstrucoes :: [Instrucoes] -> Instrucoes
verticalInstrucoes (h:h':t) = verticalInstrucoes  ((introduzirInstrucoes h' h):t)
verticalInstrucoes l = concat l

-- | Função que permite criar o padrão vertical, ao criar 'Instrucoes' que resultam de introduzir novas 'Instrucoes' às 'Instrucoes' iniciais

introduzirInstrucoes :: Instrucoes -> Instrucoes -> Instrucoes
introduzirInstrucoes (instrucao:resto) l@(_:_)  | null l2 = instrucao:introduzirInstrucoes resto l
                                           | otherwise = l1 ++ (somaPistas instrucao ins):introduzirInstrucoes resto t
    where
        (l1, l2) = span (instrucoesDiff instrucao) l
        (ins:t) = l2
introduzirInstrucoes l [] = l
introduzirInstrucoes _ l = l

-- | Se tivermos pelo menos um dos casos, então dá /True/ 

-- funcao or recebe uma lista e retorna True se pelo menos um for verdadeiro
instrucoesDiff :: Instrucao -> Instrucao -> Bool
instrucoesDiff (Anda _ piso) (Anda _ piso') = piso /= piso'
instrucoesDiff (Sobe _ piso h) (Sobe _ piso' h') = piso /= piso' || h/=h'
instrucoesDiff (Desce _ piso h) (Desce _ piso' h') = piso /= piso' || h/=h'
instrucoesDiff (Repete i ins) (Repete i' ins') = i/=i' ||( not (primeiraDentroSegundaLista ins ins') && not (primeiraDentroSegundaLista ins' ins))
instrucoesDiff _ _ = True


-- | Verifica se uma lista de instruções está contida noutra lista de instruções, independentemente da lista de pistas.

primeiraDentroSegundaLista :: Instrucoes -> Instrucoes -> Bool
primeiraDentroSegundaLista l@(h:t) (h':t') | not (instrucoesDiff h h') = primeiraDentroSegundaLista t t'
                                        | otherwise = primeiraDentroSegundaLista l t'
primeiraDentroSegundaLista [] _  = True
primeiraDentroSegundaLista _ _ = False


-- | Converge 2 listas de instruções.

juntaPrimeiraASegundaLista :: Instrucoes -> Instrucoes -> Instrucoes
juntaPrimeiraASegundaLista l@(h:t) (h':t') | not (instrucoesDiff h h') = (somaPistas h h'):juntaPrimeiraASegundaLista t t'
                                                | otherwise = h':juntaPrimeiraASegundaLista l t'
juntaPrimeiraASegundaLista _ l = l




-- | Função que que recebe 'Instrucao' e 'Instrucao' e junta os indíces das 'Pista's com a mesma 'Instrucao'

-- no caso dos repetes se forem iguais entao a soma de cada x pertencente a um dos repetes com y pertencente a ao outro tem de dar [x] e nao [x,y]
somaPistas :: Instrucao -> Instrucao -> Instrucao
somaPistas (Anda l piso) (Anda l' _) = Anda (l++l') piso
somaPistas (Sobe l piso h) (Sobe l' _ _) = Sobe (l++l') piso h
somaPistas (Desce l piso h) (Desce l' _ _) = Desce (l++l') piso h
somaPistas (Repete i ins) (Repete _ ins') | length ins > length ins' = Repete i (juntaPrimeiraASegundaLista ins' ins)
                                          | otherwise = Repete i (juntaPrimeiraASegundaLista ins ins')
