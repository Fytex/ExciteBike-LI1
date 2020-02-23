{- |
Module: Tarefa1_2019li1g068
Description: Módulo Haskell contendo as funções necessárias, relativas à Tarefa 1 do Projeto da Unidade Curricular de LI1 
Copyright: Fytex;
           Arkimedez

Um módulo contendo definições Haskell para executar com sucesso a Tarefa 1.
A Tarefa 1 consiste no ato de gerar um mapa(/output/), tendo-se como /inputs/ 3 números inteiros:
->O número de 'Pista's;
->O comprimento de cada 'Pista';
->Um número inteiro positivo para usar como semente num gerador pseudo-aleatório)
-}

--Este módulo define funções comuns da Tarefa 1 do trabalho prático.

module Tarefa1_2019li1g068 where

import LI11920
import System.Random

-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é um triplo (/número de 'Pista's/,/comprimento de cada 'Pista' do 'Mapa'/,/semente de aleatoriedades/).
testesT1 :: [(Int,Int,Int)]
testesT1 = [(1, 1, 0), (5, 6, 7), (12, 5, 9), (1, 25, 5), (30, 1, 4), (15, 2, 8), (25, 30, 1),(88,34,2),(125,1,5),(8,78,3),(9,2,0)]

-- * Funções pré-definidas da Tarefa 1.

{- |

Dado um número aleatório entre 0 e 9 e dada um outro número, uma semente:gera-se uma lista de inteiros.

-}
geraAleatorios :: Int -- ^ Nº. de aleatórios que se pretende gerar
               -> Int -- ^ Semente inicial
               -> [Int] -- ^ Lista de aleatórios
geraAleatorios n seed = take n (randomRs (0,9) (mkStdGen seed))

{- ^

==Forma de atuar:

Num mapa de dimensão 2x6, o número de células cujo conteúdo tem que ser gerado é 10 
Não é 12 porque se assume que as 2 pistas começam com a peça (Recta Terra 0)
Deste modo, para determinar o respectivo conteúdo, iremos precisar de gerar uma sequência de 20 números aleatórios (2 para cada peça).


==Exemplo de utilização:

>>>geraAleatorios 16 1
[5,6,5,7,8,7,4,5,0,8,1,5,4,1,6,3]

-}

-- * Funções principais da Tarefa 1.

-- | Gera um 'Mapa' recebendo o n.º 'Pista's, o comprimento das 'Pista's e a semente como parâmetros.

gera :: Int  -- ^ Nº.'Pista's 
     -> Int  -- ^ Comprimento das 'Pista's
     -> Int  -- ^ Semente
     -> Mapa -- ^ 'Mapa' gerado
gera npistas comprimento semente | blocos == 0 = replicate npistas [primeiraPeca]
                                 | otherwise = map (geraPista primeiraPeca) (chunksOf (blocos*2) aleatorios)  
    where
        blocos = comprimento - 1
        aleatorios = geraAleatorios (npistas * blocos * 2) semente
        primeiraPeca = (Recta Terra 0)


{- ^

==Exemplo de utilização:

>>>gera 2 5 1
[[Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Recta Lama 0],[Recta Terra 0, Recta Terra 0, Recta Terra 0, Rampa Lama 0 2, Rampa Lama 2 0]]

-}

-- * Funções auxiliares da Tarefa 1.

{- | Gera o 'Piso' de cada 'Peca'. Para isso recebe o 'Piso' da 'Peca' anterior e o inteiro aleatório.
Cada 'Peca' tem 2 aleatórios oriundos da 'geraAleatorios'. Desses 2, esta função tem em conta o primeiro e utiliza-o, sendo esse aleatório uma gama, que originará um 'Piso':
0 a 1: 'Terra';
2 a 3: 'Relva';
4: 'Lama'; 
5: 'Boost';
6 a 9: 'Piso' da 'Peca' anterior

-}
 
geraPiso :: Piso -> Int -> Piso
geraPiso anterior x | x <= 1 = Terra
                    | x <= 3 = Relva
                    | x == 4 = Lama
                    | x == 5 = Boost
                    | otherwise = anterior


-- | Dada uma 'Peca' são retornadas as alturas da mesma

pegaAlturas :: Peca -> (Int,Int)
pegaAlturas (Recta _ y) = (y,y)
pegaAlturas (Rampa _ y y') = (y,y')


-- | Dada uma 'Peca' é retornado o 'Piso' da mesma.

pegaPiso :: Peca -> Piso
pegaPiso (Recta piso _) = piso
pegaPiso (Rampa piso _ _) = piso


{- |
Recebe a 'Peca' anterior e um par de dois números aleatórios para gerar a nova 'Peca'.
Para o fazer, terá em conta o valor(/Gama/) do segundo número aleatório para gerar 'Peca's com um determinado tipo:
-> Se a /Gama/ desse número vai de 0 e 1, então teremos uma 'Rampa' que sobe com diferença de /Gama+1/ para a altura anterior;
-> Se a /Gama/ desse número vai de 2 e 5, então teremos uma 'Rampa' que Rampa que desce com diferença máxima de /Gama-1/ para a altura anterior;
-> Se a /Gama/ desse número vai de 6 a 9, então teremos uma 'Recta' com a altura da 'Peca' anterior.
-}

geraPeca :: Peca -> (Int, Int) -> Peca
geraPeca anterior (x, y)| y <= 1 = Rampa piso hAnterior (hAnterior + y + 1) 
                        | y >= 6 || hFinalDesce == hAnterior = Recta piso hAnterior
                        | otherwise = Rampa piso hAnterior hFinalDesce
        where -- Retorna uma reta caso calhe para ser reta ou então calhe uma rampa com 0 de declive
            hAnterior = snd (pegaAlturas anterior)
            pisoAnterior = pegaPiso anterior
            piso = geraPiso pisoAnterior x
            hFinalDesce = max (hAnterior - (y-1)) 0


-- | Recebe a 'Peca' anterior e uma lista de inteiros aleatórios para gerar as 'Peca's relativas aos números da lista fornecida, retornando uma Pista. 

geraPista :: Peca ->[Int] -> Pista
geraPista anterior (x:y:ys) = anterior:(geraPista peca ys)
    where
        peca = geraPeca anterior (x,y)
geraPista anterior _ = anterior:[]

-- | Função que, dado um número inteiro e uma lista, dá uma lista de listas, em que essas listas possuem um número de elementos igual ao número inteiro fornecido.
-- Optamos por definir esta função, pois estava a ocorrer um erro no /GHC-Wall/ quando colocávamos "__import Data.List.Split__".

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf i l = l1:chunksOf i l2
    where (l1, l2) = splitAt i l
