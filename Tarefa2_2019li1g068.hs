{- |
Module: Tarefa2_2019li1g068
Description: Módulo Haskell contendo as funções necessárias, relativas à Tarefa 2 do Projeto da Unidade Curricular de LI1 
Copyright: Fytex;
           Arkimedez

Um módulo contendo definições Haskell para executar com sucesso a Tarefa 2.
Na Tarefa 2 temos como /inputs/:
-Um número inteiro(Int) relativo ao número da 'Pista' do 'Jogador' na qual queremos efetuar a 'Jogada';
-A 'Jogada' que pretendemos efetuar;
-O 'Estado', que contém o estado do mapa('mapaEstado') e o estado dos jogadores('jogadoresEstado').
O /output/ será um novo 'Estado', que reflete as alterações provocadas pela 'Jogada' no 'mapaEstado' e/ou no 'jogadoresEstado'.
O objetivo desta Tarefa é, portanto, criar funções que permitam executar a função final('jogada').
-}


-- Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2019li1g068 where

import Data.Fixed
import LI11920

-- * Testes

-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [(0, Movimenta C, Estado [[Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Recta Lama 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0]] [Jogador 0 1 1 1 (Chao True), Jogador 1 2.3 1 5 (Chao False)]), (2, Acelera, Estado [[Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Recta Lama 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0],[Recta Terra 0,Recta Lama 0,Recta Boost 0, Rampa Terra 0 2, Rampa Lama 2 0]] [Jogador 0 1 1 1 (Chao True), Jogador 1 2.3 1 5 (Chao False), Jogador 2 0.5 2 3 (Chao True)]),(1, Movimenta B, Estado [[Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Recta Lama 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0]] [Jogador 0 1 1 1 (Chao False), Jogador 1 2.3 1 5 (Chao True)]), (2, Movimenta D, Estado [[Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Recta Lama 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0],[Recta Terra 0, Rampa Lama 0 2,Rampa Lama 2 0, Recta Boost 0, Recta Lama 0]] [Jogador 0 1 1 1 (Chao True), Jogador 1 2.3 1 5 (Chao False), Jogador 2 2.9 1 2 (Chao False)]) , (2, Movimenta E, Estado [[Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Recta Lama 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0],[Recta Terra 0, Rampa Lama 0 2,Rampa Lama 2 0, Recta Boost 0, Recta Lama 0]] [Jogador 0 1 1 1 (Chao True), Jogador 1 2.3 1 5 (Chao False), Jogador 2 2.9 1 2 (Chao False)]) , (2, Desacelera, Estado [[Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Recta Lama 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0],[Recta Terra 0,Recta Lama 0,Recta Boost 0, Rampa Terra 0 2, Rampa Lama 2 0]] [Jogador 0 1 1 1 (Chao True), Jogador 1 2.3 1 5 (Chao False), Jogador 2 0.5 2 3 (Chao True)]), (2, Dispara, Estado [[Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Recta Lama 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0],[Recta Terra 0,Recta Lama 0,Recta Boost 0, Rampa Terra 0 2, Rampa Lama 2 0]] [Jogador 0 1 1 1 (Chao True), Jogador 1 2.3 1 5 (Chao False), Jogador 2 0.5 2 3 (Chao True)]), (2, Dispara, Estado [[Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Recta Lama 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0],[Recta Terra 0,Recta Lama 0,Recta Boost 0, Rampa Terra 0 2, Rampa Lama 2 0]] [Jogador 0 1 1 1 (Chao True), Jogador 1 2.3 1 5 (Chao False), Jogador 2 0.5 2 0 (Chao True)]), (2, Dispara, Estado [[Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Recta Lama 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0],[Recta Terra 0,Recta Lama 0,Recta Boost 0, Rampa Terra 0 2, Rampa Lama 2 0]] [Jogador 0 1 1 1 (Chao True), Jogador 1 2.3 1 5 (Chao False), Jogador 2 0.5 2 3 (Chao False)]), (1, Movimenta C, Estado [[Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Recta Lama 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0]] [Jogador 0 1 1 1 (Chao True), Jogador 1 2.3 1 5 (Chao False)]),(0, Movimenta D, Estado [[Recta Terra 0,Recta Boost 0,Recta Lama 0,Recta Boost 0,Recta Lama 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0]] [Jogador 0 3.7 1 1 (Chao True), Jogador 1 2.3 0 5 (Morto 1.0)]), (1, Movimenta D, Estado [[Recta Terra 0,Recta Boost 0,Recta Lama 0,Recta Boost 0,Recta Lama 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0]] [Jogador 0 1.3 1 1 (Chao True), Jogador 1 2.3 1 5 (Ar 1.0 4.0 2.0)]), (2, Dispara, Estado [[Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Recta Lama 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0],[Recta Terra 0,Recta Lama 0,Recta Boost 0, Rampa Terra 0 2, Rampa Lama 2 0]] [Jogador 0 1 1 1 (Chao True), Jogador 1 2.3 1 5 (Chao False), Jogador 2 0 2 3 (Chao True)]), (1, Movimenta C, Estado [[Recta Terra 0, Recta Boost 0, Rampa Terra 0 3, Recta Terra 3, Rampa Lama 3 0, Recta Terra 0],[Recta Terra 0 , Recta Lama 0 , Recta Lama 0, Recta Boost 0, Recta Lama 0, Recta Terra 0]] [Jogador 0 1 0 9 (Morto 2.0), Jogador 1 3.6 3 4 (Chao True)]), (0, Movimenta B, Estado [[Recta Terra 0, Recta Boost 0, Rampa Terra 0 3, Recta Terra 3, Rampa Lama 3 0, Recta Terra 0],[Recta Terra 0 , Recta Lama 0 , Recta Lama 0, Recta Boost 0, Recta Lama 0, Recta Terra 0]] [Jogador 0 1 0 9 (Morto 1.0), Jogador 1 3.6 3 4 (Chao True)]), (0, Movimenta D, Estado [[Recta Terra 0, Recta Boost 0, Rampa Terra 0 3, Recta Terra 3, Rampa Lama 3 0, Recta Terra 0],[Recta Terra 0 , Recta Lama 0 , Recta Lama 0, Recta Boost 0, Recta Lama 0, Recta Terra 0]] [Jogador 0 3.1 3 9 (Ar 3.4 90 10), Jogador 1 3.6 3 4 (Chao True)]), (0, Movimenta E, Estado [[Recta Terra 0, Recta Boost 0, Rampa Terra 0 3, Recta Terra 3, Rampa Lama 3 0, Recta Terra 0],[Recta Terra 0 , Recta Lama 0 , Recta Lama 0, Recta Boost 0, Recta Lama 0, Recta Terra 0]] [Jogador 0 3.1 3 9 (Ar 3.4 (-90) 10), Jogador 1 3.6 3 4 (Chao True)]), (0, Movimenta D, Estado [[Recta Terra 0, Recta Boost 0, Rampa Terra 0 3, Recta Terra 3, Rampa Lama 3 0, Recta Terra 0],[Recta Terra 0 , Recta Lama 0 , Recta Lama 0, Recta Boost 0, Recta Lama 0, Recta Terra 0]] [Jogador 0 3.1 3 9 (Chao True), Jogador 1 3.6 3 4 (Chao True)]), (0, Movimenta B, Estado [[Recta Terra 0, Recta Boost 0, Rampa Terra 0 3, Recta Terra 3, Rampa Lama 3 0, Recta Terra 0],[Recta Terra 0 , Recta Lama 0 , Recta Lama 0, Recta Boost 0, Recta Lama 0, Recta Terra 0]] [Jogador 0 3.1 3 9 (Chao True), Jogador 1 3.6 3 4 (Chao True)]),(1, Dispara, Estado [[Recta Terra 0, Recta Boost 0, Rampa Terra 0 3, Recta Terra 3, Rampa Lama 3 0, Recta Terra 0],[Recta Terra 0 , Recta Lama 0 , Recta Lama 0, Recta Boost 0, Recta Lama 0, Recta Terra 0]] [Jogador 0 2.1 3 9 (Ar 3.4 90 10), Jogador 1 3.6 3 0 (Chao True)]),(1, Dispara, Estado [[Recta Terra 0, Recta Boost 0, Rampa Terra 0 3, Recta Terra 3, Rampa Lama 3 0, Recta Terra 0],[Recta Terra 0 , Recta Lama 0 , Recta Lama 0, Recta Boost 0, Recta Lama 0, Recta Terra 0]] [Jogador 0 2.1 3 9 (Ar 3.4 90 10), Jogador 1 3.6 3 4 (Chao True)]),(1, Movimenta C, Estado [[Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Recta Lama 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0]] [Jogador 0 1 1 1 (Chao True), Jogador 1 4.6 1 5 (Ar 3.9 90 2)]), (1, Dispara, Estado [[Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Recta Lama 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0]] [Jogador 0 1 1 1 (Chao True), Jogador 1 4.1 1 5 (Chao True)]),(1, Movimenta C, Estado [[Recta Terra 0,Recta Boost 0,Recta Boost 0,Rampa Terra 0 2,Rampa Lama 2 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0]] [Jogador 0 1 1 1 (Chao True), Jogador 1 4.6 1 5 (Chao True)]),(0, Movimenta B, Estado [[Recta Terra 0,Recta Boost 0,Recta Boost 0,Rampa Terra 0 2,Rampa Lama 2 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Lama 0]] [Jogador 0 4.1 4 1 (Chao True), Jogador 1 4.6 1 5 (Chao True)]),(1, Movimenta D, Estado [[Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Recta Lama 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0]] [Jogador 0 1 1 1 (Chao True), Jogador 1 2.3 1 5 (Ar 3.0 80.0 2.0) ]),(1, Movimenta D, Estado [[Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Recta Lama 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0]] [Jogador 0 1 1 1 (Chao True), Jogador 1 2.3 1 5 (Ar 3.0 (-80.0) 2.0) ])]

-- * Funções principais da Tarefa 2.

-- | Efetua uma 'jogada'.
-- Esta é a função principal desta Tarefa.

jogada :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
       -> Jogada -- ^ A 'Jogada' a efetuar.
       -> Estado -- ^ O 'Estado' anterior.
       -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.
jogada idJ jogada' (Estado m js) = (Estado mapa (l ++ (jogador:l')))
    where
        (l, (j:l')) = splitAt idJ js
        (jogador, mapa) = executarJogada j m jogada'


-- * Funções auxiliares da Tarefa 2.

-- | Funcão que recebe um ângulo e simplifica-o. Ex: 370º -> 10º
dgrSimplify :: Double -> Double
dgrSimplify x | x <= 90 && x >= (-90) = x
              | x > 0 = dgrSimplify (x-360)
              | otherwise = dgrSimplify (x+360)
-- | Função que executa uma determinada 'Jogada', tendo como inputs: o 'Jogador', o 'Mapa', a 'Jogada' a efetuar e dará como output um par('Jogador','Mapa').Esta função é essencial para a função principal.

executarJogada :: Jogador
               -> Mapa
               -> Jogada
               -> (Jogador, Mapa)
executarJogada j@(Jogador {estadoJogador=(Morto {})}) m _ = (j,m) -- Morto: retorna (Jogador, Mapa)
executarJogada j@(Jogador {estadoJogador=(ar@(Ar {inclinacaoJogador=i}))}) m (Movimenta D) = (j{estadoJogador=(ar{inclinacaoJogador= (max ((dgrSimplify i)-15) (-90))})}, m)
executarJogada j@(Jogador {estadoJogador=(ar@(Ar {inclinacaoJogador=i}))}) m (Movimenta E) = (j{estadoJogador=(ar{inclinacaoJogador= (min((dgrSimplify i)+15) 90)})}, m)
executarJogada j@(Jogador {estadoJogador=(Ar {})}) m _ = (j, m) -- a mota no ar mas a jogada não ser permitida
executarJogada j m (Movimenta C) = ((transitaPista j m C), m)
executarJogada j m (Movimenta B) = ((transitaPista j m B), m)
executarJogada j m Dispara = disparaCola j m
executarJogada j m Acelera = (j{estadoJogador=(Chao True)}, m)
executarJogada j m Desacelera = (j{estadoJogador=(Chao False)}, m)
executarJogada j m _ = (j, m)

-- | Função que dada uma Peca e um Double(a distância do jogador ao inicio da 'Pista'), devolve a altura exata do 'Jogador' nessa 'Peca'.

alturaExata :: Peca -> Double -> Double
alturaExata (Recta _ y) _ = fromIntegral y
alturaExata (Rampa _ y1 y2) x = fromIntegral y1 + (fromIntegral(y2-y1))*x

{-|
Função que dada uma 'Peca', devolve um Double(a inclinação dessa 'Peca'). 
A inclinação é dada em graus.
-}

inclinacao :: Peca -> Double
inclinacao peca = (atan (declive peca) * 180) / pi

{-| 
Função que dada uma 'Peca', retorna úm 'Double', o declive dessa 'Peca'. Se a 'Peca' fôr uma /Rampa/, então o 'declive' será a diferença entre a altura final da /Rampa/ e a altura inicial da mesma
Se a 'Peca' fôr uma /Recta/, então o 'declive' será igual a 0.0(zero).
-}

declive :: Peca -> Double
declive (Rampa _ y1 y2) = fromIntegral (y2 - y1)
declive _ = 0.0

{- | 
Função que tem como inputs:
-um par(Int,Double), sendo o Int o indice da 'Pista' do 'Jogador' e o Double a distância do 'Jogador' ao inicio dessa 'Pista';
-um 'Mapa';
-uma função que transforma um Int num Int, a /direcao2funcao/, que será referida em baixo. 
O output será:(Diferença de altura do local de transição para o pretendido, última altura, última inclinação)
-}

infoAlturas :: (Int, Double) -- (pista e distancia do bloco a origem)
           -> Mapa
           -> (Int -> Int)
           -> (Double, Double, Double) -- (Diferença de altura do local de transição para o pretendido, última altura, última inclinação)
infoAlturas (p,x) m func = ((altura - alturaAtual), alturaAtual, inclinacaoAtual)
    where
        distBloco = x - fromInteger (floor x) -- distancia a que está do inicio da peça
        peca = \f -> (m!!(f p))!!(floor x)
        altura = alturaExata (peca func) distBloco

        pecaAtual = peca (+0)
        alturaAtual = alturaExata pecaAtual distBloco
        inclinacaoAtual = inclinacao pecaAtual
        
{- |
Função que dada uma direção tranforma essa direção numa função que adiciona 1 ou (-1) a um certo valor
O intuito é que esta função seja usada pela função 'executarJogada' para, dada a direção C, o Jogador passar a ter nas suas informações um valor de 'pistaJogador' inferior em 1 unidade ao antigo.
Dada a direção B passará a ter nas suas informações um valor de 'pistaJogador' 1 unidade superior ao anterior. 
-}

direcao2Funcao :: Direcao -> (Int -> Int)
direcao2Funcao C = (+(-1))
direcao2Funcao _ = (+1)

{- | Função que dado um 'Jogador', um 'Mapa' e uma 'Direcao' irá alterar as Informações do 'Jogador':
->Se o 'Jogador' estiver na pista 0 ('pistaJogador'=0)e fôr dada a 'Direcao' 'C', não acontece nada, porque ele não pode ir mais para cima;
->Se o 'Jogador' estiver na última pista e fôr dada a 'Direcao' 'B', também não acontece nada, porque ele não pode descer mais;
->Se /diffAlturas/ > 0.2,ou seja, o 'Jogador' quer passar para uma 'Pista', onde a altura da 'Peca' verticalmente adjacente àquela em que ele está, é muito maior, então o 'Jogador' espeta-se, ficando morto na 'Pista' onde estava e com um 'timeoutJogador'=1.0;
->Se /diffAlturas/ <(-0.2),ou seja, o 'Jogador' quer passar para uma 'Pista', onde a altura da 'Peca' verticalmente adjacente àquela em que ele está, é muito menor, então o 'Jogador' passa para essa 'Pista' e passa a possuir um 'estadoJogador'=(Ar altura inclincao 0).
Neste caso os valores de 'alturaJogador' e 'inclinacaoJogador' mantêm-se, mas a 'gravidadeJogador' passa a ser obrigatoriamente 0.
->Noutro caso qualquer, a transição de 'Pista' é efetuada normalmente.
-}

transitaPista :: Jogador
              -> Mapa
              -> Direcao
              -> Jogador
transitaPista j@(Jogador {pistaJogador=n, distanciaJogador=x}) m dir | (dir==C && n==0) || (dir==B && (n+1)==(length m)) = j
                                                                     | diffAlturas > 0.2 = j{velocidadeJogador=0.0, estadoJogador=(Morto 1.0)}
                                                                     | diffAlturas < (-0.2) = j{pistaJogador=(f n),estadoJogador=(Ar altura inclincao 0)}
                                                                     | otherwise = j{pistaJogador=(f n)}                                                         
    where
        f = direcao2Funcao dir
        (diffAlturas, altura, inclincao) = infoAlturas (n, x) m f


{-|
Função que dado um 'Jogador', com as suas informações('pistaJogador', 'distanciaJogador', 'velocidadeJogador', 'colaJogador' e 'estadoJogador')e um 'Mapa' irá dar origem a um par('Jogador', 'Mapa').
->Se /blocoCola/ < 0 , então o jogador encontra-se ainda na primeira 'Peca' e, por isso, não pode disparar cola. (Usa-se a função 'floor' porque assim, se 'distanciaJogador' fôr inferior a 1, será considerado que ele se encontra na primeira 'Peca'. 
Temos que 0-1=-1 e por isso o jogador não poderá disparar)
->Se tivermos outro caso qualquer, então o jogador poderá disparar cola:
A 'colaJogador' subtraído 1, alterando-se desta forma a informação do 'Jogador'.
Contudo, o 'Mapa' também irá sofrer alterações: A 'Peca' anterior àquela onde o 'Jogador' está, passará agora a ter um 'Piso' de 'Cola'.
-}  

disparaCola :: Jogador
            -> Mapa
            -> (Jogador, Mapa)
disparaCola j@(Jogador n x _ cola _) mapa | blocoCola < 0 || cola == 0 = (j,mapa)
                                          | otherwise = (j{colaJogador=(cola-1)}, novoMapa)
    where
        blocoCola = (floor x) - 1
        (p1, pista:p2) = splitAt n mapa
        (b1, peca:b2) = splitAt blocoCola pista
        novoMapa = p1 ++ ((b1 ++ ((adicionarColaPeca peca):b2)):p2)

-- | Função que transforma uma certa 'Peca' numa 'Peca' igual em todas as características, exceto que o seu 'Piso' passará a ser 'Cola'.

adicionarColaPeca :: Peca -> Peca
adicionarColaPeca (Recta _ h) = Recta Cola h
adicionarColaPeca (Rampa _ h h') = Rampa Cola h h'

