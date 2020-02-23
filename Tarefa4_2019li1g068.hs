{- |
Module: Tarefa4_2019li1g068
Description: Módulo Haskell contendo as funções necessárias, relativas à Tarefa 2 do Projeto da Unidade Curricular de LI1 
Copyright: Fytex;
           Arkimedez

Um módulo contendo definições Haskell para executar com sucesso a Tarefa 4.
Na Tarefa 4 a função principal será a 'passo', que tem como /inputs/:
-Um 'Double' relativo ao intervalo de tempo no qual queremos movimentar o 'Jogador';
-O 'Mapa' no qual o 'Jogador' se vai movimentar;
-O 'Jogador' a movimentar;
O /output/ será o 'Jogador'com as alterações provocadas pelas funções auxiliares 'acelera' e 'move'.
O objetivo desta Tarefa é, portanto, criar funções que permitam ao 'Jogador' movimentar-se no 'Mapa'.
-}


--Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2019li1g068 where

import LI11920
import Tarefa0_2019li1g068
import Tarefa1_2019li1g068
import Tarefa2_2019li1g068


-- * Testes
{- |
Testes unitários da Tarefa 4.
Cada teste é um par (/tempo/,/'Mapa'/,/'Jogador'/), em que /tempo/ é um /'Double'/, que nos nossos testes definimos sempre como um valor <=1.
Esses valores pequenos devem-se ao facto de não fazer sentido que a função 'passo' fosse definida para um tempo superior a 1 segundo.
-}

testesT4 :: [(Double,Mapa,Jogador)]
testesT4 = [(0.2,[[Recta Terra 0, Recta Boost 0, Rampa Relva 0 1, Rampa Relva 1 0, Recta Boost 0]], (Jogador {pistaJogador = 0, distanciaJogador = 3.349133973210858, velocidadeJogador = 4.9375, colaJogador = 0, estadoJogador = Ar {alturaJogador = 1.3391339732108576, inclinacaoJogador = 45.0, gravidadeJogador = 0.1}})), (0.2,[[Recta Terra 0, Recta Boost 0, Rampa Relva 0 1, Rampa Relva 1 0, Recta Boost 0]] ,(Jogador 0 3 5 0 (Ar 1 45 0))), (1  ,[[Recta Terra 0, Recta Terra 0,Recta Terra 0,Recta Terra 0]]   ,(Jogador 0 2 2   3 (Ar 2   0 0 ))),(0.1, [[Recta Terra 0, Rampa Terra 0 1, Rampa Terra 1 0]], (Jogador 0 1.4 1 0 (Ar 4 (-50) 1))), (0.4, [[Recta Terra 0, Rampa Terra 0 1, Rampa Terra 1 0]], Jogador 0 1.9 2 0 (Chao True)),(0.5, gera 2 2 4, Jogador 0 1 1 1 (Chao True)) , (0.8, gera 3 2 3, Jogador 0 1.3 1.9 1 (Ar 3.4 50 10)) , (0.2, gera 4 7 1, Jogador 2 1.4 4.5 4 (Chao True)) , (0.4, gera 2 2 5, Jogador 1 1.1 1.3 3 (Ar 2.3 45 3)), (0.9, gera 3 5 7, Jogador 2 1.0 0 3 (Morto 1.0)), (0.2, gera 2 4 6, Jogador 1 2.3 0.8 5 (Chao False)) , (0.3, gera 2 4 9, Jogador 1 2.8 0.2 5 (Ar 3.4 21 2)), (0.8, gera 5 4 6, Jogador 3 3.4 0.9 4 (Ar 4.4 12.0 9.8)), (0.3, gera 3 4 5, Jogador 0 3.6 2.9 3 (Ar 23 4.5 5.6)) , (0.4, [[Recta Terra 0, Rampa Terra 0 1, Rampa Terra 1 0, Rampa Relva 0 2, Rampa Boost 2 0,Recta Terra 0]], Jogador 0 2.8 3.4 4 (Chao True)) , (1.4, [[Recta Terra 0, Rampa Terra 0 1, Rampa Lama 1 0, Rampa Relva 0 2, Rampa Boost 2 0,Recta Terra 0]], Jogador 0 2.8 3.4 4 (Chao True)) , (0.4, [[Recta Terra 0, Rampa Boost 0 1, Rampa Terra 1 0, Rampa Relva 0 2, Rampa Boost 2 0,Recta Terra 0]], Jogador 0 1.8 3.4 4 (Chao True)), (0.4, [[Recta Terra 0, Rampa Boost 0 1, Rampa Terra 1 0, Rampa Relva 0 2, Rampa Boost 2 0,Recta Terra 0]], Jogador 0 3.8 3.4 4 (Chao True)) , (1.4, gera 2 2 5, Jogador 1 1.1 1.3 3 (Ar 2.3 45 3)), (0.9, gera 3 5 7, Jogador 2 1.0 0 3 (Morto 0.2)), (0.4, [[Recta Terra 0, Rampa Boost 0 1, Rampa Terra 1 0, Rampa Cola 0 2, Rampa Boost 2 0,Recta Terra 0]], Jogador 0 3.8 3.4 4 (Chao True)) , (0.4, [[Recta Terra 0, Rampa Boost 0 1, Rampa Terra 1 0, Rampa Relva 0 100, Rampa Boost 100 0,Recta Terra 0]], Jogador 0 2.8 3.4 4 (Chao True)), (0.2, [[Recta Terra 0,  Recta Terra 0, Rampa Lama 0 10, Rampa Lama 10 0, Rampa Lama 0 10]], Jogador 0 2.0001 2.0 2 (Ar 2.0 45.0 1.0)), (3.1, [[Recta Terra 0,  Recta Terra 0, Rampa Lama 0 10, Rampa Lama 10 0, Rampa Lama 0 10]], Jogador 0 2.0001 2.0 2 (Ar 2.0 45.0 1.0)) ]


-- * Funções principais da Tarefa 4.

{-|
Avança o estado de um 'Jogador' um 'passo' em frente, durante um determinado período de tempo.
A função 'passo' é a principal. No entanto, os parâmetros principais desta Tarefa estão definidos nas duas funções auxiliares 'acelera' e 'move'.
A função 'passo' usa a 'move', que, obviamente tem os mesmos /inputs/ que a função principal: um 'Double', relativo ao intervalo de tempo, um 'Mapa' e um 'Jogador'.
O 'Jogador' que a 'move' recebe é já aquele que é resultante de aplicar a função 'acelera' aos 3 /inputs/ anteriormente referidos.
A função 'acelera' não altera a posição do jogador, pois o seu /output/ é o estado do 'Jogador ' depois de acelerar, ou seja com valores de /velocidade/ e, possivelmente de /gravidade/ alterados.
-}

passo :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após um 'passo'.
passo t m j = move t m (acelera t m j)

{- |
A função 'acelera' altera a /velocidade/ de um 'Jogador', durante um determinado período de tempo.
A atualização da /velocidade/ do jogador está dividida por casos:
- O primeiro que pusemos na função é aquele em que o 'Jogador' está /Morto/. Neste caso, o output será o 'Jogador' com exatamente os mesmos parâmetros.
-Depois definimos o que acontece quando o 'Jogador' está no /Ar/:A /velocidade/ do /output/ será o valor máximo entre o resultado da fórmula apresentada no enunciado para a /velocidade/ no /Ar/ e 0(zero). 
Isto, porque é dito no enunciado da /Tarefa/ que se o resultado da fórmulo for um valor negativo, então a /velocidade/ terá de ser zero(0).
Quanto à 'gravidadeJogador', terá um novo valor: que será igual à soma do valor anterior com o intervalo de tempo. Isto, porque é-nos dito no enunciado que o valor de /accelGravidade/ na fórmula do enunciado é igual a 1.
- Por último, definimos o que acontece no resto dos casos:
 >Se a /velociade/ do 'Jogador' for inferior a 2 e o 'Jogador' estiver no /Chao/, então a /accelMota/ será igual a 1 e usa-se a respetiva fórmula do enunciado, a que chamamos /nVcT/.
 >Noutro caso qualquer, o valor de /accelMota/ será igual a 0 e usámos a respetiva fórmula do enunciado, que aqui definimos como /nVc/.
NOTA: para obtermos o valor do /atrito/ que é utilizado pelas fórmulas /nVcT/ e /nVc/, usámos a função 'pegaAtrito' do 'Piso' da 'Peca' onde o 'Jogador' está.
-Para encontrar-mos a 'Peca':Usamos a função 'encontraPosicaoMatriz', que havia sido definida na /Tarefa 0/.
-Para sabermos qual era o 'Piso', usamos a função 'pegaPiso', que havia sido definida na /Tarefa 1/.       
-}

acelera :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após acelerar.
acelera _ _ j@(Jogador {estadoJogador=(Morto {})}) = j
acelera t _ j@(Jogador {velocidadeJogador=v, estadoJogador=(ar@(Ar {gravidadeJogador=g}))}) = j{ velocidadeJogador=(max (v-0.125*v*t) 0) , estadoJogador=(ar{gravidadeJogador=(g+t)})}
acelera t m j@(Jogador p x v _ e)  | v < 2 && (e == (Chao True)) = j{velocidadeJogador=(max nVcT 0)}
                                   | otherwise = j{velocidadeJogador=(max nVc 0)} --Não faz sentido colocar mais casos, visto que a velocidade nunca pode ser negativa...
   where nVcT = v + (1.0 - atrito * v) * t 
         nVc = v + (-atrito) * v * t   
         atrito = pegaAtrito( pegaPiso (encontraPosicaoMatriz (p, (floor x)) m)) --peça onde onde o Jogador está    

{- |
Função que dado um 'Piso' devolve um 'Double', referente ao valor de /atrito/ que, no enunciado desta Tarefa, foi associado a cada 'Piso'. 
O /atrito/ terá influência nas fórmulas que calculam a /velocidade/.
-}

pegaAtrito :: Piso -> Double
pegaAtrito p | p == Terra = 0.25
             | p == Relva = 0.75
             | p == Lama = 1.50
             | p == Boost = (-0.50)
             | otherwise = 3.00



{- |
A função 'move' altera a /posição/ de um 'Jogador', durante um determinado período de /tempo/.
A atualização da /posição/ do jogador está dividida por casos:
-Quando o 'Jogador' está /Morto/ com um certo /timeout/, se a diferença entre esse /timeout/ e o período de /tempo/ for > que 0, então o 'Jogador' continuará morto, mas o valor do /timeout/, será substituido pelo resultado da diferença referida atrás, que nós designamos por /delta/.
Se a diferença dos valores for <= que 0, então o 'estadoJogador' altera-se para /Chao False/.
-Quando o 'Jogador' está no /Ar/:
 >Se os segmentos de reta da /trajetória/ do 'Jogador' e da 'Peca' naquela posição não se intersetarem, então o 'Jogador' terá os valores de 'distanciaJogador' e 'alturaJogador' alterados para aqueles que são o resultado da reta da /trajetoria/.
 >Se o valor absolutos da diferença de /inclinações/ do 'Jogador' e da 'Peca' for >= a 45, então o 'Jogador' passará a estar /Morto/ e a 'velocidadeJogador' passará a ser igual a 0(zero), na /posição x/ resultante da interseção entre os segmentos de reta.
 >No outro caso, ele nem fica /Morto/, nem no /Ar/, por isso teremos de ver a /posição x/ onde ele ficará, alterar o valor de /velociadeJogador/ e dizer que o 'estadoJogador' fica /Chao False/.
-Quando o 'Jogador' está noutro estado qualquer, ou seja está no /Chao/:
 >Pode ficar no /Chao/ e nesse caso, ficará com um novo valor de /distanciaPercorrida/
 >Pode também passar a estar no /Ar/ e nesse caso, ficará com a /inclinacaoJogador/ igual à da 'Peca' onde se encontra, /velocidadeJogador/ igual a 0(zero), /posicaoJogador/ igual ao valor mínimo entre a /posicaoJogador/ que teria se fosse até ao fim da trajetória e a /distância da próxima 'Peca'/.
 Além disso, a /alturaJogador/, também será o valor mínimo entre a /alturaJogador/ que teria se fosse até ao fim da trajetória e a que teria no inicio da próxima 'Peca'. 
-}


-- | Altera a posição de 'Jogador', durante um determinado período de tempo.
move :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após se movimentar.
move t _ j@(Jogador{estadoJogador=(Morto tO)}) | diff > 0 = j{estadoJogador=(Morto diff)}
                                               | otherwise = j{estadoJogador=(Chao False)}
     where
          diff = tO - t

move t m (Jogador p x v c (Chao b)) | x' > xFinal && (declive peca) > (declive pecaPost) = Jogador p xFinal v c (Ar (fromIntegral (snd (alturaPeca peca))) (inclinacao peca) 0)
                                    | otherwise = Jogador p xFinal v c (Chao b)
     where 
          peca = (m !! p) !! (floor x)
          pecaPost = (m !! p) !! (floor x + 1)
          x' = (x + v * t / (sqrt (((declive peca)^2) + 1)))
          xFinal = min (fromIntegral (floor x + 1)) x'


move t m j@(Jogador p x v c (Ar h inc g)) | not seInterseta = Jogador p xFinal v c (Ar yFinal inc g)
                                          | abs ((inclinacao peca) - inc) < 45 = Jogador p xColisao  (v* (cos(dgr2rad ((inclinacao peca) - inc)))) c (Chao False)
                                          | otherwise = Jogador p xColisao 0 c (Morto 1)
     where 
          peca = (m !! p) !! (floor x)
          inc' = dgr2rad inc
          hI = fromIntegral.fst $ alturaPeca peca
          tColisao = min t (-h+hI+((declive peca)*(x-(fromIntegral (floor x)))) / (v*(sin inc')-v*(cos inc')*(declive peca)-g))
          
          t' = ((fromIntegral(floor x) + 1) - x)/(v* (cos (dgr2rad inc)))
          tFinal = min t t'
          xFinal = x + v*cos inc' * tFinal
          yFinal = h + (v*sin inc' - g ) * tFinal

          xColisao =  x + (v * (cos (inc')) * tColisao)



          -- | Verifica se um jogador se interseta com a peça
          seInterseta :: Bool
          seInterseta = (yPeca peca)>= yFinal
            where
              -- Dada uma peça retorna a altura da peça na posição do jogador
              yPeca :: Peca -> Double
              yPeca p@(Rampa _ h _) = (fromIntegral h) + (xFinal - (fromIntegral(floor x)))*(declive peca)
              yPeca (Recta _ h) = fromIntegral h 

-- | Dada uma peça entrega a sua altura inicial
alturaPeca :: Peca -> (Int, Int)
alturaPeca (Rampa _ h h') = (h, h')
alturaPeca (Recta _ h) = (h, h)
