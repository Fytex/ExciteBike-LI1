{- |
Module: Tarefa6_2019li1g068
Description: Módulo Haskell contendo as funções necessárias, relativas à Tarefa 6 do Projeto da Unidade Curricular de LI1 
Copyright: Fytex;
           Arkimedez

Um módulo contendo definições Haskell para executar com sucesso a Tarefa 6.
A Tarefa 6 consiste no ato de programar um ro'bot', que tenha capacidade de percorrer autonomamente um 'Mapa'. O objetivo é 
que ele o faça da forma mais rápida e inteligente possível. Isto, porque ele o fará em competição com outros 'bot's.
-}

{- |

=Introdução

Na Tarefa 6 tivemos de programar um 'bot' que fosse capaz de percorrer automamente um 'Mapa'. Para tal tivemos de definir algumas funções auxiliares.
Na maior parte delas o resultado era um booleano. 

= Objetivos

O nosso Objetivo principal foi o de dar ao 'bot' os recursos necessários para o mesmo tomar algumas decisões inteligentes:
-A primeira coisa que o nosso 'bot' verifica é se o seu /estadoJogador/ é /Chao False/. Se fôr, ele executa a 'Jogada' /Acelera/, para passar a estar /Chao True/.
-O nosso 'bot' também tem a capacidade de verificar se existem 'Jogador'es atrás dele e se a 'Peca' exatamente atrás dele é um /boost/. Se ambas se verificarem, 
ele vai disparar cola(faz a 'Jogada' /Dispara/). Esta é uma forma de passarmos uma ratoeira a 'bot's "preguiçosos", que só andam para a frente.
-Outra coisa que o nosso 'bot' faz é mudar de /Pistas/ quando compensa fazê-lo, ou seja, quando os /atritos/ das outras /Pistas/ são melhores que os das /Pistas/ 
em que o nosso 'bot' está, ou seja, o /atrito/ é inferior noutro local. Uma coisa de que o nosso 'bot' se certifica é que não vai bater, como é explicado em baixo
para as funções 'cimaBate' e 'baixoBate'.
-Por último, ele também certifica se quando está no /Ar/ tem a inclinação adequada para, quando voltar, ao solo, não bater/morrer, e por isso, tem a capacidade de adaptar
a sua inclinação se fôr necessário.
-Se nenhum dos casos especiais acima descrito se veriicar, o nosso 'bot' limita-se a acelerar. 

= Discussão e Conclusão

Acho que programamos um bot bom. Não é de facto o 'bot' ideal mas é um 'bot' que consegue ser inteligente, apesar de em certos momentos tomar certas decisões 
que o ajudam no momento, mas que em momentos posteriores o podem prejudicar. O principal motivo de não melhorar-mos ainda mais o 'bot' foi o de não estar-mos 
100% seguros se as nossas modificações iriam funcionar pelo melhor, visto que só os torneios online só acontecem uma vez por dia e testar o bot no Terminal não
é tão prático como testá-lo em competição com os outros 'bot's. 

-}



-- | Este módulo define funções comuns da Tarefa 6 do trabalho prático.
module Tarefa6_2019li1g068 where

import LI11920
import Tarefa0_2019li1g068
import Tarefa1_2019li1g068
import Tarefa2_2019li1g068
import Tarefa3_2019li1g068
import Tarefa4_2019li1g068

-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot idx (Estado m jogadores) | isChaoFalse j = Just Acelera
                             | existemJogadoresAtras j js && blocoAnteriorBoost j m = Just Dispara
                             | (compensaMudarPistaCima j m) && (not(cimaBate j m)) && (not (compensaMudarPistaBaixo j m)) = Just (Movimenta C)
                             | (compensaMudarPistaBaixo j m) && (not(baixoBate j m)) && (not (compensaMudarPistaCima j m)) = Just (Movimenta B)
                             | (jogNoAr j) && (inclinarDireita j m ) = Just (Movimenta D)
                             | (jogNoAr j) && (inclinarEsquerda j m ) = Just (Movimenta E)
                             | otherwise = Just Acelera
    where
        (j1, j:j2) = splitAt idx jogadores
        js = j1 ++ j2

-- | Função que devolve /True/ se o /estadoJogador/ fôr /Chao False/        

isChaoFalse:: Jogador -> Bool
isChaoFalse (Jogador _ _ _ _ e) | (e == (Chao False)) = True
                                | otherwise = False 

-- | Função que devolve /True/ se dado o nosso 'Jogador' e uma lista de 'Jogador'es, se existirem 'Jogador'es atrás do nosso 'Jogador', a função devolve /True/.                                

existemJogadoresAtras :: Jogador -> [Jogador] -> Bool
existemJogadoresAtras (Jogador{distanciaJogador=x}) l | (length (map (\(Jogador{distanciaJogador=x'}) -> (fromIntegral (floor x) > x')) l) >= 1) = True
                                                      | otherwise = False 

-- | Função que dado o 'Jogador' e o 'Mapa', devolve /True/ se a 'Peca' exatamente atrás da 'Peca' onde o nosso 'Jogador' se encontra fôr uma 'Peca' /boost/.

blocoAnteriorBoost :: Jogador -> Mapa -> Bool
blocoAnteriorBoost (Jogador{pistaJogador=n, distanciaJogador=x}) m | (x >= 2 && (pegaPiso bloco) ==  Boost) = True
                                                                   | otherwise = False
    where
        bloco = (m !! n) !! ((floor x) - 1)

-- | Função que dado o 'Jogador' e o 'Mapa' nos devolve um 'Double', referente ao /atrito/ na 'Peca' onde o 'Jogador se encontra

atritoXjogador :: Jogador -> Mapa -> Double
atritoXjogador j@(Jogador{pistaJogador=n, distanciaJogador=x}) m = pegaAtrito(pegaPiso b)
    where
        b = (m !! n) !! (floor x)

{- |
Função que dado um 'Jogador' e um 'Mapa' nos devolve um lista de 'Double's, em que os valores são referentes ao /atritos/ nas 'Peca's que se encontram na mesma posição 
do 'Jogador' nas outas /Pistas/ e na do 'Jogador'
-}

atritosNaquelex :: Jogador -> Mapa -> [Double] 
atritosNaquelex j@(Jogador{pistaJogador=n, distanciaJogador=x}) m@(p:ps) = atritoXpista : (atritosNaquelex j ps)
    where 
        atritoXpista = pegaAtrito(pegaPiso a)
        a = p !! (floor x)
atritosNaquelex _ _ = []


{- | 
Função que devolve /True/ se compensar mudar para a /Pista/ de cima.
Compensa mudar para a /Pista/ de cima quando o 'Jogador' não está na primeira pista(a de indice 0), quando a lista dos /atritos/ das 'Peca's acima do 'Jogador' não é nula
e quando o valor mínimo da lista referida anteriormente é menor que o /atrito/ da 'Peca' onde o 'Jogador' está.
-}

compensaMudarPistaCima :: Jogador -> Mapa -> Bool
compensaMudarPistaCima j@(Jogador{pistaJogador=n, distanciaJogador=x}) m = n>0 && (not (null a)) && ((minimum a) < (atritoXjogador j m))
     where 
      (a,b) = splitAt n (atritosNaquelex j m)


{- | 
Função que devolve /True/ se compensar mudar para a /Pista/ de baixo.
Compensa mudar para a /Pista/ de baixo quando o 'Jogador' não está na última pista(a de indice igual a /length m - 1/), quando a lista dos /atritos/ das 'Peca's abaixo do 'Jogador' 
não é nula e quando o valor mínimo da lista referida anteriormente é menor que o /atrito/ da 'Peca' onde o 'Jogador' está.
-}

compensaMudarPistaBaixo :: Jogador -> Mapa -> Bool  
compensaMudarPistaBaixo j@(Jogador{pistaJogador=n, distanciaJogador=x}) m = (n<=(length m - 1)) && (not (null b)) &&  ((minimum b) < (atritoXjogador j m))
     where
      (a,b) = splitAt (n+1) (atritosNaquelex j m)

--Ter cuidado para que a movimentaçao não seja feita em direção a uma rampa
--Saber o que acontece se a pista for uma das que estão nas pontas


-- | Função que devolve /True/ se a 'Peca' na /Pista/ acima e no mesmo indice da /Pista/ onde o 'Jogador' está tiver uma altura em que o 'Jogador' vai bater

cimaBate :: Jogador -> Mapa -> Bool
cimaBate j@(Jogador{pistaJogador=n, distanciaJogador=x}) m@(p:ps) = n==0 || (n>0 && ePisoNchao pecaCima)
     where 
       pecaCima = (m !! (n-1)) !! (floor x)


-- | Função que devolve /True/ se a 'Peca' na /Pista/ abaixo e no mesmo indice da /Pista/ onde o 'Jogador' está tiver uma altura em que o 'Jogador' vai bater      

baixoBate :: Jogador -> Mapa -> Bool
baixoBate j@(Jogador{pistaJogador=n, distanciaJogador=x}) m@(p:ps) = n==(length m - 1) || (n>=0 && n<length m - 1  && ePisoNchao pecaBaixo) 
      where 
       pecaBaixo = (m !! (n+1)) !! (floor x)
        

-- | Função que só retorna /False/ se o /Piso/ da /Peca/ fôr um Recta no /Chao/ de altura 0

ePisoNchao :: Peca -> Bool
ePisoNchao (Recta _ s) | s >= 1 = True 
                       | otherwise = False
ePisoNchao (Rampa _ _ _) = True


-- | Função que retorna /True/, se o /estadoJogador/ fôr /Ar/. 

jogNoAr :: Jogador -> Bool
jogNoAr Jogador{estadoJogador=Ar{}}  = True
jogNoAr _ = False



{- | Função que retorna /True/, se a /inclinacaoJogador/ for maior que a soma de 45 com a /inclinação/ da /Peca/. Ou seja, é verdade que  
é necessário que o 'Jogador' se incline para a 'Direita'.
-}

inclinarDireita :: Jogador -> Mapa -> Bool
inclinarDireita j@(Jogador{pistaJogador=n, distanciaJogador=x, estadoJogador= ar@(Ar y i g)}) m | i > (iPeca + 45) = True
                                                                                                | otherwise = False
      where
       iPeca = inclinacao ((m !! n) !! (floor x)) 




{- | Função que retorna /True/, se a /inclinacaoJogador/ for menor que a diferença da /inclinação/ da /Peca/ com 45. Ou seja, é verdade que  
é necessário que o 'Jogador' se incline para a 'Esquerda'.
-}

inclinarEsquerda :: Jogador -> Mapa -> Bool
inclinarEsquerda j@(Jogador{pistaJogador=n, distanciaJogador=x, estadoJogador= ar@(Ar y i g)}) m | i < (iPeca - 45) = True
                                                                                                 | otherwise = False
      where
       iPeca = inclinacao ((m !! n) !! (floor x))
 


--quando esta numa rampa demasiado alta nao pode movimentar-se/não convém que o faça


