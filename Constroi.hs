module Constroi where

import LI11920
import Tarefa0_2019li1g068

import Control.Monad

data Construtor = Construtor
    { instrucoesConstrutor :: Instrucoes
    , bulldozersConstrutor :: [Int]
    , mapaConstrutor :: MapaConstrutor
    }
  deriving (Show,Eq)

type MapaConstrutor = [[Maybe Peca]]

constroi :: Instrucoes -> Mapa
constroi = construtorMapa . mapaConstrutor . instrucoes . construtorInicial 

construtorMapa :: MapaConstrutor -> Mapa
construtorMapa m = maybe (error $ "Mapa incompleto:\n" ++show m) id $ mapM (mapM id) m

construtorInicial :: Instrucoes -> Construtor
construtorInicial is = Construtor is (replicate npistas 0) (replicate npistas [Just $ Recta Terra 0])
    where
    npistas = numeroPistasInstrucoes is

instrucoes :: Construtor -> Construtor
instrucoes c@(Construtor is posicoes mapa) = case is of
    [] -> c
    (i:is) -> instrucoes $ instrucao i (Construtor is posicoes mapa)

instrucao :: Instrucao -> Construtor -> Construtor
instrucao (Anda pistas piso) c = foldl (flip (move $ Recta piso)) c pistas
instrucao (Sobe pistas piso alt) c = foldl (flip (move $ \from -> Rampa piso from $ max 0 $ from+alt)) c pistas
instrucao (Desce pistas piso alt) c = foldl (flip (move $ \from -> Rampa piso from $ max 0 $ from-alt)) c pistas
instrucao (Teleporta pistas dist) c = foldl (flip (teleporta dist)) c pistas
instrucao (Repete n is) c = repete is n c

move :: (Int -> Peca) -> Int -> Construtor -> Construtor
move mkPeca pista c@(Construtor is posicoes mapa) = Construtor is posicoes' mapa'
    where
    (pos) = maybe (-1) id $ atMay posicoes pista
    from = maybe 0 (snd . alturasPeca) $ join $ atMatriz mapa (pista,pos)
    pos' = succ pos
    mapa' = extendeMatrix pista pos' Nothing (const $ Just $ mkPeca from) mapa
    posicoes' = extendeLista pista 0 (const pos') posicoes

teleporta :: Int -> Int -> Construtor -> Construtor
teleporta dist pista c@(Construtor is posicoes mapa) = Construtor is posicoes' mapa'
    where
    (pos) = maybe (-1) id $ atMay posicoes pista
    pos' = max (-1) $ pos+dist
    mapa' = extendeMatrix pista pos' Nothing id mapa
    posicoes' = extendeLista pista 0 (const pos') posicoes

repete :: Instrucoes -> Int -> Construtor -> Construtor
repete ris 0 c = c
repete ris n c@(Construtor is posicoes mapa) = Construtor (ris++Repete (pred n) ris:is) posicoes mapa

extendeMatrix :: Int -> Int -> a -> (a -> a) -> [[a]] -> [[a]]
extendeMatrix i j def f ls | i < 0 = ls
extendeMatrix 0 j def f (l:ls) = extendeLista j def f l : ls
extendeMatrix 0 j def f [] = [extendeLista j def f []]
extendeMatrix i j def f [] = replicate (succ j) def : extendeMatrix (pred i) j def f []
extendeMatrix i j def f (l:ls) = (l ++ replicate (succ j - length l) def) : extendeMatrix (pred i) j def f ls

extendeLista :: Int -> a -> (a -> a) -> [a] -> [a]
extendeLista i def f cs | i < 0 = cs
extendeLista 0 def f (c:cs) = f c : cs
extendeLista 0 def f [] = [f def]
extendeLista i def f [] = def : extendeLista (pred i) def f []
extendeLista i def f (c:cs) = c : extendeLista (pred i) def f cs

atMay :: [a] -> Int -> Maybe a
atMay xs i = if eIndiceListaValido i xs then Just (encontraIndiceLista i xs) else Nothing

atMatriz :: [[a]] -> (Int,Int) -> Maybe a
atMatriz xs p = if ePosicaoMatrizValida p xs then Just (encontraPosicaoMatriz p xs) else Nothing

alturasPeca :: Peca -> (Int,Int)
alturasPeca (Recta _ n) = (n,n)
alturasPeca (Rampa _ n1 n2) = (n1,n2)

numeroPistasInstrucoes :: Instrucoes -> Int
numeroPistasInstrucoes = maximum0 . map numeroPistasInstrucao

numeroPistasInstrucao :: Instrucao -> Int
numeroPistasInstrucao (Anda pistas piso) = succ $ maximum0 pistas
numeroPistasInstrucao (Sobe pistas piso _) = succ $ maximum0 pistas
numeroPistasInstrucao (Desce pistas piso _) = succ $ maximum0 pistas
numeroPistasInstrucao (Teleporta pistas _) = succ $ maximum0 pistas
numeroPistasInstrucao (Repete _ is) = numeroPistasInstrucoes is

maximum0 [] = 0
maximum0 xs = maximum xs