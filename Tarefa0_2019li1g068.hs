-- | Este módulo define funções genéricas sobre vetores e matrizes, que serão úteis na resolução do trabalho prático.
module Tarefa0_2019li1g068 where

    -- * Funções não-recursivas.
    
    -- | Um ponto a duas dimensões dado num referencial cartesiado (distâncias aos eixos vertical e horizontal)
    --
    -- <<http://li1.lsd.di.uminho.pt/images/cartesiano.png cartesisano>>
    -- , ou num referencial polar (distância à origem e ângulo do respectivo vector com o eixo horizontal).
    --
    -- <<http://li1.lsd.di.uminho.pt/images/polar.png polar>>
    data Ponto = Cartesiano Double Double | Polar Double Angulo
     deriving (Show)
    
    -- | Um ângulo em graus.
    type Angulo = Double
    
    -- ** Funções sobre vetores
    
    -- | Um 'Vetor' na representação escalar é um 'Ponto' em relação à origem.
    type Vetor = Ponto
    -- ^ <<http://li1.lsd.di.uminho.pt/images/vetor.png vetor>>
    
    -- *** Funções gerais sobre 'Vetor'es.
    dgr2rad a = a * pi / 180

    -- | Soma dois 'Vetor'es.
    somaVetores :: Vetor -> Vetor -> Vetor
    somaVetores v1 v2 = Cartesiano (x2 + x1) (y2 + y1)
        where
            Cartesiano x1 y1 = polar2Cartesiano v1
            Cartesiano x2 y2 = polar2Cartesiano v2

    -- | Passa de Polar para Cartesiano
    polar2Cartesiano :: Vetor -> Vetor
    polar2Cartesiano (Polar dist ang) = Cartesiano (dist * cos (dgr2rad ang)) (dist * sin (dgr2rad ang))
    polar2Cartesiano (Cartesiano x y) = Cartesiano x y -- se não é polar então é cartesiano
    
    -- | Subtrai dois 'Vetor'es.
    subtraiVetores :: Vetor -> Vetor -> Vetor
    subtraiVetores v1 v2 = Cartesiano (x1 - x2) (y1 - y2)
        where
            Cartesiano x1 y1 = polar2Cartesiano v1
            Cartesiano x2 y2 = polar2Cartesiano v2
    
    -- | Multiplica um escalar por um 'Vetor'.
    multiplicaVetor :: Double -> Vetor -> Vetor
    multiplicaVetor a v = Cartesiano (a * x) (a* y)
        where
            Cartesiano x y = polar2Cartesiano v
    
    -- ** Funções sobre rectas.
    
    -- | Um segmento de reta é definido por dois pontos.
    type Reta = (Ponto,Ponto)
    
    -- | Testar se dois segmentos de reta se intersetam.
    --
    -- __NB:__ Aplique as equações matemáticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
    intersetam :: Reta -> Reta -> Bool
    intersetam ( p1, p2 ) ( p3, p4 ) = 0 <= t_a && t_a <= 1 && 0 <= t_b && t_b <= 1 
        where
            Cartesiano x1 y1 = polar2Cartesiano p1
            Cartesiano x2 y2 = polar2Cartesiano p2
            Cartesiano x3 y3 = polar2Cartesiano p3
            Cartesiano x4 y4 = polar2Cartesiano p4

            t_a = ( (y3-y4)*(x1-x3)+(x4-x3)*(y1-y3) ) / ( (x4-x3)*(y1-y2)-(x1-x2)*(y4-y3) )
            t_b = ( (y1-y2)*(x1-x3)+(x2-x1)*(y1-y3) ) / ( (x4-x3)*(y1-y2)-(x1-x2)*(y4-y3) )
    
    -- | Calcular o ponto de intersecao entre dois segmentos de reta.
    --
    -- __NB:__ Aplique as equações matemáticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
    intersecao :: Reta -> Reta -> Ponto
    intersecao ( p1, p2 ) (p3, p4) = if (intersetam ( p1, p2 ) ( p3, p4 ))
                                        then Cartesiano (x1 + t_a*(x2-x1)) (y1 + t_a*(y2 - y1))
                                        else error "Não se intersetam"
        where
            Cartesiano x1 y1 = polar2Cartesiano p1
            Cartesiano x2 y2 = polar2Cartesiano p2
            Cartesiano x3 y3 = polar2Cartesiano p3
            Cartesiano x4 y4 = polar2Cartesiano p4

            t_a = ( (y3-y4)*(x1-x3)+(x4-x3)*(y1-y3) ) / ( (x4-x3)*(y1-y2)-(x1-x2)*(y4-y3) )
    
    -- ** Funções sobre listas
    
    -- *** Funções gerais sobre listas.
    --
    -- Funções não disponíveis no 'Prelude', mas com grande utilidade.
    
    -- | Verifica se o indice pertence à lista.
    --
    -- __Sugestão:__ use a função 'length' que calcula tamanhos de listas
    eIndiceListaValido :: Int -> [a] -> Bool
    eIndiceListaValido idx l = idx >= 0 && length l > idx -- 0-index
    
    -- ** Funções sobre matrizes.
    
    -- *** Funções gerais sobre matrizes.
    
    -- | A dimensão de um mapa dada como um par (/número de linhas/,/número de colunhas/).
    type DimensaoMatriz = (Int,Int)
    
    -- | Uma posição numa matriz dada como um par (/linha/,/colunha/).
    -- As coordenadas são dois números naturais e começam com (0,0) no canto superior esquerdo, com as linhas incrementando para baixo e as colunas incrementando para a direita:
    --
    -- <<http://li1.lsd.di.uminho.pt/images/posicaomatriz.png posicaomatriz>>
    type PosicaoMatriz = (Int,Int)
    
    -- | Uma matriz é um conjunto de elementos a duas dimensões.
    --
    -- Em notação matemática, é geralmente representada por:
    --
    -- <<https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
    type Matriz a = [[a]]
    
    -- | Calcula a dimensão de uma matriz.
    --
    -- __NB:__ Note que não existem matrizes de dimensão /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensão /0 * 0/.
    --
    -- __Sugestão:__ relembre a função 'length', referida anteriormente.
    dimensaoMatriz :: Matriz a -> DimensaoMatriz
    dimensaoMatriz matriz = (if n > 0 then m else 0, n)
        where
            m = length matriz
            n = if m>0 then length (head matriz) else 0
    
    -- | Verifica se a posição pertence à matriz.
    ePosicaoMatrizValida :: PosicaoMatriz -> Matriz a -> Bool 
    ePosicaoMatrizValida (pm, pn) matriz = m > pm && n > pn
        where
            (m, n) = dimensaoMatriz matriz
    
    -- * Funções recursivas.
    
    -- ** Funções sobre ângulos
    
    -- | Normaliza um ângulo na gama [0..360).
    --  Um ângulo pode ser usado para representar a rotação
    --  que um objecto efectua. Normalizar um ângulo na gama [0..360)
    --  consiste, intuitivamente, em extrair a orientação do
    --  objecto que resulta da aplicação de uma rotação. Por exemplo, é verdade que:
    --
    -- prop> normalizaAngulo 360 = 0
    -- prop> normalizaAngulo 390 = 30
    -- prop> normalizaAngulo 720 = 0
    -- prop> normalizaAngulo (-30) = 330
    normalizaAngulo :: Angulo -> Angulo
    normalizaAngulo a = if a >= 0 then angleDiff else 360 - angleDiff
        where
            angleDiff = anguloMod (abs a)

    anguloMod a = if (a > 360) then anguloMod (a - 360) else a
    
    -- ** Funções sobre listas.
    
    -- | Devolve o elemento num dado índice de uma lista.
    --
    -- __Sugestão:__ Não use a função (!!) :: [a] -> Int -> a :-)
    encontraIndiceLista :: Int -> [a] -> a
    encontraIndiceLista idx (h:t) = if idx == 0 then h else encontraIndiceLista (idx-1) t
    
    -- | Modifica um elemento num dado índice.
    --
    -- __NB:__ Devolve a própria lista se o elemento não existir.
    atualizaIndiceLista :: Int -> a -> [a] -> [a]
    atualizaIndiceLista idx elem (h:t) = if idx == 0 then (elem:t) else h:(atualizaIndiceLista (idx-1) elem t)
    
    -- ** Funções sobre matrizes.
    
    -- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
    encontraPosicaoMatriz :: PosicaoMatriz -> Matriz a -> a
    encontraPosicaoMatriz (m, n) (h:t) = if m == 0 
                            then encontraIndiceLista n h
                            else encontraPosicaoMatriz (m-1,n) t 


    
    -- | Modifica um elemento numa dada 'Posicao'
    --
    -- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.
    atualizaPosicaoMatriz :: PosicaoMatriz -> a -> Matriz a -> Matriz a
    atualizaPosicaoMatriz (m,n) elem (h:t) = if m == 0 then (atualizaIndiceLista n elem h):t else h:(atualizaPosicaoMatriz (m-1, n) elem t)
