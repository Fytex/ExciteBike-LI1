-- | Este módulo define os tipos de dados comuns a todos os alunos, tal como descrito e utilizado no enunciado do trabalho prático. 
module LI11920 where

-- * Tipos de dados auxiliares.

type Mapa = [Pista]

type Pista = [Peca]

data Peca
    = Recta Piso Int
    | Rampa Piso Int Int
  deriving (Read,Show,Eq)

data Piso = Terra | Relva | Lama | Boost | Cola
  deriving (Read,Show,Eq)

data Jogador
    = Jogador { pistaJogador :: Int, distanciaJogador :: Double, velocidadeJogador :: Double, colaJogador :: Int, estadoJogador :: EstadoJogador }
  deriving (Read,Show,Eq)

data EstadoJogador
    = Chao { aceleraJogador :: Bool }
    | Morto { timeoutJogador :: Double }
    | Ar { alturaJogador :: Double, inclinacaoJogador :: Double, gravidadeJogador :: Double }
  deriving (Read,Show,Eq)

-- | Estado do jogo.
data Estado = Estado
    { mapaEstado      :: Mapa
    , jogadoresEstado :: [Jogador] -- ^ lista de jogadores com identificador igual ao índice na lista
    }
  deriving (Read,Show,Eq)

-- | Uma direção.
data Direcao
    = C -- ^ Cima
    | D -- ^ Direita
    | B -- ^ Baixo
    | E -- ^ Esquerda
  deriving (Read,Show,Eq,Enum,Bounded)

data Jogada
    = Movimenta Direcao
    | Acelera
    | Desacelera
    | Dispara -- ^ cola
  deriving (Read,Show,Eq)
    
type Instrucoes = [Instrucao]

data Instrucao
    = Anda [Int] Piso
    | Sobe [Int] Piso Int
    | Desce [Int] Piso Int
    | Teleporta [Int] Int
    | Repete Int Instrucoes
  deriving (Read,Show,Eq)

tamanhoInstrucoes :: Instrucoes -> Int
tamanhoInstrucoes is = sum (map tamanhoInstrucao is)

tamanhoInstrucao :: Instrucao -> Int
tamanhoInstrucao (Repete _ is) = succ $ tamanhoInstrucoes is
tamanhoInstrucao _ = 1
