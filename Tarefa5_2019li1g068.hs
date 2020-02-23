{- |
Module: Tarefa5_2019li1g068
Description: Módulo Haskell contendo as funções necessárias, relativas à Tarefa 5 do Projeto da Unidade Curricular de LI1 
Copyright: Fytex;
           Arkimedez

Um módulo contendo definições Haskell para executar com sucesso a Tarefa 5.
A Tarefa 5 é o jogo propriamente dito. Ou seja, o utilizador pode interajir diretamente com o conteúdo que foi por nós criado, sem ter de contactar com qualquer tipo de código.
-}


{- |

=Introdução

Na Tarefa 5 tivemos de fazer o jogo propriamente dito. Optamos por um estilo mais versátil, que pudesse ter o seu quê de inovador e, por isso, distanciamo-nos da 
clássica visualização 2.5D, presente no site(no jogo dos stores) e no jogo original de 1984. 

= Objetivos

Como foi dito em cima, o nosso principal objetivo foi fazer um jogo com um estilo que se destacasse.
No nosso jogo implementamos as seguintes funcionalidades:
-Altera o tamanho das 'Peca's em função da resolução do monitor;
-Os mapas são obliquos;
-Criamos as nossas próprias imagens para as 'Peca's, usando o Photoshop;
-Limitamos o Jogo a 4 jogadores, por questões estéticas. No entanto, o algoritmo criado permite um número infinito de jogadores e caberia tudo na Tela;
-É possivel ter a opção Multiplayer;
-É possível jogar contra o nosso 'bot' da nossa Tarefa 6;
-Tivemos em atenção centralizar os 'Mapa's, deixando sempre um espaço entre eles, para não se sobreporem. Este método de centralização foi adaptado para os eixos vertical e horizontal.
É de notar que tal só se nota no eixo vertical se houver um número reduzido de 'Peca's. Ex: 5;
-Criamos Menus e teclas de atalhos, com imagens criadas por nós;
-Usamos a função randomRIO para conseguirmos Pistas aleatórias;
-No inicio da pista, o jogador move-se até ao centro do ecrã, depois a pista move-se enquanto o 'Jogador' fica parado e, por fim, quando a pista chega ao seu limite, essa mesma para e o
'Jogador' move-se, dando assim um efeito suave ao Jogo.

No entanto, nem tudo ficou do nosso agrado:
-O facto de termos escolhido a perspetiva obliqua, deu-nos bastante trabalho para encontrar valores(coordenadas) que nos fossem úteis. Foi bastante complicado encontrar valores para o espaço entre 
os mapas, posições dos mapas e tamanho das peças, que nos permitissem ter mapas centrados, como os que referimos em cima; 
-Esta perspetiva impossiblitou o uso de pistas diferentes umas das outras.

= Discussão e Conclusão

Em suma consideramos que a realização desta Tarefa foi dificil, devido à pouca informação que nos foi fornecida e aos problemas que fomos encontrando. O Gloss não é
muito fácil de aprender e o facto de termos escolhido esta perspetiva não ajudou. Porém, achamos que o resultado final é muito bom, tendo em conta que foi complexo e trabalhoso.
-}


-- | Este módulo define funções comuns da Tarefa 5 do trabalho prático.
module Main where
import Tarefa1_2019li1g068
import Tarefa2_2019li1g068
import Tarefa4_2019li1g068
import Tarefa6_2019li1g068
import LI11920
import Data.List
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment
import Data.Char
import System.Random
import Data.Maybe


-- | Função principal da Tarefa 5.
--
-- __NB:__ Esta Tarefa é completamente livre. Deve utilizar a biblioteca <http://hackage.haskell.org/package/gloss gloss> para animar o jogo, e reutilizar __de forma completa__ as funções das tarefas anteriores.

type EstadoGloss = (Estado, Menu, JogoInfo, Int)
type Coordenadas = (Float, Float)
data Imagens = Imagens {terra::Picture, terra1::Picture, terra2::Picture, terra_1::Picture, terra_2::Picture, terra_3::Picture, terra_4::Picture, relva::Picture, relva1::Picture, relva2::Picture, relva_1::Picture, relva_2::Picture, relva_3::Picture, relva_4::Picture, lama::Picture, lama1::Picture, lama2::Picture, lama_1::Picture, lama_2::Picture, lama_3::Picture, lama_4::Picture, boost::Picture, boost1::Picture, boost2::Picture, boost_1::Picture, boost_2::Picture, boost_3::Picture, boost_4::Picture, cola::Picture, cola1::Picture, cola2::Picture, cola_1::Picture, cola_2::Picture, cola_3::Picture, cola_4::Picture, mota::Picture, motaJ::Picture, motaMorto::Picture}
  deriving (Show,Eq)

data JogoInfo = JogoInfo {jogadores::Int, comBot::Bool, topJogador::Maybe Int}


data Menu = Definicoes Opcoes
            | MenuInicial
            | MenuIniciar
            | JogoFinalizado
            | Jogo
            | Pausa AposPausa


data AposPausa = Retomar | Abandonar

data Opcoes = Jogadores Int
        | Comprimento
        | Pistas
        | Bot Bool

data TeclasJogo = Cima | Baixo | Direita | Esquerda | DisparaCola
                        
-- Retorna Verdadeiro se o jogador estiver na pista dada pelo índice
jogadorNaPista :: Int -- Indice da pista
             -> Jogador 
             -> Bool
jogadorNaPista n (Jogador{pistaJogador=n'}) = n' == n

-- Retorna a Imagem da Peca fornecida
pegaImagemPeca :: Peca -> Imagens -> Picture
pegaImagemPeca (Recta Terra _) Imagens{terra=img} = img
pegaImagemPeca (Recta Relva _) Imagens{relva=img} = img
pegaImagemPeca (Recta Lama _) Imagens{lama=img} = img
pegaImagemPeca (Recta Boost _) Imagens{boost=img} = img
pegaImagemPeca (Recta Cola _) Imagens{cola=img} = img
pegaImagemPeca (Rampa Terra h h') Imagens{terra1=img1,terra2=img2, terra_1=img3, terra_2=img4, terra_3=img5, terra_4=img6}| h'-h == 1 = img1
                                                                                | h'-h == 2 = img2
                                                                                | h-h' == 1 = img3
                                                                                | h-h' == 2 = img4
                                                                                | h-h' == 3 = img5
                                                                                | otherwise = img6
pegaImagemPeca (Rampa Relva h h') Imagens{relva1=img1,relva2=img2, relva_1=img3, relva_2=img4, relva_3=img5, relva_4=img6}| h'-h == 1 = img1
                                                                                | h'-h == 2 = img2
                                                                                | h-h' == 1 = img3
                                                                                | h-h' == 2 = img4
                                                                                | h-h' == 3 = img5
                                                                                | otherwise = img6
pegaImagemPeca (Rampa Lama h h') Imagens{lama1=img1,lama2=img2, lama_1=img3, lama_2=img4, lama_3=img5, lama_4=img6}| h'-h == 1 = img1
                                                                                | h'-h == 2 = img2
                                                                                | h-h' == 1 = img3
                                                                                | h-h' == 2 = img4
                                                                                | h-h' == 3 = img5
                                                                                | otherwise = img6
pegaImagemPeca (Rampa Boost h h') Imagens{boost1=img1,boost2=img2, boost_1=img3, boost_2=img4, boost_3=img5, boost_4=img6}| h'-h == 1 = img1
                                                                                | h'-h == 2 = img2
                                                                                | h-h' == 1 = img3
                                                                                | h-h' == 2 = img4
                                                                                | h-h' == 3 = img5
                                                                                | otherwise = img6
pegaImagemPeca (Rampa Cola h h') Imagens{cola1=img1,cola2=img2, cola_1=img3, cola_2=img4, cola_3=img5, cola_4=img6}| h'-h == 1 = img1
                                                                                | h'-h == 2 = img2
                                                                                | h-h' == 1 = img3
                                                                                | h-h' == 2 = img4
                                                                                | h-h' == 3 = img5
                                                                                | otherwise = img6

-- Dada uma peca e a largura retorna a quantidade de píxeis que se tem de deslocar, no eixo horizontal, para a peça encaixar direito
xInicialPx :: Peca -> Float -> Float 
xInicialPx (Rampa _ h h') lPeca | h' > h = ((realToFrac (h-h')) * 44 + 5) * lPeca / (fst imgPixeis)
                                | otherwise = ((realToFrac (h-h')) * 130) * lPeca / (fst imgPixeis)
xInicialPx _ _ = 0


-- Dada uma peca e a largura retorna a quantidade de píxeis, no eixo horizontal, desde o inicio do caminho até ao final do mesmo
xFinalPx :: Peca -> Float -> Float -- inicio da imagem a ser posta
xFinalPx (Rampa _ h h') lPeca | h' > h = (realToFrac (h-h')) * 80 * lPeca / (fst imgPixeis)
                              | h-h'==1 = 480 * lPeca / (fst imgPixeis)
                              | h-h'==2 = 715 * lPeca / (fst imgPixeis)
                              | h-h'==3 = 978 * lPeca / (fst imgPixeis)
                              | otherwise = 1230 * lPeca / (fst imgPixeis)
xFinalPx _ lPeca = razao*lPeca


-- Desenha a pista de imagens
desenhaPista :: Pista 
             -> Coordenadas -- coordenadas da peca
             -> Imagens
             -> Coordenadas -- altura e largura da peca a colocar
             -> [Picture]
desenhaPista (h:t) (x,y) imgs c@(lPeca, hPeca) = (Translate (x + xInicialPx h lPeca) y (pegaImagemPeca h imgs)):desenhaPista t (x + xFinalPx h lPeca, y+hPeca) imgs c
desenhaPista _ _ _ _ = []


-- Desenha os jogadores nas respetivas posicões e devidas inclinações
desenhaJogadores :: Pista
                -> [Jogador] -- lista de jogadores
                -> Coordenadas -- coordenadas iniciais da pista
                -> Imagens
                -> Coordenadas -- altura e largura da peca a colocar
                -> Jogador -- JogadorOficial do Mapa para colorir
                -> [Picture]           
desenhaJogadores pista (j@(Jogador{estadoJogador=eJ}):js) c@(x,y) imgs@(Imagens{mota=img, motaJ=img2, motaMorto=img'}) c'@(lPeca, hPeca) jO = (Translate (x+ (xJogadorChao pista j) - adicionarYAr eJ) (y-hPeca/2+hPeca*x') (jogadorImagem j)):desenhaJogadores pista js c imgs c' jO
                    where x' = distJogador j
                          peca = pista!!(floor x')

                          -- Retorna Verdade se for uma rampa a subir caso contrário retorna Falso
                          rampaASubir :: Peca -> Bool
                          rampaASubir (Rampa _ h h') = h' > h
                          rampaASubir _ = False

                          -- Retorna a quantidade de píxeis a adicionar (considerando o jogador no chão)
                          xJogadorChao :: Pista -> Jogador -> Float
                          xJogadorChao (h:t) j@(Jogador{distanciaJogador=x}) | floor x == 0 && rampaASubir h = decimaisX*(xInicialPx h lPeca)
                                                                             | floor x == 0 = decimaisX*(xFinalPx h lPeca)
                                                                             | otherwise = xFinalPx h lPeca + xJogadorChao t j{distanciaJogador=(x-1)}

                          -- Retorna a Altura Exata do jogador (considerando o jogador no Chão)
                          alturaJogador :: Jogador -> Float
                          alturaJogador (Jogador n x _ _ _) = realToFrac(alturaExata peca (realToFrac decimaisX))

                          -- Retorna em a distância do jogador à peca onde se encontra
                          decimaisX :: Float
                          decimaisX = x'-(fromIntegral (floor x'))

                          -- Retorna a quantidade de píxeis a adicionar desde a altura onde ele devia estar até a altura onde se encontra
                          adicionarYAr :: EstadoJogador -> Float
                          adicionarYAr (Ar h _ _) = ((realToFrac h) - alturaJogador j)*190*lPeca/(fst imgPixeis)
                          adicionarYAr _ = 0

                          imagemMota :: Picture
                          imagemMota = if j==jO then img2 else img
                        
                          -- Retorna a imagem correspondente ao estado do jogador já com a sua inclinação
                          jogadorImagem :: Jogador -> Picture
                          jogadorImagem (Jogador{estadoJogador=Morto{}}) = img'
                          jogadorImagem (Jogador{estadoJogador=Ar{inclinacaoJogador=inc}}) = Rotate (realToFrac (-inc)) imagemMota
                          jogadorImagem _ = Rotate (realToFrac (-(inclinacao (pista!!(floor x'))))) imagemMota




desenhaJogadores _ _ _ _ _ _ = []





-- Desenha o mapa retornando uma lista de Picture
desenhaMapa :: Estado 
            -> Int -- nº da pista
            -> Coordenadas -- x inicial e altura do mapa (x inicial e y final)
            -> Imagens -- imagens para o mapa
            -> Coordenadas -- altura e largura da peca
            -> Jogador -- JogadorOficial do Mapa para colorir
            -> [Picture]
desenhaMapa (Estado (p:ps) js) n c@(w,h) imgs c'@(xPeca, hPeca) jO = (Pictures pistaComJogadores):desenhaMapa (Estado ps js') (n+1) c imgs c' jO
                        where
                            (jsPista, js') = partition (jogadorNaPista n) js
                            cIniciais = (w+xPeca*(fromIntegral n),h)
                            pistaImagem = Pictures (desenhaPista p cIniciais imgs c')
                            pistaComJogadores = pistaImagem:desenhaJogadores p jsPista ((\(x,y)-> (x-xPeca/3,y)) cIniciais) imgs c' jO -- função adicionada corresponde a distancia na pista a que a mota é colocada (central)
desenhaMapa _ _ _ _ _ _ = []


-- Retorna a distância do jogador ao início da pista
distJogador :: Jogador -> Float
distJogador (Jogador{distanciaJogador=x}) = realToFrac x


-- Desenha os mapas (um para cada jogador, excluíndo o bot) retornando uma lista de Picture
desenhaMapas :: Estado
            -> Coordenadas -- Tamanho do display
            -> Int -- nº de jogadores
            -> Coordenadas -- inicio do mapa (em x e y)
            -> Float -- distancia entre mapas
            -> Imagens
            -> Coordenadas -- largura e altura da peca
            -> Int -- identificador do Mapa
            -> [Picture]
desenhaMapas _ _ 0 _ _ _ _ _ = []
desenhaMapas e@(Estado m js) cDisplay@(_, hD) n c@(w,h) distMapas imgs c'@(xPeca,hPeca) idMapa = mapaImagem:desenhaMapas e cDisplay (n-1) (w+distMapas, h) distMapas imgs c' (idMapa-1)
                        where
                            jogadorOficial = js!!idMapa
                            nPistas = fromIntegral (length m)
                            nPecas = fromIntegral (length (head m))
                            posJogador = distJogador (js!!(n-1))
                            xyIdeal = (w+(hIdeal-h)/(1.5/razao), hIdeal)
                            mapaImagem = Pictures (desenhaMapa e 0 xyIdeal imgs c' jogadorOficial)

                            -- Retorna a altura ideal para centralizar o mapa caso o nº de pecas por pista não cubra a tela na vertical
                            hIdeal :: Float
                            hIdeal  | (nPecas - posJogador)*hPeca - hPeca/2 <= (hD/2) = min h (-nPecas*hPeca + hPeca/2 + hD/2)
                                    | h+(posJogador*hPeca) > 0 = -posJogador*hPeca
                                    | otherwise = h


-- Funcao definida pelo Graphics.Gloss.Game para retornar o EstadoGloss Inicial
estadoGlossInicial :: Int -- Random gerado para ser usado como seed
                    -> EstadoGloss
estadoGlossInicial random = (Estado [] [], MenuInicial, (JogoInfo 1 False Nothing), random)
 


-- Funcao definida pelo Graphics.Gloss.Game para desenhar o Jogo/Menus
desenhaEstadoGloss :: Coordenadas -- Resolução do Monitor
                    -> [Picture] -- Lista de Pictures referentes às pecas e às motas
                    -> [Picture] -- Lista de Pictures referentes aos menus
                    -> EstadoGloss
                    -> Picture
desenhaEstadoGloss (w,h) _ l (_, MenuInicial, _,_) = scale (w/1920) (h/1080) (head l)
desenhaEstadoGloss (w,h) _ l (_, Definicoes (Jogadores x), _,_) = scale (w/1920) (h/1080) (l!!(x + 1))
desenhaEstadoGloss (w,h) _ l (_, Definicoes (Bot True), _,_) = scale (w/1920) (h/1080) (l!!6)
desenhaEstadoGloss (w,h) _ l (_, Definicoes (Bot False), _,_) = scale (w/1920) (h/1080) (l!!7)
desenhaEstadoGloss (w,h) _ l (_, Pausa Retomar, _,_) = scale (w/1920) (h/1080) (l!!13)
desenhaEstadoGloss (w,h) _ l (_, Pausa Abandonar, _,_) = scale (w/1920) (h/1080) (l!!14)
desenhaEstadoGloss (w,h) _ l (_, MenuIniciar, JogoInfo{jogadores=x},_) = scale (w/1920) (h/1080) (l!!(14+x))
desenhaEstadoGloss (w,h) _ l (Estado _ js, JogoFinalizado, JogoInfo{topJogador=(Just id), comBot=x}, _) = scale (w/1920) (h/1080) $ l!!( if (length js -1 == id) && x then 12 else (8+id))
desenhaEstadoGloss (w,h) _ l (e, JogoFinalizado, _,_) = scale (w/1920) (h/1080) (l!!12)
desenhaEstadoGloss cDisplay@(w,h) l _ (e@(Estado m@(p:_) js), Jogo, (JogoInfo n bot _),_) = Pictures (desenhaMapas e cDisplay n (xInicial+distPeca/1.5, yInicial+alturaPeca/2) wMapa imgs (distPeca, alturaPeca) (n-1))
        where
            n' = fromIntegral n
            p' = fromIntegral (length p)
            nPistas = fromIntegral (length m)
            wMapa = (w - h/5)/n' -- largura do mapa com espaço incluido
            distPeca = min ((wMapa - espaco)/nPistas) 100
            alturaPeca = distPeca * 1.5
            (terra:terra1:terra2:terra_1:terra_2:terra_3:terra_4:relva:relva1:relva2:relva_1:relva_2:relva_3:relva_4:lama:lama1:lama2:lama_1:lama_2:lama_3:lama_4:boost:boost1:boost2:boost_1:boost_2:boost_3:boost_4:cola:cola1:cola2:cola_1:cola_2:cola_3:cola_4:mota:motaJ:motaMorto:[]) = map (scale ((distPeca+distPeca*razao)/xImgPixeis) (alturaPeca/yImgPixeis)) l
            imgs = Imagens terra terra1 terra2 terra_1 terra_2 terra_3 terra_4 relva relva1 relva2 relva_1 relva_2 relva_3 relva_4 lama lama1 lama2 lama_1 lama_2 lama_3 lama_4 boost boost1 boost2 boost_1 boost_2 boost_3 boost_4 cola cola1 cola2 cola_1 cola_2 cola_3 cola_4 mota motaJ motaMorto
            xInicial = max ((-w + h/5 + wMapa - (distPeca*nPistas + espaco + (min (h/alturaPeca) p')*distPeca*razao))/2) (-w/2)
            yInicial = max (-alturaPeca*p'/2) (-h/2)

            (xImgPixeis, yImgPixeis) = imgPixeis
            
-- Pixeis das Imagens das Pecas em tamanho real
imgPixeis :: (Float, Float)
imgPixeis = (491, 521)

-- Razao correspondente à inclinacão da peca (Recta)
razao :: Float
razao = 2/5

-- Espaço deixado entre os Mapas para evitar que os Mapas se sobreponham
espaco :: Float
espaco = 10

-- Pesquisa na lista de jogadores se um deles (através do identificador) possui "Chao{}" como estado
jogadorNoChao :: Estado
            -> Int -- Identificador do jogador
            -> Bool
jogadorNoChao (Estado m ((Jogador{estadoJogador=(Chao _)}):_)) 0 = True
jogadorNoChao _ 0 = False
jogadorNoChao (Estado m (_:js)) n = jogadorNoChao (Estado m js) (n-1)


-- Funcao definida pelo Graphics.Gloss.Game que dado um Evento executa uma alteração sobre o EstadoGloss
reageEventoGloss :: Event -> EstadoGloss -> EstadoGloss

reageEventoGloss (EventKey key Down _ _ ) (e, JogoFinalizado, info, seed) | key == (SpecialKey KeyEnter) || key == (Char '\\') = estadoGlossInicial seed

reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _ ) (e, MenuInicial, info, seed) = (e, Definicoes (Jogadores 1), info, seed)

reageEventoGloss (EventKey (Char '\\') Down _ _ ) (e,Definicoes (Jogadores _), info, seed) = (e, MenuInicial, info, seed)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (e,Definicoes (Jogadores x), info, seed) = (e, (Definicoes (Jogadores (max 1 (x-1)))), info, seed)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (e,Definicoes (Jogadores x), info, seed) = (e, (Definicoes (Jogadores (min 4 (x+1)))), info, seed)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (e,Definicoes (Jogadores x), info, seed) = (e, (Definicoes (Bot True)), info{jogadores=x}, seed)

reageEventoGloss (EventKey (Char '\\') Down _ _ ) (e,Definicoes (Bot _), info@JogoInfo{jogadores=x}, seed) = (e, Definicoes (Jogadores x), info, seed)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _ ) (e, Definicoes (Bot True), info, seed) = (e, (Definicoes (Bot False)), info, seed)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _ ) (e, Definicoes (Bot False), info, seed) = (e, (Definicoes (Bot True)), info, seed)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _ ) (e, Definicoes (Bot x), info, seed) = (e, MenuIniciar, info{comBot=x}, seed)

reageEventoGloss (EventKey (Char '\\') Down _ _ ) (e, MenuIniciar, info@JogoInfo{comBot=x}, seed) = (e, Definicoes (Bot x), info, seed)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _ ) (_, MenuIniciar, info@JogoInfo{jogadores=x, comBot=bot}, seed) = ((Estado mapa jogadores), Jogo, info, seeds!!4)
                                where
                                    seeds = take 5 (randomRs (0,100) (mkStdGen seed))
                                    nJs = x+(if bot then 1 else 0)
                                    pistas = fst (randomR (max 3 nJs,7) (mkStdGen (seeds!!0)))
                                    comp = fst (randomR (20,50) (mkStdGen (seeds!!1)))
                                    mapaSeed = fst (randomR (0,9) (mkStdGen (seeds!!2)))
                                    mapa = concat.(replicate pistas) $ gera 1 comp mapaSeed
                                    jogadores = map (\n -> (Jogador n 0 0 5 (Chao False))) (aux nJs [] (randomRs (0,pistas-1) (mkStdGen (seeds!!3))))

                                    -- Gera as pistas correspondentes de cada jogador aleatoriamente sem sobrepor jogadores
                                    aux :: Int -- nº jogadores
                                         -> [Int] -- lista de pistas ocupadas
                                         -> [Int] -- lista de números aleatorios
                                         -> [Int]
                                    aux n l (h:t) | n==0 = l
                                                  | h `elem` l = aux n l t
                                                  | otherwise = aux (n-1) (h:l) t 




reageEventoGloss (EventKey (Char '\\') Down _ _ ) (e, Jogo, info, seed) = (e, Pausa Retomar, info, seed)

reageEventoGloss (EventKey (Char '\\') Down _ _ ) (e, Pausa _, info, seed) = (e, Jogo, info, seed)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _ ) (e, Pausa Retomar, info, seed) = (e, Pausa Abandonar, info, seed)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _ ) (e, Pausa Abandonar, info, seed) = (e, Pausa Retomar, info, seed)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _ ) (e, Pausa Retomar, info, seed) = (e, Jogo, info, seed)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _ ) (e, Pausa Abandonar, info, seed) = (e, MenuInicial, info, seed)

reageEventoGloss (EventKey k Down _ _) (e, Jogo, info@(JogoInfo{jogadores=n}), seed) | teclaDir k Cima && idTecla k n <= n = if jogadorNoChao e (idTecla k n) then ((jogada (idTecla k n) Acelera e), Jogo, info, seed) else ((jogada (idTecla k n) (Movimenta E) e), Jogo, info, seed)
reageEventoGloss (EventKey k Down _ _) (e, Jogo, info@(JogoInfo{jogadores=n}), seed) | teclaDir k Baixo && idTecla k n <= n = if jogadorNoChao e (idTecla k n) then ((jogada (idTecla k n) Desacelera e), Jogo, info, seed) else ((jogada (idTecla k n) (Movimenta D) e), Jogo, info, seed)
reageEventoGloss (EventKey k Down _ _) (e, Jogo, info@(JogoInfo{jogadores=n}), seed) | teclaDir k Esquerda && idTecla k n <= n = ((jogada (idTecla k n) (Movimenta C) e), Jogo, info, seed) 
reageEventoGloss (EventKey k Down _ _) (e, Jogo, info@(JogoInfo{jogadores=n}), seed) | teclaDir k Direita && idTecla k n <= n = ((jogada (idTecla k n) (Movimenta B) e), Jogo, info, seed) 
reageEventoGloss (EventKey k Down _ _) (e, Jogo, info@(JogoInfo{jogadores=n}), seed) | teclaDir k DisparaCola && idTecla k n <= n = ((jogada (idTecla k n) Dispara e), Jogo, info, seed) 

reageEventoGloss _ s = s -- ignora qualquer outro evento


-- Dada uma tecla e a sua função retorna se uma se relaciona com a outra
teclaDir :: Key -> TeclasJogo -> Bool
teclaDir k Cima | elem k [SpecialKey KeyUp, Char 'w',Char 'y',Char '9'] = True
teclaDir k Baixo | elem k [SpecialKey KeyDown, Char 's',Char 'h',Char 'o'] = True
teclaDir k Esquerda | elem k [SpecialKey KeyLeft, Char 'a',Char 'g',Char 'i'] = True
teclaDir k Direita | elem k [SpecialKey KeyRight, Char 'd',Char 'j',Char 'p'] = True
teclaDir k DisparaCola | elem k [Char '-', Char 'e',Char 'u',Char '0'] = True
teclaDir _ _ = False


-- Dada uma tecla e o nº de jogadores retorna o identificador do jogador correspondente a essa mesma tecla
idTecla :: Key
         -> Int -- nº jogadores
         -> Int -- identificador do jogador
idTecla key _ | elem key [SpecialKey KeyUp, SpecialKey KeyDown, SpecialKey KeyLeft, SpecialKey KeyRight, Char '-'] = 0
idTecla (Char i) 4 | elem i ['w','s','a','d','e'] = 3
                   | elem i ['y','h','g','j','u'] = 2
                   | elem i ['9','o','i','p','0'] = 1
idTecla (Char i) 3 | elem i ['w','s','a','d','e'] = 2
                   | elem i ['y','h','g','j','u'] = 1
idTecla (Char i) 2 | elem i ['w','s','a','d','0'] = 1
idTecla _ _ = 99 -- número maior que n (jogadores) para não dar pattern match



-- Funcao definida pelo Graphics.Gloss.Game que a cada 1/fr segundos retorna as consequências do estado em função do tempo
reageTempoGloss :: Float -> EstadoGloss -> EstadoGloss
reageTempoGloss _ (e@(Estado m js), Jogo, info@(JogoInfo n comBot _), seed) | null vitoriosos = (Estado m' (map (passo (1/(fromIntegral fr)) m) js'), Jogo, info, seed)
                                                                     | otherwise = (Estado m js, JogoFinalizado, (JogoInfo n comBot vitorioso), seed)
                where
                    idBot = length js - 1
                    jogadaBot = fromMaybe Acelera (bot idBot e)
                    (Estado m' js') = if comBot then (jogada idBot jogadaBot e) else e
                    vitoriosos = filter chegouNaMeta js
                    vitorioso = elemIndex (head vitoriosos) js
                    meta = fromIntegral (length (head m)) - 0.5

                    -- Retorna se o jogador já chegou na meta
                    chegouNaMeta :: Jogador -> Bool
                    chegouNaMeta (Jogador{distanciaJogador=x}) = x >= meta
reageTempoGloss _ eG = eG

-- Funcao definida pelo Graphics.Gloss.Game associada a uma constante que corresponde à quantidade de frames por segundo
fr :: Int
fr = 30

-- Funcao definida pelo Graphics.Gloss.Game associada a uma constante que retorna o Display
dm :: Display
dm = FullScreen --InWindow "ExciteBike" (1000,1000) (0,0)


-- Funcao principal para executar o jogo automaticamente
main :: IO ()
main = do
    s <- getScreenSize --return (1000,1000)
    Just terra <- loadJuicy "RecursosT5/Pecas/terra.png"
    Just terra1 <- loadJuicy "RecursosT5/Pecas/terra1.png"
    Just terra2 <- loadJuicy "RecursosT5/Pecas/terra2.png"
    Just terra_1 <- loadJuicy "RecursosT5/Pecas/terra_1.png"
    Just terra_2 <- loadJuicy "RecursosT5/Pecas/terra_2.png"
    Just terra_3 <- loadJuicy "RecursosT5/Pecas/terra_3.png"
    Just terra_4 <- loadJuicy "RecursosT5/Pecas/terra_4.png"
    Just relva <- loadJuicy "RecursosT5/Pecas/relva.png"
    Just relva1 <- loadJuicy "RecursosT5/Pecas/relva1.png"
    Just relva2 <- loadJuicy "RecursosT5/Pecas/relva2.png"
    Just relva_1 <- loadJuicy "RecursosT5/Pecas/relva_1.png"
    Just relva_2 <- loadJuicy "RecursosT5/Pecas/relva_2.png"
    Just relva_3 <- loadJuicy "RecursosT5/Pecas/relva_3.png"
    Just relva_4 <- loadJuicy "RecursosT5/Pecas/relva_4.png"
    Just lama <- loadJuicy "RecursosT5/Pecas/lama.png"
    Just lama1 <- loadJuicy "RecursosT5/Pecas/lama1.png"
    Just lama2 <- loadJuicy "RecursosT5/Pecas/lama2.png"
    Just lama_1 <- loadJuicy "RecursosT5/Pecas/lama_1.png"
    Just lama_2 <- loadJuicy "RecursosT5/Pecas/lama_2.png"
    Just lama_3 <- loadJuicy "RecursosT5/Pecas/lama_3.png"
    Just lama_4 <- loadJuicy "RecursosT5/Pecas/lama_4.png"
    Just boost <- loadJuicy "RecursosT5/Pecas/boost.png"
    Just boost1 <- loadJuicy "RecursosT5/Pecas/boost1.png"
    Just boost2 <- loadJuicy "RecursosT5/Pecas/boost2.png"
    Just boost_1 <- loadJuicy "RecursosT5/Pecas/boost_1.png"
    Just boost_2 <- loadJuicy "RecursosT5/Pecas/boost_2.png"
    Just boost_3 <- loadJuicy "RecursosT5/Pecas/boost_3.png"
    Just boost_4 <- loadJuicy "RecursosT5/Pecas/boost_4.png"
    Just cola <- loadJuicy "RecursosT5/Pecas/cola.png"
    Just cola1 <- loadJuicy "RecursosT5/Pecas/cola1.png"
    Just cola2 <- loadJuicy "RecursosT5/Pecas/cola2.png"
    Just cola_1 <- loadJuicy "RecursosT5/Pecas/cola_1.png"
    Just cola_2 <- loadJuicy "RecursosT5/Pecas/cola_2.png"
    Just cola_3 <- loadJuicy "RecursosT5/Pecas/cola_3.png"
    Just cola_4 <- loadJuicy "RecursosT5/Pecas/cola_4.png"
    Just mota <- loadJuicy "RecursosT5/Motas/mota.png"
    Just motaJ <- loadJuicy "RecursosT5/Motas/motaj.png"
    Just motaMorto <- loadJuicy "RecursosT5/Motas/motamorto.png"
    Just menuInicial <- loadJuicy "RecursosT5/Menus/menuinicial.png"
    Just js1 <- loadJuicy "RecursosT5/Menus/1jogadores.png"
    Just js2 <- loadJuicy "RecursosT5/Menus/2jogadores.png"
    Just js3 <- loadJuicy "RecursosT5/Menus/3jogadores.png"
    Just js4 <- loadJuicy "RecursosT5/Menus/4jogadores.png"
    Just botSim <- loadJuicy "RecursosT5/Menus/botsim.png"
    Just botNao <- loadJuicy "RecursosT5/Menus/botnao.png"
    Just vencedor1 <- loadJuicy "RecursosT5/Menus/vencedor1.png"
    Just vencedor2 <- loadJuicy "RecursosT5/Menus/vencedor2.png"
    Just vencedor3 <- loadJuicy "RecursosT5/Menus/vencedor3.png"
    Just vencedor4 <- loadJuicy "RecursosT5/Menus/vencedor4.png"
    Just vencedorbot <- loadJuicy "RecursosT5/Menus/vencedorbot.png"
    Just retomar <- loadJuicy "RecursosT5/Menus/retomar.png"
    Just abandonar <- loadJuicy "RecursosT5/Menus/abandonar.png"
    Just controlos1 <- loadJuicy "RecursosT5/Menus/controlos1.png"
    Just controlos2 <- loadJuicy "RecursosT5/Menus/controlos2.png"
    Just controlos3 <- loadJuicy "RecursosT5/Menus/controlos3.png"
    Just controlos4 <- loadJuicy "RecursosT5/Menus/controlos4.png"
    
    let pics = [terra, terra1, terra2, terra_1, terra_2, terra_3, terra_4, relva, relva1, relva2, relva_1, relva_2, relva_3, relva_4, lama, lama1, lama2, lama_1, lama_2, lama_3, lama_4, boost, boost1, boost2, boost_1, boost_2, boost_3, boost_4, cola, cola1, cola2, cola_1, cola_2, cola_3, cola_4, mota, motaJ, motaMorto]
    let pics2 = [menuInicial, menuInicial, js1, js2, js3, js4, botSim, botNao, vencedor1, vencedor2, vencedor3, vencedor4, vencedorbot, retomar, abandonar, controlos1, controlos2, controlos3, controlos4]
    let screenSize = (fromIntegral (fst s), fromIntegral (snd s))
    random <- randomRIO (0,9) 
    play
        dm                   -- janela onde irá correr o jogo
        white --(greyN 0.5)             -- côr do fundo da janela
        fr                      -- frame rate
        (estadoGlossInicial random) -- estado inicial
        (desenhaEstadoGloss screenSize pics pics2)      -- desenha o estado do jogo
        reageEventoGloss        -- reage a um evento
        reageTempoGloss         -- reage ao passar do tempo



   


