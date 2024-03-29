{- |
Module      : Tarefa6_2022li1g061
Description : Interface em Gloss
Copyright   : Fernando Pires <a77399@alunos.uminho.pt>
              Tomás Cunha <a100481@alunos.uminho.pt>

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2022/23.
-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import LI12223
import Tarefa1_2022li1g061
import Tarefa2_2022li1g061
import Tarefa3_2022li1g061
import Tarefa4_2022li1g061
import Tarefa5_2022li1g061
import System.Random

type Estado = (Jogo,Jogada)
type EstadoGloss = (Estado, Images, Time)
type Time = Float
data Opcao = Jogar
            | Sair
            | JogoNormal
            | JogoPinguim
            | JogoInvencivel
data Menu = Opcoes Opcao
          | ModoJogo
          | PerdeuJogo Opcao
          | MenuJogo Opcao
          | ModoJogoPinguim
          | ModoJogoInvencivel
type World = (Menu, EstadoGloss)

{- |
     FUNÇÃO QUE ESTABELECE O ESTADO INICIAL
-}

estadoInicial :: Estado
estadoInicial = ((Jogo (Jogador (5,0)) mapaIni), Parado)

{- |
     FUNÇÃO QUE EXTRAI O MAPA
         Esta função recebe um Estado e retorna uma lista de tuplos como o Terreno e a lista de Obstaculos
-}

getMap :: Estado -> [(Terreno,[Obstaculo])]
getMap ((Jogo (Jogador (x,y)) (Mapa l lista)),jogada) = lista

{- |
    FUNÇÃO QUE ESTABELECE O ESTADO GLOSS INICIAL
-}

estadoGlossInicial:: Images -> EstadoGloss
estadoGlossInicial images = (estadoInicial,images,0)

{- |
    FUNÇÃO QUE ESTABELECE O WORLD INICIAL
-}

worldInicial :: Images -> World
worldInicial images = (Opcoes Jogar, (estadoGlossInicial images))

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{- |
    ALTURA DO ECRA
-}

altura :: Float
altura = 445

{- |
    COMPRIMENTO DO ECRA
-}

comprimento :: Float
comprimento = (-445)

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

data Images = Images 
                {
                    galinha :: Picture
                    , relva :: Picture
                    , estrada :: Picture
                    , rio :: Picture
                    , arvore :: Picture
                    , tronco :: Picture
                    , carro :: Picture
                    , playBtn :: Picture
                    , exitBtn :: Picture
                    , backGround :: Picture
                    , playSel :: Picture
                    , exitSel :: Picture
                    , defeat :: Picture
                    , pinguim :: Picture
                    , normalGame :: Picture
                    , pinguimGame :: Picture
                    , invencivelGame :: Picture
                    , urso :: Picture
                    , gelo :: Picture
                    , cubo :: Picture
                    , aviao :: Picture
                }

{- |
    FUNÇÃO QUE CARREGA AS images
        Esta função extrai as images contidas na pasta Tarefa 6 e transforma-as em tipos Picture no contexto do programa
-}

loadImages :: IO Images
loadImages = do
    galinha <- loadBMP "images/galinha.bmp"
    relva <- loadBMP "images/relva.bmp"
    estrada <- loadBMP "images/estrada.bmp"
    rio <- loadBMP "images/rio.bmp"
    arvore <- loadBMP "images/arvore.bmp"
    tronco <- loadBMP "images/tronco.bmp"
    carro <- loadBMP "images/carro.bmp"
    backGround <- loadBMP "images/crossyRoad.bmp"
    exitBtn <- loadBMP "images/Sair.bmp"
    playBtn <- loadBMP "images/Jogar.bmp"
    playSel <- loadBMP "images/JogarSel.bmp"
    exitSel <- loadBMP "images/SairSel.bmp"
    defeat <- loadBMP "images/defeat.bmp"
    pinguim <- loadBMP "images/pinguim.bmp"
    normalGame <- loadBMP "images/menuJogoNormal.bmp"
    pinguimGame <- loadBMP "images/menuJogoPinguim.bmp"
    invencivelGame <- loadBMP "images/menuJogoInvencivel.bmp"
    urso <- loadBMP "images/urso.bmp"
    gelo <- loadBMP "images/gelo.bmp"
    cubo <- loadBMP "images/cubo.bmp"
    aviao <- loadBMP "images/aviao.bmp"
    return Images   {
                    galinha = Scale 0.35 0.35 galinha, relva = relva, rio = rio, estrada = estrada, arvore = arvore,
                    tronco = tronco, carro = carro, backGround = backGround, playBtn = playBtn, exitBtn = exitBtn,
                    playSel = playSel, exitSel = exitSel, defeat = defeat, pinguim = Scale 0.75 0.75 pinguim, normalGame = normalGame,
                    pinguimGame = pinguimGame, invencivelGame = invencivelGame, urso = urso, cubo = cubo, gelo = gelo, aviao = aviao
                    }

{- |
    FUNÇÃO QUE GERE O JOGO MEDIANTE O INPUT DO TECLADO
        Esta função recebe um Event e um World e retorna um World
            Quando o Jogo se encontra no Menu (denotado por "Opcoes Jogar" e "Opcoes Sair") a função pode alterar a opção seleccionada ou fechar o programa
            Quando o Jogo se encontra no Modo de Jogo (denotado por "ModoJogo") a função movimenta o jogador no mapa consoante o input dado pelo teclado
-}

reageEvento :: Event -> World -> World
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo,(((Jogo (Jogador (x,y)) mapa),jogada),images,time)) = (ModoJogo,(((Jogo (movePlayer (Jogo(Jogador(x,y))mapa) (Move Cima)) mapa),jogada),images,time))
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo,(((Jogo (Jogador (x,y)) mapa),jogada),images,time)) = (ModoJogo,(((Jogo (movePlayer (Jogo(Jogador(x,y))mapa) (Move Baixo)) mapa),jogada),images,time))
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo,(((Jogo (Jogador (x,y)) mapa),jogada),images,time)) = (ModoJogo,(((Jogo (movePlayer (Jogo(Jogador(x,y))mapa) (Move Esquerda)) mapa),jogada),images,time))
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo,(((Jogo (Jogador (x,y)) mapa),jogada),images,time)) = (ModoJogo,(((Jogo (movePlayer (Jogo(Jogador(x,y))mapa) (Move Direita)) mapa),jogada),images,time))
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Jogar, jogo) = (MenuJogo JogoNormal, jogo)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Jogar, jogo) = (Opcoes Sair, jogo)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Jogar, jogo) = (Opcoes Sair, jogo)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Sair, jogo) = (Opcoes Jogar, jogo)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Sair, jogo) = (Opcoes Jogar, jogo)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Sair, jogo) = error "Fim de Jogo"
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (PerdeuJogo Jogar,jogo) = (MenuJogo JogoNormal,jogo)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (PerdeuJogo Jogar, jogo) = (PerdeuJogo Sair, jogo)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (PerdeuJogo Jogar, jogo) = (PerdeuJogo Sair, jogo)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (PerdeuJogo Sair, jogo) = (PerdeuJogo Jogar, jogo)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (PerdeuJogo Sair, jogo) = (PerdeuJogo Jogar, jogo)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (PerdeuJogo Sair, jogo) = error "Fim de Jogo"
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (MenuJogo JogoNormal,(((Jogo(Jogador(x,y)) mapa),jogada),images,time)) = (ModoJogo,(((Jogo (Jogador(5,0)) mapaIni),Parado),images,0))
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (MenuJogo JogoNormal, jogo) = (MenuJogo JogoInvencivel, jogo)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (MenuJogo JogoNormal, jogo) = (MenuJogo JogoPinguim, jogo)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (MenuJogo JogoPinguim, jogo) = (MenuJogo JogoNormal, jogo)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (MenuJogo JogoPinguim, jogo) = (MenuJogo JogoInvencivel, jogo)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (MenuJogo JogoInvencivel, jogo) = (MenuJogo JogoPinguim, jogo)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (MenuJogo JogoInvencivel, jogo) = (MenuJogo JogoNormal, jogo)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (MenuJogo JogoPinguim,(((Jogo(Jogador(x,y)) mapa),jogada),images,time)) = (ModoJogoPinguim,(((Jogo (Jogador(5,0)) mapaIni),Parado),images,0))
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (MenuJogo JogoInvencivel,(((Jogo(Jogador(x,y)) mapa),jogada),images,time)) = (ModoJogoInvencivel,(((Jogo (Jogador(5,0)) mapaIni),Parado),images,0))
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogoPinguim,(((Jogo (Jogador (x,y)) mapa),jogada),images,time)) = (ModoJogoPinguim,(((Jogo (movePlayer (Jogo(Jogador(x,y))mapa) (Move Cima)) mapa),jogada),images,time))
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogoPinguim,(((Jogo (Jogador (x,y)) mapa),jogada),images,time)) = (ModoJogoPinguim,(((Jogo (movePlayer (Jogo(Jogador(x,y))mapa) (Move Baixo)) mapa),jogada),images,time))
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogoPinguim,(((Jogo (Jogador (x,y)) mapa),jogada),images,time)) = (ModoJogoPinguim,(((Jogo (movePlayer (Jogo(Jogador(x,y))mapa) (Move Esquerda)) mapa),jogada),images,time))
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogoPinguim,(((Jogo (Jogador (x,y)) mapa),jogada),images,time)) = (ModoJogoPinguim,(((Jogo (movePlayer (Jogo(Jogador(x,y))mapa) (Move Direita)) mapa),jogada),images,time))
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogoInvencivel,(((Jogo (Jogador (x,y)) mapa),jogada),images,time)) = (ModoJogoInvencivel,(((Jogo (Jogador (x,(y+1))) mapa),jogada),images,time))
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogoInvencivel,(((Jogo (Jogador (x,y)) mapa),jogada),images,time)) = (ModoJogoInvencivel,(((Jogo (Jogador (x,(y-1))) mapa),jogada),images,time))
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogoInvencivel,(((Jogo (Jogador (x,y)) mapa),jogada),images,time)) = (ModoJogoInvencivel,(((Jogo (Jogador ((x-1),y)) mapa),jogada),images,time))
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogoInvencivel,(((Jogo (Jogador (x,y)) mapa),jogada),images,time)) = (ModoJogoInvencivel,(((Jogo (Jogador ((x+1),y)) mapa),jogada),images,time))
reageEvento _ s = s 

{- |
    FUNÇÃO QUE GERE O JOGO MEDIANTE A PASSAGEM DO TEMPO
        Esta função recebe um Float e um World e retorna um World
            A cada 2 segundos, averigua-se o estado, caso o jogador perca, acaba o jogo, caso contrario incrementa-se o tempo e anima-se o movimento do mapa atraves da auxiliar deslizaJogo
-}

reageTempo :: Float -> World -> World
reageTempo t (ModoJogo,(((Jogo (Jogador(x,y)) (Mapa larg l)),jogada),images,time))
  | mod (milissecondsPassed) 1000 < 10 =    if gameOver (Jogo (Jogador(x,y)) (Mapa larg l)) 
                                            then (PerdeuJogo Jogar,(((Jogo (Jogador(x,y)) (Mapa larg l)),jogada),images,time))
                                            else (ModoJogo,((deslizar,jogada),images,(time+t)))
  | otherwise = if gameOver (Jogo (Jogador(x,y)) (Mapa larg l))
                then (PerdeuJogo Jogar,(((Jogo (Jogador(x,y)) (Mapa larg l)),jogada),images,time))
                else (ModoJogo,(((Jogo (Jogador(x,y)) (Mapa larg l)),jogada),images,(time+t)))
    where milissecondsPassed = round ((time+t)*1000)
          deslizar = deslizaJogo seed animate
          animate = animaJogo (Jogo (Jogador(x,y)) (Mapa larg l)) Parado
          seed = createRandom (take 100000 (randoms rand :: [Int])) (round (time+t))
          rand = mkStdGen 1000
reageTempo t (ModoJogoPinguim,(((Jogo (Jogador(x,y)) (Mapa larg l)),jogada),images,time))
  | mod (milissecondsPassed) 1000 < 10 =    if gameOver (Jogo (Jogador(x,y)) (Mapa larg l)) 
                                            then (PerdeuJogo Jogar,(((Jogo (Jogador(x,y)) (Mapa larg l)),jogada),images,time))
                                            else (ModoJogoPinguim,((deslizar,jogada),images,(time+t)))
  | otherwise = if gameOver (Jogo (Jogador(x,y)) (Mapa larg l))
                then (PerdeuJogo Jogar,(((Jogo (Jogador(x,y)) (Mapa larg l)),jogada),images,time))
                else (ModoJogoPinguim,(((Jogo (Jogador(x,y)) (Mapa larg l)),jogada),images,(time+t)))
    where milissecondsPassed = round ((time+t)*1000)
          deslizar = deslizaJogo seed animate
          animate = animaJogo (Jogo (Jogador(x,y)) (Mapa larg l)) Parado
          seed = createRandom (take 100000 (randoms rand :: [Int])) (round (time+t))
          rand = mkStdGen 1000
reageTempo t (ModoJogoInvencivel,(((Jogo(Jogador(x,y)) (Mapa larg l)),jogada),images,time))
  | mod (milissecondsPassed) 1000 < 10 = (ModoJogoInvencivel,((deslizar,jogada),images,(time+t)))
  | otherwise = (ModoJogoInvencivel,(((Jogo (Jogador(x,y)) (Mapa larg l)),jogada),images,(time+t)))
    where milissecondsPassed = round ((time+t)*1000)
          deslizar = deslizaJogo seed animate
          animate = animaJogo (Jogo (Jogador(x,y)) (Mapa larg l)) Parado
          seed = createRandom (take 100000 (randoms rand :: [Int])) (round (time+t))
          rand = mkStdGen 1000
reageTempo _ w = w

{- |
    FUNÇÃO QUE CRIA UM NUMERO ALEATORIO
        Esta função recebe uma lista de Inteiros e um Inteiro isolado e retorna outro Inteiro
           Se a lista for vazia, retorna 0
           Caso contrario retorna o elemento da lista que se encontra na posicao x+1
-}

createRandom :: [Int] -> Int -> Int
createRandom [] _ = 0
createRandom l x = (!!) l (x+1)

--------------------------------------------------------------------------------------------------------------------------------

{- |
    FRAME RATE
-}

fr:: Int
fr = 50

{- |
    DEFINICOES DA JANELA
-}

dm :: Display
dm = InWindow
      "Crossy Road do Ipiranga"  
      (990, 990)   
      (500,25)        

--------------------------------------------------------------------------------------------------------------------------------

{- |
    TAMANHO DO LADO
-}

l :: Float
l = 90

{- |
    FUNÇÃO QUE DESENHA UMA UNIDADE DO MAPA
        Esta função recebe dois Floats, um par com Terreno e Obstaculo e a lista de images, retorna a imagem correspondente
           Se o Terreno for Relva, será averiguado o obstaculo, mediante isto, a imagem correspondente será desenhada nas coordenadas dadas
           Se o Terreno for Rio, será averiguado o obstaculo, mediante isto, a imagem correspondente será desenhada nas coordenadas dadas
           Se o Terreno for Estrada, será averiguado o obstaculo, mediante isto, a imagem correspondente será desenhada nas coordenadas dadas
-}

desenhaPeca :: Float -> Float -> (Terreno,Obstaculo) -> Images -> Picture
desenhaPeca x y (Relva,obs) images
    | obs == Nenhum = Translate x y (relva images)
    | otherwise = Translate x y (arvore images)
desenhaPeca x y (Rio _,obs) images
    | obs == Nenhum = Translate x y (rio images)
    | otherwise = Translate x y (tronco images)
desenhaPeca x y (Estrada v,obs) images
    | obs == Nenhum = Translate x y (estrada images)
    | otherwise = Translate x y (carro images) 

{- |
    FUNÇÃO QUE DESENHA A LINHA DO MAPA
        Esta função recebe dois Floats, um par com Terreno e Obstaculo e a lista de images, retorna a lista de images correspondentes
           Se a lista nao for vazia, invoca a auxiliar desenhaPeca e avança recursivamente, incrementando as coordenadas para a proxima casa
           Caso contrario retorna lista vazia
-}

desenhaLinha :: Float -> Float -> (Terreno,[Obstaculo]) -> Images -> [Picture]
desenhaLinha x y (t,(ob:obs)) images = peca : resto
  where peca = desenhaPeca x y (t,ob) images
        resto = desenhaLinha (x+l) y (t,obs) images
desenhaLinha _ _ _ _ = []

{- |
    FUNÇÃO QUE DESENHA O MAPA
        Esta função recebe dois Floats, um par com Terreno e Obstaculo e a lista de images, retorna a lista de images correspondentes
           Se a lista nao for vazia, invoca a auxiliar desenhaLinha e avança recursivamente, decrementando as coordenadas para a proxima linha
           Caso contrario retorna lista vazia
-}

desenhaMapa :: Float -> Float -> [(Terreno,[Obstaculo])] -> Images -> [Picture]
desenhaMapa x y (h:t) images = linha ++ resto
  where linha = desenhaLinha x y h images
        resto = desenhaMapa x (y-l) t images
desenhaMapa _ _ _ _ = []



---------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------
                                                                 --PINGUIM--
---------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------

desenhaMapaPinguim :: Float -> Float -> [(Terreno,[Obstaculo])] -> Images -> [Picture]
desenhaMapaPinguim x y (h:t) images = linhaPinguim ++ restoPinguim
  where linhaPinguim = desenhaLinhaPinguim x y h images
        restoPinguim = desenhaMapaPinguim x (y-l) t images
desenhaMapaPinguim _ _ _ _ = []

desenhaLinhaPinguim :: Float -> Float -> (Terreno,[Obstaculo]) -> Images -> [Picture]
desenhaLinhaPinguim x y (t,(ob:obs)) images = pecaPinguim : restoPinguim
  where pecaPinguim = desenhaPecaPinguim x y (t,ob) images
        restoPinguim = desenhaLinhaPinguim (x+l) y (t,obs) images
desenhaLinhaPinguim _ _ _ _ = []

desenhaPecaPinguim :: Float -> Float -> (Terreno,Obstaculo) -> Images -> Picture
desenhaPecaPinguim x y (Relva,obs) images
    | obs == Nenhum = Translate x y (gelo images)
    | otherwise = Translate x y (cubo images)
desenhaPecaPinguim x y (Rio _,obs) images
    | obs == Nenhum = Translate x y (rio images)
    | otherwise = Translate x y (gelo images)
desenhaPecaPinguim x y (Estrada v,obs) images
    | obs == Nenhum = Translate x y (gelo images)
    | otherwise = Translate x y (urso images) 

---------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------


{- |
    FUNÇÃO QUE DESENHA O JOGO
        Esta função recebe um World e retorna images
           Se o Jogo estiver no Menu, o mesmo será desenhado tendo em conta qual a opcao seleccionada
           Se o Jogo estiver em Modo de Jogo, será desenhado o mapa atraves da auxiliar desenhaMapa, o personagem e a pontuacao atraves da auxiliar mostraTempo
           Se o Jogador perder, será emitida a mensagem de derrota com o respectivo score obtido
-}

desenhaEstadoGloss :: World -> Picture
desenhaEstadoGloss (Opcoes Jogar,(a@((Jogo (Jogador (x,y)) mapa),jogada),images,time)) = Pictures [Translate 0 0 (backGround images), Translate 0 (-200) (playSel images), Translate 0 (-300) (exitBtn images)]
desenhaEstadoGloss (Opcoes Sair,(a@((Jogo (Jogador (x,y)) mapa),jogada),images,time)) = Pictures [Translate 0 0 (backGround images), Translate 0 (-200) (playBtn images), Translate 0 (-300) (exitSel images)]
desenhaEstadoGloss (ModoJogo,(a@((Jogo (Jogador (x,y)) mapa),jogada),images,time)) = Pictures (desenhoMapa ++ [Translate (fromIntegral (x*90-445)) (fromIntegral (y*90-445)) (galinha images)] ++ [mostraTempo time])
    where   desenhoMapa = desenhaMapa comprimento altura mapaInicial images
            mapaInicial = getMap a
            mostraTempo:: Float -> Picture
            mostraTempo n = Translate 420 430 $ scale 0.5 0.5 $ Color red $ Text (show $ round n)
desenhaEstadoGloss (PerdeuJogo Jogar,(a@((Jogo (Jogador (x,y)) mapa),jogada),images,time)) = Pictures [Translate 0 0 (defeat images), Translate 0 (-200) (playSel images), Translate 0 (-300) (exitBtn images), Translate (-150) 0 $ Color black $ Text "Score:", Translate 0 (-100) $ Color black $ Text (show $ round time)]
desenhaEstadoGloss (PerdeuJogo Sair,(a@((Jogo (Jogador (x,y)) mapa),jogada),images,time)) = Pictures [Translate 0 0 (defeat images), Translate 0 (-200) (playBtn images), Translate 0 (-300) (exitSel images), Translate (-150) 0 $ Color black $ Text "Score:", Translate 0 (-100) $ Color black $ Text (show $ round time)]
desenhaEstadoGloss (MenuJogo JogoNormal,(((Jogo (Jogador (x,y)) mapa),jogada),images,time)) = Pictures [Translate 0 0 (normalGame images)]
desenhaEstadoGloss (MenuJogo JogoPinguim,(((Jogo (Jogador (x,y)) mapa),jogada),images,time)) = Pictures [Translate 0 0 (pinguimGame images)]
desenhaEstadoGloss (MenuJogo JogoInvencivel,(((Jogo (Jogador (x,y)) mapa),jogada),images,time)) = Pictures [Translate 0 0 (invencivelGame images)]
desenhaEstadoGloss (ModoJogoPinguim,(a@((Jogo (Jogador (x,y)) mapa),jogada),images,time)) = Pictures (desenhoMapaPinguim ++ [Translate (fromIntegral (x*90-445)) (fromIntegral (y*90-445)) (pinguim images)] ++ [mostraTempo time])
    where   desenhoMapaPinguim = desenhaMapaPinguim comprimento altura mapaInicial images
            mapaInicial = getMap a
            mostraTempo:: Float -> Picture
            mostraTempo n = Translate 420 430 $ scale 0.5 0.5 $ Color red $ Text (show $ round n)
desenhaEstadoGloss (ModoJogoInvencivel,(a@((Jogo (Jogador (x,y)) mapa),jogada),images,time)) = Pictures (desenhoMapa ++ [Translate (fromIntegral (x*90-445)) (fromIntegral (y*90-445)) (aviao images)] ++ [mostraTempo time])
    where   desenhoMapa = desenhaMapa comprimento altura mapaInicial images
            mapaInicial = getMap a
            mostraTempo:: Float -> Picture
            mostraTempo n = Translate 420 430 $ scale 0.5 0.5 $ Color red $ Text (show $ round n)



---------------------------------------------------------------------------------------------------------------------------------

{- |
    MAIN
       Função responsavel por correr o Jogo
-}

main :: IO ()
main = do 
        images <- loadImages
        play  dm             
            (greyN 0.5)   
            fr             
            (worldInicial images)
            desenhaEstadoGloss   
            reageEvento    
            reageTempo     

{- |
    MAPA INICIAL
-}

mapaIni = Mapa 11  [
    (Rio 1,[Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Estrada 1,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Rio 1,[Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Relva,[Nenhum,Arvore,Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Relva,[Nenhum,Nenhum,Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Relva,[Nenhum,Nenhum,Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Nenhum]),
    (Relva,[Nenhum,Nenhum,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Relva,[Nenhum,Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Nenhum]),
    (Relva,[Nenhum,Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Relva,[Nenhum,Nenhum,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Relva,[Nenhum,Nenhum,Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore])
                ]
 