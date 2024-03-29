{- |
Module      : Tarefa3_2022li1g061
Description : Movimentação do personagem e obstáculos
Copyright   : Fernando Pires <a77399@alunos.uminho.pt>
              Tomás Cunha <a100481@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa3_2022li1g061 where

import LI12223
import Tarefa4_2022li1g061
import Tarefa1_2022li1g061

{- |
    FUNÇÃO PRINCIPAL QUE FAZ O JOGO CORRER
        Esta função anima o jogo recebendo o jogo e a jogada jogadas, estando constantemente a gerar novos jogos, conforme as jogadas recebidas
-}
animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo(Jogador (x,y)) (Mapa larg l)) play = 
        let obs = moveObs l; jog = movePlayer (Jogo(Jogador(x,y))(Mapa larg l)) play in (Jogo jog (Mapa larg obs))
{- |
    FUNÇÃO AUXILIAR PARA TRANSFORMAR O JOGO APENAS NO JOGADOR
        Esta função serve para, conforme o jogo que for gerado na movePlayer, devolver apenas a posição em que o jogador se encontra
-}
gameToPlayer :: Jogo -> Jogador
gameToPlayer (Jogo (Jogador (x,y)) mapa) = (Jogador (x,y))

--PASSO 2 3 4 
{- |
    FUNÇÃO PARA CALCULAR OS MOVIMENTOS DO JOGADOR
        Esta função é um pouco complicada, mas no geral recebe um Jogo, a Jogada efetuada e devolve novamente o Jogo
            Caso a jogada seja "Parado", caso o jogador esteja em cima de um Rio e de um Tronco, deve acompanhar esse tronco com auxilio da funçao auxiliar movePlayerObs.
                Caso nao esteja no tronco, fica parado.
            Caso a jogada seja "Move Direçao", tendo em conta as várias direções o jogador não pode ultrapassar arvores, nem pode sair do mapa com os seus movimentos.
-}
movePlayer :: Jogo -> Jogada -> Jogador
movePlayer (Jogo (Jogador (x,y)) mapa) Parado = 
    case (fst (busca (Jogo (Jogador (x,y)) mapa))) of 
        Rio v ->    if (snd(busca (Jogo (Jogador(x,y)) mapa))) == Tronco
                    then (Jogador (x+v,y-2))
                    else (Jogador (x,y-2))
        _ -> (Jogador (x,y-2))
movePlayer (Jogo (Jogador (x,y)) mapa) jogada
    | jogada == Move Cima = if (offMap (Jogo (Jogador (x,y+1)) mapa) || busca(Jogo (Jogador (x,y+1)) mapa) == (Relva, Arvore))  then (Jogador (x,y))
                                                                                                                                else (Jogador (x,y+1))
    | jogada == Move Baixo = if (offMap (Jogo (Jogador (x,y-1)) mapa) || busca(Jogo (Jogador (x,y-1)) mapa) == (Relva, Arvore)) then (Jogador (x,y))
                                                                                                                                else (Jogador (x,y-1))
    | jogada == Move Direita = if (offMap (Jogo (Jogador (x+1,y)) mapa) || busca(Jogo (Jogador (x+1,y)) mapa) == (Relva, Arvore))   then (Jogador (x,y))
                                                                                                                                    else (Jogador (x+1,y))
    | jogada == Move Esquerda = if (offMap (Jogo (Jogador (x-1,y)) mapa) || busca(Jogo (Jogador (x-1,y)) mapa) == (Relva, Arvore))  then (Jogador (x,y))
                                                                                                                                    else (Jogador (x-1,y))
{- |
    FUNÇÃO PARA MOVIMENTAR O JOGADOR DE ACORDO COM O TRONCO
        Esta função recebe um jogador, o par com o terreno e o obstaculo em que se encontra e devolve a posição do jogador com a movimentação do tronco
            Caso a velocidade seja maior que 0 o jogador move-se da esquerda para a direita à mesma velocidade que o tronco
            Caso a velocidade seja menor que 0 é o caso inverso
            Caso a velocidade seja 0 o jogador fica na mesma posição
-}
movePlayerObs :: Jogador -> (Terreno,Obstaculo) -> Jogador
movePlayerObs (Jogador (x,y)) (Rio v,obs)
    | v > 0 = (Jogador (x+v,y))
    | v < 0 = (Jogador (x-v,y))
    | otherwise = (Jogador (x,y))
movePlayerObs (Jogador(x,y)) _ = (Jogador (x,y))

-- PASSO 1
{- |
    FUNÇÃO PARA MOVIMENTAR OBSTACULOS PRINCIPAL 
        Esta funçao recebe uma lista de terrenos e obstaculos e devolve a mesma lista com as movimentações devidas
            Caso a lista recebida seja a lista vazia ele devolve a mesma lista vazia
            Caso seja um rio ou uma estrada (l.52 e 53) ele move o obstaculo de acordo com a funçao auxiliar
            Caso seja relva ele devolve a mesma lista, visto que relva nao tem velocidade
-}
moveObs :: [(Terreno,[Obstaculo])] -> [(Terreno,[Obstaculo])]
moveObs [] = []
moveObs ((Rio v, obs):t) = (Rio v, moveObsAux v obs 0):moveObs t
moveObs ((Estrada v,obs):t) = (Estrada v, moveObsAux v obs 0):moveObs t
moveObs ((Relva,obs):t) = (Relva,obs) : moveObs t

{- |
    FUNÇÃO AUXILIAR PARA MOVIMENTAR OBSTACULOS
        Esta função recebe uma velocidade, uma lista de obstaculos e um contador e devolve uma lista de obstaculos com as movimentaçoes implementadas
            Caso a velocidade seja maior que 0 (mov da esq para dir) entao caso o contador seja menos que a velocidade (contador caso a velocidade seja maior que 1)
                Entao ele move o obstaculo ate a velocidade ser igual ao contador
            Caso a velocidade seja menor que 0 o caso é inverso
-}
moveObsAux :: Velocidade -> [Obstaculo] -> Int -> [Obstaculo]
moveObsAux v (h:t) x
    | v > 0 && x < v = moveObsAux v (last t : h : (init t)) (x+1)
    | v < 0 && x > v = moveObsAux v (t ++ [h]) (x-1)
    | otherwise = (h:t)
