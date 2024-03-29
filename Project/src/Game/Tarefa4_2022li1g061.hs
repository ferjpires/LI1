{- |
Module      : Tarefa4_2022li1g061
Description : Determinar se o jogo terminou
Copyright   : Fernando Pires <a77399@alunos.uminho.pt>
              Tomás Cunha <a100481@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}
module Tarefa4_2022li1g061 where

import LI12223
import Tarefa1_2022li1g061


{- |
        FUNÇÃO QUE INDICA QUE O JOGO ACABOU
                Esta função verifica todas as auxiliares e, caso algumas deles retorne true, então o jogador está morto ou fora do mapa, o que indica que o jogo acaba
-}
gameOver :: Jogo -> Bool --funçao principal, decide se jogo acabou
gameOver jogo
        | offMap jogo = True
        | dead jogo = True
        | otherwise = False
{- |
        FUNÇÃO QUE INDICA SE O JOGADOR ESTÁ FORA DO MAPA
                Esta função recebe o jogo e, caso a posição do jogador seja fora do mapa, retorna True indicando que o jogador está fora.
-}
offMap :: Jogo -> Bool --verifica se o jogador esta fora do mapa
offMap (Jogo (Jogador (x,y)) (Mapa larg map))
        | x < 0 = True --fora do lado esquerdo
        | x > (larg - 1) = True --fora do lado direito
        | y < 0 = True --fora por cima
        | y >= length map = True --fora por baixo, posiçao maior que comp do mapa
        | otherwise = False
{- |
        FUNÇAO QUE VERIFICA SE O JOGADOR ESTÁ MORTO
                Esta função, pega nas funções atropelado e afogado e, caso alguma retorne que o jogador está morto, então esta função indica que ele está morto.
-}
dead :: Jogo -> Bool --verifica se o jogador está morto
dead jogo --caso alguma das funçoes auxiliares de atropelado ou afogado retorne verdadeiro ele esta morto
        | atropelado jogo || afogado jogo = True
        | otherwise = False
{- |
        FUNÇÃO QUE VERIFICA SE O JOGADOR ESTÁ NUMA ESTRADA COM CARRO
                Esta função, como o nome indica, verifica se o jogador foi atropelado. Ou seja, a posição onde ele está é uma estrada e um carro.
                        Retorna true caso ele esteja morto.
-}
atropelado :: Jogo -> Bool --verifica se foi atropelado
atropelado jogo
        | (isEstrada (fst(busca jogo)) && snd(busca jogo) == Carro) = True
        | otherwise = False 
        --se o primeiro elemento do par for estrada e o segundo for carro

{- |
        FUNÇÃO QUE VERIFICA SE O JOGADOR ESTÁ NUM RIO SEM TRONCO
                Esta função, como o nome indica, verifica se o jogador está afogado. Ou seja, a posição onde ele está é um rio e não tem tronco.
                        Retorna true caso ele esteja morto.
-}
afogado :: Jogo -> Bool --verifica se está afogado
afogado jogo
        | (isRio (fst(busca jogo)) && snd(busca jogo) == Nenhum) = True
        | otherwise = False
        --se o primeiro elemento do par for rio e o segundo for Nenhum
{- | 
        FUNÇÃO QUE DEVOLVE A LINHA DO MAPA ONDE O JOGADOR SE ENCONTRA
                Esta função recebe o mapa inteiro (exceto a largura) e a linha em que o jogador se encontra e devolve
                        o par em que o jogador se encontra, ou seja a linha da matriz onde ele está
-}
selectT :: [(Terreno,[Obstaculo])] -> Int -> (Terreno, [Obstaculo])
selectT mapa y = (reverte mapa) !! y --devolve o par Terreno e lista de Obstaculos daquela linha
{- | 
        FUNÇÃO QUE PROCURA A POSIÇÃO DO JOGADOR
                Esta função, com auxilio da função acima, devolve o par terreno obstaculo daquela posição da matriz
                        Ou seja, diz-nos com precisão em que terreno o jogador está
-}
busca :: Jogo -> (Terreno,Obstaculo)
busca (Jogo (Jogador (x,y)) (Mapa larg lista)) = (ter,obs) --devolve o par terreno obstaculo daquela posiçao na matriz
        where   ter = fst (selectT lista y)
                obs = snd (selectT lista y) !! x

reverte :: [a] -> [a]
reverte [] = []
reverte (h:t) = reverte t ++ [h]