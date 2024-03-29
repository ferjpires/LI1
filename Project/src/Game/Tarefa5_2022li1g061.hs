{- |
Module      : Tarefa5_2022li1g061
Description : Fazer deslizar o jogo
Copyright   : Fernando Pires <a77399@alunos.uminho.pt>
              Tomás Cunha <a100481@alunos.uminho.pt>

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2022/23.
-}
module Tarefa5_2022li1g061 where

import LI12223
import Tarefa2_2022li1g061

{- |
    FUNÇÃO QUE ESTENDE O JOGO DADO
-}

increaseGame :: Int -> Jogo -> Jogo
increaseGame random (Jogo jogador mapa) = (Jogo jogador $ estendeMapa mapa random)

{- | 
    FUNÇÃO QUE ACERTA AS COORDENADAS DO JOGADOR
-}

changePosition :: Jogo -> Jogo
changePosition (Jogo (Jogador (x,y)) mapa) = (Jogo (Jogador (x,y+1)) mapa)

{- |
    FUNÇÃO QUE APAGA A ULTIMA LINHA DO MAPA
-}

eraseLastLine :: Jogo -> Jogo
eraseLastLine (Jogo jogador (Mapa x map)) = (Jogo jogador (Mapa x $ init map))

{- |
    FUNÇÃO PRINCIPAL QUE FAZ DESLIZAR O JOGO
-}

deslizaJogo :: Int -> Jogo -> Jogo
deslizaJogo random jogo = eraseLastLine $ changePosition $ increaseGame random jogo