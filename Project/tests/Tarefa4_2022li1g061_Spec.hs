module Tarefa4_2022li1g061_Spec where

import LI12223
import Tarefa4_2022li1g061
import Test.HUnit

testsT4 :: Test
testsT4 = TestLabel "Testes Tarefa 4" $ test ["Teste 1" ~: 1 ~=? 1]

testTarefa4 = test  [
    "funcao busca" ~: (Estrada 3, Carro) ~=? busca (Jogo (Jogador (1,1)) (Mapa 3 [(Rio 2,[Tronco,Nenhum,Nenhum]),(Estrada 3,[Nenhum,Carro,Carro]),(Relva,[Arvore,Nenhum,Nenhum])])),
    "funcao selectT" ~: (Estrada 3,[Nenhum,Carro,Carro]) ~=? selectT [(Rio 2,[Tronco,Nenhum,Nenhum]),(Estrada 3,[Nenhum,Carro,Carro]),(Relva,[Arvore,Nenhum,Nenhum])] 1,
    "funcao afogado true" ~: True ~=? afogado (Jogo (Jogador (1,0)) (Mapa 3 [(Rio 2,[Tronco,Nenhum,Nenhum]),(Estrada 3,[Nenhum,Carro,Carro]),(Relva,[Arvore,Nenhum,Nenhum])])),
    "funcao afogado false" ~: False ~=? afogado (Jogo (Jogador (0,0)) (Mapa 3 [(Rio 2,[Tronco,Nenhum,Nenhum]),(Estrada 3,[Nenhum,Carro,Carro]),(Relva,[Arvore,Nenhum,Nenhum])])),
    "funcao atropelado true" ~: True ~=? atropelado (Jogo (Jogador (1,1)) (Mapa 3 [(Rio 2,[Tronco,Nenhum,Nenhum]),(Estrada 3,[Nenhum,Carro,Carro]),(Relva,[Arvore,Nenhum,Nenhum])])),
    "funcao atropelado false" ~: False ~=? atropelado (Jogo (Jogador (0,1)) (Mapa 3 [(Rio 2,[Tronco,Nenhum,Nenhum]),(Estrada 3,[Nenhum,Carro,Carro]),(Relva,[Arvore,Nenhum,Nenhum])])),
    "funcao dead true" ~: True ~=? dead (Jogo (Jogador (1,1)) (Mapa 3 [(Rio 2,[Tronco,Nenhum,Nenhum]),(Estrada 3,[Nenhum,Carro,Carro]),(Relva,[Arvore,Nenhum,Nenhum])])),
    "funcao dead falso" ~: False ~=? dead (Jogo (Jogador (1,2)) (Mapa 3 [(Rio 2,[Tronco,Nenhum,Nenhum]),(Estrada 3,[Nenhum,Carro,Carro]),(Relva,[Arvore,Nenhum,Nenhum])])),
    "funcao offmap true no x pela direita" ~: True ~=? offMap (Jogo (Jogador (3,1)) (Mapa 3 [(Rio 2,[Tronco,Nenhum,Nenhum]),(Estrada 3,[Nenhum,Carro,Carro]),(Relva,[Arvore,Nenhum,Nenhum])])),
    "funcao offmap true no y por baixo" ~: True ~=? offMap (Jogo (Jogador (1,3)) (Mapa 3 [(Rio 2,[Tronco,Nenhum,Nenhum]),(Estrada 3,[Nenhum,Carro,Carro]),(Relva,[Arvore,Nenhum,Nenhum])])),
    "funcao offmap true no x pela esquerda" ~: True ~=? offMap (Jogo (Jogador (-1,1)) (Mapa 3 [(Rio 2,[Tronco,Nenhum,Nenhum]),(Estrada 3,[Nenhum,Carro,Carro]),(Relva,[Arvore,Nenhum,Nenhum])])),
    "funcao offmap true no y por cima" ~: True ~=? offMap (Jogo (Jogador (1,-1)) (Mapa 3 [(Rio 2,[Tronco,Nenhum,Nenhum]),(Estrada 3,[Nenhum,Carro,Carro]),(Relva,[Arvore,Nenhum,Nenhum])])),
    "funcao offmap false" ~: False ~=? offMap (Jogo (Jogador (1,0)) (Mapa 3 [(Rio 2,[Tronco,Nenhum,Nenhum]),(Estrada 3,[Nenhum,Carro,Carro]),(Relva,[Arvore,Nenhum,Nenhum])])),
    "funcao gameover true na dead" ~: True ~=? gameOver (Jogo (Jogador (1,1)) (Mapa 3 [(Rio 2,[Tronco,Nenhum,Nenhum]),(Estrada 3,[Nenhum,Carro,Carro]),(Relva,[Arvore,Nenhum,Nenhum])])),
    "funcao gameover true na offmap no y" ~: True ~=? gameOver (Jogo (Jogador (3,1)) (Mapa 3 [(Rio 2,[Tronco,Nenhum,Nenhum]),(Estrada 3,[Nenhum,Carro,Carro]),(Relva,[Arvore,Nenhum,Nenhum])])),
    "funcao gameover true na offmap no x" ~: True ~=? gameOver (Jogo (Jogador (1,3)) (Mapa 3 [(Rio 2,[Tronco,Nenhum,Nenhum]),(Estrada 3,[Nenhum,Carro,Carro]),(Relva,[Arvore,Nenhum,Nenhum])])),
    "funcao gameover false" ~: False ~=? gameOver (Jogo (Jogador (0,1)) (Mapa 3 [(Rio 2,[Tronco,Nenhum,Nenhum]),(Estrada 3,[Nenhum,Carro,Carro]),(Relva,[Arvore,Nenhum,Nenhum])]))
                    ]