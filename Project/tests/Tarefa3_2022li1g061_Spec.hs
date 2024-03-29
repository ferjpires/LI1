module Tarefa3_2022li1g061_Spec where

import LI12223
import Tarefa3_2022li1g061
import Test.HUnit

testsT3 :: Test
testsT3 = TestLabel "Testes Tarefa 3" $ test ["Teste 1" ~: 1 ~=? 1]

testTarefa3 = test  [
    "funcao moveObsAux v1 larg3" ~: [Tronco,Nenhum,Nenhum] ~=? moveObsAux 1 [Nenhum,Nenhum,Tronco] 0,
    "funcao moveObsAux v2 larg3" ~: [Nenhum,Tronco,Nenhum] ~=? moveObsAux 2 [Nenhum,Nenhum,Tronco] 0,
    "funcao moveObsAux v-1 larg3" ~: [Nenhum,Tronco,Nenhum] ~=? moveObsAux (-1) [Nenhum,Nenhum,Tronco] 0,
    "funcao moveObsAux v-2 larg3" ~: [Tronco,Nenhum,Nenhum] ~=? moveObsAux (-2) [Nenhum,Nenhum,Tronco] 0,
    "funcao moveObsAux v1 larg4" ~: [Tronco,Nenhum,Nenhum,Nenhum] ~=? moveObsAux 1 [Nenhum,Nenhum,Nenhum,Tronco] 0,
    "funcao moveObsAux v2 larg4" ~: [Tronco,Nenhum,Nenhum,Tronco] ~=? moveObsAux 2 [Nenhum,Tronco,Tronco,Nenhum] 0,
    "funcao moveObsAux v-1 larg4" ~: [Tronco,Nenhum,Nenhum,Nenhum] ~=? moveObsAux (-1) [Nenhum,Tronco,Nenhum,Nenhum] 0,
    "funcao moveObsAux v-2 larg4" ~: [Tronco,Nenhum,Nenhum,Tronco] ~=? moveObsAux (-2) [Nenhum,Tronco,Tronco,Nenhum] 0,
    "funcao moveObs larg 3" ~: [(Rio 2,[Tronco,Nenhum,Nenhum]),
                                (Estrada 1,[Nenhum,Carro,Carro]),
                                (Relva,[Arvore,Nenhum,Nenhum])]
                            ~=? moveObs [(Rio 2,[Nenhum,Tronco,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])],
    "funcao movePlayer MoveCima s/Arvore"   ~: (Jogo (Jogador (2,1)) (Mapa 3 [(Rio 2,[Nenhum,Tronco,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])]))
                                            ~=? movePlayer (Jogo (Jogador (2,2)) (Mapa 3 [(Rio 2,[Nenhum,Tronco,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])])) (Move Cima),
    "funcao movePlayer MoveCima c/Arvore"   ~: (Jogo (Jogador (2,2)) (Mapa 3 [(Rio 2,[Nenhum,Tronco,Nenhum]),(Relva,[Arvore,Nenhum,Arvore]),(Relva,[Arvore,Nenhum,Nenhum])]))
                                            ~=? movePlayer (Jogo (Jogador (2,2)) (Mapa 3 [(Rio 2,[Nenhum,Tronco,Nenhum]),(Relva,[Arvore,Nenhum,Arvore]),(Relva,[Arvore,Nenhum,Nenhum])])) (Move Cima),
    "funcao movePlayer MoveCima s/Arvore offMap"    ~: (Jogo (Jogador (2,0)) (Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])]))
                                                    ~=? movePlayer (Jogo (Jogador (2,0)) (Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])])) (Move Cima),
    "funcao movePlayer MoveBaixo s/Arvore offMap"   ~: (Jogo (Jogador (1,2)) (Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])]))
                                                    ~=? movePlayer (Jogo (Jogador (1,2)) (Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])])) (Move Baixo),
    "funcao movePlayer MoveEsquerda s/Arvore offMap"    ~: (Jogo (Jogador (0,0)) (Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])]))
                                                        ~=? movePlayer (Jogo (Jogador (0,0)) (Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])])) (Move Esquerda),
    "funcao movePlayer MoveDireita s/Arvore offMap" ~: (Jogo (Jogador (2,0)) (Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])]))
                                                    ~=? movePlayer (Jogo (Jogador (2,0)) (Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])])) (Move Direita),
    "funcao movePlayer MoveBaixo s/Arvore"  ~: (Jogo (Jogador (2,2)) (Mapa 3 [(Rio 2,[Nenhum,Tronco,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])]))
                                            ~=? movePlayer (Jogo (Jogador (2,1)) (Mapa 3 [(Rio 2,[Nenhum,Tronco,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])])) (Move Baixo),
    "funcao movePlayer MoveBaixo c/Arvore"  ~: (Jogo (Jogador (2,1)) (Mapa 3 [(Rio 2,[Nenhum,Tronco,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Arvore])]))
                                            ~=? movePlayer (Jogo (Jogador (2,1)) (Mapa 3 [(Rio 2,[Nenhum,Tronco,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Arvore])])) (Move Baixo),
    "funcao movePlayer MoveEsquerda s/Arvore"   ~: (Jogo (Jogador (1,2)) (Mapa 3 [(Rio 2,[Nenhum,Tronco,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])]))
                                                ~=? movePlayer (Jogo (Jogador (2,2)) (Mapa 3 [(Rio 2,[Nenhum,Tronco,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])])) (Move Esquerda),
    "funcao movePlayer MoveEsquerda c/Arvore"   ~: (Jogo (Jogador (2,2)) (Mapa 3 [(Rio 2,[Nenhum,Tronco,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Relva,[Arvore,Arvore,Nenhum])]))
                                                ~=? movePlayer (Jogo (Jogador (2,2)) (Mapa 3 [(Rio 2,[Nenhum,Tronco,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Relva,[Arvore,Arvore,Nenhum])])) (Move Esquerda),
    "funcao movePlayer MoveDireita s/Arvore"    ~: (Jogo (Jogador (2,2)) (Mapa 3 [(Rio 2,[Nenhum,Tronco,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])]))
                                                ~=? movePlayer (Jogo (Jogador (1,2)) (Mapa 3 [(Rio 2,[Nenhum,Tronco,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])])) (Move Direita),
    "funcao movePlayer MoveDireita c/Arvore"    ~: (Jogo (Jogador (1,2)) (Mapa 3 [(Rio 2,[Nenhum,Tronco,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Arvore])]))
                                                ~=? movePlayer (Jogo (Jogador (1,2)) (Mapa 3 [(Rio 2,[Nenhum,Tronco,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Arvore])])) (Move Direita),
    "funcao movePlayer Parado no tronco"    ~: (Jogo (Jogador (2,0)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])]))
                                            ~=? movePlayer (Jogo (Jogador (1,0)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])])) Parado,
    "funcao movePlayer Parado na relva" ~: (Jogo (Jogador (2,2)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])]))
                                        ~=? movePlayer (Jogo (Jogador (2,2)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])])) Parado,
    "funcao animaJogo Parado na relva"   ~: (Jogo (Jogador (2,2)) (Mapa 3 [(Rio 1,[Nenhum,Nenhum,Tronco]),(Estrada 1,[Nenhum,Carro,Carro]),(Relva,[Arvore,Nenhum,Nenhum])]))
                                                    ~=? animaJogo (Jogo (Jogador (2,2)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])])) Parado,
    "funcao animaJogo Parado no tronco" ~: (Jogo (Jogador (2,0)) (Mapa 3 [(Rio 1,[Nenhum,Nenhum,Tronco]),(Estrada 1,[Nenhum,Carro,Carro]),(Relva,[Arvore,Nenhum,Nenhum])]))
                                        ~=? animaJogo (Jogo (Jogador (1,0)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])])) Parado,
    "funcao animaJogo MoveCima s/Arvore"    ~: (Jogo (Jogador (2,1)) (Mapa 3 [(Rio 1,[Nenhum,Nenhum,Tronco]),(Relva,[Arvore,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])]))
                                            ~=? animaJogo (Jogo (Jogador (2,2)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])])) (Move Cima),
    "funcao animaJogo MoveCima c/Arvore"    ~: (Jogo (Jogador (2,2)) (Mapa 3 [(Rio 1,[Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Arvore]),(Relva,[Arvore,Nenhum,Nenhum])]))
                                            ~=? animaJogo (Jogo (Jogador (2,2)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Arvore]),(Relva,[Arvore,Nenhum,Nenhum])])) (Move Cima),
    "funcao animaJogo MoveBaixo s/Arvore"   ~: (Jogo (Jogador (2,2)) (Mapa 3 [(Rio 1,[Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])]))
                                            ~=? animaJogo (Jogo (Jogador (2,1)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])])) (Move Baixo),
    "funcao animaJogo MoveBaixo c/Arvore"   ~: (Jogo (Jogador (1,2)) (Mapa 3 [(Rio 1,[Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum]),(Relva,[Arvore,Nenhum,Arvore])]))
                                            ~=? animaJogo (Jogo (Jogador (1,2)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum]),(Relva,[Arvore,Nenhum,Arvore])])) (Move Baixo)
                    ]