module Tarefa2_2022li1g061_Spec where

import LI12223
import Tarefa2_2022li1g061
import Test.HUnit

testsT2 :: Test
testsT2 = TestLabel "Testes Tarefa 2" $ test ["Teste 1" ~: 1 ~=? 1]


testProximosTerrenos = test [
    "test 4 rios seguidos com lista maior que 4" ~: [Estrada 0, Relva] ~=? proximosTerrenosValidos (Mapa 12 [(Rio 3,[Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco,Nenhum]),
                                                (Rio (-3),[Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco,Nenhum]),
                                                (Rio 3,[Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco,Nenhum]),
                                                (Rio (-2),[Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco,Nenhum]),
                                                (Estrada(-5),[Carro,Carro,Carro,Nenhum,Nenhum,Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum])]),
    "test 4 rios seguidos com lista igual a 4" ~: [Estrada 0, Relva] ~=? proximosTerrenosValidos (Mapa 12 [(Rio 3,[Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco,Nenhum]),
                                                (Rio (-3),[Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco,Nenhum]),
                                                (Rio 3,[Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco,Nenhum]),
                                                (Rio (-2),[Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco,Nenhum])]),
    "test 3 rios seguidos com lista igual a 4" ~: [Rio 0, Estrada 0, Relva] ~=? proximosTerrenosValidos (Mapa 12 [(Rio 3,[Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco,Nenhum]),
                                                (Rio (-3),[Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco,Nenhum]),
                                                (Rio 3,[Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco,Nenhum]),
                                                (Estrada(-5),[Carro,Carro,Carro,Nenhum,Nenhum,Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum])])
                            ]