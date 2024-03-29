{- |
Module      : Tarefa2_2022li1g061
Description : Geração contínua de um mapa
Copyright   : Fernando Pires <a77399@alunos.uminho.pt>
              Tomás Cunha <a100481@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g061 where

import LI12223
import System.Random 
import Tarefa1_2022li1g061

--rand = randomRIO (0,100) :: IO Int

{- | FUNÇAO QUE ACRESCENTA UMA NOVA LINHA AO MAPA
-}

estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa x ((ter,obs):t)) rand = (Mapa x ((terreno, obstaculo) : (ter,obs):t))
    where   terreno = geraTerreno (Mapa x ((ter,obs):t)) rand
            obstaculo = geraLista x (terreno,[]) 0
            --num2 = length ((ter,obs):t)


{- | FUNÇAO QUE GERA UMA NOVA LISTA DE OBSTACULOS AO MAPA
-}


geraLista :: Int -> (Terreno,[Obstaculo]) -> Int -> [Obstaculo]
geraLista larg (t,obs) x
    | x == 11 = []
    | otherwise = obstacle : geraLista larg (t,obs) (x+1)
        where   obstacle = (proximosObstaculosValidos larg (t,obs)) !! (mod change size)
                change = randList !! x
                size = length (proximosObstaculosValidos larg (t,obs))
                randList = take 11 (randoms (mkStdGen 10))
                --randlist = [10,13..45]


--geraObstaculo :: [Obstaculo] -> Int -> Obstaculo
--geraObstaculo arr rand = arr!!(mod rand (length arr))



{- |
    FUNÇÃO QUE GERA UM NOVO TERRENO
-}

geraTerreno :: Mapa -> Int -> Terreno
geraTerreno mapa num = setSpeed (proximosTerrenosValidos mapa !! mod num (length (proximosTerrenosValidos mapa)))  (getRandomSpeed num [-1,1])

{- | 
    FUNÇÃO QUE DÁ UMA VELOCIDADE ALEATORIA
-}

getRandomSpeed :: Int -> [Int] -> Int
getRandomSpeed num l = l !! mod num (length l)

{- | 
    FUNÇÃO QUE ATRIBUI A VELOCIDADE A UM TERRENO
        Esta função recebe um terreno e um numero inteiro e devolve o terreno com a velocidade correta
-}

setSpeed :: Terreno -> Int -> Terreno
setSpeed (Rio v) x     = (Rio (x))
setSpeed (Estrada v) x = (Estrada (x))
setSpeed terreno _     = terreno


{- |
    FUNÇÃO QUE DETERMINA QUAIS OS TIPOS DE TERRENO VÁLIDOS PARA A NOVA LINHA GERADA
        Esta função recebe o mapa e retorna uma lista com todos os tipos de terreno aceitáveis para a continuação do mapa,
            Caso tenha 4 rios seguidos devolve as outras duas opções
            Caso tenha 5 estradas seguidas devolve as outras duas opções
            Caso tenha 5 relvas seguidas devolve as outras duas opções
            Se não cair em nenhum dos outros casos devolve todas as opções
-}

--verificar a proximos terrenos validos
proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa _ []) = [Rio 0, Estrada 0, Relva]
proximosTerrenosValidos (Mapa _ ((terreno,l):t))
    | isRio terreno && riosSeguidosT2 ((terreno,l):t) 1 = [Estrada 0, Relva] 
    | isEstrada terreno && estradasSeguidasT2 ((terreno,l):t) 1 = [Rio 0, Relva] 
    | terreno == Relva && relvaSeguidaT2 ((terreno,l):t) 1 = [Rio 0, Estrada 0] 
    | otherwise = [Rio 0, Estrada 0, Relva] 

{- |
    FUNÇÃO QUE DETERMINA QUAIS OS OBSTÁCULOS VÁLIDOS PARA A POSIÇÃO SEGUINTE
        Esta função recebe a largura do mapa e um tuplo com o tipo de terreno e a lista de obstáculos contidos na linha
            Caso o comprimento da lista de obstáculos seja igual ou superior à largura, retorna uma lista vazia
            Caso contrário, invocará a sua função auxiliar para dar continuidade ao raciocínio
-}

proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos largura (terreno, obstaculos)
    | (length obstaculos) >= largura = []
    | otherwise = proximosObstaculosValidosAux (terreno,obstaculos)

{- |
    FUNÇÃO QUE ENTREGA AS LISTAS DE OBSTÁCULOS VÁLIDOS
        Esta função recebe a largura e um tuplo com o terreno e a lista de obstáculos
            Se o terreno for Rio e o comprimento da lista de obstáculos for inferior à largura, e, por sua vez, 
            o limite de Troncos seguidos não for atingido, retorna [Nenhum, Tronco], caso esta última condição não se verifique retornará apenas [Nenhum]
            Se o terreno for Estrada e o comprimento da lista de obstáculos for inferior à largura, e, por sua vez, 
            o limite de Carros seguidos não for atingido, retorna [Nenhum, Carro], caso esta última condição não se verifique retornará apenas [Nenhum]
            Se o terreno for Relva e o comprimento da lista de obstáculos for inferior à largura, e, por sua vez,
            o obstáculo "Nenhum" fizer parte da lista de obstáculos, retorna [Nenhum, Arvore], caso esta última condição não se verifique retornará apenas [Nenhum]
-}

proximosObstaculosValidosAux :: (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidosAux (terreno,obstaculos)
    | isRio terreno = if (troncosSeguidosT2 obstaculos 0) then [Nenhum] --caso seja rio verifica se existem 5 troncos seguidos
                                                        else [Nenhum, Tronco] --se existirem dá so a nenhum, se nao existirem dá ambas opçoes
    | isEstrada terreno = if (carrosSeguidosT2 obstaculos 0)  then [Nenhum] --igual ao de cima
                                                            else [Nenhum, Carro]--igual
    | terreno == Relva = if (Nenhum `elem` obstaculos)  then [Nenhum, Arvore]--caso seja relva, verifica se o nenhum existe na lista
                                                        else [Nenhum] --caso exista dá ambas as opçoes, se nao existir so pode dar nenhum, segundo tarefa 1

{- |
    FUNÇÃO QUE VERIFICA SE JÁ FOI ATINGIDO O LIMITE DE TRONCOS SEGUIDOS
        Esta função recebe uma lista de obstáculos e utiliza um contador recursivo
             Se o contador ainda não tiver atingido 5 e o último obstáculo da lista for um Tronco, é incrementado e a função recomeça, se esta última condição não se verificar retorna False
             Se o contador chegar a 5, a função retorna True
             Caso a lista termine (significando que é menor ou igual a 5), se o contador tiver atingido 5, retorna True, caso não tenha atingido retorna False
-}

troncosSeguidosT2 :: [Obstaculo] -> Int -> Bool
troncosSeguidosT2 [] i
    | i < 5 = False
    | i == 5 = True
troncosSeguidosT2 obstaculos i
    | i < 5 = if ((last obstaculos) == Tronco)  then troncosSeguidosT2 (init obstaculos) (i+1)
                                                else False
    | i == 5 = True


{- |
    FUNÇÃO QUE VERIFICA SE JÁ FOI ATINGIDO O LIMITE DE CARROS SEGUIDOS
        Esta função recebe uma lista de obstáculos e utiliza um contador recursivo
             Se o contador ainda não tiver atingido 3 e o último obstáculo da lista for um Carro, é incrementado e a função recomeça, se esta última condição não se verificar retorna False
             Se o contador chegar a 3, a função retorna True
             Caso a lista termine (significando que é menor ou igual a 3), se o contador tiver atingido 3, retorna True, caso não tenha atingido retorna False
-}

carrosSeguidosT2 :: [Obstaculo] -> Int -> Bool
carrosSeguidosT2 [] i
    | i < 3 = False
    | i == 3 = True
carrosSeguidosT2 obstaculos i
    | i < 3 = if ((last obstaculos) == Carro)   then carrosSeguidosT2 (init obstaculos) (i+1)
                                                else False
    | i == 3 = True



{- |
    FUNÇÃO QUE VERIFICA SE JÁ FOI ATINGIDO O LIMITE DE RIOS SEGUIDOS
        Esta função recebe uma lista de tuplos com o terreno e uma lista de obstáculos e utiliza um contador recursivo
             Se o contador ainda não tiver atingido 4 e o terreno seguinte for Rio, é incrementado e a função recomeça, se esta última condição não se verificar retorna False
             Se o contador chegar a 4, a função retorna True
             Caso a lista termine (significando que é menor ou igual a 4), se o contador tiver atingido 4, retorna True, caso não tenha atingido retorna False
-}

riosSeguidosT2 :: [(Terreno,[Obstaculo])] -> Int -> Bool
riosSeguidosT2 ((terreno1,l1):(terreno2,l2):t) i
    | i < 4 = if (isRio terreno2)   then riosSeguidosT2 ((terreno2,l2):t) (i+1)
                                    else False 
    | i == 4 = True 
riosSeguidosT2 (_:[]) i 
    | i < 4 = False
    | i == 4 = True

{- |
    FUNÇÃO QUE VERIFICA SE JÁ FOI ATINGIDO O LIMITE DE ESTRADAS SEGUIDAS
        Esta função recebe uma lista de tuplos com o terreno e uma lista de obstáculos e utiliza um contador recursivo
            Se o contador ainda não tiver atingido 5 e o terreno seguinte for Estrada, é incrementado e a função recomeça, se esta última condição não se verificar retorna False
            Se o contador chegar a 5, a função retorna True
            Caso a lista termine (significando que é menor ou igual a 5), se o contador tiver atingido 5, retorna True, caso não tenha atingido retorna False
-} 

estradasSeguidasT2 :: [(Terreno,[Obstaculo])] -> Int -> Bool
estradasSeguidasT2 ((terreno1,l1):(terreno2,l2):t) i
    | i < 5 = if (isEstrada terreno2)   then estradasSeguidasT2 ((terreno2,l2):t) (i+1)
                                        else False                
    | i == 5 = True
estradasSeguidasT2 (_:[]) i
    | i < 4 = False
    | i == 5 = True

{- |
    FUNÇÃO QUE VERIFICA SE JÁ FOI ATINGIDO O LIMITE DE RELVAS SEGUIDAS
        Esta função recebe uma lista de tuplos com o terreno e uma lista de obstáculos e utiliza um contador recursivo
            Se o contador ainda não tiver atingido 5 e os terrenos subsequentes forem ambos Relva, é incrementado e a função recomeça, se esta última condição não se verificar retorna False
            Se o contador chegar a 4, a função retorna True
            Caso a lista termine (significando que é menor ou igual a 4), se o contador tiver atingido 4, retorna True, caso não tenha atingido retorna False
-} 

relvaSeguidaT2 :: [(Terreno,[Obstaculo])] -> Int -> Bool
relvaSeguidaT2 ((terreno1,l1):(terreno2,l2):t) i
    | i < 5 = if (terreno1 == terreno2) then relvaSeguidaT2 ((terreno2,l2):t) (i+1)
                                        else False                
    | i == 4 = True
relvaSeguidaT2 (_:[]) i
    | i < 4 = False
    | i == 5 = True
