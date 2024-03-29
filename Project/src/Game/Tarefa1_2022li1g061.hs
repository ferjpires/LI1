{- |
Module      : Tarefa1_2022li1g061
Description : Validação de um mapa
Copyright   : Fernando Pires <a77399@alunos.uminho.pt>
              Tomás Cunha <a100481@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g061 where

import LI12223

{- |
    FUNÇÃO QUE DETERMINA SE O MAPA É VÁLIDO
        Esta função recebe um Mapa e retorna um Bool
            Se todas as funções auxiliares confirmarem válidos os seus parâmetros, a função retornará True
            Caso contrário retornará False
-}

mapaValido :: Mapa -> Bool --falta juntar todas as funçoes auxiliares
mapaValido (Mapa x ((terreno1,l1):(terreno2,l2):t))
    | validMapa map && checkRio map && checkTronco map && checkCarro map && compLista map && linhaValida map && terrenosSeguidos map = True
    | otherwise = False
    where map = (Mapa x ((terreno1,l1):(terreno2,l2):t)) --para diminuir codigo

{- |
    FUNÇÃO QUE VERIFICA SE O TERRENO E OS OBSTÁCULOS COMBINAM
        Esta função recebe um Mapa e retorna um Bool
            O resultado será determinado por recurso à função auxiliar "validMap"
-}

validMapa :: Mapa -> Bool
validMapa (Mapa x []) = True
validMapa (Mapa x ((terreno,l):t)) 
    | validMap terreno l = validMapa (Mapa x t)
    | otherwise = False

{- |
    FUNÇÃO QUE INVOCA AS AUXILIARES NECESSÁRIAS TENDO EM CONTA O TERRENO
        Esta função recebe um Terreno e uma lista de Obstáculos e retorna um Bool
            Se a lista for vazia, significa que chegou ao fim e retorna True
            Se o terreno for relva, invoca a função "validRelva"
            Se o terreno for rio, invoca a função "validRio"
            Se o terreno for estrada, invoca a função "validEstrada"
-}

validMap :: Terreno -> [Obstaculo] -> Bool 
validMap _ [] = True
validMap terreno l
    | terreno == Relva = validRelva l 
    | isRio terreno = validRio l 
    | isEstrada terreno = validEstrada l

{- |
    FUNÇÃO QUE VERIFICA SE OS OBSTÁCULOS SÃO VÁLIDOS PARA O TERRENO RELVA
        Esta função recebe uma lista de obstáculos e retorna um Bool
            Se a lista for vazia, significa que chegou ao fim e retorna True
            Se o obstáculo actual for "Nenhum" ou "Árvore" a função avança
            Caso Contrário retorna False
-}

validRelva :: [Obstaculo] -> Bool 
validRelva [] = True 
validRelva (h:t)
    | h == Nenhum || h == Arvore = validRelva t 
    | otherwise = False 

{- |
    FUNÇÃO QUE VERIFICA SE OS OBSTÁCULOS SÃO VÁLIDOS PARA O TERRENO RIO
        Esta função recebe uma lista de obstáculos e retorna um Bool
            Se a lista for vazia, significa que chegou ao fim e retorna True
            Se o obstáculo actual for "Nenhum" ou "Tronco" a função avança
            Caso Contrário retorna False
-}

validRio :: [Obstaculo] -> Bool
validRio [] = True
validRio (h:t)
    | h == Nenhum || h == Tronco = validRio t
    | otherwise = False

{- |
    FUNÇÃO QUE VERIFICA SE OS OBSTÁCULOS SÃO VÁLIDOS PARA O TERRENO ESTRADA
        Esta função recebe uma lista de obstáculos e retorna um Bool
            Se a lista for vazia, significa que chegou ao fim e retorna True
            Se o obstáculo actual for "Nenhum" ou "Carro" a função avança
            Caso Contrário retorna False
-}

validEstrada :: [Obstaculo] -> Bool
validEstrada [] = True 
validEstrada (h:t)
    | h == Nenhum || h == Carro = validEstrada t 
    | otherwise = False

{- |
    FUNÇÃO QUE VERIFICA SE EXISTEM RIOS CONTÍGUOS COM A MESMA DIRECÇÃO
        Esta função recebe um Mapa e retorna um Bool
            Se a lista for vazia, significa que chegou ao fim e retorna True
            Se ambos os terrenos contíguos são Rio e as suas direcções são opostas, retorna True, caso esta última condição não se verifique retorna False
            Se os terrenos contíguos não forem ambos Rio, a função avança para o resto do mapa
-}

checkRio :: Mapa -> Bool
checkRio (Mapa _ (_:[])) = True
checkRio (Mapa l ((terreno1,l1):(terreno2,l2):t))
    | isRio terreno1 && isRio terreno2 = if checkVelocidade terreno1 terreno2 == True 
                                                    then checkRio (Mapa l ((terreno2,l2):t)) 
                                                    else False
    | otherwise = checkRio (Mapa l ((terreno2,l2):t))

{- |
    FUNÇÃO QUE VERIFICA SE 2 RIOS TÊM DIRECÇÕES OPOSTAS
        Esta função recebe dois terrenos e retorna um Bool
            Se a velocidade do primeiro for positiva e a do segundo também, retorna Falso, caso a do segundo seja negativa retorna True
            Se a velocidade do primeiro for negativa e a do segundo também, retorna Falso, caso a do segundo seja positiva retorna True
-}

checkVelocidade :: Terreno -> Terreno -> Bool
checkVelocidade (Rio x) (Rio y) 
    | x > 0 = if (y > 0) then False else True 
    | x < 0 = if (y < 0) then False else True 

{- |
    FUNÇÃO QUE VERIFICA SE UM DADO TERRENO É UM RIO
        Esta função recebe um terreno e retorna um Bool
            Se o terreno for Rio retorna True
            Caso contrário retorna False
-}

isRio :: Terreno -> Bool 
isRio (Rio _) = True 
isRio (Estrada _) = False 
isRio Relva = False

{- |
    FUNÇÃO QUE VERIFICA SE UM TRONCO TEM O COMPRIMENTO VÁLIDO
        Esta função recebe uma lista de obstáculos e utiliza um contador recursivo
            Se a lista for vazia, significa que chegou ao fim e retorna True
            Se o contador for menor ou igual que 5 e o obstáculo for um tronco, 
            a função avança e o contador é incrementado caso a cabeça seja um tronco
            Caso contrário retorna False
-}

validTronco :: [Obstaculo] -> Int -> Bool
validTronco [] _ = True 
validTronco (h:t) i
    | i <= 5 = if (h==Tronco)   then validTronco t (i+1) 
                                else validTronco t 0 
    | otherwise = False

{- |
    FUNÇAO QUE VERIFICA SE EXISTEM TRONCOS SEGUIDOS EM LADOS OPOSTOS DA LISTA
        Esta funçao recebe a lista de obstaculos e verifica se existemtroncos seguidos em lados opostos da lista, coisa que a validTronco nao faz
            Lista vazia vai retornar True pois é valido
            Contador menor que 4 continua a verificar pois pode ser valido
            Contador igual a 4 ele ja altera pois nao podem existir 6. Entao caso exista nos 2 lados o Rio é invalido.
-}
troncosValidos :: [Obstaculo] -> Int -> Bool
troncosValidos [] _ = True
troncosValidos (h:t) i
    | i < 4 = if (h == Tronco && (last t) == Tronco)    then troncosValidos (init t) (i+2)
                                                        else True
    | i == 4 = if (h == Tronco && (last t) == Tronco)   then False
                                                        else (validTronco (h:t) i && validTail (h:t) i)
{- | FUNÇAO QUE VERIFICA SE EXISTEM TRONCOS SEGUIDOS NA CAUDA
    Verifica se ha troncos seguidos na causa caso eles nao sejam iguais na funçao troncosValidos
-}
validTail :: [Obstaculo] -> Int -> Bool
validTail [] _ = True
validTail l i
    | i <= 5 = if ((last l) == Tronco)  then validTail (init l) (i+1)
                                        else True
    | otherwise = False

{- |
    FUNÇÃO QUE VERIFICA SE OS TRONCOS DO MAPA SÃO VÁLIDOS
        Esta função recebe um Mapa e retorna um Bool
            Se a lista for vazia, significa que chegou ao fim e retorna True
            Se o terreno for Rio e o comprimento do tronco for válido a função avança, caso esta última condição não se verifique retorna False
            Caso contrário a função avança
-}

checkTronco :: Mapa -> Bool
checkTronco (Mapa x []) = True 
checkTronco (Mapa x ((terreno,l):t)) 
    | isRio terreno = if (validTronco l 0 && existeTronco l && troncosValidos l 0)  then checkTronco (Mapa x t) 
                                                                                            else False 
    | otherwise = checkTronco (Mapa x t) 

{- |
    FUNÇAO QUE VERIFICA SE EXISTE TRONCO NO RIO
        Para o Rio ser ultrapassavel deve existir pelo menos um tronco nele.
        Esta função verifica se o elemento 'Tronco' é elemento da lista de obstaculos no Rio.
-}

existeTronco :: [Obstaculo] -> Bool
existeTronco l = Tronco `elem` l

{- |
    FUNÇÃO QUE VERIFICA SE UM DADO TERRENO É UMA ESTRADA
        Esta função recebe um terreno e retorna um Bool
            Se o terreno for de facto Estrada, retorna True
            Caso contrário retorna False
-}

isEstrada :: Terreno -> Bool
isEstrada (Estrada _) = True 
isEstrada (Rio _) = False
isEstrada Relva = False

{- |
    FUNÇÃO QUE VERIFICA SE UM CARRO TEM O COMPRIMENTO VÁLIDO
        Esta função recebe uma lista de obstáculos e utiliza um contador recursivo
            Se a lista for vazia, significa que chegou ao fim e retorna True
            Se o contador for menor ou igual que 3 e o obstáculo for um Carro, 
            a função avança e o contador é incrementado caso a cabeça seja um Carro
            Caso contrário retorna False
-}
    
validCarro :: [Obstaculo] -> Int -> Bool
validCarro [] _ = True 
validCarro (h:t) i
    | i <= 3 = if (h==Carro)    then validCarro t (i+1) 
                                else validCarro t 0 
    | otherwise = False
{- |
    FUNÇAO QUE VERIFICA SE EXISTEM CARROS SEGUIDOS EM LADOS OPOSTOS DA LISTA
        Esta funçao recebe a lista de obstaculos e verifica se existem carros seguidos em lados opostos da lista, coisa que a validCarro nao faz
            Lista vazia vai retornar True pois é valido
            Contador menor que 2 continua a verificar pois pode ser valido
            Contador igual a 2 ele ja altera pois nao podem existir 4. Entao caso exista nos 2 lados a Estrada é invalida.
-}
carrosValidos :: [Obstaculo] -> Int -> Bool
carrosValidos [] _ = True
carrosValidos (h:t) i
    | i < 2 = if (h == Carro && (last t) == Carro)  then carrosValidos (init t) (i+2)
                                                    else True
    | i == 2 = if (h == Tronco && (last t) == Carro)    then False
                                                        else (validCarro (h:t) i && validTailC (h:t) i)
{- | FUNÇAO QUE VERIFICA SE EXISTEM CARROS SEGUIDOS NA CAUDA
    Verifica se ha carros seguidos na causa caso eles nao sejam iguais na funçao carrosValidos
-}
validTailC :: [Obstaculo] -> Int -> Bool
validTailC [] _ = True
validTailC l i
    | i <= 3 = if ((last l) == Carro)  then validTailC (init l) (i+1)
                                        else True
    | otherwise = False
{- |
    FUNÇÃO QUE VERIFICA SE OS CARROS DO MAPA SÃO VÁLIDOS
        Esta função recebe um Mapa e retorna um Bool
            Se a lista for vazia, significa que chegou ao fim e retorna True
            Se o terreno for Estrada e o comprimento do Carro for válido a função avança, caso esta última condição não se verifique retorna False
            Caso contrário a função avança
-}

checkCarro :: Mapa -> Bool
checkCarro (Mapa x []) = True 
checkCarro (Mapa x ((terreno,l):t))
    | isEstrada terreno = if (validCarro l 0 && carrosValidos l 0)  then checkCarro (Mapa x t) 
                                                                    else False 
    | otherwise = checkCarro (Mapa x t) 

{- |
    FUNÇÃO QUE VERIFICA SE TODAS AS LINHAS DO MAPA POSSUEM O ELEMENTO "NENHUM"
        Esta função recebe um Mapa e retorna um Bool
            Se a lista for vazia, significa que chegou ao fim e retorna True
            Se a linha actual possuir o elemento "Nenhum" a função avança
            Caso contrário retorna False
-}

linhaValida :: Mapa -> Bool
linhaValida (Mapa x []) = True
linhaValida (Mapa x ((_,l):t))
    | validLine l = linhaValida (Mapa x t)
    | otherwise = False

{- |
    FUNÇÃO QUE VERIFICA SE "NENHUM" É UM ELEMENTO DE UMA DETERMINADA LINHA
        Esta função recebe uma lista de obstáculos e retorna um Bool
            Se "Nenhum" pertencer à linha retorna True
            Caso contrário retorna False
-}

validLine :: [Obstaculo] -> Bool
validLine l = Nenhum `elem` l 


{- |
    FUNÇÃO QUE VERIFICA SE O COMPRIMENTO DA LISTA DE OBSTÁCULOS É IGUAL À LARGURA
        Esta função recebe um Mapa e retorna um Bool
            Se a lista for vazia significa que chegou ao fim e retorna True
            Se a largura for igual ao comprimento da lista, a função avança para o resto do Mapa
            Caso contrário retorna False
-}

compLista :: Mapa -> Bool
compLista (Mapa x []) = True
compLista (Mapa x ((terreno,l):t))
    | x == (length l) = compLista (Mapa x t) 
    | otherwise = False 

{- |
    FUNÇÃO QUE DETERMINA SE EXISTEM DEMASIADOS TERRENOS IGUAIS CONTÍGUOS
        Esta função recebe um Mapa e retorna um Bool
            Se a lista for vazia significa que chegou ao fim e retorna True
            Se o terreno for Rio, irá verificar se o número de Rios contíguos não excede o limite, caso não exceda a função avança para o resto do Mapa
            Se o terreno for Estrada, irá verificar se o número de Estradas contíguos não excede o limite, caso não exceda a função avança para o resto do Mapa
            Se o terreno for Relva, irá verificar se o número de Relvas contíguos não excede o limite, caso não exceda a função avança para o resto do Mapa
-}

terrenosSeguidos :: Mapa -> Bool
terrenosSeguidos (Mapa x []) = True
terrenosSeguidos (Mapa x ((terreno,l):t))
    | isRio terreno =       if (riosSeguidos ((terreno,l):t) 1 == True) then terrenosSeguidos (Mapa x t) 
                                                                        else False
    | isEstrada terreno =   if (estradasSeguidas ((terreno,l):t) 1 == True) then terrenosSeguidos (Mapa x t)
                                                                            else False
    | terreno == Relva =    if (relvaSeguida ((terreno,l):t) 1 == True) then terrenosSeguidos (Mapa x t)
                                                                        else False

{- |
    FUNÇÃO QUE VERIFICA SE NÃO EXISTEM DEMASIADOS RIOS CONTÍGUOS
        Esta função recebe uma lista de tuplos terreno e lista de obstáculos, utiliza um contador recursivo e retorna um Bool
            Se a lista for vazia significa que chegou ao fim e retorna True
            Se o contador for inferior a 4 e os terrenos forem iguais, a função avança, caso esta última condição não se verique retorna True
            Caso contrário retorna False
-}

riosSeguidos :: [(Terreno,[Obstaculo])] -> Int -> Bool
riosSeguidos [] _ = True
riosSeguidos (_:[]) _ = True
riosSeguidos ((terreno1,l1):(terreno2,l2):t) i
    | i < 4 = if (terreno1 == terreno2) then riosSeguidos ((terreno2,l2):t) (i+1)
                                        else True
    | otherwise = False

{- |
    FUNÇÃO QUE VERIFICA SE NÃO EXISTEM DEMASIADAS ESTRADAS CONTÍGUAS
        Esta função recebe uma lista de tuplos terreno e lista de obstáculos, utiliza um contador recursivo e retorna um Bool
            Se a lista for vazia significa que chegou ao fim e retorna True
            Se o contador for inferior a 5 e os terrenos forem iguais, a função avança, caso esta última condição não se verique retorna True
            Caso contrário retorna False
-}

estradasSeguidas :: [(Terreno,[Obstaculo])] -> Int -> Bool
estradasSeguidas [] _ = True
estradasSeguidas ((terreno1,l1):(terreno2,l2):t) i
    | i < 5 = if (terreno1 == terreno2) then estradasSeguidas ((terreno2,l2):t) (i+1)
                                        else True
    | otherwise = False

{- |
    FUNÇÃO QUE VERIFICA SE NÃO EXISTEM DEMASIADAS RELVAS CONTÍGUAS
        Esta função recebe uma lista de tuplos de Terreno e lista de Obstáculos, utiliza um contador recursivo e retorna um Bool
            Se a lista for vazia significa que chegou ao fim e retorna True
            Se o contador for inferior a 5 e os terrenos forem iguais, a função avança, caso esta última condição não se verique retorna True
            Caso contrário retorna False
-}

relvaSeguida :: [(Terreno,[Obstaculo])] -> Int -> Bool
relvaSeguida [] _ = True
relvaSeguida ((terreno1,l1):(terreno2,l2):t) i
    | i < 5 = if (terreno1 == terreno2) then relvaSeguida ((terreno2,l2):t) (i+1)
                                        else True
    | otherwise = False