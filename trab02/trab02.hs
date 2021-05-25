-- Aluno
-- Carlos Henrique Silva de Oliveira Bueno
-- 11911BCC005

l1=[1..2000]
l2=[2000,1999..1]
l3=l1++[0]
l4=[0]++l2
l5=l1++[0]++l2
l6=l2++[0]++l1
l7=l2++[0]++l2
x1=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
x2=[20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
x3=[11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
x4=[10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
x5=[11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
x6=[1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
x7 = [20,8,2,11,13,3,7,18,14,4,16,10,15,1,9,17,19,12,5,6]

addHeaderToTuple :: a -> ([a], b) -> ([a], b)
addHeaderToTuple newHeader (lst, n) = (newHeader:lst, n)

-- bubbleSort original + contador de trocas
-- Resultado: ([Vetor Ordenado], Número de Trocas)

bubbleSwap :: Ord a => ([a], Int) -> ([a], Int)
bubbleSwap ([x], swapCount) = ([x], swapCount)
bubbleSwap ((h1:h2:tail), swapCount) | h1 > h2 = addHeaderToTuple h2 (bubbleSwap ((h1:tail), swapCount+1))
                               | otherwise =  addHeaderToTuple h1 (bubbleSwap ((h2:tail), swapCount))
--    where swapAux head (list, count) = (head:list, count)

bubbleAux :: Ord a => ([a], Int) -> Int -> ([a], Int)
bubbleAux x 0 = x
bubbleAux (list, swapCount) n = bubbleAux (bubbleSwap (list, swapCount)) (n - 1)

bubble :: Ord a => [a] -> ([a], Int)
bubble [] = ([], 0)
bubble list = bubbleAux (list, 0) (length list)

-- Variação 1
bubble1 :: Ord a => [a] -> ([a], Int)
bubble1 [] = ([], 0) 
bubble1 list = format (bubbleAux1 (list, 0, -1) (length list))
    where format (list, swapCounter, _) = (list, swapCounter)

bubbleAux1 :: Ord a => ([a], Int, Int) -> Int -> ([a], Int, Int)
bubbleAux1 x 0 = x
bubbleAux1 (list, swapCount, swapFlag) n | swapFlag == 0 = (list, swapCount, swapFlag)
                                         | otherwise = bubbleAux1 (bubbleSwap1 (list, swapCount, 0)) (n-1)

bubbleSwap1 :: Ord a => ([a], Int, Int) -> ([a], Int, Int)
bubbleSwap1 ([x], swapCount, swapFlag) = ([x], swapCount, swapFlag)
bubbleSwap1 ((h1:h2:tail), swapCount, swapFlag) | h1 > h2 = swapAux h2 (bubbleSwap1 ((h1:tail), swapCount+1, 1))
                                                | otherwise =  swapAux h1 (bubbleSwap1 ((h2:tail), swapCount, swapFlag))
    where swapAux head (list, count, flag) = (head:list, count, flag)

-- Variação 2
bubble2 :: Ord a => [a] -> ([a], Int)
bubble2 [] = ([], 0)
bubble2 lst = format (bubbleSort2 (lst, 0, -1))
    where format (l,c,_) = (l,c)

-- se tiver só um elemento, não tem o que ordenar. Retorna o que foi passado
-- recursivamente vai separando os valores já ordenados das partes por ordenar
-- se a flag de troca obtidentificadora recursivamente for 0, significa que da ultima vez que a função executou não teve o que ordenar. Retorna o que foi passado
    -- recFlag recebe o valor da flag de troca da lista passada. Então primeiro de tudo, tem-se uma tentativa de troca para colocar o maior elemento no final
-- se não, retorna a tupla com concatenação da parte a ser ordenada com a parte ja ordenada, o contador de trocas e a flag de trocas
-- toSortArea recebe o que aconteceu com unsortedArea depois de chamar a própria bubbleSort2.
    -- Então a primeira concatenação acontece só quando se chega ao final da pilha de execução
-- unsortedArea recebe os n - 1 elementos da lista que houve a tentativa de swap no começo do if
-- sortedLast recebe o ultimo elemento da lista nessa chamada
-- recCounter é atualizado nas chamadas da função de troca a cada recursão


bubbleSort2 :: Ord a => ([a], Int, Int) -> ([a], Int, Int)
bubbleSort2 ([x], swapCounter, swapFlag) = ([x], swapCounter, swapFlag)
bubbleSort2 (lst, swapCounter, swapFlag) = if recFlag == 0
                                            then (lst, swapCounter, swapFlag)
                                            else (toSortArea ++ sortedLast, recCounter, 0)
                                                where (swappedList, postSwapCounter, recFlag) = bubbleSwap1 (lst, swapCounter, swapFlag)
                                                      (unsortedArea, sortedLast) = splitLst swappedList
                                                      (toSortArea, recCounter, _) = bubbleSort2 (unsortedArea, postSwapCounter, 0)

splitLst :: [a] -> ([a],[a])
splitLst lst = (take ((length lst) - 1) lst, [last lst])

-- selectionSort original + contador de trocas
-- fiz mais uma alteração, que é para o caso da cabeça da lista já ser o menor elemento da área não ordenada
-- dessa forma não há uma troca ou chamada da função remove desnecessária
selection :: (Ord a) => [a] -> ([a], Int)
selection [] = ([], 0)
selection [x] = ([x], 0)
selection (x:xs) = selectionAux ((x:xs), 0)

selectionAux :: (Ord a) => ([a], Int) -> ([a], Int)
selectionAux ([x], swapCount) = ([x], swapCount)
selectionAux ((x:xs), swapCount) | x <= y = addHeaderToTuple x (selectionAux(xs, swapCount))
                                 | otherwise = addHeaderToTuple y (selectionAux((remove y (x:xs)), swapCount+1))
    where y = minValue (x:xs)


remove :: (Ord a) => a -> [a] -> [a]
remove a [] = []
remove a (x:xs) | a == x = xs
                | otherwise = x : (remove a xs)

minValue :: (Ord a) => [a] -> a
minValue [] = undefined
minValue [x] = x
minValue (x:xs) | x <= (minValue xs) = x
              | otherwise = minValue xs

-- Variação 1
-- Por algum motivo tá contando mais swaps do que o selection normal
-- Talvez seja algum problema com a parte de remoção do menor ou uma consequencia de ter uma função unificada
removeMin :: (Ord a) => ([a], a) -> ([a], a)
removeMin ([x], m) | m <= x = ([x], m)
                   | otherwise = ([m], x)
removeMin ((x:xs), currentMin) | x < currentMin = addHeaderToTuple currentMin (removeMin(xs, x))
                               | otherwise = addHeaderToTuple x (removeMin(xs, currentMin))

selectionAux1 :: (Ord a) => ([a], Int) -> ([a], Int)
selectionAux1 ([], swapCount) = ([], swapCount)
selectionAux1 ([x], swapCount) = ([x], swapCount)
selectionAux1 ((x:xs), swapCount) | x <= min = addHeaderToTuple x (selectionAux1(xs, swapCount))
                                  | otherwise = addHeaderToTuple min (selectionAux1(lst, swapCount+1))
    where (lst, min) = removeMin((xs), x)

selection1 :: (Ord a) => [a] -> ([a], Int)
selection1 [] = ([], 0)
selection1 [x] = ([x], 0)
selection1 lst = selectionAux1 (lst, 0)

-- Variação 2
selectionFold :: (Ord a) => [a] -> ([a], Int)
selectionFold [] = ([], 0)
selectionFold [x] = ([x], 0)
selectionFold lst = selFoldAux (lst, 0)
    where selFoldAux ([l], counter) = ([l], counter)
          selFoldAux (x:xs, counter) | x <= y = addHeaderToTuple x (selFoldAux(xs, counter))
                                     | otherwise = addHeaderToTuple y (selFoldAux((remove y (x:xs)), counter+1)) 
                where y = foldr1 (min) (x:xs)

-- insertionSort + contador de comparações 
insertion :: (Ord a) => [a] -> ([a], Int)
insertion [x] = ([x], 0)
insertion (x:xs) = insertionAux x (insertion xs)

insertionAux :: (Ord a) => a -> ([a], Int) -> ([a], Int)
insertionAux x ([], compCounter) = ([x], compCounter)
insertionAux x ((y:ys), compCounter) | x <= y = ((x:y:ys), compCounter+1)
                                     | otherwise = addHeaderToTuple y (insertionAux x (ys, compCounter+1))

-- Variação 1
insertionF :: (Ord a) => [a] -> ([a], Int)
insertionF l = foldr (insereOrd) ([],0) l
  where
    insereOrd x ([],compCounter) = ([x], compCounter)
    insereOrd x ((h : t), compCounter) =
      if x <= h
        then ((x : h : t), compCounter+1)
        else addHeaderToTuple h (insereOrd x (t,compCounter+1))

-- Quicksort + contador de comparações
-- Como as listas da esquerda e da direita são construídas por compreensão verificando
-- quais valores do tail são menores ou maiores/iguais que o pivô tem que se comparar
-- com o pivô cada valor do tail. Então a cada vez que se dividentificadore a lista, se compara
-- duas vezes o tail com o pivô
quick :: (Ord a) => [a] -> ([a], Int)
quick [] = ([],0)
quick [x] = ([x],0)
quick (piv:unsortedTail) = (sortedLeft ++ [piv] ++ sortedRight, totalComps)
    where (sortedLeft, leftCounter) = quick [x | x <- unsortedTail, x < piv]
          (sortedRight, rightCounter) = quick [x | x <- unsortedTail, x >= piv]
          totalComps = 2*(length unsortedTail) + leftCounter + rightCounter

-- Variação 1
quick1 :: (Ord a) => [a] -> ([a], Int)
quick1 [] = ([],0)
quick1 (piv:unsortedTail) = ((sortedLeft ++ [piv] ++ sortedRight), totalComps)
    where (unsortedLeft, unsortedRight, preRecCompCounter) = divList piv unsortedTail 0
          (sortedLeft, leftCompCounter) = quick1 unsortedLeft
          (sortedRight, rightCompCounter) = quick1 unsortedRight
          totalComps = preRecCompCounter + leftCompCounter + rightCompCounter

-- pivô -> lista -> contador -> (lista da esquerda, lista da direita, numero de comparações)
divList :: (Ord a) => a -> [a] -> Int -> ([a], [a], Int)
divList _ [] n = ([],[],n)
divList piv [x] n | x < piv = ([x], [], n+1)
                  | otherwise = ([], [x], n+1)
divList piv (x:xs) n | x < piv = addToLeft x (divList piv xs (n+1))
                     | otherwise = addtoRight x (divList piv xs (n+1))
    where addToLeft x (left, right, count) = (x : left, right, count)
          addtoRight x (left, right, count) = (left, x : right, count)

-- Variação 2
-- Usar isso aqui na l2 quase matou meu PC. Nunca mais executo esse código
quick2 :: (Ord a) => [a] -> ([a], Int)
quick2 [] = ([],0)
quick2 [x] = ([x],0)
quick2 lst = ((sortedLeft ++ [piv] ++ sortedRight), totalComps)
    -- Se a lista tem tamanho menor que 3, head é o pivô. Uma comparação para verificar se length é menor que 3
    -- É pra pegar o valor mediano. Então pega o segundo valor de uma lista ordenada composta pelos 3 primeiros elementos da lista original
    where getPivot (x:xs) | length (x:xs) < 3 = (x, 1)
                          | otherwise = (midentificadordleE, plOrdComp)
                where (fst3Ord, plOrdComp) = insertionF (x:xs)
                      midentificadordleE = head (tail fst3Ord)
          removeFstOc _ [] n = ([], n)
          removeFstOc x (y:ys) n | x == y = (ys, n + 1)
                                 | otherwise = addHeaderToTuple y (removeFstOc x ys (n+1))
          (piv, pivComp) = getPivot lst
          (newList, removalComps) = removeFstOc piv lst 0
          (unsortedLeft, unsortedRight, preRecCompCounter) = divList piv newList 0
          (sortedLeft, leftCompCounter) = quick2 unsortedLeft
          (sortedRight, rightCompCounter) = quick2 unsortedRight
          totalComps = preRecCompCounter + leftCompCounter + rightCompCounter + pivComp 

-- MergeSort
merge :: (Ord a) => [a] -> ([a], Int)
merge [] = ([],0)
merge [x] = ([x],0)
merge lst = mergeAux left right
    where left = merge (take ((length lst) `div` 2) lst)
          right = merge (drop ((length lst) `div` 2) lst)

mergeAux :: (Ord a) => ([a], Int) -> ([a], Int) -> ([a], Int)
mergeAux ([], x) ([], y) = ([], x + y)
mergeAux (l1, x) ([], y) = (l1, x + y)
mergeAux ([], x) (l2, y) = (l2, x + y)
mergeAux ((h1:t1), x) ((h2:t2), y) | h1 > h2 = addHeaderToTuple h2 (mergeAux ((h1:t1), x+1) (t2, y))
                                   | otherwise = addHeaderToTuple h1 (mergeAux (t1, x) ((h2:t2), y+1))

-- Exercicio 06
data Exp a
  = Val a -- um numero
  | Add (Exp a) (Exp a) -- soma de duas expressoes
  | Sub (Exp a) (Exp a) --subtração
  | Mult (Exp a) (Exp a)
  | Div (Exp a) (Exp a)
  deriving (Show)

avalia :: Fractional a => Num a => Exp a -> a
avalia (Val x) = x
avalia (Add exp1 exp2) = (avalia exp1) + (avalia exp2)
avalia (Sub exp1 exp2) = (avalia exp1) - (avalia exp2)
avalia (Mult exp1 exp2) = (avalia exp1) * (avalia exp2)
avalia (Div exp1 exp2) = (avalia exp1) / (avalia exp2)

-- (Div (Mult (Add (Val 3) (Val 12)) (Sub (Val 15) (Val 5))) (Mult (Val 1) (Val 3)))
-- (Sub (Val 0) (Mult (Add (Sub (Add (Val 6) (Val 8)) (Val 5)) (Val 1)) (Add (Val 2) (Div (Val 6) (Val 2)))))

-- Exercicio 07
data Jogada = Pedra | Papel | Tesoura deriving (Eq, Show)

vence :: Jogada -> Jogada -> Bool
vence Pedra Papel = False
vence Papel Tesoura = False
vence Tesoura Pedra = False
vence j1 j2 | j1 == j2 = False
            | otherwise = not (vence j2 j1)

vencedoras :: [(Jogada, Jogada)] -> [Jogada]
vencedoras [(j1, j2)] | vence j1 j2 = [j1]
                      | otherwise = [j2]
vencedoras ((j1,j2):js) | vence j1 j2 = j1:vencedoras js
                        | otherwise = j2:vencedoras js

-- Exercício 08
data Nebuloso = Verdadeiro | Falso | Talvez Float deriving (Show)

fuzzifica :: Float -> Nebuloso
fuzzifica pert | pert <= 0 = Falso
               | pert >= 1 = Verdadeiro
               | otherwise = (Talvez pert)

verificaAlto :: Float -> Nebuloso
verificaAlto height = fuzzifica ((height-1.7)/0.2)

carroBarato :: Float -> Nebuloso
carroBarato price = fuzzifica ((50000 - price)/200000)

data Ano = Primeiro | Segundo | Terceiro deriving (Show)
data Colegio = Nacional | Olimpo | Gabarito deriving (Show)
data Universidade = UFU | UNITRI | UNA deriving (Show)
data Curso = Computacao | Medicina | Direito | Musica deriving (Show)
type Altura = Float
type Peso = Float
type Idade = Int
type Matricula = Int
data ID = EM | Uni deriving (Show)

data Estudante = Colegial {identificador :: ID, ano :: Ano, colegio :: Colegio, matricula :: Matricula, altura :: Altura, peso :: Peso} |
                 Universitario {identificador :: ID, universidade :: Universidade, curso :: Curso, matricula :: Matricula, altura :: Altura, idade :: Idade} deriving (Show)


baseEst :: [Estudante]
baseEst = [(Colegial {identificador=EM, ano= Primeiro, colegio= Nacional, matricula= 93836, altura= 1.70, peso = 65.1}), 
           (Colegial {identificador=EM, ano= Segundo, colegio= Olimpo, matricula= 56356, altura= 1.88, peso = 80.9}),
           (Colegial {identificador=EM, ano= Terceiro, colegio= Gabarito, matricula= 49756, altura= 1.73, peso = 70.7}),
           (Colegial {identificador=EM, ano= Segundo, colegio= Olimpo, matricula= 52153, altura= 1.78, peso = 72.2}),
           (Colegial {identificador=EM, ano= Segundo, colegio= Nacional, matricula= 27686, altura= 1.82, peso = 68.4}),
           (Colegial {identificador=EM, ano= Primeiro, colegio= Gabarito, matricula= 14194, altura= 1.65, peso = 52.6}),
           (Colegial {identificador=EM, ano= Primeiro, colegio= Olimpo, matricula= 68970, altura= 1.59, peso = 51.5}),
           (Colegial {identificador=EM, ano= Primeiro, colegio= Nacional, matricula= 23094, altura= 1.69, peso = 63.5}),
           (Colegial {identificador=EM, ano= Terceiro, colegio= Gabarito, matricula= 26570, altura= 1.98, peso = 78.3}),
           (Colegial {identificador=EM, ano= Terceiro, colegio= Olimpo, matricula= 17236, altura= 1.96, peso = 82}),
           (Universitario {identificador=Uni, universidade= UFU, curso= Computacao, matricula= 60043274, altura= 1.95, idade= 21}),
           (Universitario {identificador=Uni, universidade= UNA, curso= Musica, matricula= 13402937, altura= 2.01, idade= 19}),
           (Universitario {identificador=Uni, universidade= UFU, curso= Musica, matricula= 13056839, altura= 1.52, idade= 22}),
           (Universitario {identificador=Uni, universidade= UNITRI, curso= Direito, matricula= 85071084, altura= 1.60, idade= 24}),
           (Universitario {identificador=Uni, universidade= UFU, curso= Medicina, matricula= 58223106, altura= 1.59, idade= 25}),
           (Universitario {identificador=Uni, universidade= UFU, curso= Computacao, matricula= 52363967, altura= 1.72, idade= 27}),
           (Universitario {identificador=Uni, universidade= UNITRI, curso= Direito, matricula= 82082830, altura= 1.88, idade= 21}),
           (Universitario {identificador=Uni, universidade= UNA, curso= Musica, matricula= 33246699, altura= 1.79, idade= 18}),
           (Universitario {identificador=Uni, universidade= UFU, curso= Computacao, matricula= 32073253, altura= 1.77, idade= 17}),
           (Universitario {identificador=Uni, universidade= UNITRI, curso= Direito, matricula= 18601447, altura= 1.83, idade= 22})]

pegaAltura :: Estudante -> Float
pegaAltura (Colegial{identificador = i, ano = a, colegio = c, matricula = m, altura = alt, peso = p}) = alt
pegaAltura (Universitario{identificador = i, universidade = u, curso = c, matricula= m, altura = alt, idade= idd}) = alt

pegaMatricula :: Estudante -> Int
pegaMatricula (Colegial{identificador = i, ano = a, colegio = c, matricula = m, altura = alt, peso = p}) = m
pegaMatricula (Universitario{identificador = i, universidade = u, curso = c, matricula= m, altura = alt, idade= idd}) = m

descobreAltos :: [Estudante] -> [(Int, Nebuloso)]
descobreAltos [] = []
descobreAltos (x:xs) = (pegaMatricula x, verificaAlto (pegaAltura x)) : descobreAltos xs

data ArvoreBinInt = Nulo | No Int ArvoreBinInt ArvoreBinInt deriving Show

arvDados :: ArvoreBinInt
arvDados = (No 8 (No 9 (No 5 Nulo Nulo) (No 6 (No 12 Nulo Nulo) (No 30 Nulo Nulo))) (No 10 Nulo (No 37 (No 20 Nulo Nulo) (No 19 Nulo Nulo))))

folha :: ArvoreBinInt -> [Int]
folha Nulo = []
folha (No noFolha Nulo Nulo) = [noFolha]
folha (No _ arvEsq arvDir) = folha arvEsq ++ folha arvDir

somaNosInternos :: ArvoreBinInt -> Int
somaNosInternos Nulo = 0
somaNosInternos (No _ Nulo Nulo) = 0
somaNosInternos (No root arvEsq arvDir) = somaNosInternos arvEsq + somaNosInternos arvDir + root

pertence :: Int -> ArvoreBinInt -> Bool
pertence x Nulo = False
pertence x (No root Nulo Nulo) | x == root = True
                               | otherwise = False
pertence x (No root arvEsq arvDir) | x == root = True
                                   | otherwise = pertence x arvEsq || False || pertence x arvDir