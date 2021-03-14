dobro :: Float -> Float
dobro x = x*2

quad :: Float -> Float
quad x = dobro (dobro x)

hip :: Float -> Float -> Float
hip cat1 cat2 = sqrt(cat1^2 + cat2^2)

type Ponto = (Float, Float)
dist2p :: Ponto -> Ponto -> Float
dist2p (x1, y1) (x2, y2) = hip x' y'
    where
        x' = x1 - x2
        y' = y1 - y2

type Moedas = (Float, Float, Float)
conversao :: Float -> Moedas
conversao real = (real, real*3.96, real*4.45)

bissexto :: Int -> Bool
bissexto ano | mod ano 400 == 0 = True
             | (mod ano 4 == 0) && (mod ano 100 /= 0) = True
             | otherwise  = False

type Data = (Int, Int, Int)
bissexto2 :: Data -> Bool
bissexto2 (d,m,a) | mod a 400 == 0 = True
                  | (mod a 4 == 0) && (mod a 100 /= 0) = True
                  | otherwise  = False

valida :: Data -> Bool
valida (d, m,a) | (d>=1 && d<=31 && (m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12)) = True
                | (d>=1 && d<=30 && (m == 4 || m == 6 || m == 9 || m == 11)) = True
                | (d>=1 && d<=28 && m == 2 && not(bissexto a)) = True
                | (d>=1 && d<=29 && m == 2 && bissexto a) = True
                | otherwise = False

precede :: Data -> Data -> Bool
precede (d1,m1,a1) (d2,m2,a2) | a1 > a2 = False
                              | a1 < a2 = True
                              | m1 > m2 = False
                              | m1 < m2 = True
                              | d1 > d2 = False
                              | d1 < d2 = True
                              | otherwise = False 

type Livro = (String, String, String, String, Int)
type Aluno = (String, String, String, String)
type Emprestimo = (String, String, Data, Data, String)

e1::Emprestimo
e1 = ("H123C9","BSI200945",(12,9,2009),(20,9,2009),"aberto")
verifica :: Emprestimo -> Data -> Bool
verifica (codLivro, codAluno, dataIni, dataFim, situ) dataHJ = precede dataHJ dataFim && (precede dataIni dataHJ || dataHJ == dataIni)
