factorial 0 = 1
factorial n = n * factorial (n - 1)

length' [] = 0
length' (x:xs ) = 1 + length' xs

last' [x] = x
last' (x:xs) = last' xs

elem' _ [] = False
elem' e ( x:xs )= e == x || elem' e xs

reverse' [] = []
reverse' (x:xs ) = reverse' xs  ++ [x]

maximo [x] = x
maximo (x:xs) =  x `max` maximo xs

type Nombre = String
type Notas = [Int]
type Nota = Int

data Persona = Alumno {nombre :: Nombre, notas :: Notas} deriving Show

juan :: Persona
juan = Alumno "juan" [7,9,5]
maria = Alumno "maria" [7,9,4]
ana = Alumno "ana" [6,2,4]

alumnos = [juan, maria, ana]

promediosAlumnos:: [Persona] -> [(Nombre, Nota)]
promediosAlumnos personas = map (\unaPer -> (nombre unaPer, (promedio.notas) unaPer))   personas

promedio :: Notas -> Nota
promedio notas = sum notas `div` length notas

promediosSinAplazos :: [Notas] ->[Nota]
promediosSinAplazos listaNotas = map (promedio.filter (>=6)) listaNotas

aprobo :: Persona -> Bool
aprobo persona = all (>=6) . notas $ persona

aprobaron :: [Persona] -> [Nombre]
aprobaron personas = map nombre . filter aprobo $ personas

productos :: [String] -> [Integer] -> [(String, Integer)]
productos nombres precios = zip nombres precios

productos' :: [String] -> [Integer] -> [(String, Integer)]
productos' nombres precios = zipWith (\nom prec -> (nom, prec))  nombres precios

data Flor= Flor{nombreFlor :: String, aplicacion:: String, cantidadDeDemanda:: Int } deriving Show
type Flores = [Flor]

rosa = Flor "rosa" "decorativo" 120
jazmin =  Flor "jazmin" "aromatizante" 100
violeta=  Flor "violeta" "infusión" 110
orquidea =  Flor "orquidea" "decorativo" 90

flores = [orquidea, rosa,violeta, jazmin]

maximoSegun :: Flores -> (Flor -> Int) -> String 
maximoSegun conjFlores f   = nombreFlor . maximaFlor' f $ conjFlores

maximaFlor' :: (Flor -> Int) -> Flores -> Flor
maximaFlor' _ [flor] = flor
maximaFlor' f (flor : otraFlor : flores) | f flor > f otraFlor  = maximaFlor' f (flor:flores)
                                         | otherwise = maximaFlor' f (otraFlor: flores)


maximaFlor :: (Flor -> Int) -> Flores -> Flor
maximaFlor _ [flor] = flor
maximaFlor f (flor : flores) | f flor >= f (maximaFlor f flores) = flor
                             | otherwise = maximaFlor f flores

{-*Main> maximoSegun flores cantidadDeDemanda
"rosa" -}

{-*Main> maximoSegun flores (length.nombreFlor)
"orquidea" -}

{-*Main> maximoSegun flores ((`mod` 4) . cantidadDeDemanda)
"violeta" -}

-- 6b
floresOrdenadas :: Flores -> Bool
floresOrdenadas [_] = True
floresOrdenadas (unaFlor:otraFlor:flores) =  cantidadDeDemanda unaFlor > cantidadDeDemanda otraFlor && floresOrdenadas (otraFlor:flores)

{-*Main> floresOrdenadas [rosa, violeta, jazmin, orquidea]
True -}

