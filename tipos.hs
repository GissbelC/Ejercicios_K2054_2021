p :: (a -> Bool) -> [a] -> a
p n = (head . filter n) 

f :: (Ord a) => (b -> a) -> [( b ,  b)] -> Bool
f x y = (x.fst.head) y > (x.snd.head) y

g :: (Eq t) => ( h  -> t ) -> h  ->  h  -> Bool
g f a b = f a == f b


p1 :: (b -> Bool)  -> (  a -> b )   -> Int   -> [ a ]  -> Bool
p1 x y z = ((> z) . length . filter x . map y)

h :: (Eq t) => t ->[(x,t,a)]-> (x,t,a)
h nom  = head.filter ((nom==).g1)
g1 (_, c, _) = c

f2 :: (Ord s , Num s, Num a ) => a -> (a -> s ) -> [a]  -> a
f2 x _ [] = x
f2 x y (z:zs) | y z > 0 = z + f2 x y zs
            | otherwise = f2 x y zs

qfsort ::(Ord t)  => (a -> t) -> [a]  -> [a]
qfsort f [ ] = [ ]
qfsort f (x:xs) = (qfsort f (filter ((> f x).f) xs)) ++ [x] ++ (qfsort f (filter ((< f x).f) xs))

data Animal= Raton {nombre :: String, edad :: Float, peso :: Float, enfermedades :: [String]} deriving Show

-- Estos son las enfermedades infecciosas
enfermedadesInfecciosas = [ "brucelosis", "tuberculosis"]

cerebro = Raton "Cerebro" 9.0 0.2 ["brucelosis", "sarampión", "tuberculosis"]

modificarEdad f raton = raton {edad = (f. edad) raton }

modificarNombre f raton = raton {nombre = f.nombre $ raton}

modificarPeso f raton = raton {peso = f.peso $ raton}

modificarEnfermedades f raton = raton {enfermedades = f.enfermedades $ raton}

hierbaBuena :: Animal -> Animal
hierbaBuena unRaton = modificarEdad sqrt unRaton

hierbaVerde :: String -> Animal -> Animal
hierbaVerde enfermedad unRaton = modificarEnfermedades (filter (/= enfermedad)) unRaton

alcachofa :: Animal -> Animal
alcachofa unRaton = modificarPeso perderPeso unRaton

perderPeso peso | peso > 2 = peso *0.9
                | otherwise = peso * 0.95

hierbaMagica :: Animal -> Animal
hierbaMagica raton = modificarEdad (*0). modificarEnfermedades (const []) $ raton

---Punto 3
medicamento :: [(Animal -> Animal)] -> Animal -> Animal
medicamento hierbas unRaton = foldl (flip ($))  unRaton  hierbas

{-*Main> medicamento [alcachofa, (hierbaVerde "tuberculosis") ] cerebro
Raton {nombre = "Cerebro", edad = 9.0, peso = 0.19, enfermedades = ["brucelosis","sarampi\243n"]} -}

antiAge :: Animal -> Animal
antiAge unRaton =  medicamento (replicate 3 hierbaBuena ++ [alcachofa]) unRaton


reduceFatFast :: Int -> Animal -> Animal
reduceFatFast potencia unRaton = medicamento ([hierbaVerde "obesidad"] ++ (replicate potencia alcachofa)) unRaton

{-*Main> reduceFatFast 4 cerebro
Raton {nombre = "Cerebro", edad = 9.0, peso = 0.16290124, enfermedades = ["brucelosis","sarampi\243n","tuberculosis"]}
-}

hierbaMilagrosa :: Animal -> Animal
hierbaMilagrosa unRaton = foldr hierbaVerde unRaton  enfermedadesInfecciosas
{-*Main> hierbaMilagrosa cerebro
Raton {nombre = "Cerebro", edad = 9.0, peso = 0.2, enfermedades = ["sarampi\243n"]} -}