import Text.Show.Functions

siguiente :: Integer -> Integer
siguiente nro = nro + 1

doble nro = nro * 2
calcular nro | even nro = siguiente nro
              | otherwise = doble nro

aproboAlumno nota = nota >= 6

segundo :: (Integer, Integer, Integer)-> Integer
segundo (_ ,x, _) = x

calcular' :: (Integer, Integer)-> (Integer, Integer)
calcular' (primero, segundo) = (calcularPrimero primero, calcularSegundo segundo)

calcularPrimero primero | even primero = doble primero
                        | otherwise = primero

calcularSegundo segundo | odd segundo = siguiente segundo
                        | otherwise = segundo

and' :: Bool -> Bool -> Bool
and' unBool otroBool | unBool = otroBool
                      | otherwise = False

and'' :: Bool -> Bool -> Bool
and'' True bool = bool
and'' _ _ = False

or' :: Bool -> Bool -> Bool
or' True _ = True
or' _ True = True
or' _ _ = False

or'' :: Bool -> Bool -> Bool
or'' False bool = bool
or'' _ _ = True

or''' :: Bool -> Bool -> Bool
or''' True _ = True
or''' _ bool = bool

type Nota = Integer
type Alumno = (String, Nota, Nota, Nota)

notaMaxima :: Alumno -> Nota
notaMaxima (_, nota, nota2, nota3) = nota `max` (nota2 `max` nota3)

cuadruple :: Integer -> Integer
cuadruple nro = doble (doble nro)

cuadruple' :: Integer -> Integer
cuadruple' nro = doble . doble $ nro

esMayorA :: Integer -> Bool
esMayorA nro = (doble.siguiente.suma 2) nro > 10

triple = (\x -> 3 * x)

siguiente' = (\x -> x + 1)

suma :: Integer -> Integer -> Integer
suma x y = x + y

suma' :: (Integer, Integer) -> Integer
suma' (x,y) = x + y

esParSiguiente :: Integer -> Bool

esParSiguiente nro = even.siguiente $ nro

esImparSigSumaSiete :: Integer -> Bool
esImparSigSumaSiete nro = (odd.siguiente.suma 7) nro

cabeza (x:_) = x

sayHello alguien = " Hello " ++ alguien ++ "!"