sumatoria lista = foldl1 (+) lista

maximo lista = foldl1 max lista

--cantidadDeElementos :: [(Integer, Integer)] -> Integer
cantidadDeElementos lista = foldl (\sem _ -> sem + 1) 0 lista

cantidadDeElementos' lista = foldr (\_ sem -> sem + 1) 0 lista

cantidadDeElementos'' lista = foldr contar 0 lista

contar _ sem = sem + 1

cantidadDeElementos''' lista = foldl (flip contar) 0 lista


masGastador (emple:resto) = foldl masGasto emple resto

masGasto emple otroEmple | snd emple > snd otroEmple = emple
                          | otherwise = otroEmple

masGastador' (emple:resto) = foldr masGasto emple  resto


monto empleados = foldl (\sem (_,gasto) -> sem + gasto) 0  empleados

monto' empleados = foldr (\(_,gasto) sem -> sem + gasto) 0 empleados

{-foldl (flip ($)) 2 [(3+), (*2), (5+)]

foldl (\sem f -> f sem )  2 [(3+), (*2), (5+)] -}

{-foldr ($) 2 [(3+), (*2), (5+)] 

foldr (\f sem -> f sem ) 2 [(3+), (*2), (5+)] -}

-- Punto 5
type Nombre  = String
type InversionInicial = Int
type Profesionales = [String]

data  Proyecto = Proy {nombre:: Nombre, inversionInicial::  InversionInicial, profesionales:: Profesionales} deriving Show

proyectos :: [Proyecto]
proyectos = [Proy "red social de arte"  20000 ["ing. en sistemas", "contador"], Proy "restaurante" 5000 ["cocinero", "adm. de empresas", "contador"], Proy "ventaChurros" 1000 ["cocinero"] ]

maximoProyectoPor ::  (Proyecto -> Int) -> [Proyecto] -> Proyecto
maximoProyectoPor criterio (unProyecto:proyectos) = foldl (maximoProyecto criterio) unProyecto  proyectos

maximoProyecto :: (Proyecto -> Int) -> Proyecto -> Proyecto -> Proyecto
maximoProyecto f unProyecto otroProyecto | f unProyecto > f otroProyecto = unProyecto
                                          | otherwise = otroProyecto

-- a
{- *Main> maximoProyectoPor inversionInicial proyectos
Proy {nombre = "red social de arte", inversionInicial = 20000, profesionales = ["ing. en sistemas","contador"]} -}

-- b
{-*Main> maximoProyectoPor (length.profesionales) proyectos
Proy {nombre = "restaurante", inversionInicial = 5000, profesionales = ["cocinero","adm. de empresas","contador"]} -}

-- c
{-*Main> maximoProyectoPor (length.words.nombre) proyectos
Proy {nombre = "red social de arte", inversionInicial = 20000, profesionales = ["ing. en sistemas","contador"]} -}

maximoProyectoPor' ::  (Proyecto -> Int) -> [Proyecto] -> Proyecto
maximoProyectoPor' criterio (unProyecto:proyectos) = foldr (maximoProyecto criterio) unProyecto  proyectos

maximoProyectoPor'' ::  (Proyecto -> Int) -> [Proyecto] -> Proyecto
maximoProyectoPor'' criterio proyectos = foldl1 (maximoProyecto criterio) proyectos
