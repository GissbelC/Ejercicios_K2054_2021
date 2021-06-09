import Text.Show.Functions
type Bien = (String,Float)       
data Ciudadano = UnCiudadano {profesion :: String, sueldo :: Float,cantidadDeHijos :: Float, bienes :: [Bien] } deriving Show
homero = UnCiudadano "SeguridadNuclear" 9000 3 [("casa",50000), ("deuda",-70000)]
frink = UnCiudadano "Profesor" 12000 1 []
krabappel = UnCiudadano "Profesor" 12000 0 [("casa",35000)]
burns = UnCiudadano "Empresario" 300000 1 [("empresa",1000000),("empresa",500000),("auto",200000)]

type Ciudad = [Ciudadano]
springfield :: Ciudad
springfield = [homero, burns, frink, krabappel]

diferenciaDePatrimonio :: Ciudad -> Float
diferenciaDePatrimonio unaCiudad = (patrimonio.ciudadanoSegun maximo) unaCiudad - (patrimonio.ciudadanoSegun minimo) unaCiudad

patrimonio :: Ciudadano -> Float
patrimonio unCiudadano = foldl (\sem (_, monto) -> sem + monto) (sueldo unCiudadano) (bienes unCiudadano)

ciudadanoSegun :: (Ciudadano -> Ciudadano ->Ciudadano) -> Ciudad -> Ciudadano
ciudadanoSegun f  ciudadanos = foldl1 f ciudadanos

maximo :: Ciudadano -> Ciudadano ->Ciudadano
maximo unCiudadano otroCiudano | patrimonio unCiudadano >= patrimonio otroCiudano = unCiudadano
                                | otherwise = otroCiudano

minimo :: Ciudadano -> Ciudadano ->Ciudadano
minimo unCiudadano otroCiudano | patrimonio unCiudadano < patrimonio otroCiudano = unCiudadano
                               | otherwise = otroCiudano

{-*Main> diferenciaDePatrimonio springfield
2011000.0 -}

-- 2
tieneAutoAltaGama :: Ciudadano -> Bool
tieneAutoAltaGama unCiudadano = any autoAltaGama . bienes $ unCiudadano

autoAltaGama :: Bien -> Bool
autoAltaGama ("auto", monto) = monto > 100000
autoAltaGama  _ = False

type Medida = Ciudadano -> Ciudadano

auh :: Medida
auh unCiudadano = aplicarMedidaGobierno (patrimonio unCiudadano < 0) (modificarSueldo ((incremento.cantidadDeHijos) unCiudadano) unCiudadano)  unCiudadano

aplicarMedidaGobierno :: Bool -> Ciudadano -> Ciudadano -> Ciudadano
aplicarMedidaGobierno cond unCiudadano otroCiudano |cond = unCiudadano
                                                | otherwise = otroCiudano

modificarSueldo :: Float -> Ciudadano -> Ciudadano
modificarSueldo cantidad unCiudadano = unCiudadano { sueldo = sueldo unCiudadano + cantidad}

incremento cantidad = cantidad * 1000

-- b
impuestoGanancias :: Float -> Medida
impuestoGanancias minimo unCiudadano = aplicarMedidaGobierno (sueldo unCiudadano > minimo)  (modificarSueldo (diferencia minimo (sueldo unCiudadano)) unCiudadano)  unCiudadano

diferencia minimo sueldo = (minimo - sueldo) * 0.3

{-*Main> impuestoGanancias 10000 frink
UnCiudadano {profesion = "Profesor", sueldo = 11400.0, cantidadDeHijos = 1.0, bienes = []}
-}
--c 
impuestoAltaGama :: Medida
impuestoAltaGama unCiudadano = aplicarMedidaGobierno (tieneAutoAltaGama unCiudadano)  (modificarSueldo (impuesto (bienes unCiudadano))  unCiudadano)  unCiudadano

impuesto bienes = (*(-0.1)).snd.head.filter autoAltaGama $ bienes

{-*Main> impuestoAltaGama burns
UnCiudadano {profesion = "Empresario", sueldo = 280000.0, cantidadDeHijos = 1.0, bienes = [("empresa",1000000.0),("empresa",500000.0),("auto",200000.0)]}
-}

negociarSueldoProfesion :: String -> Float -> Medida
negociarSueldoProfesion unaProfesion porcentaje unCiudadano = aplicarMedidaGobierno ((unaProfesion==).profesion $ unCiudadano)  (modificarSueldo (calcularAumento porcentaje (sueldo unCiudadano)) unCiudadano) unCiudadano

calcularAumento unPorcentaje sueldo = (sueldo * unPorcentaje) /100

{-*Main> negociarSueldoProfesion "SeguridadNuclear" 30 homeoUnCiudadano {profesion = "SeguridadNuclear", sueldo = 11700.0, cantidadDeHijos = 3.0, bienes = [("casa",50000.0),("deuda",-70000.0)]}
-}

data Gobierno = UnGobierno {años :: [Int], medidas :: [Medida]} deriving Show

gobiernoA :: Gobierno
gobiernoA = UnGobierno [1999..2003]  [impuestoGanancias 30000, negociarSueldoProfesion "Profesor" 10, negociarSueldoProfesion "Empresario" 40, impuestoAltaGama, auh]

gobiernoB :: Gobierno
gobiernoB = UnGobierno [2004..2008]  [ impuestoGanancias 40000, negociarSueldoProfesion "Profesor" 30, negociarSueldoProfesion "Camionero" 40]

-- c
gobernarUnAño :: Gobierno -> Ciudad -> Ciudad
gobernarUnAño unGobierno unaCiudad =  map (aplicarMedidas unGobierno) unaCiudad

aplicarMedidas :: Gobierno -> Ciudadano -> Ciudadano
aplicarMedidas unGobierno unCiudadano = foldl (\unCiu unaMedida -> unaMedida unCiu) unCiudadano (medidas unGobierno)

aplicarMedidas' unGobierno unCiudadano = foldl (flip ($)) unCiudadano (medidas unGobierno)

gobernarPeriodoCompleto :: Gobierno -> Ciudad -> Ciudad
gobernarPeriodoCompleto unGobierno unaCiudad = foldl (\ciudad _ -> gobernarUnAño unGobierno ciudad) unaCiudad (años unGobierno) 


gobernarPeriodoCompleto' unGobierno unaCiudad = foldl (flip ($)) unaCiudad (replicate (length.años $ unGobierno) (gobernarUnAño unGobierno))

--e
distribuyoRiqueza :: Gobierno -> Ciudad -> Bool
distribuyoRiqueza unGobierno unaCiudad = diferenciaDePatrimonio unaCiudad > (diferenciaDePatrimonio.gobernarPeriodoCompleto unGobierno) unaCiudad

{-*Main> distribuyoRiqueza gobiernoB springfield
True -}

kane = UnCiudadano "Empresario" 100000 0 [("Rosebud", valor)| valor <- [5,10..]]

f1 :: Num t => t-> ( a -> t -> Bool ) ->  a  -> [t]  ->[t]
f1 x y z = map (*x) . filter (y z) 