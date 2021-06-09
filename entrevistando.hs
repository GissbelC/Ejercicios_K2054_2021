data Postulante = UnPostulante {nombre :: String, edad :: Int, remuneracion :: Float, conocimientos :: [String]}  | Estudiante {legajo:: String, conocimientos::[String]} deriving Show

pepe = UnPostulante "Jose Perez" 35 15000.0 ["Haskell", "Prolog", "Wollok", "C"]
tito = UnPostulante "Roberto Gonzalez" 20 12000.0 ["Haskell", "Php"]

type Nombre = String
data Puesto = UnPuesto {puesto:: String, conocimientoRequeridos :: [String]} deriving Show

jefe = UnPuesto "gerente de sistemas" ["Haskell", "Prolog", "Wollok"]
chePibe = UnPuesto "cadete" ["ir al banco"] 

apellidoDueno:: Nombre
apellidoDueno = "Gonzalez"

type Requisito = Postulante -> Bool

tieneConocimientos :: Puesto -> Requisito
tieneConocimientos puesto postulante = all (\conociRequ -> elem conociRequ (conocimientos postulante)) (conocimientoRequeridos puesto)

tieneConocimientos' puesto postulante = all (flip elem (conocimientos postulante)).conocimientoRequeridos $ puesto

{-*Main> tieneConocimientos' jefe pepe
True -}

edadAceptable :: Int -> Int -> Requisito
edadAceptable edadMin edadMax unPostulante = edad unPostulante >= edadMin && edad unPostulante <= edadMax 

{-*Main> edadAceptable 30 45 pepe
True -}

sinArreglo :: Requisito
sinArreglo unPostulante =  (apellidoDueno /=).last.words.nombre $ unPostulante


preseleccion :: [Postulante] -> [Requisito] -> [Postulante]
preseleccion postulantes requisitos = filter  (cumpleRequisitos requisitos)   postulantes

cumpleRequisitos :: [Requisito] -> Postulante -> Bool
cumpleRequisitos requisitos unPostulante = all ($ unPostulante) requisitos
-- a
{- preseleccion [pepe, tito] [tieneConocimientos jefe, edadAceptable 30 40, sinArreglo]
[UnPostulante {nombre = "Jose Perez", edad = 35, remuneracion = 15000.0, conocimientos = ["Haskell","Prolog","Wollok","C"]}]
-}

-- b
{- preseleccion [pepe, tito] [tieneConocimientos jefe, edadAceptable 30 40, sinArreglo,  (not.elem "repetir logica".conocimientos)]
-}

incrementarEdad :: Postulante -> Postulante
incrementarEdad postulante = postulante { edad = edad postulante + 1}

aumentarSueldo :: Float -> Postulante -> Postulante
aumentarSueldo porcentaje unPostulante = unPostulante {remuneracion = nuevaRemuneracion porcentaje unPostulante}

nuevaRemuneracion porcentaje postulante = remuneracion postulante + (remuneracion postulante) * porcentaje /100

-- a
actualizarPostulantes :: [Postulante] -> [Postulante]
actualizarPostulantes postulantes = [aumentarSueldo 27.incrementarEdad $ unPostulante |unPostulante <-postulantes]

--b
actualizarPostulantes' :: [Postulante] -> [Postulante]
actualizarPostulantes' postulantes = map (aumentarSueldo 27.incrementarEdad) postulantes

--actualizarPostulantes.repeat $ pepe

capacitar :: Postulante -> String -> Postulante
capacitar postulante conocimiento = postulante { conocimientos = nuevosConocimientos postulante conocimiento}

nuevosConocimientos :: Postulante -> String -> [String]
nuevosConocimientos (UnPostulante _ _ _ conocimientos) unConocimiento =  agregarConocimiento conocimientos unConocimiento
nuevosConocimientos (Estudiante _ conocimientos) unConocimiento = agregarConocimiento (init conocimientos) unConocimiento

agregarConocimiento conocimientos unConocimiento = [unConocimiento] ++ conocimientos
