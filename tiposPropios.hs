data Figura = Circulo { radio :: Double } | Rectangulo { base :: Double , altura ::
Double } | Cuadrado {lado :: Double} deriving Show

area ::Figura -> Double
area (Circulo radio) = pi * radio^2
area (Rectangulo base altura ) = base * altura
area (Cuadrado lado) = lado * lado

rectangulo :: Figura
rectangulo = Rectangulo 7 9

circulo :: Figura
circulo = Circulo 10

data Persona = Persona {nombre:: String, edad::Int} deriving Show

ana = Persona "ana" 20

cumplirAños :: Persona -> Persona
cumplirAños (Persona nom edad) = Persona nom (edad + 1)

numerosPares numeros = [num | num <-numeros , even num]


data Empleado = Comun { nombreEmple :: String, sueldoBasico :: Double} | Jefe {sueldoBasico::Double, cantACargo::Double, nombreEmple:: String}

sueldo :: Empleado -> Double
sueldo (Comun _ sueldoB) = sueldoB
sueldo (Jefe  sueldoB cantidad _) = sueldoB + plus cantidad

plus cant = cant * 500

analia = Jefe 40000 3 "analia"
julio = Comun "julio" 30000


data Bebida = Cafe {nombreBebida :: String} | Gaseosa {sabor ::String , azucar :: Integer}

esEnergizante :: Bebida -> Bool
esEnergizante (Cafe "capuchino") = True
esEnergizante (Gaseosa "pomelo" cantAzucar) = cantAzucar > 10
esEnergizante  _  = False

find' :: (a-> Bool) -> [a] -> a
find' criterio lista = (head . filter criterio) lista

data Politico = Politico {proyectosPresentados :: [String], sueldoPolitico :: Float, edadPolitico :: Int } deriving Show
 
politicos :: [Politico] 
politicos = [ Politico ["ser libres", "libre estacionamiento coches politicos", "ley no fumar", "ley 19182"] 20000 81, Politico ["tratar de reconquistar luchas sociales"] 10000 63, Politico ["tolerancia 100 para delitos"] 15500 49 ]

--a)  un político joven (menos de 50 años)
-- find' ((<50).edadPolitico)  politicos

--b)  alguno que haya presentado más de 3 proyectos
-- find' ((>3).length.proyectosPresentados) politicos

