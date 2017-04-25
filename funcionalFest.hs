import Text.Show.Functions
import Data.List 

--tipo de dato del cliente esta compuesto por un string que representa el nombre, un int que representa resistencia y una lista para repreentar sus amigos.--
data Cliente = Cliente {_nombre::String, _resistencia::Int, _amigos::[Cliente], _tragosTomados::[Cliente->Cliente]} deriving (Show) 

--Definimos manualmente que Cliente pertenece a Eq, y definimos la igualdad usando la igualdad de los nombres
instance Eq Cliente where  
    --No es necesario definir /= ya que esta definida como not ==
    cliente1 == cliente2 = (_nombre cliente1) == (_nombre cliente2)

data Itinerario = Itinerario {_nombreItinerario::String, _duracion::Float, _tragos::[Cliente->Cliente]} deriving (Show) 

intensidad (Itinerario _ duracion tragos) = (genericLength tragos) / duracion

-- Definimos manualmente que Itinerario pertenece a Eq (Para luego poder definir que pertenece a Ord)
instance Eq Itinerario where  
    itinerario1 == itinerario2 = (_nombreItinerario itinerario1) == (_nombreItinerario itinerario2)

-- Definimos manualmente que Itinerario pertenece a Ord
instance Ord Itinerario where  
    -- (>) y (<) estan definidos en base a compare, por lo que no hacen falta
    -- Definimos la comparacion de itinerarios como la comparacion de sus intensidades (Que son numeros)
    compare itinerario1 itinerario2 = compare (intensidad itinerario1) (intensidad itinerario2)

-- Definimos algunos Clientes
rodri = Cliente "Rodri" 55 [] [tintico]
marcos = Cliente "Marcos" 40 [rodri] [klusener "guinda"]
cristian = Cliente "Cristian" 2 [] [grogXD, jarraLoca]
ana = Cliente "Ana" 120 [marcos, rodri] []
robertoCarlos = Cliente "Roberto Carlos" 165 [] []
chuckNorris = Cliente "Chuck" 1000 [ana] [soda i |i<-[1,2..]]
-- B: Chuck no puede tomar otro trago(Nunca se alcanzara el ultimo trago tomado de la lista, es un intento de recorrer una lista infinita acotada inferiormente)
--D: Si, porque Haskell es lazy(perezoso) y no evalua la expresion hasta que lo necesite

-- Definimos algunos Itinerarios
mezclaExplosiva = Itinerario "Mezcla Explosiva" 2.5 [klusener "Frutilla", klusener "Huevo", grogXD, grogXD]
itinerarioBasico = Itinerario "Itinerario Basico" 5 [klusener "Huevo", rescatarse 2, klusener "Chocolate", jarraLoca]
salidaDeAmigos = Itinerario "Salida De Amigos" 1 [jarraLoca, (flip hacerseAmigo) robertoCarlos, tintico, soda 1]
itinerarioVacio = Itinerario "" 1 []


-- Definimos comoEsta con guardas
comoEsta (Cliente _ resistencia amigos _) 
    | resistencia > 50 = "Fresco"
    | length amigos > 1 = "Piola"
    | otherwise = "Duro"

-- Definimos esAmigo, que recae en la igualdad de Eq definida por igualdad de nombres
esAmigo cliente posibleAmigo = elem posibleAmigo (_amigos cliente)
-- Definimos puedeHacerseAmigo viendo que no sea si mismo ni sea amigo
-- Point Free: puedeHacerseAmigo cliente posibleNuevoAmigo  = cliente /= posibleNuevoAmigo && (not (esAmigo cliente posibleNuevoAmigo))
puedeHacerseAmigo cliente posibleNuevoAmigo  = cliente /= posibleNuevoAmigo && ((.)(.)(.) not esAmigo) cliente posibleNuevoAmigo
-- Definimos la funcion hacerseAmigo, que agrega un cliente a la lista de amigos de otro cliente
hacerseAmigo (Cliente nombre resis amigos tragosTomados) nuevoAmigo  = (Cliente nombre resis (nuevoAmigo : amigos) tragosTomados)
-- Definimos agregarAmigo
agregarAmigo cliente posibleNuevoAmigo 
    | puedeHacerseAmigo cliente posibleNuevoAmigo = hacerseAmigo cliente posibleNuevoAmigo
    | otherwise = cliente

nombrePorSoda nombre fuerza = ['e'] ++ (replicate fuerza 'r') ++ ['p'] ++ nombre

editarResistencia nuevaResistencia (Cliente nombre _ amigos tragosTomados) = Cliente nombre nuevaResistencia amigos tragosTomados
sumarResistencia resistenciaASumar cliente = editarResistencia  ((_resistencia cliente)+resistenciaASumar) cliente
restarResistencia resistenciaARestar = sumarResistencia (-resistenciaARestar)

--editarResistenciaAAmigos nuevaResistencia amigos = map (editarResistencia nuevaResistencia) amigos 
editarResistenciaAAmigos nuevaResistencia = map (editarResistencia nuevaResistencia)
sumarResistenciaAAmigos resistenciaASumar = map (sumarResistencia resistenciaASumar)
restarResistenciaAAmigos resistenciaARestar = map (restarResistencia resistenciaARestar)

aplicarFuncionAAmigosDeCliente funcion (Cliente nombre resistencia amigos tragosTomados) = Cliente nombre resistencia (funcion amigos) tragosTomados
--editarResistenciaAAmigosDeCliente nuevaResistencia cliente = aplicarFuncionAAmigosDeCliente (editarResistenciaAAmigos nuevaResistencia) cliente
editarResistenciaAAmigosDeCliente nuevaResistencia = aplicarFuncionAAmigosDeCliente (editarResistenciaAAmigos nuevaResistencia)
--sumarResistenciaAAmigosDeCliente resistenciaASumar cliente = aplicarFuncionAAmigosDeCliente (editarResistenciaAAmigos resistenciaASumar) cliente
sumarResistenciaAAmigosDeCliente resistenciaASumar = aplicarFuncionAAmigosDeCliente (sumarResistenciaAAmigos resistenciaASumar)
--restarResistenciaAAmigosDeCliente resistenciaARestar cliente = aplicarFuncionAAmigosDeCliente (restarResistenciaAAmigos resistenciaARestar) cliente
restarResistenciaAAmigosDeCliente resistenciaARestar = aplicarFuncionAAmigosDeCliente (restarResistenciaAAmigos resistenciaARestar)

grogXD cliente = restarResistencia (_resistencia cliente) cliente
jarraLoca cliente = (((restarResistenciaAAmigosDeCliente 10).(restarResistencia 10)) cliente)
klusener gusto cliente = restarResistencia (length gusto) cliente 
tintico cliente = sumarResistencia (5*(length (_amigos cliente))) cliente 
soda fuerza (Cliente nombre resistencia amigos tragosTomados)  = Cliente (nombrePorSoda nombre fuerza) resistencia amigos tragosTomados

-- agregarTrago solo agrega el trago a la lista de tragosTomados
agregarTrago funcionTrago (Cliente nombre resistencia amigos tragosTomados) = Cliente nombre resistencia amigos (funcionTrago:tragosTomados)
-- tomarTrago toma el trago y lo agrega usando agregarTrago
--tomarTrago funcionTrago cliente = ((agregarTrago funcionTrago).funcionTrago) cliente
tomarTrago funcionTrago = ((agregarTrago funcionTrago).funcionTrago)
-- Aplica tomarTragos sobre cada elemento de tragos, aplicando al trago de mas a la derecha el cliente.
tomarTragos tragos cliente = foldr tomarTrago cliente tragos
dameOtro cliente = head (_tragosTomados cliente) cliente

-- puedeTomar devuelve True si la resistencia luego de tomar el trago es mayor a 0 y sino False
puedeTomar trago = (>0).(_resistencia.trago)
-- cualesPuedeTomar aplica puede tomar 
cualesPuedeTomar cliente = map ((flip puedeTomar) cliente)
cuantasPuedeTomar cliente tragos = length (filter (==True) (cualesPuedeTomar cliente tragos))

itinerarioMasIntenso itinerario1 itinerario2 | itinerario1 > itinerario2 = itinerario1
    | otherwise = itinerario2
itinerarioMasIntensoEntreMuchos itinerarios = foldr itinerarioMasIntenso itinerarioVacio itinerarios
--realizarItinerario itinerario cliente = tomarTragos (_tragos itinerario) cliente
realizarItinerario itinerario = tomarTragos (_tragos itinerario)
--realizarItinerarioMasIntensoEntreMuchos itinerarios cliente = realizarItinerario (itinerarioMasIntensoEntreMuchos itinerarios) cliente
realizarItinerarioMasIntensoEntreMuchos itinerarios cliente = realizarItinerario (itinerarioMasIntensoEntreMuchos itinerarios) cliente

-- Funcion rescatarse utilizando guardas
--rescatarse tiempo cliente 
--    | tiempo > 3 = sumarResistencia 200 cliente
--    | otherwise = sumarResistencia 100 cliente
rescatarse tiempo 
    | tiempo > 3 = sumarResistencia 200
    | otherwise = sumarResistencia 100


-- Point Free: consultaItinerario1 cliente = klusener "Huevo" (rescatarse 2 (klusener "Chocolate" (jarraLoca cliente))) 
consultaItinerario1 cliente = ((klusener "Huevo").(rescatarse 2).(klusener "Chocolate").jarraLoca) cliente



-- Jarra Popular

--Primero hago la funcion agregarAmigos que recibe un cliente y una lista de clientes y agrega los clientes de la lista al cliente primer parametro
agregarAmigos:: Cliente->[Cliente]->Cliente
agregarAmigos cliente amigos  = foldl agregarAmigo cliente amigos

{-
-- A continuacion analizo algunos casos particulares de jarraPopular, para luego abstraerme

--jarraPopular 1 cliente = foldr agregarAmigoDeAmigo cliente (_amigos cliente)
jarraPopular 1 cliente = agregarAmigoDeAmigos cliente (_amigos cliente)

--agregarAmigosDeAmigo cliente amigo = agregarAmigos cliente (_amigos amigo)
--agregarAmigosDeAmigo cliente amigo = ((agregarAmigos cliente)._amigos) amigo
--agregarAmigosDeAmigo cliente amigo = ((.) (.)) agregarAmigos cliente _amigos amigo
agregarAmigosDeAmigo cliente = ((.) (.)) agregarAmigos cliente _amigos

--agregarAmigosDeAmigos cliente [] = agregarAmigosDeAmigos cliente (_amigos cliente)
--agregarAmigosDeAmigos cliente [amigo] = agregarAmigosDeAmigo cliente amigo
--agregarAmigosDeAmigos cliente (amigo:amigos) = agregarAmigosDeAmigos (agregarAmigosDeAmigo cliente amigo)) amigos
agregarAmigosDeAmigos cliente [] = foldr agregarAmigosDeAmigo cliente (_amigos cliente)
--agregarAmigosDeAmigos cliente amigos = foldr agregarAmigosDeAmigo cliente amigos
agregarAmigosDeAmigos cliente = foldr agregarAmigosDeAmigo cliente


--jarraPopular 2 cliente = foldl agregarAmigoDeAmigo (foldr agregarAmigoDeAmigos cliente (_amigos cliente)) (_amigos cliente)
--jarraPopular 2 cliente = foldl agregarAmigoDeAmigo (jarraPopular 1 cliente) (_amigos cliente)
jarraPopular 2 cliente = agregarAmigosDeAmigosDeAmigos (jarraPopular 1 cliente) (_amigos cliente)

--agregarAmigosDeAmigosDeAmigo cliente amigo = agregarAmigosDeAmigos cliente (_amigos amigo)
--agregarAmigosDeAmigosDeAmigo cliente amigo = ((agregarAmigosDeAmigos cliente)._amigos) amigo
--agregarAmigosDeAmigosDeAmigo cliente amigo = ((.) (.)) agregarAmigosDeAmigos cliente _amigos amigo
agregarAmigosDeAmigosDeAmigo cliente = ((.) (.)) agregarAmigosDeAmigos cliente _amigos

--agregarAmigosDeAmigosDeAmigos cliente [] = foldl agregarAmigosDeAmigosDeAmigo cliente (_amigos cliente)
--agregarAmigosDeAmigosDeAmigos cliente amigos = foldl agregarAmigosDeAmigosDeAmigo cliente amigos
agregarAmigosDeAmigosDeAmigos cliente [] = foldl agregarAmigosDeAmigosDeAmigo cliente (_amigos cliente)
--agregarAmigosDeAmigosDeAmigos cliente amigos = foldl agregarAmigosDeAmigosDeAmigo cliente amigos
agregarAmigosDeAmigosDeAmigos cliente = foldl agregarAmigosDeAmigosDeAmigo cliente

-}
-- Hago funciones que generalizen las posibles agregarAmigosdeAmigo y agregarAmigosdeAmigos

agregarAmigosDeAmigoRecur :: Int->Cliente->Cliente->Cliente
--agregarAmigosDeAmigoRecur 1 cliente amigo = agregarAmigos cliente (_amigos amigo)
--agregarAmigosDeAmigoRecur 1 cliente amigo = ((.) (.)) agregarAmigos cliente _amigos amigo
agregarAmigosDeAmigoRecur 1 cliente = ((.) (.)) agregarAmigos cliente _amigos
--agregarAmigosDeAmigoRecur 2 cliente = ((.) (.)) (agregarAmigosDeAmigosRecur 1) cliente _amigos
agregarAmigosDeAmigoRecur nivel cliente = ((.) (.)) (agregarAmigosDeAmigosRecur (nivel-1)) cliente _amigos

agregarAmigosDeAmigosRecur :: Int->Cliente->[Cliente]->Cliente
agregarAmigosDeAmigosRecur 1 cliente [] = foldl (agregarAmigosDeAmigoRecur 1) cliente (_amigos cliente)
agregarAmigosDeAmigosRecur 1 cliente amigos = foldl (agregarAmigosDeAmigoRecur 1) cliente amigos
--agregarAmigosDeAmigosRecur 2 cliente [] = foldl (agregarAmigosDeAmigoRecur 2) cliente (_amigos cliente)
--agregarAmigosDeAmigosRecur 2 cliente amigos = foldl (agregarAmigosDeAmigoRecur 2)  cliente amigos
agregarAmigosDeAmigosRecur nivel cliente [] = foldl (agregarAmigosDeAmigoRecur nivel) cliente (_amigos cliente)
agregarAmigosDeAmigosRecur nivel cliente amigos = foldl (agregarAmigosDeAmigoRecur nivel) cliente amigos

-- Hago la funcion jarraPopular usando las funciones que arme
jarraPopular :: Int->Cliente->Cliente
jarraPopular 0 cliente = cliente
jarraPopular 1 cliente = agregarAmigosDeAmigosRecur 1 cliente []
--jarraPopular 2 cliente = agregarAmigosDeAmigosRecur 2 (jarraPopular 1 cliente) (_amigos cliente)
jarraPopular espirituosidad cliente = agregarAmigosDeAmigosRecur espirituosidad (jarraPopular (espirituosidad-1) cliente) (_amigos cliente)


{-
Casos de Prueba

****** Punto 1b
-- length (_tragosTomados (tomarTrago (soda 3) marcos))
2
-- _resistencia (tomarTrago (soda 3) marcos)
4

****** Punto 1c
-- _nombre (tomarTrago (soda 2) (tomarTrago (soda 1) rodri))
"errperpRodri"
-- _resistencia (tomarTrago (jarraLoca) (tomarTrago (tintico) (tomarTrago (klusener "Huevo") marcos)))
30
-- length (_tragosTomados (tomarTrago (jarraLoca) (tomarTrago (tintico) (tomarTrago (klusener "Huevo") marcos))))
4

****** Punto 1d
-- dameOtro ana
Exception: Prelude.head: empty list
-- length (_tragosTomados (dameOtro marcos))
1
-- _resistencia (dameOtro marcos)
34
-- length (_tragosTomados (tomarTrago (soda 1) rodri))
2
-- _nombre (tomarTrago (soda 1) rodri)
"erpRodri"

****** Punto 2b
-- cuantasPuedeTomar rodri [grogXD, tintico, klusener "Frutilla"]
2

****** Punto 3b
-- length (_amigos (realizarItinerarioMasIntensoEntreMuchos [salidaDeAmigos, itinerarioBasico, mezclaExplosiva] rodri))
1
-- _nombre (realizarItinerarioMasIntensoEntreMuchos [salidaDeAmigos, itinerarioBasico, mezclaExplosiva] rodri)
"erpRodri"
-- _resistencia (realizarItinerarioMasIntensoEntreMuchos [salidaDeAmigos, itinerarioBasico, mezclaExplosiva] rodri)
45
-- _resistencia (head (_amigos (realizarItinerarioMasIntensoEntreMuchos [salidaDeAmigos, itinerarioBasico, mezclaExplosiva] rodri)))
155
-- length (_tragosTomados (realizarItinerarioMasIntensoEntreMuchos [salidaDeAmigos, itinerarioBasico, mezclaExplosiva] rodri))
5

****** Punto 4a
-- intensidad salidaDeAmigos
0.6
-- intensidad mezclaExplosiva
1.6
-- intensidad itinerarioBasico
4.0

****** Punto 4b
-- length (_amigos (realizarItinerarioMasIntensoEntreMuchos [salidaDeAmigos, itinerarioBasico, mezclaExplosiva] rodri))
1
-- _nombre (head (_amigos (realizarItinerarioMasIntensoEntreMuchos [salidaDeAmigos, itinerarioBasico, mezclaExplosiva] rodri)))
"Roberto Carlos"
-- _nombre (realizarItinerarioMasIntensoEntreMuchos [salidaDeAmigos, itinerarioBasico, mezclaExplosiva] rodri)
erprodri
-- _resistencia (realizarItinerarioMasIntensoEntreMuchos [salidaDeAmigos, itinerarioBasico, mezclaExplosiva] rodri)
45
-- itinerarioMasIntensoEntreMuchos [mezclaExplosiva, itinerarioBasico, salidaDeAmigos]
salidaDeAmigos

****** Punto 6
-- length (_amigos(jarraPopular  0 agregarAmigo robertoCarlos ana))
1
-- length (_amigos(jarraPopular  3 agregarAmigo robertoCarlos ana))
3
-- length (_amigos(jarraPopular  4 (agregarAmigo robertoCarlos (agregarAmigo cristian ana))))
4

-}