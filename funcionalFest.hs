import Text.Show.Functions
import Data.List 

--tipo de dato del cliente esta compuesto por un string que representa el nombre, un int que representa resistencia y una lista para repreentar sus amigos.--
data Cliente = Cliente {_nombre::String, _resistencia::Int, _amigos::[Cliente], _tragosTomados::[Cliente->Cliente]} deriving (Show) 

--Definimos manualmente que Cliente pertenece a Eq, y definimos la igualdad usando la igualdad de los nombres
instance Eq Cliente where  
    --No es necesario definir /= ya que esta definida como not ==
    cliente1 == cliente2 = (_nombre cliente1) == (_nombre cliente2)

data Itinerario = Itinerario {_nombreItinerario::String, _duracion::Float, _tragos::[Cliente->Cliente]} deriving (Show) 

intensidad (Itinerario nombre duracion tragos) = (genericLength tragos) / duracion

-- Definimos manualmente que Itinerario pertenece a Eq
instance Eq Itinerario where  
    itinerario1 == itinerario2 = (_nombre itinerario1) == (_nombre itinerario2)

-- Definimos manualmente que Itinerario pertenece a Ord
instance Ord Itinerario where  
    -- (>) y (<) estan definidos en base a compare, por lo que no hacen falta
    -- Definimos la comparacion de itinerarios como la comparacion de sus intensidades
    compare itinerario1 itinerario2 = compare (intensidad itinerario1) (intensidad itinerario2)

-- Definimos algunos Clientes
rodri = Cliente "Rodri" 55 [] [tintico]
marcos = Cliente "Marcos" 40 [rodri] [klusener "guinda"]
cristian = Cliente "Cristian" 2 [] []
ana = Cliente "Ana" 120 [marcos, rodri] [grogXD, jarraLoca]
robertoCarlos = Cliente "Roberto Carlos" 165 [] []
chuckNorris = Cliente "Chuck" 1000 [ana] [soda i |i<-[1,2..]]
-- B: Chuck no puede tomar otro trago(Nunca se alcanzara el ultimo trago tomado de la lista, es un intento de recorrer una lista infinita acotada inferiormente)
--D: Si, porque Haskell es lazy

-- Definimos algunos Itinerarios
mezclaExplosiva = Itinerario "Mezcla Explosiva" 2.5 [grogXD, grogXD, klusener "Huevo", klusener "Frutilla"]
itinerarioBasico = Itinerario "Itinerario Basico" 5 [klusener "Huevo", rescatarse 2, klusener "Chocolate"]
salidaDeAmigos = Itinerario "Salida De Amigos" 1 [soda 1, tintico, (flip hacerseAmigo) robertoCarlos, jarraLoca]
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

nombrePorSoda nombre fuerza = ['E'] ++ (replicate fuerza 'r') ++ ['p'] ++ nombre

editarResistencia nuevaResistencia (Cliente nombre _ amigos tragosTomados) = Cliente nombre nuevaResistencia amigos tragosTomados
sumarResistencia resistenciaASumar cliente = editarResistencia  ((_resistencia cliente)+resistenciaASumar) cliente
restarResistencia resistenciaARestar = sumarResistencia (-resistenciaARestar)

editarResistenciaAAmigos nuevaResistencia = map (editarResistencia nuevaResistencia)
sumarResistenciaAAmigos resistenciaASumar = map (sumarResistencia resistenciaASumar)
restarResistenciaAAmigos resistenciaARestar = map (restarResistencia resistenciaARestar)

aplicarFuncionAAmigosDeCliente funcion (Cliente nombre resistencia amigos tragosTomados) = Cliente nombre resistencia (funcion amigos) tragosTomados
editarResistenciaAAmigosDeCliente nuevaResistencia = aplicarFuncionAAmigosDeCliente (editarResistenciaAAmigos nuevaResistencia)
sumarResistenciaAAmigosDeCliente resistenciaASumar cliente = editarResistenciaAAmigosDeCliente ((_resistencia cliente)+resistenciaASumar) cliente
restarResistenciaAAmigosDeCliente resistenciaARestar = sumarResistenciaAAmigosDeCliente (-resistenciaARestar)

grogXD cliente = restarResistencia (_resistencia cliente) cliente
jarraLoca cliente = (((restarResistenciaAAmigosDeCliente 10).(restarResistencia 10)) cliente)
klusener gusto cliente = editarResistencia (length gusto) cliente 
tintico cliente = sumarResistencia (5*(length (_amigos cliente))) cliente 
soda fuerza (Cliente nombre resistencia amigos tragosTomados)  = Cliente (nombrePorSoda nombre fuerza) resistencia amigos tragosTomados
jarraPopular 0 cliente = cliente

-- agregarTrago solo agrega el trago a la lista de tragosTomados
agregarTrago funcionTrago (Cliente nombre resistencia amigos tragosTomados) = Cliente nombre resistencia amigos (funcionTrago:tragosTomados)
-- tomarTrago toma el trago y lo agrega usando agregarTrago
tomarTrago funcionTrago = ((agregarTrago funcionTrago).funcionTrago)
-- Aplica tomarTragos sobre cada elemento de tragos, aplicando al trago de mas a la derecha el cliente.
tomarTragos cliente tragos = foldr tomarTrago cliente tragos
-- Ejemplo: tomarTragos ana [grogXD, grogXD] 
dameOtro cliente = head (_tragosTomados cliente) cliente

-- puedeTomar devuelve True si la resistencia luego de tomar el trago es mayor a 0 y sino False
puedeTomar trago = (>0).(_resistencia.trago)
-- cualesPuedeTomar aplica puede tomar 
cualesPuedeTomar cliente = map ((flip puedeTomar) cliente)
cuantasPuedeTomar  cliente tragos = length (filter (==True) (cualesPuedeTomar cliente tragos))

itinerarioMasIntenso itinerario1 itinerario2 | itinerario1 > itinerario2 = itinerario1
    | otherwise = itinerario2
itinerarioMasIntensoEntreMuchos itinerarios = foldr itinerarioMasIntenso itinerarioVacio itinerarios
realizarItinerario itinerario cliente = tomarTragos cliente (_tragos itinerario)
realizarItinerarioMasIntensoEntreMuchos itinerarios cliente = realizarItinerario (itinerarioMasIntensoEntreMuchos itinerarios) cliente
-- realizarItinerarioMasIntensoEntreMuchos [salidaDeAmigos, itinerarioBasico]

-- Funcion rescatarse utilizando guardas
rescatarse tiempo 
    | tiempo > 3 = sumarResistencia 200
    | otherwise = sumarResistencia 100


-- Point Free: consultaItinerario1 cliente = klusener "Huevo" (rescatarse 2 (klusener "Chocolate" (jarraLoca cliente))) 
consultaItinerario1 cliente = ((klusener "Huevo").(rescatarse 2).(klusener "Chocolate").jarraLoca) cliente

