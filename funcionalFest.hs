import Text.Show.Functions

--tipo de dato del cliente esta compuesto por un string que representa el nombre, un int que representa resistencia y una lista para repreentar sus amigos.--
data Cliente = Cliente {_nombre::String, _resistencia::Int, _amigos::[Cliente], _bebidasTomadas::[Cliente->Cliente]} deriving (Show) 

--Definimos manualmente que Cliente pertenece a Eq, y definimos la igualdad usando la igualdad de los nombres
instance Eq Cliente where  
    cliente1 == cliente2 = (_nombre cliente1) == (_nombre cliente2)

-- Definimos Clientes de prueba
rodri = Cliente "Rodri" 55 [] []
marcos = Cliente "Marcos" 40 [rodri] []
cristian = Cliente "Cristian" 2 [] []
ana = Cliente "Ana" 120 [marcos, rodri] []

--Definimos comoEsta con guardas
comoEsta (Cliente _ resistencia amigos _) 
    | resistencia > 50 = "fresco"
    | length amigos > 1 = "piola"
    | otherwise = "duro"

-- Definimos esAmigo, que recae en la igualdad de Eq definida por igualdad de nombres
esAmigo cliente posibleAmigo = elem posibleAmigo (_amigos cliente)
-- Definimos puedeHacerseAmigo viendo que no sea si mismo ni sea amigo
puedeHacerseAmigo cliente posibleNuevoAmigo  = cliente /= posibleNuevoAmigo && ((.)(.)(.) not esAmigo) cliente posibleNuevoAmigo
-- Definimos la funcion hacerseAmigo, que agrega un cliente a la lista de amigos de otro cliente
hacerseAmigo (Cliente nombre resis amigos bebidasTomadas) nuevoAmigo  = (Cliente nombre resis (nuevoAmigo : amigos) bebidasTomadas)
--Definimos agregarAmigo
agregarAmigo cliente posibleNuevoAmigo 
    | puedeHacerseAmigo cliente posibleNuevoAmigo = hacerseAmigo cliente posibleNuevoAmigo
    | otherwise = cliente

grogXD cliente = restarResistencia (_resistencia cliente) cliente
jarraLoca cliente = (((restarResistenciaAAmigosDeCliente 10).(restarResistencia 10)) cliente)
klusener gusto cliente = editarResistencia (length gusto) cliente 
tintico cliente = sumarResistencia (5*(length (_amigos cliente))) cliente 
soda fuerza (Cliente nombre resistencia amigos bebidasTomadas)  = Cliente (nombrePorSoda nombre fuerza) resistencia amigos bebidasTomadas

editarResistencia nuevaResistencia (Cliente nombre _ amigos bebidasTomadas) = Cliente nombre nuevaResistencia amigos bebidasTomadas
sumarResistencia resistenciaASumar cliente = editarResistencia  ((_resistencia cliente)+resistenciaASumar) cliente
restarResistencia resistenciaARestar = sumarResistencia (-resistenciaARestar)

editarResistenciaAAmigos nuevaResistencia = map (editarResistencia nuevaResistencia)
sumarResistenciaAAmigos resistenciaASumar = map (sumarResistencia resistenciaASumar)
restarResistenciaAAmigos resistenciaARestar = map (restarResistencia resistenciaARestar)

aplicarFuncionAAmigosDeCliente funcion (Cliente nombre resistencia amigos bebidasTomadas) = Cliente nombre resistencia (funcion amigos) bebidasTomadas
editarResistenciaAAmigosDeCliente nuevaResistencia = aplicarFuncionAAmigosDeCliente (editarResistenciaAAmigos nuevaResistencia)
sumarResistenciaAAmigosDeCliente resistenciaASumar cliente = editarResistenciaAAmigosDeCliente ((_resistencia cliente)+resistenciaASumar) cliente
restarResistenciaAAmigosDeCliente resistenciaARestar = restarResistenciaAAmigosDeCliente (-resistenciaARestar)

nombrePorSoda nombre fuerza = ['e'] ++ (replicate fuerza 'r') ++ ['p'] ++ nombre

rescatarse tiempo 
    | tiempo > 3 = sumarResistencia 200
    | otherwise = sumarResistencia 100

consultaItinerario1 cliente = klusener "huevo" (rescatarse 2 (klusener "chocolate" (jarraLoca cliente))) 

