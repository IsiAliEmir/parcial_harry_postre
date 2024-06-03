module Library where
import PdePreludat

--  1. Postres

{-  A) Modelar los postres. Un mismo postre puede tener muchos sabores, tiene un peso y se sirve a cierta temperatura.
Por ejemplo, un bizcocho borracho de fruta y crema de 100 gramos servido a 25°C. -}

data Postre = UnPostre {
    sabores :: [Sabor],
    peso :: Number,
    temp :: Number
} deriving (Show, Eq)

type Sabor = String

{-  B) Modelar los hechizos, sabiendo que deberían poderse agregar más sin modificar el código existente. 
Por ahora existen los siguientes:
    ● Incendio: calienta el postre 1 grado y lo hace perder 5% de su peso.
    ● Immobulus: congela el postre, llevando su temperatura a 0.
    ● Wingardium Leviosa: levanta el postre en el aire y lo deja caer, lo que agrega a sus sabores el sabor “concentrado”. 
    Además, pierde 10% de su peso.
    ● Diffindo: Corta el postre, disminuyendo su peso en el porcentaje indicado. 
    ● Riddikulus: Requiere como información adicional un sabor y lo agrega a los sabores que tiene un postre, pero invertido.
    ● Avada kedavra: Hace lo mismo que el immobulus pero además hace que el postre pierda todos sus sabores. -}

type Hechizo = Postre -> Postre

incendio :: Hechizo
incendio postre = postre {peso = peso postre * 0.95, temp = temp postre + 1}

immobulus :: Hechizo
immobulus postre = postre {temp = 0}

wingardiumLeviosa :: Hechizo
wingardiumLeviosa postre = postre {sabores = sabores postre ++ ["Concentrado"], peso = peso postre * 0.9}

diffindo :: Number -> Hechizo
diffindo porcentaje postre = postre {peso = peso postre * (1 - porcentaje/100)}

ridikkulus :: Sabor -> Hechizo
ridikkulus sabor postre = postre {sabores = sabores postre ++ [reverse sabor]}

avadaKedavra :: Hechizo
avadaKedavra postre = immobulus postre {sabores = []}

{- C) Dado un conjunto de postres en la mesa, saber si hacerles un determinado hechizo los dejará listos 
(un postre está listo cuando pesa algo más que cero, tiene algún sabor y además no está congelado). 
Por ejemplo, si en la mesa está el bizcocho mencionado anteriormente y una tarta de melaza de 0 grados y 50 gramos,
y les hago el hechizo incendio, quedan listos, pero si les hago el hechizo riddikulus con el sabor “nomil” no, 
porque la tarta sigue congelada. -}

type Mesa = [Postre]

losDejaListos :: Hechizo -> Mesa -> Bool
losDejaListos hechizo = all (estaListo . hechizo)

estaListo :: Postre -> Bool
estaListo postre = all ($ postre) condiciones

{-  ● En este caso considero que no hace falta pasar la lista de condiciones como parámetro de estaListo,
    pues no habrá más de una lista de condiciones en este problema (se puede redefinir pero es siempre una sola).
    ● ($ postre) es equivalente a (\f -> f postre): ambas expresiones esperan una función que recibe un Postre, y se lo pasan.
    Esto es posible dado el tipo de all, que toma directamente cada función individual de la lista de condiciones.
    ● Se podría hacer una función "aplicarCondicion postre condicion = condicion postre", pero por ahora me parece redundante. -}

condiciones :: [Postre -> Bool]
condiciones = [pesaAlgo, tieneSabor, not . estaCongelado]

pesaAlgo :: Postre -> Bool
--pesaAlgo postre = peso postre /= 0
pesaAlgo = (/= 0) . peso

tieneSabor :: Postre -> Bool
--tieneSabor postre = sabores postre /= []
tieneSabor = (/= []) . sabores

estaCongelado :: Postre -> Bool
--estaCongelado postre = temp postre == 0
estaCongelado = (== 0) . temp

{- D) Dado un conjunto de postres en la mesa, conocer el peso promedio de los postres listos. -}

promediarPesoListos :: Mesa -> Number
promediarPesoListos = promediarPeso . filter estaListo

promediarPeso :: Mesa -> Number
promediarPeso mesa = promediar (map peso mesa)

promediar :: [Number] -> Number
promediar lista = sum lista / length lista

--  2. Magos

{-  De un mago se conocen sus hechizos aprendidos y la cantidad de horrorcruxes que tiene. -}
data Mago = UnMago {
    hechizos :: [Hechizo],
    horrorcruxes :: Number
} deriving (Show)

{-  A) Hacer que un mago asista a la clase de defensa contra las cocinas oscuras y practique con un hechizo sobre un postre 
(se espera obtener el mago). Cuando un mago practica con un hechizo, lo agrega a sus hechizos aprendidos. Además, si el 
resultado de usar el hechizo en el postre es el mismo que aplicarle “avada kedavra” al postre, entonces suma un horrorcrux. -}

practicar :: Hechizo -> Mago -> Postre -> Mago
practicar hechizo mago postre
    | esComoAvada hechizo postre = (sumarHorrorcrux . sumarHechizo hechizo) mago
    | otherwise = sumarHechizo hechizo mago

esComoAvada :: Hechizo -> Postre -> Bool
esComoAvada hechizo postre = hechizo postre == avadaKedavra postre

sumarHechizo :: Hechizo -> Mago -> Mago
sumarHechizo hechizo mago = mago {hechizos = hechizos mago ++ [hechizo]}

sumarHorrorcrux :: Mago -> Mago
sumarHorrorcrux mago = mago {horrorcruxes = horrorcruxes mago + 1}

{-  B) Dado un postre y un mago obtener su mejor hechizo, que es aquel de sus hechizos que deja al postre con más cantidad 
de sabores luego de usarlo. -}

mejorHechizo :: Mago -> Postre -> Hechizo
mejorHechizo mago postre = foldl1 (elMejor postre) (hechizos mago)

elMejor :: Postre -> Hechizo -> Hechizo -> Hechizo
elMejor postre hechizo1 hechizo2
    | hechizo1 postre `tieneMasSaboresQue` hechizo2 postre = hechizo1
    | otherwise = hechizo2

tieneMasSaboresQue :: Postre -> Postre -> Bool
postre1 `tieneMasSaboresQue` postre2 = (length . sabores) postre1 > (length . sabores) postre2

--Nota: la defino de forma infija para que quede claro que no devuelvo el postre con más sabores, sino que evalúo si el
--primero tiene más sabores que el segundo.

--  3. Infinita magia 

{-  A) Construir una lista infinita de postres, y construir un mago con infinitos hechizos. -}

infinitosBizcochos :: [Postre]
infinitosBizcochos = repeat bizcochoBorracho

infinitasTartas :: [Postre]
infinitasTartas = repeat tartaMelaza

mesaInfinita :: [Postre]
mesaInfinita = infinitosBizcochos ++ infinitasTartas

magoInfinito :: Mago
magoInfinito = UnMago {hechizos = repeat incendio, horrorcruxes = 0}

{-  B) Suponiendo que hay una mesa con infinitos postres, y pregunto si algún hechizo los deja listos, 
¿Existe alguna consulta que pueda hacer para que me sepa dar una respuesta? Justificar conceptualmente. -}

{-  Rta.: sí, existe. Dada una mesa con infinitos postres, puedo preguntar si Immobulus los deja listos, ya que dicha evaluación
    siempre devolverá False, independientemente de cuál sea el primer Postre. Con lo cual, ya tomando el primer Postre de la mesa
    se devuelve False y toda la evaluación conjunta (el "all", o sea si TODOS están listos) da False (EVALUACIÓN DIFERIDA). -}

{-  C) Suponiendo que un mago tiene infinitos hechizos ¿Existe algún caso en el que se puede encontrar el mejor hechizo? 
Justificar conceptualmente. -}

{-  Rta.: no existe, puesto que para encontrar el mejor hechizo se deben comparar necesariamente TODOS los hechizos de la lista.
En este caso, la evaluación diferida (o short circuit evaluation) no nos "salva" de recorrer toda la lista. -}

------------------------------------------------Instanciaciones-----------------------------------------------
bizcochoBorracho :: Postre
bizcochoBorracho = UnPostre {sabores = ["Fruta", "Crema"], peso = 100, temp = 25}

tartaMelaza :: Postre
tartaMelaza = UnPostre {sabores = ["Melaza"], peso = 50, temp = 0}

mesa1 :: Mesa
mesa1 = [bizcochoBorracho, tartaMelaza]

lupin :: Mago
lupin = UnMago {hechizos = [immobulus, wingardiumLeviosa, ridikkulus "enraC"], horrorcruxes = 0}

voldy :: Mago
voldy = UnMago {hechizos = [incendio, immobulus, wingardiumLeviosa, diffindo 50, avadaKedavra], horrorcruxes = 6}
