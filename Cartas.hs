-- Wilfredo Graterol
-- Diego Peña

import Prelude
import Data.List
import System.Random

data Palo = Treboles | Diamantes | Picas | Corazones

instance Show Palo where
    show Treboles = "♣"
    show Diamantes = "♦"
    show Picas = "♠"
    show Corazones = "♥"

instance Eq Palo where 
    (==) Treboles Treboles = True
    (==) Diamantes Diamantes = True
    (==) Picas Picas = True
    (==) Corazones Corazones = True
    (==) _ _ = False
    
    (/=) a b = not (a==b)

data Rango = N Int | Jack | King | Queen | Ace

instance Eq Rango where
    (==) Jack Jack = True
    (==) Queen Queen = True
    (==) King King = True
    (==) Ace Ace = True
    (==) (N a) (N b) = a==b
    (==) _ _ = False
    
    (/=) a b = not (a==b)

instance Show Rango where
    show (N a) = show a
    show Jack = "J"
    show King = "K"
    show Queen = "Q"
    show Ace = "A" 

data Carta = Carta {
                    rango :: Rango,
                    palo :: Palo
                   }

instance Eq Carta where
    (==) (Carta {rango = rga, palo = pa}) Carta {rango = rgb, palo = pb} = if rga==rgb && pa==pb then True else False

    (/=) a b = not (a==b)

instance Show Carta where
    show (Carta {rango = rg, palo = p}) = show p ++ "" ++ show rg

data Jugador = Dealer | Player deriving(Show, Read)

newtype Mano = Mano [Carta] --deriving(Show)

instance Show Mano where
    show (Mano c) = concatMap (show) c


{--
Funciones de mano
--}

vacia :: Mano
vacia = Mano []

{-- 
baraja :: Mano
baraja = Mano $ concatMap (crearPalo) [Treboles, Diamantes, Picas, Corazones]
    where crearPalo p = map (\rg -> Carta {rango = rg, palo = p}) $ map (N) [1..9] ++ [Jack, King, Queen, Ace]
--}

-- Crea listas correspondientes a cada palo y luego las une en una sola para formar la baraja
baraja :: Mano
baraja =    
    let rangos = map (N) [2..10] ++ [Jack, King, Queen, Ace]
        crearPalo p = map (\rg -> Carta {rango = rg, palo = p}) rangos
    in Mano $ concatMap (crearPalo) [Treboles, Diamantes, Picas, Corazones]

cantidadCartas :: Mano -> Int
cantidadCartas (Mano c) = length c

valorCarta :: Carta -> Int
valorCarta (Carta (N i) _) = i
valorCarta (Carta Ace _)   = 11
valorCarta (Carta _ _)     = 10

-- Antes podía haber casos donde tomara unos aces como 11 y otros como 1, pero ya lo arreglé
-- No quedó tan bonito sin embargo
valor :: Mano -> Int
valor (Mano c) =
    let (sumaDeNoAces, cantidadAces) = (\(x, y) -> (sum x, length y)) $ span (/= 11) $ sort (map (valorCarta) c)
        (suma11, suma1) = (sumaDeNoAces + 11 * cantidadAces, sumaDeNoAces + cantidadAces)
    in  if suma11 <= 21 then suma11 else suma1

busted :: Mano -> Bool
busted mano = if valor mano > 21 then True else False


--En principio no le voy a poner que verifique que la mano tenga dos cartas, porque la idea sería solo llamarla
-- cuando la mano tenga dos cartas
blackjack :: Mano -> Bool
blackjack mano = if valor mano == 21 then True else False

-- Verificar que no hay un bust antes
ganador :: Mano -> Mano -> Jugador
ganador manoDealer manoPlayer = if valor manoDealer >= valor manoPlayer then Dealer else Player

separar :: Mano -> (Mano, Carta, Mano)
separar (Mano []) = error "No hay cartas en la mano"
separar (Mano c) =
    let mitad      = length c `div` 2
        (izq, der) = (take mitad c, drop mitad c)
    in (Mano izq, head der, Mano (tail der))

barajar :: StdGen -> Mano -> Mano
barajar gen (Mano c) = Mano (barajarAux gen c)

barajarAux :: StdGen -> [Carta] -> [Carta]
barajarAux _ [] = []
barajarAux gen listaCartas =
    let (index, newGen) = randomR(1, subtract 1 $ length listaCartas) gen :: (Int, StdGen)
        carta = listaCartas !! index
    in carta:(barajarAux newGen $ delete carta listaCartas)


inicialLambda :: Mano -> (Mano, Mano)
inicialLambda (Mano c) = (Mano $ take 2 c, Mano $ drop 2 c)


data Mazo = Vacio | Mitad Carta Mazo Mazo

data Eleccion = Izquierdo | Derecho

-- instance Read Eleccion where 
    -- readsPrec a =
    -- readPrec a = 

desdeMano :: Mano -> Mazo
desdeMano (Mano []) = Vacio
desdeMano mano = desdeManoAux $ separar mano

desdeManoAux :: (Mano, Carta, Mano) -> Mazo
desdeManoAux (Mano [], center, Mano []) = Mitad center Vacio Vacio
desdeManoAux (Mano [], center, right) = Mitad center Vacio (desdeMano right)
desdeManoAux (left, center, Mano []) = Mitad center (desdeMano left) Vacio
desdeManoAux (left, center, right) =  Mitad center (desdeMano left) (desdeMano right)

puedePicar :: Mazo -> Bool
puedePicar (Vacio) = False
puedePicar (Mitad _ Vacio Vacio) = False
puedePicar (Mitad _ _ _) = True 

aplanar :: Mazo -> Mano
aplanar Vacio = vacia
aplanar (Mitad carta left right) = let
    Mano listaIzq = aplanar left
    listaCent = [carta]
    Mano listaDer = aplanar right
    in Mano (listaIzq++listaCent++listaDer)

reconstruir :: Mazo ->Mano ->Mazo
reconstruir mazo mano = desdeMano $ (\ (Mano listaMazo) (Mano listaMano) -> Mano (listaMazo \\ listaMano)) (aplanar mazo) mano

-- Nota: Aqui se depuran las cartas repetidas
robar :: Mazo -> Mano -> Eleccion -> Maybe (Mazo,Mano)
robar Vacio _ _ = Nothing
robar (Mitad center Vacio Vacio) _ _ = Nothing
robar (Mitad center left Vacio) _ Derecho = Nothing
robar (Mitad center Vacio right) _ Izquierdo = Nothing
robar (Mitad center (Mitad leftCenter leftLeft rightLeft) right) (Mano listaMano) Izquierdo = 
    Just (reconstruir (Mitad center (Mitad leftCenter leftLeft rightLeft) right) (Mano (leftCenter:listaMano)), (Mano (leftCenter:listaMano)))
robar (Mitad center left (Mitad rightCenter rightLeft rightRight)) (Mano listaMano) Derecho = 
    Just (reconstruir (Mitad center left (Mitad rightCenter rightLeft rightRight)) (Mano (rightCenter:listaMano)), (Mano (rightCenter:listaMano)))

juegaLambda :: Mazo ->Mano ->Maybe Mano
juegaLambda Vacio _ = Nothing
juegaLambda mazo mano = let
    Mano listaMazo = aplanar mazo
    Mano listaMano = mano
    in if valor (Mano (listaMazo++listaMano)) <= 16 then Nothing else Just $ auxJuegaLambda (17 - valor mano) (aplanar mazo) $ mano
-- Check this again i in case of aces, it's still not correct
auxJuegaLambda :: Int -> Mano -> Mano -> Mano
auxJuegaLambda deseado (Mano (x:xs)) (Mano listaMano) = if deseado < 0 then (Mano (x:xs)) else auxJuegaLambda (17-(valor (Mano (x:listaMano)))) (Mano xs) (Mano (x:listaMano))

-- Esto es para probar las funciones sobre manos que no sean la baraja completa o vacía
manoPrueba = Mano [
                    Carta Jack Treboles,
                    Carta (N 2) Diamantes
                  ]

manoPrueba2 = Mano [
                    Carta Jack Treboles,
                    Carta (N 2) Diamantes,
                    Carta Ace Picas,
                    Carta (N 10) Diamantes
                  ]