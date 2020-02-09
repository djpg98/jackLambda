import System.IO
import System.Random
import System.Directory
import Control.DeepSeq
import Cartas

data GameState = GS {
                    juegosJugados :: Int,
                    victoriasLambda :: Int,
                    nombre :: String,
                    generador :: StdGen,
                    dinero :: Int,
                    objetivo :: Int,
                    apuesta :: Int
                    }

instance Read GameState where
    readsPrec _ file = (\[jj, vL, nomb, gen, din, obj, ap] -> 
        [((GS (read jj) (read vL) nomb (read gen) (read din) (read obj) (read ap)), " ")]) $ lines file

instance Show GameState where
    show game = (show . juegosJugados $ game) ++ "\n" ++ (show . victoriasLambda $ game) ++ "\n" ++ (nombre $ game) ++ 
                "\n" ++ (show . generador $ game) ++ "\n" ++ (show . dinero $ game) ++ "\n" ++ (show . objetivo $ game) ++ 
                "\n" ++ (show . apuesta $ game)
        
main = do 
    x <- menuInicial
    menuPrincipal x
    
{-- Verifica que las cantidades extraídas del archivo tengan sentido --}
estadoValido :: GameState -> Bool
estadoValido game = if victoriasLambda game <= juegosJugados game && dinero game < objetivo game && apuesta game <= dinero game
                    then True else False

{-- Esta es la función que lee del archivo y guarda los datos en un gamestate. Si no se carga un estado válido se
    devuelve al menú desde donde fue llamado. menuDeRetorno le indica el menú al que puede volver: menuInicial o 
    menuPrincipal--}
extraerContenidos :: FilePath -> IO GameState -> IO GameState
extraerContenidos archivo menuDeRetorno = do
    handle <- openFile archivo ReadMode
    contents <- hGetContents handle
    let x = read contents :: GameState
    contents `deepseq` hClose handle --Lazy IO me estaba dando problemas, así que tuve que obligar a que leyera el archivo
    if estadoValido x then return x else do putStrLn "Este archivo tiene algún error de contenido. No será cargado"
                                            menuDeRetorno

{-- Esta función verifica que exista el archivo que se desea cargar. Si existe, llama a extraerContenidos, si no se
    devuelve al menú desde donde se fue llamado (Recordar que se puede cargar desde dos puntos distintos del juego)
    menuDeRetorno le indica el menú al que puede volver: menuInicial o menuPrincipal--}
leerArchivo :: IO GameState -> IO GameState
leerArchivo menuDeRetorno = do
    putStrLn "Ingrese el nombre del archivo: "
    nombre <- getLine
    existencia <- doesFileExist nombre
    if existencia then extraerContenidos nombre menuDeRetorno else do putStrLn $ "El archivo " ++ nombre ++ " no existe" 
                                                                      menuDeRetorno

{-- Solicita los datos del jugador, llama a las funciones de verificación y crea un gameState nuevo --}
solicitarDatos :: IO GameState
solicitarDatos = do
    putStrLn "Ingrese su nombre: "
    nombre <- getLine
    gen <- getStdGen
    dinero <- revisarCantidad 0 (maxBound :: Int) "Ingrese la cantidad de dinero inicial con que se quiere contar (Debe ser mayor a 0)"
    objetivo <- revisarCantidad dinero (maxBound :: Int) "Ingrese la cantidad de dinero que se debe alcanzar para ganar la partida (Debe ser mayor al monto seleccionado anteriormente)"
    apuesta <- revisarCantidad 0 dinero "Ingrese el monto a apostar en cada ronda (Debe ser menor que la cantidad de dinero inicial y  mayor que 0)"
    return $ GS 0 0 nombre gen dinero objetivo apuesta

{-- Esta función verifica que la cantidad de dinero sea válida de acuerdo a las normas establecidas por el juego
    Si se introduce una cantidad que no se corresponde con las mismas, se le da al usuario la oportunidad de
    introducir de nuevo el valor--}
revisarCantidad :: Int -> Int -> String -> IO Int
revisarCantidad montoMin montoMax mensaje = do
    putStrLn mensaje
    dineroStr <- getLine
    let dinero = (read dineroStr)::Int
    if montoMin < dinero && dinero <= montoMax then return dinero  else do putStrLn "Ese no es un monto válido"
                                                                           revisarCantidad montoMin montoMax mensaje

{-- Es la responsable de llamar a la acción del menú inicial (Cargar Juego, nuevo juego) que el usuario desea realizar --}
seleccionarAccion :: String -> IO GameState
seleccionarAccion "y" = do leerArchivo menuInicial
seleccionarAccion "n" = do solicitarDatos
seleccionarAccion  x  = do putStrLn $ "La opción " ++ x ++ " no es válida" 
                           menuInicial

{-- Despliega el menú inicial del juego --}
menuInicial :: IO GameState
menuInicial = do
    putStrLn "¿Desea cargar una partida? [y/n]"
    respuesta <- getLine
    seleccionarAccion respuesta

{-- Guarda la partida --}
guardarPartida :: GameState -> IO ()
guardarPartida game = do
    putStrLn "Introduzca el nombre del archivo por favor"
    nombre <- getLine
    handle <- openFile nombre WriteMode
    hPutStr handle (show game)
    hClose handle
    putStrLn "El archivo ha sido guardado exitosamente"

{-- Calcula la cantidad de partidasganadas por el usuario--}
victoriasUsuario :: GameState -> String
victoriasUsuario game = show $ juegosJugados game - victoriasLambda game

{-- Muestra los datos relevantes de la partida --}
mostarDatos :: GameState -> IO ()
mostarDatos game = do
    putStrLn $ "Partidas Jugadas: " ++ (show . juegosJugados $ game)
    putStrLn $ "Partidas ganadas por Jack Lambda: " ++ (show . victoriasLambda $ game)
    putStrLn $ "Partidas ganadas por " ++ nombre game ++ ": " ++ victoriasUsuario game
    putStrLn $ "Cantidad de dinero restante: " ++ (show . dinero $ game)

{-- Ejecuta la acción del menú principal seleccionada por el usuario --}
seleccionarOpcion :: String -> GameState -> IO GameState
seleccionarOpcion "1" game = do iniciarRonda (descontarApuesta game)
seleccionarOpcion "2" game = do guardarPartida game
                                menuPrincipal game
seleccionarOpcion "3" game = do newGame <- leerArchivo (menuPrincipal game)
                                menuPrincipal newGame
seleccionarOpcion "4" game = do putStrLn $ "Gracias por jugar " ++ (nombre game)
                                return game
seleccionarOpcion  _  game = do putStrLn $ "Esa opción no es válida " ++ (nombre game)
                                menuPrincipal game

{-- Despliega el menú principal --}
menuPrincipal :: GameState -> IO GameState
menuPrincipal game = do
    mostarDatos game
    putStrLn $ "1 - Jugar ronda"
    putStrLn $ "2 - Guardar Partida"
    putStrLn $ "3 - Cargar Partida"
    putStrLn $ "4 - Salir"
    putStrLn $ "Ingrese el número de la opción a seleccionar por favor"
    opcion <- getLine
    seleccionarOpcion opcion game


iniciarRonda :: GameState -> IO GameState
iniciarRonda match@(GS {juegosJugados = games, victoriasLambda = victsLambda, nombre = name, generador = gen, dinero = cash, objetivo = obj, apuesta = bet}) = do
    putStrLn $ name ++ ", esta es mi primera carta: " ++ (show . head $ manoL)
    if blackjack manoLambda then do 
        putStrLn $ name ++ ", he sacado blackjack. Yo gano."
        printLambdaVictory match
    else do
        putStrLn $ "Es tu turno "++ name ++", robaras de la izquierda o la derecha? [i/d]"
        seleccion <- getLine
        (mazoNuevo, manoJugador) <- manoInicial manoLambda (desdeMano mazo) seleccion
        putStrLn $ name ++ ", tu mano es " ++ (show manoJugador)
        if blackjack manoJugador then do
            putStrLn $ name ++ ", tu mano es blackjack"
            printPlayerVictory match
        else do
            putStrLn $ "Suma " ++ (show . valor $ manoJugador)
            continuarRonda mazoNuevo manoLambda manoJugador match
    where 
        (manoLambda@(Mano manoL), mazo) = inicialLambda (barajar gen baraja)

continuarRonda :: Mazo -> Mano -> Mano -> GameState -> IO GameState
continuarRonda mazo manoLambda manoJugador gs = do
    putStrLn $ "Selecciona tu próxima jugada " ++ (show . nombre $ gs)
    putStrLn "1- Hit"
    putStrLn "2- Stand"
    putOptions manoJugador gs
    opcion <- getLine
    seleccionarJugada opcion mazo manoLambda manoJugador gs

putSurrender :: String -> [Carta] -> IO ()
putSurrender opcionNum lcartas = do if length (lcartas) == 2 then  putStrLn $ opcionNum ++ "- Surrender" else return ()

putOptions :: Mano -> GameState -> IO ()
putOptions (Mano j) game 
    | (dinero game >= apuesta game) = do
        putStrLn "3- Double down"
        putSurrender "4" j
    | otherwise =
        putSurrender "3" j

-- changed lcartas by j
opcion3 :: Mazo -> Mano -> Mano -> GameState -> IO GameState
opcion3 mazo manoLambda (Mano j) gs
    | (dinero gs >= apuesta gs) = do doubleDown mazo manoLambda (Mano j) gs
    | (dinero gs < apuesta gs) && (length (j) == 2) = do surrender mazo manoLambda (Mano j) gs
    | otherwise = do putStrLn $ "Esa no es una jugada válida, " ++ (show . nombre $ gs)
                     continuarRonda mazo manoLambda (Mano j) gs

opcion4 :: Mazo -> Mano -> Mano -> GameState -> IO GameState
opcion4 mazo manoLambda (Mano j) gs
    | length (j) == 2 = do surrender mazo manoLambda (Mano j) gs
    | otherwise = do putStrLn $ "Esa no es una jugada válida, " ++ (show . nombre $ gs)
                     continuarRonda mazo manoLambda (Mano j) gs

seleccionarJugada :: String -> Mazo -> Mano -> Mano -> GameState -> IO GameState
seleccionarJugada "1" mazo manoLambda manoJugador gs = do hit False mazo manoLambda manoJugador gs
seleccionarJugada "2" mazo manoLambda manoJugador gs = do stand mazo manoLambda manoJugador gs
seleccionarJugada "3" mazo manoLambda manoJugador gs = do opcion3 mazo manoLambda manoJugador gs
seleccionarJugada "4" mazo manoLambda manoJugador gs = do opcion4 mazo manoLambda manoJugador gs
seleccionarJugada  x  mazo manoLambda manoJugador gs = do putStrLn $ "Esa no es una jugada válida, " ++ (show . nombre $ gs)
                                                          continuarRonda mazo manoLambda manoJugador gs


manoInicial :: Mano -> Mazo -> String -> IO (Mazo, Mano)
manoInicial manoLambda (Mitad center left right) "i" = return $ checkDraw manoLambda center (robar (Mitad center left right) (Mano []) Izquierdo) 
manoInicial manoLambda (Mitad center left right) "d" = return $ checkDraw manoLambda center (robar (Mitad center left right) (Mano []) Derecho)
manoInicial manoLambda mazo _ = do 
                    putStrLn $ "Opcion invalida, intentelo de nuevo. Izquierda o derecha? [i/d]"
                    x <- getLine
                    manoInicial manoLambda mazo x

checkDraw :: Mano -> Carta -> Maybe (Mazo, Mano) -> (Mazo, Mano)
checkDraw _ card (Just (mazo, (Mano (listaMano)))) = (reconstruir mazo (Mano (card:listaMano)), (Mano (card:listaMano)))
checkDraw manoLambda@(Mano listaMano) card Nothing = checkDraw manoLambda card (Just (reconstruir (desdeMano baraja) (Mano (card:listaMano)), (Mano [])))

-- Aquí están las 4 jugadas posibles

surrender :: Mazo -> Mano -> Mano -> GameState -> IO GameState
surrender mazo manoLambda manoJugador gs = do
    putStrLn $ (nombre gs) ++ " te has renido. Yo gano"
    printPlayerSurrenders gs

-- En esta funcion no deberia recibirse solo el caso con Just x, hay que considerar el caso nothing
stand :: Mazo -> Mano -> Mano -> GameState -> IO GameState
stand mazo manoLambda manoJugador gs = do
    let mano = getMano (juegaLambda mazo manoLambda)
    putStrLn $ "Mi mano es " ++ (show mano)
    putStrLn $ "Suma " ++ (show . valor $ mano)
    if busted manoLambda then do
        putStrLn "Tu ganas"
        printPlayerVictory gs
    else
        determinarGanador manoLambda manoJugador gs

-- Duda: que hacer si el mazo se acaba? Nunca deberia pasar, y siempre deberia estar balanceado el mazo
hit :: Bool -> Mazo -> Mano -> Mano -> GameState -> IO GameState
hit double mazo manoLambda manoJugador gs@(GS {juegosJugados = games, victoriasLambda = victsLambda, nombre = name, generador = gen, dinero = cash, objetivo = obj, apuesta = bet}) = do 
    putStrLn $ name ++", robaras de la izquierda o la derecha? [i/d]"
    x <- getLine
    if x /= "i" && x /= "d" then do
        putStrLn $ "Eleccion invalida, intentalo de nuevo."
        hit double mazo manoLambda manoJugador gs
    else continueHit double mazo manoLambda manoJugador gs x

continueHit :: Bool -> Mazo -> Mano -> Mano -> GameState -> String -> IO GameState
continueHit double mazo@(Mitad _ (Mitad leftCenter leftLeft leftRight) _) manoLambda (Mano manoJugador) gs@(GS {juegosJugados = games, victoriasLambda = victsLambda, nombre = name, generador = gen, dinero = cash, objetivo = obj, apuesta = bet}) "i" = do
    if busted (Mano (leftCenter:manoJugador)) 
        then do
        putStrLn $ name ++ ", tu mano es" ++ show (Mano (leftCenter:manoJugador))
        putStrLn $ "Suma " ++ (show $ valor (Mano (leftCenter:manoJugador))) ++ ". Perdiste."
        printLambdaVictory gs  
    else do
        putStrLn $ name ++ ", tu mano es" ++ show (Mano (leftCenter:manoJugador))
        putStrLn $ "Suma " ++ (show $ valor (Mano (leftCenter:manoJugador))) ++ "."
        if double 
            then continueDouble (reconstruir mazo (Mano (leftCenter:manoJugador))) manoLambda (Mano (leftCenter:manoJugador)) gs 
        else continuarRonda (reconstruir mazo (Mano (leftCenter:manoJugador))) manoLambda (Mano (leftCenter:manoJugador)) gs 

continueHit double mazo@(Mitad _ _ (Mitad rightCenter rightLeft rightRight)) manoLambda (Mano manoJugador) gs@(GS {juegosJugados = games, victoriasLambda = victsLambda, nombre = name, generador = gen, dinero = cash, objetivo = obj, apuesta = bet}) "d" = do
    if busted (Mano (rightCenter:manoJugador)) 
        then do
        putStrLn $ name ++ ", tu mano es" ++ show (Mano (rightCenter:manoJugador))
        putStrLn $ "Suma " ++ (show $ valor (Mano (rightCenter:manoJugador))) ++ ". Perdiste."
        printLambdaVictory gs  
    else do
        putStrLn $ name ++ ", tu mano es" ++ show (Mano (rightCenter:manoJugador))
        putStrLn $ "Suma " ++ (show $ valor (Mano (rightCenter:manoJugador))) ++ "."
        if double 
            then continueDouble (reconstruir mazo (Mano (rightCenter:manoJugador))) manoLambda (Mano (rightCenter:manoJugador)) gs 
        else continuarRonda (reconstruir mazo (Mano (rightCenter:manoJugador))) manoLambda (Mano (rightCenter:manoJugador)) gs 


doubleDown :: Mazo -> Mano -> Mano -> GameState -> IO GameState
doubleDown mazo manoLambda manoJugador gs = hit True mazo manoLambda manoJugador gs

continueDouble :: Mazo -> Mano -> Mano -> GameState -> IO GameState
continueDouble mazo manoLambda manoJugador gs@(GS {juegosJugados = games, victoriasLambda = victsLambda, nombre = name, generador = gen, dinero = cash, objetivo = obj, apuesta = bet}) = do
    let mano = getMano (juegaLambda mazo manoLambda)
    putStrLn $ "Mi mano es " ++ (show mano)
    if busted manoLambda || (ganador manoLambda manoJugador == Player) then do
        putStrLn "Tu ganas"
        printPlayerVictory (GS {juegosJugados = games, victoriasLambda = victsLambda, nombre = name, generador = gen, dinero = cash+bet, objetivo = obj, apuesta = bet})
    else do
        putStrLn "Yo gano"
        printLambdaVictory gs

getMano :: Maybe Mano -> Mano
getMano Nothing = Mano []
getMano (Just mano) = mano

-- Funciones utilizadas para determinar y anunciar el ganador

determinarGanador :: Mano -> Mano -> GameState -> IO GameState
determinarGanador manoLambda manoJugador gs = do
    if ganador manoLambda manoJugador == Dealer then do
        putStrLn "Yo gano"
        printLambdaVictory gs
    else do
        putStrLn "Tu ganas"
        printPlayerVictory gs

printPlayerVictory :: GameState -> IO GameState
printPlayerVictory game = do
    newGen <- newStdGen
    if (dinero game)+(2*(apuesta game)) >= (objetivo game) then do
        putStrLn $ "Felicidades, " ++ (nombre game) ++ ", me has derrotado es el fin del juego para mí"
        return (playerWins game newGen)
    else
        menuPrincipal (playerWins game newGen)

printLambdaVictory :: GameState -> IO GameState
printLambdaVictory game@(GS {juegosJugados = games, victoriasLambda = victsLambda, nombre = name, generador = gen, dinero = cash, objetivo = obj, apuesta = bet}) = do
    newGen <- newStdGen
    if cash < bet then do
        putStrLn $ name ++ ", no te queda dinero. Es el fin del juego para ti."
        return (lambdaWins game newGen)
    else do
        menuPrincipal (lambdaWins game newGen)

printPlayerSurrenders :: GameState -> IO GameState
printPlayerSurrenders game = do
    newGen <- newStdGen
    if (dinero game) + (apuesta game) `div` 2 < (apuesta game) then do
        putStrLn $ (nombre game) ++ ", no te queda dinero. Es el fin del juego para ti."
        return (playerSurrenders game newGen)
    else do
        menuPrincipal (playerSurrenders game newGen)

-- Funciones que actualizan el estado de la partida

lambdaWins :: GameState -> StdGen -> GameState
lambdaWins game newGen = game {
    juegosJugados = (juegosJugados game) + 1,
    victoriasLambda = (victoriasLambda game) + 1,
    generador = newGen
}

playerWins :: GameState -> StdGen -> GameState
playerWins game newGen = game {
    juegosJugados = (juegosJugados game) + 1,
    generador = newGen,
    dinero = (dinero game) + 2 * (apuesta game) 
}

playerSurrenders :: GameState -> StdGen -> GameState
playerSurrenders game newGen = game {
    juegosJugados = (juegosJugados game) + 1,
    victoriasLambda = (victoriasLambda game) + 1,
    generador = newGen,
    dinero = (dinero game) + (apuesta game) `div` 2
}

descontarApuesta :: GameState -> GameState
descontarApuesta game = game {dinero = (dinero game) - (apuesta game)}
 




