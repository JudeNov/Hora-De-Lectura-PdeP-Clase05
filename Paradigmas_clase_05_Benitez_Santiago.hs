type Libro = (String, Int, String)

elVisitante :: Libro
elVisitante = ("Stephen King", 592, "El Visitante")

oyasumiPunpun1 :: Libro
oyasumiPunpun1 = ("Asano Inio", 40, "Oyasumi Punpun")

oyasumiPunpun3 :: Libro
oyasumiPunpun3 = ("Asano Inio", 40, "Oyasumi Punpun")

oyasumiPunpun127 :: Libro
oyasumiPunpun127 = ("Asano Inio", 40, "Oyasumi Punpun")

fundacion :: Libro
fundacion = ("Isaac Asimov", 230, "Fundacion")

sandman5 :: Libro 
sandman5 = ("Neil Gaiman", 35, "Sandman")

sandman10 :: Libro 
sandman10 = ("Neil Gaiman", 35, "Sandman")

sandman12 :: Libro 
sandman12 = ("Neil Gaiman", 35, "Sandman")

eragon :: Libro
eragon = ("Christopher Paolini", 544, "Eragon")

eldest :: Libro
eldest = ("Christopher Paolini", 704, "Eldest")

brisignr :: Libro
brisignr = ("Christopher Paolini", 700, "Brisgnr")

legado :: Libro
legado = ("Crhistopher Paolini", 811, "Legado")

type Saga = [Libro]

sagaEragon :: Saga
sagaEragon = [eragon, eldest, brisignr, legado]

type Biblioteca = [Libro]

bibliotecaPdeP :: Biblioteca
bibliotecaPdeP = [elVisitante, oyasumiPunpun1, oyasumiPunpun3, oyasumiPunpun127, fundacion, sandman5, sandman10, sandman12, eragon, 
  eldest, brisignr, legado]
  
promedioDeHojas :: Biblioteca -> Int
promedioDeHojas biblioteca = div (sum (map obtenerNroDeHojas biblioteca)) (length biblioteca)

-- Se podría delegar sum (map obtenerNroDeHojas biblioteca) en una función para que sea más declarativo.
-- Recordar que numero de hojas no es lo mismo que numeo de págs. Habría que hacer nroDePags/2.

obtenerTitulo :: Libro -> String
obtenerTitulo (_ , _, titulo) = titulo

obtenerNroDeHojas :: Libro -> Int 
obtenerNroDeHojas (_, nroDeHojas, _) = nroDeHojas

obtenerAutor :: Libro -> String
obtenerAutor (autor, _, _) = autor

-- Hacer guardas para retornar True o False es algo que tiene que disparar una alarma de que algo no está bien.
-- Para eso tenemos los operadores  && y ||.
-- Si retorna un booleano, las condiciones puede plantearse en una sola línea como:
-- lecturaObligatoria lectura = (esDelAutor "Stephen King" lectura)  || (esDeLaSaga sagaEragon lectura) || (lectura == fundacion)
-- esDelAutor unAutor = (==unAutor).obtenerAutor 
-- esDeLaSaga unaSaga = flip elem sagaEragon

-- Para mejorar la expresividad, se puede definir la función como "esLecturaObligatoria".

lecturaObligatoria :: Libro -> Bool
lecturaObligatoria lectura
  | obtenerAutor lectura == "Stephen King" = True
  | elem lectura sagaEragon = True
  | lectura == fundacion = True
  | otherwise = False

-- Para mejorar la expresividad, se puede definir la función como "esFantasiosa".

-- Aunque funciona, podemos hacerlo de forma mucho más simple sabiendo que hay una función que hace lo mismo
-- en una sola aplicación: la función any. Recibe una condición y nos dice si algún elemento de la lista cumple 
-- con la misma.
-- En este caso lo simplificaría a:
-- any ((=="Christopher Paolini").obtenerAutor) biblioteca 
-- en lugar de
-- ((elem "Christopher Paolini") . (map obtenerAutor)) biblioteca
-- El detalle algoritmico en el primer caso es menor. También estaría bueno abstraer la 
-- lógica de saber si un libro es de un autor, con lo cual podríamos dejarlo como:
-- any (esDelAutor "Christopher Paolini") biblioteca 
-- Con lo cual queda mucho más declarativo, y hasta estamos usando aplicación parcial.
-- Fijate que en el caso original, hasta se repite la lógica de saber si un libro es de un autor en 
-- dos casos.


fantasiosa :: Biblioteca -> Bool
fantasiosa biblioteca = ((elem "Christopher Paolini") . (map obtenerAutor)) biblioteca
  || ((elem "Neil Gaiman") . (map obtenerAutor)) biblioteca

-- Existe una función muy bella que está en la guía de lenguajes, y nos permite hacer
-- las dos cosas en una única aplicación: concatMap!

nombreDeLaBiblioteca :: Biblioteca -> String
nombreDeLaBiblioteca = concat . (map (eliminarVocales)) . (map (obtenerTitulo))

eliminarVocales :: String -> String
eliminarVocales cadena = filter (noEsVocal) cadena

-- Usemos siempre la expresividad y evitemos nombres de variables como x, 
-- aunque sabemos que en este caso es algo obvio lo que hace la función.

noEsVocal :: Char -> Bool
noEsVocal x = ( not . (elem x)) ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U'] 
 
-- Para mejorar la expresividad, se puede definir la función como "esBibliotecaLigera". 
-- Recordar diferenciar nuevamente lo que son paginas de lo que son hojas. Habría que hacer 
-- algo como cantidadHojas*2.
-- Se puede (de hecho estaría muy copado) delegar la composición en una función, que le vamos 
-- a pasar a all como parámetro, para mejorar la declaractividad y que nuestro código se lea más fácil.

bibliotecaLigera :: Biblioteca -> Bool
bibliotecaLigera = all ((< 41) . obtenerNroDeHojas)