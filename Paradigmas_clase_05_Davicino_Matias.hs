type Libro = (String, String, Int)

elVisitante :: Libro
elVisitante = ("El Visitante", "Stephen King", 592)

osayumi1 :: Libro
osayumi1 = ("Oyasumi Punpun capitulo 1", "Asano Inio", 40)

osayumi3 :: Libro
osayumi3 = ("Oyasumi Punpun capitulo 3", "Asano Inio", 40)

osayumi127 :: Libro
osayumi127 = ("Oyasumi Punpun capitulo 127", "Asano Inio", 40)

fundacion :: Libro
fundacion = ("Fundacion", "Isaac Asimov", 230)

sandman5 :: Libro
sandman5 = ("Sandman capitulo 5", "Neil Gaiman", 35)

sandman10 :: Libro
sandman10 = ("Sandman capitulo 10", "Neil Gaiman", 35)

sandman12 :: Libro
sandman12 = ("Sandman capitulo 12", "Neil Gaiman", 35)

eragon :: Libro
eragon = ("Eragon", "Christopher Paolini", 544)

eldest :: Libro
eldest = ("Eldest", "Christopher Paolini", 704)

brisignr :: Libro
brisignr = ("Brisignr", "Christopher Paolini", 700)

legado :: Libro
legado = ("Legado", "Christopher Paolini", 811)

type Saga = [Libro]

sagaEragon :: Saga
sagaEragon = [eragon, eldest, brisignr, legado]

type Biblioteca = [Libro]

miBiblioteca :: Biblioteca
miBiblioteca = [elVisitante, osayumi1, osayumi3, osayumi127, fundacion, sandman5, sandman10, sandman12, eragon, eldest, brisignr, legado]

-- Una vez que estableciste que 
-- type Libro = (String, String, Int)
-- las funciones que definas para obtener los elementos de este tipo de 
-- tuplas va a respetar a su vez esos tipos.
-- Sino tomar hojas podría aplicarse a una tupla como 
-- (1, [], "hola")
-- lo cual en este contexto no tendría sentido.

tomarTitulo :: (a, b, c) -> a
tomarTitulo (titulo, autor, hojas) = titulo

tomarAutor :: (a, b, c) -> b
tomarAutor (titulo, autor, hojas) = autor

tomarHojas :: (a, b, c) -> c
tomarHojas (titulo, autor, hojas) = hojas

-- Se podría delegar sum (map obtenerNroDeHojas biblioteca) en una función para que sea más declarativo.
-- Recordar que numero de hojas no es lo mismo que numeo de págs. Habría que hacer nroDePags/2.

promedioDeHojas :: Biblioteca -> Int
promedioDeHojas unaBiblioteca = div (sum (map tomarHojas unaBiblioteca)) (length unaBiblioteca)

-- Hermosa abstracción, lo podríamos ampliar un poco para que nos deje saber si el libro es un autor 
-- arbitrario, no sólo de Stephen King.
-- Algo así como:
-- esLibroDe unAutor unLibro = ((== unAutor).tomarAutor) unLibro
-- Pero la idea de abtraer la lógica está buenísimo :)

esDeStephenKing :: Libro -> Bool
esDeStephenKing = (=="Stephen King").tomarAutor

esDeSagaEragon :: Libro -> Bool
esDeSagaEragon unLibro = elem unLibro sagaEragon

esFundacion :: Libro -> Bool
esFundacion = (==fundacion)

-- Muy bien las abstracciones

lecturaObligatoria :: Libro -> Bool
lecturaObligatoria unLibro = esDeStephenKing unLibro || esDeSagaEragon unLibro || esFundacion unLibro

-- Fijate que acá estás repitiendo la lógica de cuando lo hiciste con Stephen King.
-- Si usáramos la función sugerida más arriba, se podría solucionar aplicando parcialmente
-- la función pero cambiando el autor que le pasamos por parámetro.

esDeCristopherPaolini :: Libro -> Bool
esDeCristopherPaolini = (=="Christopher Paolini").tomarAutor

esDeNeilGaiman :: Libro -> Bool
esDeNeilGaiman = (=="Neil Gaiman").tomarAutor

-- Para mejorar la expresividad, se puede definir la función como "esFantasiosa".
-- Incluso podría usarse un solo any con ambas condiciones, si las delegáramos en una función que haga las dos cosas.

fantasiosa :: Biblioteca -> Bool
fantasiosa unaBiblioteca = any esDeCristopherPaolini unaBiblioteca || any esDeNeilGaiman unaBiblioteca

vocalesYEspacio :: [Char]
vocalesYEspacio = ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U', ' ']

noEsVocalNiEspacio :: Char -> Bool
noEsVocalNiEspacio caracter = not(elem caracter vocalesYEspacio)

sacarVocalesYEspacios :: String -> String
sacarVocalesYEspacios titulo = filter noEsVocalNiEspacio titulo

-- Existe una función muy bella que está en la guía de lenguajes, y nos permite hacer
-- las dos cosas en una única aplicación: concatMap!

nombreDeLaBiblioteca :: Biblioteca -> String
nombreDeLaBiblioteca unaBiblioteca = concat (map sacarVocalesYEspacios (map tomarTitulo unaBiblioteca))

-- Para mejorar la expresividad, se puede definir la función como "esBibliotecaLigera". 
-- Recordar diferenciar nuevamente lo que son paginas de lo que son hojas.
-- Si ya estás usando un all, el map no debería ser necesario.
-- Podés simplemente hacer una función que chequee exactamente lo que vos necesitás y definirlo como:
-- bibliotecaLigera unaBiblioteca = all (condicion) unaBiblioteca
-- Con lo cual nos evitamos el choclazo.

bibliotecaLigera :: Biblioteca -> Bool
bibliotecaLigera unaBiblioteca = all (<40) (map tomarHojas unaBiblioteca)


