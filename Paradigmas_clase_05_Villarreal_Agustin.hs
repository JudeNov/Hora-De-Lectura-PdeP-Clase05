
-- Estamos en cuarentena, lo sabemos. ¿Qué se puede hacer además de estudiar para la facu? 😬 
-- Leer es una actividad que solemos dejar de lado por el tiempo que lleva hacerlo. 
-- Pero ahora que tenemos un poquito más de tiempo, podríamos retomar ese lindo hábito, ¿no?
-- En PdeP tenemos una biblioteca con nuestros libros favoritos; te contamos algunos 📚. 
-- El visitante de Stephen King que tiene 592 páginas, 
-- Oyasumi Punpun capítulo 1, 3 y 127 que tienen 40 páginas cada uno y los escribió Asano Inio, 
-- Fundación de Isaac Asimov con 230 páginas, 
-- los capítulos  5, 10 y 12 de Sandman de Neil Gaiman con 35 páginas cada uno y, por último, 
-- la saga de Eragon: Eragon (544 páginas), Eldest (704 páginas), Brisignr (700 páginas) y Legado (811 páginas) de Christopher Paolini.

type Libro = (String, String, Int)
type Biblioteca = [Libro]
type Saga = [Libro]


elVisitante :: Libro
elVisitante = ("Stephen King", "El Visitante", 592)

oyasumiPunpunCap1 :: Libro
oyasumiPunpunCap1   = ("Asano Inio", "Oyasumi Punpun", 40)

oyasumiPunpunCap3 :: Libro
oyasumiPunpunCap3   = ("Asano Inio", "Oyasumi Punpun", 40)

oyasumiPunpunCap127 :: Libro
oyasumiPunpunCap127 = ("Asano Inio", "Oyasumi Punpun", 40)

fundacion :: Libro
fundacion = ("Isaac Asimov", "Fundacion", 230)

sandmanCap5 :: Libro
sandmanCap5 = ("Neil Gaiman", "Sandman", 35)

sandmanCap10 :: Libro
sandmanCap10 = ("Neil Gaiman", "Sandman", 35)

sandmanCap12 :: Libro
sandmanCap12 = ("Neil Gaiman", "Sandman", 35)

eragon :: Libro
eragon = ("Christopher Paolini", "Eragon", 544)

eldest :: Libro
eldest = ("Christopher Paolini", "Eldest", 704)

brisignr :: Libro
brisignr = ("Christopher Paolini", "Brisignr", 700)

legado :: Libro 
legado = ("Christopher Paolini", "Legado", 811)

sagaDeEragon :: Saga
sagaDeEragon = [eragon, eldest, brisignr, legado]

biblioteca :: Biblioteca
biblioteca = [elVisitante, oyasumiPunpunCap1, oyasumiPunpunCap3, oyasumiPunpunCap127, sandmanCap5, sandmanCap10, sandmanCap12, fundacion] ++ sagaDeEragon

--1) 
-- Una vez que estableciste que 
-- type Libro = (String, String, Int)
-- las funciones que definas para obtener los elementos de este tipo de 
-- tuplas va a respetar a su vez esos tipos.
-- Sino tomar hojas podría aplicarse a una tupla como 
-- (1, [], "hola")
-- lo cual en este contexto no tendría sentido.

tomarHojas :: (a, b, c) -> c
tomarHojas (autor, nombre, hojas) = hojas

tomarNombre :: (a, b, c) -> b
tomarNombre (autor, nombre, hojas) = nombre

tomarAutor :: (a, b ,c ) -> a
tomarAutor (autor, nombre, hojas) = autor

-- Se podría delegar sum (map obtenerNroDeHojas biblioteca) en una función para que sea más declarativo.
-- Recordar que numero de hojas no es lo mismo que numeo de págs. Habría que hacer nroDePags/2.

promedioDeHojas :: Biblioteca -> Int
promedioDeHojas unaBiblioteca = div (sum (map tomarHojas unaBiblioteca)) (length unaBiblioteca) 

-- Para mejorar la expresividad, se puede definir la función como "esLecturaObligatoria".
-- Recomendar abstraer en funciones las condiciones que usó. Están bien pero el delegarlas/abstraerlas 
-- puede servir para reutilizarla en futuros casos (lo cual de hecho sucede más adelante en el enunciado).

lecturaObligatoria :: Libro -> Bool
lecturaObligatoria libro = tomarAutor libro == "Stephen King" || elem libro sagaDeEragon || libro == fundacion

-- Se puede (debe) evitar la repetición de la comparación al delegarla en una función como
-- esDelAutor unAutor unLibro = ((== unAutor).tomarAutor) unLibro  
-- y simplemente utilizarla cuando queremos saber si un libro corresponde a un autor.

librosFantasios :: Libro -> Bool
librosFantasios libro = tomarAutor libro == "Christopher Paolini"  || tomarAutor libro == "Neil Gaiman"
esBibliotecaFantasiosa :: Biblioteca -> Bool
esBibliotecaFantasiosa unaBiblioteca = any librosFantasios unaBiblioteca

esVocal :: Char -> Bool
esVocal letra = elem letra vocales_Espacios
vocales_Espacios :: [Char]
vocales_Espacios = ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U', ' ']
sacarVocales :: [Char] -> [Char]
sacarVocales = filter (not.esVocal)
nombreDeLaBiblioteca :: Biblioteca -> [Char]
nombreDeLaBiblioteca unaBiblioteca = sacarVocales (concat(map tomarNombre biblioteca))

-- Existe una función muy bella que está en la guía de lenguajes, y nos permite hacer
-- las dos cosas en una única aplicación: concatMap!

-- Recordar diferenciar nuevamente lo que son paginas de lo que son hojas.
-- Para mejorar la expresividad, se puede definir la función como "esLecturaLigera", de otra forma
-- ¿cómo nos damos cuenta de lo que la función toma por parámetro para decir que es ligero?.

esLigera :: Libro -> Bool
esLigera = (<= 40).tomarHojas 
esBibliotecaLigera :: Biblioteca -> Bool
esBibliotecaLigera unaBiblioteca =  all esLigera unaBiblioteca
