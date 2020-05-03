type Libro = (String,String,Int)
type Biblioteca = [Libro]
type Saga = [Libro]

elVisitante :: Libro
elVisitante = ("Stephen King","El Visitante", 592)

oyasumiPunpun1 :: Libro
oyasumiPunpun1 = ("Asana Inio","Oyasumi Punpun", 40 )
oyasumiPunpun3:: Libro
oyasumiPunpun3 = ("Asana Inio","Oyasumi Punpun", 40 )
oyasumiPunpun127 :: Libro
oyasumiPunpun127 = ("Asana Inio","Oyasumi Punpun", 40 )

fundacion :: Libro
fundacion = ("Isaac Asimov","Fundacion",230)

sandman5 :: Libro
sandman5 = ("Nel Gaiman","Sandman",35)
sandman10 :: Libro
sandman10 = ("Nel Gaiman","Sandman",35)
sandman12 :: Libro
sandman12 = ("Nel Gaiman","Sandman",35)

eragon :: Libro
eragon = ("Christopher Paolini","Eragon",544)

eldest :: Libro
eldest = ("Christopher Paolini","Eldest",704)

brisignr :: Libro
brisignr = ("Christopher Paolini","Brisignr",700)

legado :: Libro
legado = ("Christopher Paolini","Legado",811)

sagaDeEragon :: Saga
sagaDeEragon = [eragon,eldest,brisignr,legado]

unaBiblioteca :: Biblioteca
unaBiblioteca = [elVisitante,oyasumiPunpun1,oyasumiPunpun3,oyasumiPunpun127,fundacion,sandman5,sandman10,sandman12,eragon,eldest,brisignr,legado]

--1)

-- Seamos lo más expresivos posibles, que es lo que obtiene la función fst de un libro?
-- Debería poder saberlo con solo leerlo, porque en realidad, así como está definida la tupla, 
-- no puedo darme cuenta de que se trata de un libro.
-- Algo mejor sería:
-- obtenerAutor :: Libro -> String
-- obtenerAutor (autor, _, _) = autor
-- Lo mismo sucede para snd3 y trd3.

fst3 (autor,_,_)=autor
snd3 (_,libro,_)=libro
trd3 (_,_,paginas) = paginas

promedioDeHojas :: Biblioteca -> Int
promedioDeHojas unaBiblioteca =  div (cantidadDeHojas unaBiblioteca) (cantidadDeLibros unaBiblioteca)

-- Muy bien la abstracción.
cantidadDeHojas :: Biblioteca -> Int
cantidadDeHojas unaBiblioteca = div ((sum.(map trd3)) unaBiblioteca) 2

cantidadDeLibros :: Biblioteca -> Int
cantidadDeLibros unaBiblioteca= length unaBiblioteca

--2)
-- Hacer guardas para retornar True o False es algo que tiene que disparar una alarma de que algo no está bien.
-- Para eso tenemos los operadores  && y ||.
-- Si retorna un booleano, las condiciones puede plantearse en una sola línea como:
-- lecturaObligatoria lectura = (esDelAutor "Stephen King" lectura)  || (esDeLaSaga sagaEragon lectura) || (lectura == fundacion)
-- esDelAutor unAutor = (==unAutor).obtenerAutor 
-- esDeLaSaga unaSaga = flip elem sagaEragon

lecturaObligatoria :: Libro -> Bool
lecturaObligatoria libro
 | (fst3 libro == "Stephen King" )= True 
 | elem libro sagaDeEragon = True
 | (snd3 libro == "Fundacion") = True
 | otherwise = False

-- No era necesario por lo que pedía el enunciado, pero está bien.

lecturasObligatorias :: Biblioteca -> [Libro]
lecturasObligatorias = filter lecturaObligatoria  

--3)

-- Para mejorar la expresividad, se puede definir la función como "esFantasiosa" y "esLecturaFantasiosa".

fantasiosa :: Biblioteca -> Bool
fantasiosa = any esFantasiosa 
-- Nuevamente nos tiene que hacer mucho ruodo hacer guardas para retornar True o False. 
-- Las guardas están buenas para usar, pero no para abusar. Si esperamos un Bool de retorno,
-- es un buen indicador para saber que no deberíamos usarlas.
-- Se puede escribir en una línea con los operadores adecuados. 
-- Además, se puede (debe) evitar la repetición de la comparación al delegarla en una función como
-- esDelAutor unAutor unLibro = ((== unAutor).obtenerAutor) unLibro  
-- y simplemente utilizarla cuando queremos saber si un libro corresponde a un autor.

esFantasiosa :: Libro -> Bool
esFantasiosa libro 
 | (fst3 libro == "Christopher Paolini") = True
 | (fst3 libro == "Neil Gaiman") = True 
 | otherwise = False

--4)

-- Existe una función muy bella que está en la guía de lenguajes, y nos permite hacer
-- las dos cosas en una única aplicación: concatMap!

nombreDeLaBibloteca :: Biblioteca -> String 
nombreDeLaBibloteca  = (sinVocales.(concat.(map snd3)))

-- En lugar de recurrir al pattern matching, podemos simplificarlo con el uso de listas.
-- Quedaría algo como:
-- noEsVocal unaLetra = not (elem unaLetra "aeiouAEIOU")
-- O lo que sería aún mejor:
-- noEsVocal unaLetra = not.(flip elem "aeiouAEIOU") 

noEsVocal :: Char -> Bool
noEsVocal 'a' = False
noEsVocal 'A' = False
noEsVocal 'e' = False
noEsVocal 'E' = False
noEsVocal 'i' = False
noEsVocal 'I' = False
noEsVocal 'o' = False
noEsVocal 'O' = False
noEsVocal 'u' = False
noEsVocal 'U' = False
noEsVocal letra = True

sinVocales :: String -> String
sinVocales = filter noEsVocal 

--5)

-- Si ya estás usando un all, el map no debería ser necesario.
-- Podés simplemente hacer una función que chequee exactamente lo que vos necesitás y definirlo como:
-- bibliotecaLigera unaBiblioteca = all (condicion) unaBiblioteca
-- Con lo cual nos evitamos el choclazo.

bibliotecaLigera :: Biblioteca -> Bool
bibliotecaLigera unaBiblioteca = all (<=40) (map trd3 unaBiblioteca) 