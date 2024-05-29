# 1

def contar_lineas(nombre_archivo: str) -> int:
    archivo: str = open(nombre_archivo)
    contenido: str = archivo.readline()
    nlineas: int = 0

    while contenido != "":
        nlineas += 1
        contenido = archivo.readline()
    
    archivo.close()

    return nlineas

def existe_palabra(palabra: str, nombre_archivo: str) -> bool:
    archivo: str = open(nombre_archivo)
    contenido: str = archivo.read()

    res = False

    palabras: list [str] = crear_lista(contenido)

    for i in range(len(palabras)):
        if palabra == palabras[i]:
            res = True
    
    archivo.close()

    return res

def crear_lista(contenido: list [str]):
    palabras: list [str] = []
    palabra: str = ""

    for c in (contenido + " "):
        if c == " " or c == "\n":
            palabras.append(palabra)
            palabra = ""
        else:
            palabra = palabra + c

    return palabras

def cantidad_apariciones(palabra: str, nombre_archivo: str) -> int:
    archivo: str = open(nombre_archivo)
    contenido: str = archivo.read()

    napa: int = 0

    palabras: list [str] = crear_lista(contenido)

    for p in palabras:
        if palabra == p:
            napa += 1
    
    archivo.close()

    return napa


# 2

def clonar_sin_comentarios(nombre_archivo: str):
    archivo: str = open(nombre_archivo)
    contenido: str = archivo.readlines()

    