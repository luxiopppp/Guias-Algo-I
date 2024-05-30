import csv

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

def crear_lista(contenido: list):
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

def clonar_sin_comentarios(nombre_archivo: str) -> str:
    archivo: str = open(nombre_archivo)
    contenido: str = archivo.readlines()

    archivo_sin_comentarios: str = open("wc_" + nombre_archivo, "w")

    for linea in contenido:
        if not es_comentario(linea):
            archivo_sin_comentarios.write(linea)
    
    archivo.close()
    archivo_sin_comentarios.close()

def es_comentario(linea: str) -> bool:
    for i in range(len(linea)):
        if linea[i] != " " and linea[i] != "#":
            return False
        elif linea[i] == "#":
            return True
        

# 3

def invertir_lineas(nombre_archivo: str) -> str:
    archivo: str = open(nombre_archivo)
    contenido: list [str] = archivo.readlines() # esto es una lista de strings

    archivo_invertido: str = open("inv_" + nombre_archivo, "w")

    for i in range(len(contenido)):
        archivo_invertido.write(sacar_enter(contenido[(len(contenido) - 1) - i]))
        if (len(contenido) - 1) - i != 0:
            archivo_invertido.write("\n")

    archivo.close()
    archivo_invertido.close()

def sacar_enter(linea: str) -> str:
    linea_sin_enter: str = ""

    for i in range(len(linea)):
        if linea[i] != "\n":
            linea_sin_enter = linea_sin_enter + linea[i]
    
    return linea_sin_enter


# 4

def agregar_frase_al_final(nombre_archivo: str, frase: str) -> str:
    archivo: str = open(nombre_archivo, "r+")
    contenido: str = archivo.read()

    for i in range(len(contenido)):
        if (len(contenido) - 1) - i == 0:
            archivo.write("\n" + frase)

    archivo.close()


# 5

def agregar_frase_al_principio(nombre_archivo: str, frase: str) -> str:
    archivo: str = open(nombre_archivo, "r")
    contenido: str = archivo.read()

    wr_archivo: str = open(nombre_archivo, "w")

    for i in range(len(contenido)):
        if i == 0:
            wr_archivo.write(frase + "\n" + contenido)

    archivo.close()
    wr_archivo.close()


# 6

def listar_palabras_de_archivo(nombre_archivo: str) -> list:
    archivo = open(nombre_archivo, "rb")
    cont_en_bytes = archivo.read()

    palabras_legibles: list [str] = []
    palabra: str = ""

    for i in range(len(cont_en_bytes)):
        if chr(cont_en_bytes[i]) == " " or chr(cont_en_bytes[i]) == "_" or (chr(cont_en_bytes[i]) >= "A" and chr(cont_en_bytes[i]) <= "Z") or (chr(cont_en_bytes[i]) >= "a" and chr(cont_en_bytes[i]) <= "z"):
            palabra = palabra + chr(cont_en_bytes[i])
            if len(palabra) >= 5:
                palabras_legibles.append(palabra)
                palabra = ""

    archivo.close()
    return palabras_legibles

# 7

def calcular_promedio_por_estudiante(nombre_archivo_notas: str, nombre_archivo_promedios: str) -> str:
    archivo_notas = open(nombre_archivo_notas, "r")
    promedios = open(nombre_archivo_promedios, "w")
    notas = csv.reader(archivo_notas, delimiter=",")

    


def promedio_estudiante(nombre_archivo: str, lu: str) -> float:
    archivo = open(nombre_archivo, "r")
    contenido = archivo.read()


# def csv_a_lista(nombre_archivo: str) -> list:
#     archivo = open(nombre_archivo, "r")
#     contenido = archivo.read()

#     lista: list = []
#     elem: str = ""

#     for i in range(len(contenido)):
#         if contenido[i] != " " or contenido[i] != "," or contenido[i] != "\n":
#             elem = elem + contenido[i]
#         else:
#             lista.append(elem)
#             elem = ""
    
#     archivo.close()
#     return lista


