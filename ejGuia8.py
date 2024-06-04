# ARCHIVOS

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

def clonar_sin_comentarios(nombre_archivo: str) -> None:
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

def invertir_lineas(nombre_archivo: str) -> None:
    archivo: str = open(nombre_archivo)
    contenido: list [str] = archivo.readlines() # esto es una lista de strings

    archivo_invertido: str = open("reverso.txt", "w")

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

def agregar_frase_al_final(nombre_archivo: str, frase: str) -> None:
    archivo: str = open(nombre_archivo, "r+")
    contenido: str = archivo.read()

    for i in range(len(contenido)):
        if (len(contenido) - 1) - i == 0:
            archivo.write("\n" + frase)

    archivo.close()

# 5

def agregar_frase_al_principio(nombre_archivo: str, frase: str) -> None:
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

def calcular_promedio_por_estudiante(nombre_archivo_notas: str, nombre_archivo_promedios: str) -> None:
    archivo_promedios = open(nombre_archivo_promedios, "w")
    archivo_notas: list = csv_a_lista(nombre_archivo_notas)

    archivo_promedios.write("LU, Promedio\n")
    promedios: list [str] = []

    for i in range(len(archivo_notas)):
        if i != 0:
            linea = f"{archivo_notas[i][0]}, {promedio_estudiante(nombre_archivo_notas, archivo_notas[i][0])}\n"
            if not pertenece_linea(promedios, linea):
                promedios.append(linea)

    
    archivo_promedios.writelines(promedios)

    archivo_promedios.close()

def promedio_estudiante(nombre_archivo: str, lu: str) -> float:
    archivo_notas: list = csv_a_lista(nombre_archivo)

    promedio: float = 0
    cant_materias: int = 0

    for i in range(len(archivo_notas)):
        if lu == (archivo_notas[i])[0]:
            promedio = promedio + float((archivo_notas[i])[3])
            cant_materias = cant_materias + 1

        if i == len(archivo_notas) - 1:
            promedio = promedio / cant_materias
        
    
    return promedio

def csv_a_lista(nombre_archivo: str) -> list:
    archivo: str = open(nombre_archivo, "r")
    contenido: list [str] = archivo.readlines()

    lista: list = []

    for i in range(len(contenido)):
        lista.append(linea_a_lista(contenido[i]))
    
    archivo.close()
    return lista

def linea_a_lista(linea: str) -> list:
    lista: list = []
    elem: str = ""

    for i in range(len(linea)):
        if linea[i] == "," or linea[i] == "\n":
            lista.append(sacar_espacios(elem))
            elem = ""
        elif len(linea) - 1 - i == 0:
            elem = elem + linea[i]
            lista.append(sacar_espacios(elem))
        else:
            elem = elem + linea[i]
            # print(i, linea[i], ord(linea[i]))
    
    return lista

def sacar_espacios(linea: str) -> str:
    linea_sin_espacios: str = ""

    for i in range(len(linea)):
        if linea[i] != " ":
            linea_sin_espacios = linea_sin_espacios + linea[i]
    
    return linea_sin_espacios

def pertenece_linea(lista: list, a) -> bool:
    res = False

    for i in range(len(lista)):
        if lista[i] == a:
            res = True
    
    return res

# PILAS

from queue import LifoQueue as Pila
import random

# 8

def generar_nros_al_azar(cantidad: int, desde: int, hasta: int) -> Pila:
    p = Pila()

    for _ in range(cantidad):
        nros = random.randint(desde, hasta)
        p.put(nros)
    
    return p

# 9

def cantidad_elementos(p: Pila) -> int:
    nros: list = []
    cantidad: int = 0

    for _ in range(len(p.queue)):
        if not p.empty():
            nros.append(p.get())
            cantidad = cantidad + 1

    for i in range(len(nros)):
        p.put(nros[(len(nros) - 1) - i])
    
    return cantidad

# 10

def buscar_el_maximo(p: Pila) -> int:
    max: int = 0

    for i in range(len(p.queue)):
        if p.queue[i] >= max:
            max = p.queue[i]
    
    return max

# 11

def esta_bien_balanceada(s: str) -> bool:
    paren_abiertos: Pila = Pila()
    res: bool = True
    
    for i in range(len(s)):
        if s[i] == "(":
            paren_abiertos.put(1)
        elif s[i] == ")" and paren_abiertos.empty():
            res = False
        elif s[i] == ")":
            paren_abiertos.get()
    
    if not paren_abiertos.empty():
        res = False
    
    return res

# 12

def evaluar_expresion(s: str) -> float:
    p: Pila = Pila()

    token: str = "" # solo los números

    for i in range(len(s)):
        if s[i] == " " and (s[i-1] >= "0" and s[i-1] <= "9"):
            p.put(int(token))
            token = "" # reset
        elif s[i] >= "0" and s[i] <= "9":
            token = token + s[i]
        elif s[i] == "+":
            a = p.get()
            b = p.get()
            r = a + b
            p.put(r)
        elif s[i] == "-":
            a = p.get()
            b = p.get()
            r = b - a
            p.put(r)
        elif s[i] == "*":
            a = p.get()
            b = p.get()
            r = a * b
            p.put(r)
        elif s[i] == "/":
            a = p.get()
            b = p.get()
            r = b / a
            p.put(r)

    res: float = float(p.get())

    return res

# COLAS

from queue import Queue as Cola

# 13

def cola_nros_al_azar(cantidad: int, desde: int, hasta: int) -> Cola:
    c = Cola()
    pila: Pila = generar_nros_al_azar(cantidad, desde, hasta)
    
    while not pila.empty():
        nro: int = pila.get()
        c.put(nro)
    
    return c

# 14

def c_cantidad_elementos(c: Cola) -> int:
    cola = c
    cantidad: int = 0

    for _ in range(len(c.queue)):
        if not c.empty():
            c.get()
            cantidad = cantidad + 1
    
    c = cola
    return cantidad

# 15

def c_buscar_el_maximo(c: Cola) -> int:
    max: int = 0

    for _ in range(len(c.queue)):
        nro: int = c.get()
        if nro >= max:
            max = nro
        c.put(nro)
    
    return max

# 16

def armar_secuencia_de_bingo() -> Cola:
    secuencia: Cola = Cola()

    nros: list = []

    for i in range(0, 100):
        nros.append(i)
    
    for n in range(len(nros)-1,0,-1):
        m = random.randint(0,n)
        nros[n], nros[m] = nros[m], nros[n]
    
    for i in range(len(nros)):
        secuencia.put(nros[i])
    
    return secuencia

def jugar_carton_de_bingo(carton: list, bolillero: Cola) -> int:
    carton_copy = carton.copy()

    jugadas: int = 0
    jugadas_totales: int = 0


    for _ in range(len(list(bolillero.queue))):
        if len(carton_copy) == 0:
            jugadas_totales = jugadas
        else:
            nro: int = bolillero.get()

            jugadas = jugadas + 1

            j: int = 0

            while j < len(carton_copy):
                if nro == carton_copy[j]:
                    carton_copy.pop(j)
                
                j = j + 1
            
            bolillero.put(nro)

    return jugadas_totales

# 17

def n_pacientes_urgentes(c: Cola) -> int:
    urgentes: int = 0

    for _ in range(len(list(c.queue))):
        paciente = c.get()

        if paciente[0] <= 3:
            urgentes = urgentes + 1
        
        c.put(paciente)

    return urgentes

# 18

def atencion_a_clientes(c: Cola) -> Cola:
    clientes: Cola = Cola()
    prioridad: Cola = Cola()
    preferencial: Cola = Cola()
    resto: Cola = Cola()

    tipos: list = [prioridad,preferencial,resto]

    for _ in range(len(list(c.queue))):
        cliente: tuple [(str,int,bool,bool)] = c.get()

        if cliente[3]:
            prioridad.put(cliente)
        elif cliente[2]:
            preferencial.put(cliente)
        else:
            resto.put(cliente)

        c.put(cliente)

    for i in range(len(tipos)):
        for _ in range(len(list(tipos[i].queue))):
            cliente: tuple [(str,int,bool,bool)] = tipos[i].get()
            clientes.put(cliente)
    
    return clientes

# DICCIONARIOS

# 19

def agrupar_por_longitud(nombre_archivo: str) -> dict:
    archivo: str = open(nombre_archivo, "r")
    contenido: str = archivo.read()
    archivo.close()

    diccionario: dict = {}

    palabra: str = ""

    for i in range(len(contenido)):
        if contenido[i] != " " and contenido[i] != "\n":
            palabra = palabra + contenido[i]
        else:
            longitud: int = longitud_palabra(palabra)

            if not pertenece_dictkeys(longitud, diccionario):
                diccionario[longitud] = 1
            else:
                diccionario[longitud] = diccionario[longitud] + 1
            
            palabra = ""
    
    return diccionario


def longitud_palabra(palabra: str) -> int:
    longitud: int = 0

    for _ in range(len(palabra)):
        longitud = longitud + 1

    return longitud

def pertenece_dictkeys(key, dict: dict) -> bool:
    res: bool = False

    for i in range(len(dict.keys())):
        if key == list(dict.keys())[i]:
            res = True

    return res

# 20

def d_calcular_promedio_por_estudiante(nombre_archivo_notas: str) -> dict[str,float]:
    archivo_notas: list = csv_a_lista(nombre_archivo_notas)
    promedios: dict [str,float] = {}

    i: int = 1

    while i < len(archivo_notas):
        if not pertenece_dictkeys((archivo_notas[i])[0], promedios):
            promedios[(archivo_notas[i])[0]] = promedio_estudiante(nombre_archivo_notas, (archivo_notas[i])[0])

        i = i + 1

    return promedios

# 21

def la_palabra_mas_frecuente(nombre_archivo: str) -> str:
    archivo: str = open(nombre_archivo, "r")
    contenido: str = archivo.read()
    archivo.close()

    diccionario: dict = {}

    palabra: str = ""
    max: int = 0

    for i in range(len(contenido)):
        if contenido[i] != " " and contenido[i] != "\n":
            palabra = palabra + contenido[i]
        elif palabra != "":
            if not pertenece_dictkeys(palabra, diccionario):
                diccionario[palabra] = 1
            else:
                diccionario[palabra] = diccionario[palabra] + 1
            
            palabra = ""

    for j in range(len(list(diccionario.values()))):
        if list(diccionario.values())[j] >= max:
            max = list(diccionario.values())[j]
            palabra = list(diccionario.keys())[j]
    
    return palabra


# 22

historiales: dict [str, Pila [str]] = {}

def visitar_sitio(historiales: dict [str, Pila [str]], usuario: str, sitio: str) -> None:
    if not pertenece_dictkeys(usuario, historiales):
        historiales[usuario] = Pila()
        historiales[usuario].put(sitio)
    else:
        historiales[usuario].put(sitio)

def navegar_atras(historiales: dict [str, Pila [str]], usuario: str) -> None:
    historiales[usuario].get()

# 23

inventario: dict [str, dict [float, int]] = {}

def agregar_producto(inventario: dict, nombre: str, precio: float, cantidad: int) -> None:
    info: dict [float, int] = {}

    if not pertenece_dictkeys(nombre, inventario):
        info["precio"], info["cantidad"] = precio, cantidad
        inventario[nombre] = info

def actualizar_stock(inventario: dict, nombre: str, cantidad: int) -> None:
    inventario[nombre]["cantidad"] = cantidad

def actualizar_precios(inventario: dict, nombre: str, precio: float) -> None:
    inventario[nombre]["precio"] = precio

def calcular_valor_inventario(inventario: dict) -> float:
    valor: float = 0

    for i in range(len(list(inventario.values()))):
        valor = valor + (list(inventario.values())[i]["precio"] * list(inventario.values())[i]["cantidad"])
    
    return valor

