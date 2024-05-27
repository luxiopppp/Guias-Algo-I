from typing import List
import math
import array

# PRIMERA PARTE

# 1.1

def pertenece(s: list [int], e: int) -> bool:
    res: bool = False

    for i in range(len(s)):
        if s[i] == e:
            res = True

    return res

# 1.2

# def divide_a_todos(s: list [int], e: int) -> bool:
#     res = False
    
#     for i in range(len(s)):
#         if (s[i] % e) == 0:
#             res = True
#         else:
#             res = False
#             return res
    
#     return res

def divide_a_todos(s: list [int], e: int) -> bool:
    res: bool = True
    
    for i in range(len(s)):
        res = res and (s[i] % e) == 0 # checkea el caso base y el siguiente y ...
    
    return res

# 1.3

# 1.4

# 1.5

# 1.6

# 1.7

def fortaleza(c: str) -> str:
    res: str = "AMARILLA"

    if len(c) > 8 and hayUnaMin(c) and hayUnaMay(c) and hayUnNum(c):
        res = "VERDE"
    elif len(c) < 5:
        res = "ROJA"

    return res

def hayUnaMin(c: str) -> bool:
    res: bool = False

    for i in range(len(c)):
        if c[i] >= "a" and c[i] <= "z":
            res = True

    return res

def hayUnaMay(c: str) -> bool:
    res: bool = False

    for i in range(len(c)):
        if c[i] >= "A" and c[i] <= "Z":
            res = True

    return res

def hayUnNum(c: str) -> bool:
    res: bool = False

    for i in range(len(c)):
        if c[i] >= "0" and c[i] <= "9":
            res = True

    return res

# 1.8

# 1.9


# PARTE 2

# in es no modificable y no "devolvible"
# out e inout son modificable pero no "devolvible"

# 2.1

def borrarPares1(s: list [int]) -> list [int]:
    for i in range(len(s)):
        if (i % 2) == 0:
            s[i] = 0


# 2.2

def borrarPares2(s: list [int]) -> list [int]:
    res: list [int] = s.copy # si no pongo .copy cambia el valor en memoria de s

    for i in range(len(s)):
        if (i % 2) == 0:
            res[i] = 0
    
    return res

# 2.3

def borrarVocales(s: str) -> str:
    res: str = ""

    for i in range(len(s)):
        if not esVocal(s[i]):
            res = res + s[i]
    
    return res


def esVocal(c: chr) -> bool:
    res: bool = False

    if c == "a" or c == "e" or c == "i" or c == "o" or c == "u":
        res = True

    return res

# 2.4

def reemplazaVocales(s: str) -> str:

    




