# 1

def acomodar(s: list [str]) -> list [str]:
  boletas: list = s.copy()
  up: list = []
  lla: list = []
  res: list = []

  for elem in boletas:
    if elem == "UP":
      up.append(elem)
    else:
      lla.append(elem)
    
  res = up + lla
  
  return res

# 2

def pos_umbral(s: list [int], u: int) -> int:
  lista: list = s.copy()
  sum: int = 0
  res: int = -1

  for i in range(len(lista)):
    sum = sum + lista[i]
    if sum > u and sum - lista[i] < u:
      res = i
    
  return res

# 3


def columnas_repetidas(mat: list[list[int]]) -> bool:
  colum: list = []
  res: bool = False

  for i in range(len(mat)):
    for j in range(len(mat[i])):
      if i == 0:
        colum.append(list([mat[i][j]]))
      else:
        colum[j].append(mat[i][j])

  h: int = int(len(colum)/2)
  c1: list = []
  c2: list = []

  for n in range(len(colum)):
    if n <= h-1:
      c1.append(colum[n])
    elif n > h-1:
      c2.append(colum[n])

  if c1 == c2:
    res = True

  return res

# 4

def cuenta_posiciones_por_nacion(naciones: list[str], torneos: dict[int,list[str]]) -> dict[str,list[int]]:
  res: dict = {}

  for nacion in naciones:
    res[nacion] = [0]*len(naciones)

  for value in torneos.values():
    for i in range(len(value)):
      res[value[i]][i] += 1
  
  return res







