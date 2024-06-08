# 1

def verificar_transacciones(s: str) -> int:
  res: int = (350 * ap_antes_corte("r", s)) - (56 * ap_antes_corte("v", s))

  return res

def ap_antes_corte(c: str, s: str) -> int:
  apariciones: int = 0
  saldo: int = 0
  
  for chr in s:
    if chr == "r":
      saldo += 350
      if chr == c:
        apariciones += 1
    elif chr == "v" and saldo > 56:
      saldo -= 56
      if chr == c:
        apariciones += 1
    elif chr == "x":
      break
        
  
  return apariciones

# 2

def valor_minimo(s: list [tuple [float, float]]) -> float:
  min: float = 1000000000

  for val in s:
    if val[0] < min:
      min = val[0]
  
  return min

# 3

valores_diarios: dict = {}

# { str : [(int,float)] }

def valores_extremos(valores_diarios: dict [str,list [tuple [int,float]]]) -> dict [str,tuple [float,float]]:
  res: dict = {}

  for key in valores_diarios.keys():
    res[key] = val_min_max(valores_diarios[key])
  
  return res


def val_min_max(tuplas: list [tuple [int,float]]) -> tuple [float, float]:
  min: float = 10000000
  max: float = -10000000
  
  for elem in tuplas:
    if elem[1] >= max:
      max = elem[1]
    if elem[1] <= min:
      min = elem[1]
  
  return tuple([min,max])

# 4

def es_sudoku_valido(m: list [list [int]]) -> bool:
  sudoku: list = m.copy()

  colum: list = []

  f_bool: bool = True
  c_bool: bool = True
  res: bool = False

  for i in range(len(sudoku)):
    for j in range(len(sudoku[i])):
      if cuanto_aparece(sudoku[i], sudoku[i][j]) > 1 and sudoku[i][j] != 0:
        f_bool = False
      
      if i == 0:
        colum.append([sudoku[i][j]])
      else:
        colum[j].append(sudoku[i][j])

  for i in range(len(colum)):
    for j in range(len(colum[i])):
      if cuanto_aparece(colum[i], colum[i][j]) > 1 and colum[i][j] != 0:
        c_bool = False


  if f_bool and c_bool:
    res = True

  return res

def cuanto_aparece(lista: list, elem) -> int:
  apariciones: int = 0

  for e in lista:
    if e == elem:
      apariciones += 1
  
  return apariciones





