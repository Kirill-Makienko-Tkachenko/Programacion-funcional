#Suma de numeros pares

numeros = [1,2,3,4,5,6,7,8,9,10]

from functools import reduce

pares = reduce(lambda x, y: x+y, (filter(lambda x: x%2 == 0, numeros)))

print(pares)

#Producto acumulado de una lista

import itertools 
import operator


#Esto srive de esta manera itertools.accumulate(iterable[, func]) –> accumulate object
resultado = itertools.accumulate(numeros, operator.mul)
for each in resultado: 
    print(each) 
    
    
#Filtrado de palabras
palabras = ["Hola", "parangaricutirimicuaro", "tangamandapio", "esternomastoideo", "ITESM", "letras"]

filtrado = list(filter(lambda x: len(x)>6, palabras))
print(filtrado)