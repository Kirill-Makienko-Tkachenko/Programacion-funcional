
#Funciones puras
def suma(a,b):
    return a+b

print(suma(2,3))
print(suma(2,3))
print(suma(2,3))


#Funciones impuras

def alCuadrado(a):
    return a*a
    
def sumaDeCuadrados(a,b):
    return alCuadrado(a) + alCuadrado(b)

def sumaDeCuadradosAntecesorYSucesor(a):
    return sumaDeCuadrados(a-1,a+1)

print(sumaDeCuadradosAntecesorYSucesor(10))

#funciones recursivas

#def saludar(mensaje):
 #   print(mensaje)
  #  saludar(mensaje)

#Funcion de orden superior

def aplicar_funcion(funcion, lista):
    return [funcion(x) for x in lista]

def cuadrado(x):
    return x*x


print(aplicar_funcion(cuadrado, [1,2,3,4,5]))

numeros = [1,2,3,4,5,6,7,8,9,10]

pares = list(filter(lambda x: x%2 == 0, numeros))

temperature = [36.5, 37, 37.5, 38, 39]

fahrenheit = list(map(lambda x: (x*9/5) + 32, temperature))
print(fahrenheit)

from functools import reduce

s = reduce(lambda x, y: x+y, numeros)
print(s)