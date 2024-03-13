def factorial(n):
    if n == 0:
        return 1
    else:
        return n * factorial(n-1)
    
    
print("Vamos a calcular el factorial de un numero")
x = int(input("Introduce un numero: "))\
    
print("El factorial de", x, "es", factorial(x))