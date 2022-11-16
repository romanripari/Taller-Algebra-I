
def es_primo(n):
  for i in range(2, int(n/2)+1):
    if (n%i) == 0:
      return False
  return True

def son_coprimos(p, a):    
    if maximo_comun_divisor_recursivo(p,a) == 1 or maximo_comun_divisor_recursivo(p,a) == 1:
        return True
    else:
        return False

def maximo_comun_divisor_recursivo(a, b):
    if b == 0:
        return a
    return maximo_comun_divisor_recursivo(b, a % b)

print(maximo_comun_divisor_recursivo(324,120))
print(maximo_comun_divisor_recursivo(120,324))

print(10 % 8)
print(8 % 10)


'''
print('12,9')
print(son_coprimos(12,9))
print('1007,1474')
print(son_coprimos(1007,1474))
print('1,9')
print(son_coprimos(1,9))


si a elevado (p-1) -1  mod p == 0: 
a coprimo p 

from pprint import pprint

listado = []
for n in range(20000):
    primos=[]
    for i in range(n):
        if es_primo(i):
            primos.append(i)

    listado.append(len(primos))

print(listado)
'''
