import numpy as np
from decimal import Decimal

x = np.array([1]+[1e-16]*(10000))
print( Decimal( sum(x) ) )


y = 1
for i in range(10000):
    y = y+1e-16
print(y)


z = 0
for i in range(10000):
    z = z+1e-16
z = z+1
print(z)