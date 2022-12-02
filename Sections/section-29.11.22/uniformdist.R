n = 1000
a =0
b =1
x = runif(n,min = a,max = b)
ex = (0+1)/2
m1 = sum(x)/n
m2 = sum(x^2)/n
aest = m1 - sqrt(3*(m2 - m1^2))
best = m1 + sqrt(3*(m2 - m1^2))
print(aest)
print(best)
print(ex)
print(m1)