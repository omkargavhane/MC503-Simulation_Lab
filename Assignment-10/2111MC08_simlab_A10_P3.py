import random
import math
import statistics as stat

normal=stat.NormalDist(mu=0,sigma=1)
samples=normal.samples(2000)

#interval=[]
#for i in range(10):


#Method 1
sample_size=2000
u1=[random.uniform(0,1) for i in range(sample_size)]
u2=[random.uniform(0,1) for i in range(sample_size)]
x1_m1=[]
x2_m1=[]
for i in range(sample_size):
    x1_m1.append((-2*math.log(u1[i]))**0.5*math.cos(2*math.pi*u2[i]))
    x2_m1.append((-2*math.log(u1[i]))**0.5*math.sin(2*math.pi*u2[i]))

x1_m1.extend(x2_m1)
x_m1=x1_m1
x_m1_e=[2*e+1 for e in samples]




#Method 2
x1_m2=[]
x2_m2=[]
cnt=0
while True:
    u1=random.uniform(0,1)
    u2=random.uniform(0,1)
    v1=2*u1-1
    v2=2*u2-1
    w=v1**2+v2**2
    if w<=1:
        y=(-(2*math.log(w))/w)**0.5
        x1=v1*y
        x2=v2*y
        x1_m2.append(x1)
        x2_m2.append(x2)
        cnt+=1
    if cnt==sample_size:
        break

x1_m2.extend(x2_m2)
x_m2=x1_m2



x_m2_e=[2*e+1 for e in samples]




