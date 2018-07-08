# -*- coding: utf-8 -*-
"""
Created on Sun Jul  8 18:58:53 2018

@author: zhangjian
"""
def name(x):
    
    y=x*x-6*x+9
    return y
a=1
b=4
Err=0.001
T=0.618
c=a+(1-T)*(b-a)
d=b-(1-T)*(b-a)
Fc = name(c)
Fd = name(d)
while(1):
    if(abs(b-a)<Err):
        x=0.5*(a+b)
        break
    if(Fc<Fd):
        a=a
        b=d
        d=c
        Fd=Fc
        c=a+(1-T)*(b-a)
        Fc=name(c)
    else :
        b=b
        a=c
        c=d
        Fc=Fd
        d=b-(1-T)*(b-a)
        Fd=name(d)
print(x)


