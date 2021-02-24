# -*- coding: utf-8 -*-
"""
Created on Wed Feb 10 12:01:59 2021

@author: RenzJoshuaVillanueva
"""
import random as r
def f():
    age_min = 0
    age_max = 120
    age = r.randint(age_min, age_max)
    
    
    sex = r.randint(0, 1)
    
    cp_min = 0
    cp_max = 3
    
    cp = r.randint(cp_min, cp_max)
    
    rbp_min = 94
    rbp_max = 150
    
    rbp = r.randint(rbp_min, rbp_max)
    
    chol_min = 126
    chol_max = 250
    
    chol = r.randint (chol_min, chol_max)
    
    fbs = r.randint(0, 1)
    
    recg = r.randint(0, 2)
    
    mhr = r.randint(71, 202)
    
    exang = r.randint(0, 1)
    
    oldpeak = round(r.uniform(0.000, 6.200), 3)
    
    slope = r.randint(0, 2)
    
    vessels = r.randint(0, 4)
    
    thal = r.randint(0, 3)
    
    hd = r.randint(0,1)

    print(f"{age}, {sex}, {cp}, {rbp}, {chol}, {fbs}, {recg}, {mhr}, {exang}, {oldpeak}, {slope}, {vessels}, {thal}, {hd}")

for i in range(20):
    f()
    

