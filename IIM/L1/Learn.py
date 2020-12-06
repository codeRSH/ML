# -*- coding: utf-8 -*-
"""
Created on Sat Jul 18 19:08:33 2020

@author: ravs
"""

import numpy as np
import pandas as pd
import matplotlib as pm
import seaborn as sb
import sklearn as sk
import os


# To execute each line press F9

income = np.array([1000, 1500, 2000, 4000])
income

expenses = income * 0.6
print(expenses)

savings = income - expenses
print(savings)

school = {"Name": ["Ram", "Shyam", "Mahesh"], "Age" : [23, 32, 45]}
school

school_df = pd.DataFrame(school)
print(school_df)

os.getcwd()
os.chdir("C:\\Users\\ravs\\Development\\ML")
os.getcwd()

data1 = pd.read_csv("pizza.csv")
data1
data1.columns
data1.shape
data1.head(10)
data1.tail(10)
data1.dtypes

data1.brand[11]
data1.describe()
data1.mois.describe()
data1.brand.value_counts()

data3 = data1[["mois", "carb"]]
print(data3)

data3

data1.isnull()
data1.isnull().sum()
data1["newmois"] = data1["mois"] * 0.3
data1
data1.shape


data1[1:50].shape

data1[["mois", "carb"]][0:100]

data7 = data1[(data1["mois"] < 30) & (data1["brand"] == "A")]
data7

data8 = data1[(data1["mois"] < 30) | (data1["brand"] == "A")]
data8
data8.to_csv("filter.csv")

data11 = data1.id.duplicated()
sum(data11)
data12 = data1.drop_duplicates(["id"])
