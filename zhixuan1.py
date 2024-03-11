# -*- coding: utf-8 -*-
"""Untitled12.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1r7sp0suPzY5R0X5EgUPvOUUhUKDr-4w-
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from statsmodels.formula.api import ols
import plotly.express as px
from scipy.optimize import leastsq
import plotly.graph_objects as go
from sklearn.preprocessing import PolynomialFeatures
from sklearn.linear_model import LinearRegression
from sklearn.pipeline import make_pipeline
import statsmodels.api as sm
import scipy.stats as stats
import numpy as np
from mpl_toolkits.mplot3d import Axes3D

data=pd.read_csv("/content/dataset09.csv",index_col=0)
print(data)

# Exploratory data analysis
sns.boxplot(x='genre', y='rating', data=data,showfliers=False)
plt.ylabel('Rating')
plt.xlabel('genre')
plt.show()

fig1=px.scatter_3d(data,x="genre",y="votes",z="rating")
fig1.show()

sns.regplot(x="length",y="rating",data=data,fit_reg=True)
plt.show()

#Data processing
data= pd.DataFrame(data)
mean_length = data['length'].mean()
data['length'].fillna(mean_length, inplace=True) #Use mean to fill NA values
data['rating'] = data['rating'].apply(lambda x: 1 if x >= 7 else 0)
print(data)

sns.boxplot(x='rating', y='budget', data=data,showfliers=False)
plt.xlabel('Rating')
plt.ylabel('Budget')
plt.show()
sns.boxplot(x='rating', y='year', data=data,showfliers=False)
plt.xlabel('Rating')
plt.ylabel('Year')
plt.show()

#total GLM
data_encoded = pd.get_dummies(data, columns=['genre'])
data_encoded = sm.add_constant(data_encoded)

X = data_encoded[['year', 'length', 'budget', 'votes']+ list(data_encoded.filter(like='genre_').columns)]
y = data_encoded['rating']

model = sm.GLM(y, X, family=sm.families.Binomial())
results = model.fit()

print(results.summary())
print("AIC:", results.aic)

#categorical explanatory variable
data_gen = data.groupby(['genre', 'rating']).size().reset_index(name='count')
data_gen['percentage'] = data_gen['count'] / data_gen['count'].sum() * 100
print(data_gen)
plt.figure(figsize=(10, 6))
sns.barplot(x='genre', y='percentage', hue='rating', data=data_gen)
plt.xlabel('Genre')
plt.ylabel('Proportion')
plt.title('Proportion of ratings by genre')
plt.legend(title='Rating', labels=['0', '1'])
plt.show()

#Log-odds
X = data[['genre']]
y = data['rating']

X_encoded = pd.get_dummies(X, columns=['genre'])
X_encoded = sm.add_constant(X_encoded)

model = sm.GLM(y, X_encoded, family=sm.families.Binomial())
results2 = model.fit()

print(results2.summary())
print("AIC:", results2.aic)

levels = data['genre'].unique()
print(levels)

conf_int=results2.conf_int()
plt.errorbar(x=conf_int.index,y=conf_int[0],yerr=conf_int[1]-conf_int[0], fmt='o')
plt.xlabel('Genre')
plt.ylabel('Log-Odds')
plt.title('Log-Odds')
plt.xticks(rotation=45)
plt.axhline(y=0, color='r', linestyle='--')
plt.show()

#odds
coef_exp = np.exp(results2.params)
print("Genre Coefficients (Exponential):\n", coef_exp)

#hierarchical assigned scores
data['genre'] = data['genre'].replace({'Documentary': 3, 'Short': 3,'Comedy':2,'Animation':2,'Action':1,'Drama':1,'Romance':1})
print(data)
sns.boxplot(x='genre', y='votes', data=data,showfliers=False)
plt.ylabel('votes')
plt.xlabel('genre')
plt.show()

#numerical explanatory variable
data = pd.get_dummies(data, drop_first=True)
model = sm. GLM(data['rating'], data.drop('rating', axis=1))
result3 = model.fit()

print(result3.summary())
print("AIC:", result3.aic)

#Probabilities
print("Coefficients:")
print(result3.params)
print("\nConfidence Intervals:")
print(result3.conf_int())

probabilities = np.exp(result3.params) / (1 + np.exp(result3.params))
print("\nProbabilities:")
print(probabilities)