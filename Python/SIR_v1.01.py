
# coding: utf-8

# # SIR version 01.01

# ## El modelo epidémico de SIR

# Una descripción matemática simple de la propagación de una enfermedad en una población es el llamado modelo SIR, que divide la población (fija) de N individuos en tres "compartimentos" que pueden variar en función del tiempo, t:
# 
# - S( t) son aquellos susceptibles pero aún no infectados con la enfermedad;
# - I( t) es la cantidad de individuos infecciosos;
# - R( t) son aquellos individuos que se han recuperado de la enfermedad y ahora tienen inmunidad contra ella.
# 
# El modelo SIR describe el cambio en la población de cada uno de estos compartimentos en términos de dos parámetros, $\beta$ y $\gamma$. 
# 
# $\beta$ describe la tasa de contacto efectiva de la enfermedad: un individuo infectado entra en contacto con otros $\beta \cdot N$ individuos por unidad de tiempo (de los cuales la fracción que es susceptible a contraer la enfermedad es $S/N$) $\gamma$ es la tasa de recuperación media: es decir, $\frac{1}{\gamma}$ es el período de tiempo promedio durante el cual un individuo infectado puede transmitirlo.
# Además hemos incluido el caso de decesos, los que representamos con el factor $\mu$.

# Las ecuaciones diferenciales que describen este modelo fueron derivadas primero por Kermack y McKendrick [Proc.R.Soc.A,115,772(1927)]:
# 
# $$\frac{dS}{dt}=−\beta \cdot \frac{S \cdot I}{N}$$
# 
# $$ \frac{dI}{dt} =\beta \cdot \frac{S\cdot I}{N}−\gamma \cdot I - \mu \cdot I$$ 
# 
# $$\frac{dR}{dt} = \gamma \cdot I $$
# 
# $$\frac{dr}{dt} = \mu \cdot I $$
# 

# El siguiente código de Python integra estas ecuaciones para una enfermedad caracterizada por los parámetros β = 0.2, 1/γ = 10 [dias] en una población de N = 1000 (quizás 'gripe en una escuela). 
# 
# El modelo se inicia con una sola persona infectada el día 0: I( 0 ) = 1. 
# 
# Las curvas trazadas de S(t), I(t) y R(t) están diseñados para ver los resultados obtenidos.

# In[37]:


import numpy as np
from scipy.integrate import odeint
import matplotlib.pyplot as plt


# In[10]:


# Una cuadrícula de puntos de tiempo (en días).
t = np.linspace(0, 160, 160)


# In[15]:


# Las ecuaciones diferenciales del modelo SIR.
def deriv(y, t, N, beta, gamma, mu):
    S, I, R, r = y
    dSdt = -beta * S * I / N
    dIdt = beta * S * I / N - gamma * I - mu * I
    dRdt = gamma * I
    drdt = mu * I
    return dSdt, dIdt, dRdt, drdt


# In[16]:


# Población total, N.
N = 10000
# Número inicial de individuos infectados, recuperados y fallecidos, I0, R0 y r0.
I0, R0, r0 = 1, 0, 0
# Todos los demás, S0, son susceptibles a la infección inicialmente.
S0 = N - I0 - R0 - r0


# In[36]:


# beta: tasa media de contacto, 
# gamma (en 1/dias): tasa media de recuperación.
# mu: tasa media de decesos    # 0.28
beta, gamma, mu = 0.335, 1./14, 0.006


# In[ ]:


# Vector de condiciones iniciales
y0 = S0, I0, R0, r0


# In[18]:


# Integra las ecuaciones SIR sobre la cuadrícula de tiempo, t.
ret = odeint(deriv, y0, t, args=(N, beta, gamma, mu))
S, I, R, r = ret.T


# In[35]:


# Grafico los datos en tres curvas separadas para S(t), I(t) y R(t)
with plt.style.context('seaborn'):
    plt.plot(t, S, 'b', alpha=0.5, lw=2, label='Susceptibles')
    plt.plot(t, I, 'r', alpha=0.5, lw=2, label='Infectados')
    plt.plot(t, R, 'g', alpha=0.5, lw=2, label='Recuperados con inmunidad')
    plt.plot(t, r, 'k', alpha=0.5, lw=2, label='Decesos')
    plt.xlabel('Tiempo [días]')
    plt.ylabel('Número ('+str(N)+'%)')
    plt.ylim(-N/20,N*1.1)
    plt.xlim(0, 120)
    plt.grid(b=True, which='major', c='w', lw=2, ls='-')
    plt.legend(loc= 'upper left', prop={'size': 16})
    plt.show()

