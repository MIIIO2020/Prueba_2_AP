library(dplyr)
library(data.table)
library(survival)
library(ggplot2)
library(dplyr)
library(ggfortify)

#data
data<-fread("/Users/antoniaindaorlandi/Desktop/Análisis Predictivo/Prueba 2/data_model.csv")

#Parte III a
#Modelo exponencial 
modelo_exp<-survreg(Surv(time_day) ~ , data, dist='exponential') #Después de la "colita de chancho" agregar todas las variables…


#Modelo weibull
modelo_wei<-survreg(Surv(time_day) ~ , data, dist='weibull') #Después de la "colita de chancho" agregar todas las variables…


#Para comparar cuál es el mejor modelo: analizar la verosimilitud
#Likelihood…

#Parte III b
#Modelo Lognormal
modelo_logn<-survreg(Surv(time_day) ~ , data, dist='lognormal') #Después de la "colita de chancho" agregar todas las variables…


#Modelo Gamma #Después de la "colita de chancho" agregar todas las variables…
modelo_gam<-survreg(Surv(time_day) ~ , data, dist='gamma') #No existe gamma … ver como generar este modelo


#Para comparar cuál es el mejor modelo: analizar la verosimilitud
#Likelihood…



#Parte III c
#Insertar modelo Kaplan-Meier:


#Modelo seleccionado entre las partes a y b:


#Test general de comparación (entre curvas de sobrevivencia):
#z=diferencia/error estándar=(S_o - S_1)/SQRT(sigma^2_{S_o}-sigma^2_{S_1})  para comparación de sobrevivencias
#z=diferencia/error estándar=(t_o - t_1)/SQRT(sigma^2_{t_o}-sigma^2_{t_1})  para comparación de percentiles
#Idea: en vez de percentiles utilizar los cuartiles




#Parte III d
#Ajustar un modelo de regresión de Cox:
#Cox<-lm()

#1º Estimación de los coeficientes ß: máxima verosimilitud (¿Del modelo elegido en los pasos anteriores?)

#2º (OPCIÓN 1) Test de la razón de verosimilitud (TRV): probar Ho
#Generar modelo basal y compararlo con un modelo basal + variable de estudio (factor ß) para probar su significancia ("Análisis de deviance")

#2º (OPCIÓN 2)Selección de variables: método ¿Stepwise?
#full<-lm(time_day~., data=data)
#null<-lm(time_day~1, data=data)
#modelo_cox<-step(null, scope = list(upper=full), data=model_data, direction="both")
#summary(modelo_cox)


