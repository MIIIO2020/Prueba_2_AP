library(dplyr)
library(data.table)
library(survival)
library(flexsurv)
library(ggplot2)
library(dplyr)
library(ggfortify)

#data
data<-fread("/Users/antoniaindaorlandi/Desktop/Análisis Predictivo/Prueba 2/data_model.csv")

#Parte III a
#Modelo exponencial 
modelo_exp<-survreg(Surv(time_day) ~ as.factor(model) + as.factor(smart_1_normalized) + as.factor(smart_3_normalized) + as.factor(smart_5_normalized) + as.factor(smart_7_normalized) + as.factor(smart_192_normalized) + as.factor(smart_193_normalized) + as.factor(smart_194_normalized) + as.factor(smart_197_normalized) + as.factor(smart_198_normalized), data, dist='exponential') #Después de la "colita de chancho" agregar todas las variables…
modelo_exp
Verosimilitud_exp<-logLik(modelo_exp)


#Modelo weibull
modelo_wei<-survreg(Surv(time_day) ~ as.factor(model) + as.factor(smart_1_normalized) + as.factor(smart_3_normalized) + as.factor(smart_5_normalized) + as.factor(smart_7_normalized) + as.factor(smart_192_normalized) + as.factor(smart_193_normalized) + as.factor(smart_194_normalized) + as.factor(smart_197_normalized) + as.factor(smart_198_normalized), data, dist='weibull') #Después de la "colita de chancho" agregar todas las variables…
modelo_wei
Verosimilitud_wei<-logLik(modelo_wei)


#Para comparar cuál es el mejor modelo: analizar la verosimilitud
#LogLikelihood…
cat("El modelo que mejor se adapta a los datos es: ", ifelse (Verosimilitud_exp>Verosimilitud_wei, "el Exponencial","el Weibull"),"\n","Dado que tiene una verosimilitud mayor y equivalente a:",ifelse(Verosimilitud_exp>Verosimilitud_wei,Verosimilitud_exp,Verosimilitud_wei))
  
#Interprete los resultados del modelo:
  #Dependiendo del modelo elegido, observar el comportamiento de las variables, a partir del riesgo relativo (HR)
  #HR=exp(ß)

#Interpretación por varibale
#model (ST40000DM000 y OTHER)


#smart_1_normalized (0.5 , 0.75 , 1)


#smart_3_normalized ()


#smart_5_normalized


#smart_7_normalized


#smart_192_normalized


#smart_193_normalized


#smart_194_normalized


#smart_197_normalized


#smart_198_normalized



#Parte III b
#Modelo Lognormal
modelo_logn<-survreg(Surv(time_day) ~ as.factor(model) + as.factor(smart_1_normalized) + as.factor(smart_3_normalized) + as.factor(smart_5_normalized) + as.factor(smart_7_normalized) + as.factor(smart_192_normalized) + as.factor(smart_193_normalized) + as.factor(smart_194_normalized) + as.factor(smart_197_normalized) + as.factor(smart_198_normalized), data, dist='lognormal') #Después de la "colita de chancho" agregar todas las variables…
modelo_logn
Verosimilitud_logn<-logLik(modelo_logn)

#Modelo Gamma #Después de la "colita de chancho" agregar todas las variables…
#modelo_gam<-survreg(Surv(time_day) ~ as.factor(model), data, dist='gamma') #No existe gamma … ver como generar este modelo

modelo_gamma<-flexsurvreg(Surv(time_day)~as.factor(model) + as.factor(smart_1_normalized) + as.factor(smart_3_normalized) + as.factor(smart_5_normalized) + as.factor(smart_7_normalized) + as.factor(smart_192_normalized) + as.factor(smart_193_normalized) + as.factor(smart_194_normalized) + as.factor(smart_197_normalized) + as.factor(smart_198_normalized),data=data,dist='gamma')
modelo_gamma
Verosimilitud_gamma<-logLik(modelo_gamma)

autoplot(modelo_gamma)
plot(modelo_gamma)


#Para comparar cuál es el mejor modelo: analizar la verosimilitud
#LogLikelihood…
cat("El modelo que mejor se adapta a los datos es: ", ifelse (Verosimilitud_logn>Verosimilitud_gamma, "el Lognormal","el Gamma"),"\n","Dado que tiene una verosimilitud mayor y equivalente a:",ifelse(Verosimilitud_logn>Verosimilitud_gamma,Verosimilitud_logn,Verosimilitud_gamma))

#Comparación respecto al modelo seleccionado en la parte a:
Verosimilitud=c(Verosimilitud_exp,Verosimilitud_wei,Verosimilitud_logn,Verosimilitud_gamma)
Modelos=c("Exponencial","Weibull","Lognormal","Gamma")
Indice=which.max(Verosimilitud)
cat("El modelo que mejor se adapta a los datos es el: ",Modelos[Indice],"\n", "Con una verosimilitud de ",Verosimilitud[Indice])

#Parte III c
#Insertar modelo Kaplan-Meier:


#Modelo seleccionado entre las partes a y b:


#Test general de comparación (entre curvas de sobrevivencia):
#z=diferencia/error estándar=(S_o - S_1)/SQRT(sigma^2_{S_o}-sigma^2_{S_1})  para comparación de sobrevivencias
#z=diferencia/error estándar=(t_o - t_1)/SQRT(sigma^2_{t_o}-sigma^2_{t_1})  para comparación de percentiles
#Idea: en vez de percentiles utilizar los cuartiles




#Parte III d (Elegimos paso 2º opción 2)
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


