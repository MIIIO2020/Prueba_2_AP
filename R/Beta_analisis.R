library(dplyr)
library(data.table)
library(survival)
library(flexsurv)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(rms)
library(Hmisc)
# install.packages("remotes")
library(remotes)
#data
data<-fread("data_fact_smart.csv")
data<-rename(data,model=model_Bin)
data$V1=NULL





### Parte III a ####
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
cat("El modelo que mejor se adapta a los datos es: ",
    ifelse (Verosimilitud_exp>Verosimilitud_wei,
            "el Exponencial","el Weibull"),
    "\n","Dado que tiene una verosimilitud mayor y equivalente a:"
    ,ifelse(Verosimilitud_exp>Verosimilitud_wei,Verosimilitud_exp,
            Verosimilitud_wei))
  

## Kaplan-Meier estimator without grouping
#km.null <- survfit(data = data, time_day ~ 1)
s <- with(data,Surv(time_day))

km <-survfit(data=data,s~ 1)
autoplot(km)


#survplot(km)


## Parametric estimation with Weibull distribution
#weibull.null <- survreg(data = lung, SurvObj ~ 1, dist = "weibull")

s_w=Surv(predict(modelo_wei))
S_wei=survfit(data=data,s_w~ 1)

s_e=Surv(predict(modelo_exp))
S_exp=survfit(data=data,s_e~ 1)

## Add legends
legenda = c("Kaplan Meier", "Exponencial", "Weibull")#, "Log normal"
colores = c("blue", "green", "red")#, "blue"

ggplot(km)+
  geom_step(aes(km$time,km$surv,colour = "Kaplan Meier"))+
  geom_line(data = S_exp, aes(S_exp$time,S_exp$surv
                              ,colour = "Exponencial"))+
  geom_line(data=S_wei,aes(x = S_wei$time,
                           y = S_wei$surv,colour = "Weibull" ))+
  scale_colour_manual("", 
                      breaks = legenda,
                      values =colores) +
  xlab("Tiempo en dias") +
  scale_y_continuous("Survival Probabiliti", limits = c(0,1)) + 
  labs(title="Grafica comparativa")







#Interprete los resultados del modelo:
  #Dependiendo del modelo elegido, observar el comportamiento de las variables, a partir del riesgo relativo (HR)
  #HR=exp(ß)

#Interpretación por varibale
#model (ST40000DM000 y OTHER)


#smart_1_normalized (0.5 , 0.75 , 1)
#Cuartil de referencia …


#smart_3_normalized (0.25 , 0.5 , 0.75 , 1)


#smart_5_normalized (0.25 , 0.5 , 0.75 , 1)


#smart_7_normalized (0.25 , 0.75 , 1)


#smart_192_normalized


#smart_193_normalized


#smart_194_normalized


#smart_197_normalized


#smart_198_normalized

#Grafico de los Beta de los modelos

B_exp=modelo_exp$coefficients
B_wei=modelo_wei$coefficients

Com_Betas<- data.frame(
  'Nombre de los coeficientes'=names(B_exp),
  'Coeficientes del modelo exponencial'=unname(B_exp),
  'Coeficientes del modelo weibull'=unname(B_wei)
)

ggplot(data= Com_Betas,aes(y=Com_Betas$Nombre.de.los.coeficientes,
                           x=Com_Betas$Coeficientes.del.modelo.exponencial)
       )+geom_point()


### Parte III b ####
#Modelo Lognormal
modelo_logn<-survreg(Surv(time_day) ~ as.factor(model) + as.factor(smart_1_normalized) + as.factor(smart_3_normalized) + as.factor(smart_5_normalized) + as.factor(smart_7_normalized) + as.factor(smart_192_normalized) + as.factor(smart_193_normalized) + as.factor(smart_194_normalized) + as.factor(smart_197_normalized) + as.factor(smart_198_normalized), data, dist='lognormal') #Después de la "colita de chancho" agregar todas las variables…
modelo_logn
Verosimilitud_logn<-logLik(modelo_logn)

#Modelo Gamma #Después de la "colita de chancho" agregar todas las variables…
#modelo_gam<-survreg(Surv(time_day) ~ as.factor(model), data, dist='gamma') #No existe gamma … ver como generar este modelo

modelo_gamma<-flexsurvreg(Surv(time_day)~as.factor(model) + as.factor(smart_1_normalized) + as.factor(smart_3_normalized) + as.factor(smart_5_normalized) + as.factor(smart_7_normalized) + as.factor(smart_192_normalized) + as.factor(smart_193_normalized) + as.factor(smart_194_normalized) + as.factor(smart_197_normalized) + as.factor(smart_198_normalized),data=data,dist='gamma')
modelo_gamma
Verosimilitud_gamma<-logLik(modelo_gamma)


#Para comparar cuál es el mejor modelo: analizar la verosimilitud
#LogLikelihood…
cat("El modelo que mejor se adapta a los datos es: ", ifelse (Verosimilitud_logn>Verosimilitud_gamma, "el Lognormal","el Gamma"),"\n","Dado que tiene una verosimilitud mayor y equivalente a:",ifelse(Verosimilitud_logn>Verosimilitud_gamma,Verosimilitud_logn,Verosimilitud_gamma))

#Comparación respecto al modelo seleccionado en la parte a:
Verosimilitud=c(Verosimilitud_exp,Verosimilitud_wei,Verosimilitud_logn,Verosimilitud_gamma)
Modelos=c("Exponencial","Weibull","Lognormal","Gamma")
Indice=which.max(Verosimilitud)
cat("El modelo que mejor se adapta a los datos es el: ",Modelos[Indice],"\n", "Con una verosimilitud de ",Verosimilitud[Indice])


## Graficas KM comparativas

s_log=Surv(predict(modelo_logn))
S_Log=survfit(data=data,s_log~ 1)

s_g=Surv(predict(modelo_gamma))
S_Gam=survfit(data=data,s_log~ 1)


## Add legends
legenda = c("Kaplan Meier", "Exponencial",
            "Weibull",'Lognormal','Gamma')#, "Log normal"
colores = c("blue", "green", "red",'black','purple')#, "blue"

ggplot(km)+
  geom_step(aes(km$time,km$surv,colour = "Kaplan Meier"))+
  geom_line(data = S_exp, aes(S_exp$time,S_exp$surv
                              ,colour = "Exponencial"))+
  geom_line(data=S_wei,aes(x = S_wei$time,
                           y = S_wei$surv,colour = "Weibull" ))+
  
  geom_line(data=S_Log,aes(x = S_Log$time,
                           y = S_Log$surv,colour = "Lognormal" ))+
  geom_line(data=S_Log,aes(x = S_Log$time,
                           y = S_Log$surv,colour = "Lognormal" ))+
  
  scale_colour_manual("", 
                      breaks = legenda,
                      values =colores) +
  xlab("Tiempo en dias") +
  scale_y_continuous("Survival Probabiliti", limits = c(0,1)) + 
  labs(title="Grafica comparativa")







### Parte III c ####
#Insertar modelo Kaplan-Meier:
km <- survfit(Surv(time_day)~ 1, data=data)
autoplot(km)


#Modelo seleccionado entre las partes a y b:
### Generación de S(t) de weibull ####
g<-modelo_wei$scale
n=205
t=data$time_day
T=sapply(t, function(x) ifelse(is.na(x),0,x^g ) )
lambda=(n/sum(T))^(1/g)

S=exp(-(lambda*t)^g)
plot(t,S)

data_S=data.frame()
data_S$S<-S
data_S<-cbind(S,t)

par(mfrow=c(2,1))
ggplot(data=data_S,aes(t,S))+geom_step()
autoplot(km)


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

### Graficas de KM####
# # Alternatively we can fully specify the plot in each layer. This
# # is not useful here, but can be more clear when working with complex
# # mult-dataset graphics
# ggplot() +
#   geom_point(data = df, aes(gp, y)) +
#   geom_point(data = ds, aes(gp, mean), colour = 'red', size = 3) +
#   geom_errorbar(
#     data = ds,
#     aes(gp, mean, ymin = mean - sd, ymax = mean + sd),
#     colour = 'red',
#     width = 0.4
#   )


## Kaplan-Meier estimator without grouping
#km.null <- survfit(data = data, time_day ~ 1)
s <- with(data,Surv(time_day))

km <-survfit(data=data,s~ 1)
autoplot(km)


#survplot(km)


## Overplot estimation from Cox regression by Efron method
cox <- coxph(Surv(time_day)~as.factor(model) + as.factor(smart_1_normalized) + as.factor(smart_3_normalized) + as.factor(smart_5_normalized) + as.factor(smart_7_normalized) + as.factor(smart_192_normalized) + as.factor(smart_193_normalized) + as.factor(smart_194_normalized) + as.factor(smart_197_normalized) + as.factor(smart_198_normalized),data = data)
S_cox=survfit(cox)
ggplot(S_cox,aes(S_cox$time,S_cox$surv), col = "green")+geom_line()

## Parametric estimation with Weibull distribution
#weibull.null <- survreg(data = lung, SurvObj ~ 1, dist = "weibull")

s_w=Surv(predict(modelo_wei))

# P_m
#

S_wei=survfit(data=data,s_w~ 1)

#data$predict_modelo_wei<-predict(modelo_wei)

# D_wei=cbind( predict(modelo_wei, type = "quantile", p = seq(0.0001, 0.99, by=.01))[1,],
#              rev(seq(0.0001, 0.99, by = 0.01)))
# 
# D_wei=cbind( predict(modelo_wei),
#              rev(seq(0.01, 0.99, by = 0.01)))

# lines(x = predict(modelo_wei, type = "quantile", p = seq(0.01, 0.99, by=.01))[1,],
#       y = rev(seq(0.01, 0.99, by = 0.01)),
#       col = "red")

## Parametric estimation with log-logistic distribution
#loglogistic.null <- survreg(data = lung, SurvObj ~ 1, dist = "loglogistic")
# lines(x = predict(modelo_logn, type = "quantile", p = seq(0.01, 0.99, by=.01))[1,],
#       y = rev(seq(0.01, 0.99, by = 0.01)),
#       col = "blue")

## Add legends
legenda = c("Kaplan Meier", "Cox", "Weibull")#, "Log normal"
colores = c("blue", "green", "red")#, "blue"

ggplot(km)+
    geom_step(aes(km$time,km$surv,colour = "Kaplan Meier"))+
    geom_line(data = S_cox, aes(S_cox$time,S_cox$surv
              ,colour = "Cox"))+
    geom_line(data=S_wei,aes(x = S_wei$time,
               y = S_wei$surv,colour = "Weibull" ))+
  scale_colour_manual("", 
                      breaks = legenda,
                      values =colores) +
  xlab("Tiempo en dias") +
  scale_y_continuous("Survival Probabiliti", limits = c(0,1)) + 
  labs(title="Grafica comparativa")
  
  
  
# legend(x = "topright",
#        legend = c("Kaplan-Meier", "Cox", "Weibull", "Log normal"),
#        lwd = 2, bty = "n",
#        col = c("black", "green", "red", "blue"))


