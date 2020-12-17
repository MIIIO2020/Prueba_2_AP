library(dplyr)
library(data.table)
library(survival)
library(flexsurv)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(rms)
library(Hmisc) 
library(remotes) 

#Lectura de los datos
data<-fread("/Users/antoniaindaorlandi/Desktop/Análisis Predictivo/Prueba 2/data_smart_skew.csv")
data<-rename(data,model=model_Bin)

### Parte III a ####
#Modelo exponencial 
modelo_exp<-survreg(Surv(time_day) ~ as.factor(model)  + as.factor(smart_3_f) + smart_5_normalized_mean + smart_7_normalized_mean + smart_192_normalized_mean  + smart_194_normalized_mean + smart_197_normalized_mean + smart_198_normalized_mean + smart_3_normalized_delta_skew + smart_7_normalized_delta_skew + smart_193_normalized_delta_skew, data, dist='exponential') #Después de la "colita de chancho" agregar todas las variables…
modelo_exp
#modelo_exp<-survreg(Surv(time_day) ~ as.factor(model) + as.factor(smart_1_f) + as.factor(smart_3_f) + as.factor(smart_5_f) + as.factor(smart_7_f) + as.factor(smart_192_f) + as.factor(smart_193_f) + as.factor(smart_194_f) + as.factor(smart_197_f) + as.factor(smart_198_f)+ smart_3_normalized_delta_skew + smart_7_normalized_delta_skew + smart_193_normalized_delta_skew, data, dist='exponential') #Después de la "colita de chancho" agregar todas las variables…
Verosimilitud_exp<-logLik(modelo_exp)


#Modelo weibull
modelo_wei<-survreg(Surv(time_day) ~ as.factor(model)  + as.factor(smart_3_f) + smart_5_normalized_mean + smart_7_normalized_mean + smart_192_normalized_mean  + smart_194_normalized_mean + smart_197_normalized_mean + smart_198_normalized_mean + smart_3_normalized_delta_skew + smart_7_normalized_delta_skew + smart_193_normalized_delta_skew, data, dist='weibull') #Después de la "colita de chancho" agregar todas las variables…
modelo_wei
Verosimilitud_wei<-logLik(modelo_wei)


#Para comparar cuál es el mejor modelo: analizar la verosimilitud
# 1ª LogLikelihood:
cat("El modelo que mejor se adapta a los datos es: ", ifelse (Verosimilitud_exp>Verosimilitud_wei, "el Exponencial","el Weibull"),"\n","Dado que tiene una verosimilitud mayor y equivalente a:",ifelse(Verosimilitud_exp>Verosimilitud_wei,Verosimilitud_exp,Verosimilitud_wei))

# 2º Gráfico comparativo curvas entre los modelos
## Exponencial tipo para graficar:
S_exp<-survfit(Surv(predict(modelo_exp))~1, data=data)

## Weibull
S_wei<-survfit(Surv(predict(modelo_wei))~1, data=data)

legenda = c("Kaplan Meier", "Cox", "Exponencial",
            "Weibull",'Lognormal','Gamma')#, 
colores = c("black","orange", "green", "red",'blue','purple')#, 

ggplot(S_exp)+
  geom_step(aes(S_exp$time,S_exp$surv,colour = "Exponencial"))+
  geom_line(data=S_wei,aes(x = S_wei$time,
                           y = S_wei$surv,colour = "Weibull" ))+
  scale_colour_manual("", 
                      breaks = legenda,
                      values =colores) +
  xlab("Tiempo en días") +
  scale_y_continuous("Survival Probability", limits = c(0,1)) + 
  labs(title="Gráfica comparativa")


#Interprete los resultados del modelo:
# 1ª Comparación de los betas de cada modelo para las diferentes variables
#Separación de los beta entre variables numéricas y categóricas
B_exp=modelo_exp$coefficients
l=length(B_exp)
B_exp_f = B_exp[2:5]
B_exp_n = B_exp[6:l]

B_wei=modelo_wei$coefficients
l=length(B_wei)
B_wei_f = B_wei[2:5]
B_wei_n = B_wei[6:l]

#Gráfico variables numéricas
Com_Betas<- data.frame(
  'Nombre de los coeficientes'=names(B_exp_n),
  'Coeficientes del modelo exponencial'=unname(B_exp_n),
  'Coeficientes del modelo weibull'=unname(B_wei_n)
)

Com_Betas$Coeficientes.del.modelo.exponencial=sapply(
  Com_Betas$Coeficientes.del.modelo.exponencial,
  function(x) ifelse(is.na(x),0,exp(x) ))

Com_Betas$Coeficientes.del.modelo.weibull=sapply(
  Com_Betas$Coeficientes.del.modelo.weibull,
  function(x) ifelse(is.na(x),0,exp(x) ))

## Add legends
legenda = c( "Exponencial", "Weibull")
colores = c("green", "red")


ggplot(Com_Betas)+
  geom_point(aes(Com_Betas$Coeficientes.del.modelo.exponencial
                 ,Com_Betas$Nombre.de.los.coeficientes
                 ,colour = "Exponencial"))+
  geom_point(aes(Com_Betas$Coeficientes.del.modelo.weibull
                 ,Com_Betas$Nombre.de.los.coeficientes
                 ,colour = "Weibull"))+
  scale_colour_manual("", 
                      breaks = legenda,
                      values =colores) +
  xlab("Tasa de riesgo") +
  ylab("Variables") + 
  labs(title="Grafica de coeficientes variables numéricas")

#Gráfico variables categóricas
Com_Betas_f<- data.frame(
  'Nombre de los coeficientes'=names(B_exp_f),
  'Coeficientes del modelo exponencial'=unname(B_exp_f),
  'Coeficientes del modelo weibull'=unname(B_wei_f)
)

Com_Betas_f$Coeficientes.del.modelo.exponencial=sapply(
  Com_Betas_f$Coeficientes.del.modelo.exponencial,
  function(x) ifelse(is.na(x),0,exp(x)**(-1) ))

Com_Betas_f$Coeficientes.del.modelo.weibull=sapply(
  Com_Betas_f$Coeficientes.del.modelo.weibull,
  function(x) ifelse(is.na(x),0,exp(x)**(-1) ))

## Add legends
legenda = c( "Exponencial", "Weibull")
colores = c("green", "red")

ggplot(Com_Betas_f)+
  geom_point(aes(Com_Betas_f$Coeficientes.del.modelo.exponencial
                 ,Com_Betas_f$Nombre.de.los.coeficientes
                 ,colour = "Exponencial"))+
  geom_point(aes(Com_Betas_f$Coeficientes.del.modelo.weibull
                 ,Com_Betas_f$Nombre.de.los.coeficientes
                 ,colour = "Weibull"))+
  scale_colour_manual("", 
                      breaks = legenda,
                      values =colores) +
  xlab("Tasa de riesgo") +
  ylab("Variables") + 
  labs(title="Grafica de coeficientes variables categóricas")


#Dependiendo del modelo elegido, observar el comportamiento de las variables, a partir del riesgo relativo (HR)
#HR=exp(ß)
#Interpretación de los resultados Weibull: en el documento.

### Parte III b ####
#Modelo Lognormal
modelo_logn<-survreg(Surv(time_day) ~ as.factor(model)  + as.factor(smart_3_f) + smart_5_normalized_mean + smart_7_normalized_mean + smart_192_normalized_mean  + smart_194_normalized_mean + smart_197_normalized_mean + smart_198_normalized_mean + smart_3_normalized_delta_skew + smart_7_normalized_delta_skew + smart_193_normalized_delta_skew, data, dist='lognormal') #Después de la "colita de chancho" agregar todas las variables…
modelo_logn
Verosimilitud_logn<-logLik(modelo_logn)

#Modelo Gamma #Después de la "colita de chancho" agregar todas las variables…
#modelo_gam<-survreg(Surv(time_day) ~ as.factor(model), data, dist='gamma') #No existe gamma … ver como generar este modelo

modelo_gamma<-flexsurvreg(Surv(time_day)~ as.factor(model)  + as.factor(smart_3_f) + smart_5_normalized_mean + smart_7_normalized_mean + smart_192_normalized_mean  + smart_194_normalized_mean + smart_197_normalized_mean + smart_198_normalized_mean + smart_3_normalized_delta_skew + smart_7_normalized_delta_skew + smart_193_normalized_delta_skew,data=data,dist='gamma')
modelo_gamma
Verosimilitud_gamma<-logLik(modelo_gamma)
#Error en gamma variables correlacionadas

#Para comparar cuál es el mejor modelo: analizar la verosimilitud
#LogLikelihood…
cat("El modelo que mejor se adapta a los datos es: ", ifelse (Verosimilitud_logn>Verosimilitud_gamma, "el Lognormal","el Gamma"),"\n","Dado que tiene una verosimilitud mayor y equivalente a:",ifelse(Verosimilitud_logn>Verosimilitud_gamma,Verosimilitud_logn,Verosimilitud_gamma))

#Comparación respecto al modelo seleccionado en la parte a:
Verosimilitud=c(Verosimilitud_exp,Verosimilitud_wei,Verosimilitud_logn,Verosimilitud_gamma)
Modelos=c("Exponencial","Weibull","Lognormal","Gamma")
Indice=which.max(Verosimilitud)
cat("El modelo que mejor se adapta a los datos es el: ",Modelos[Indice],"\n", "Con una verosimilitud de ",Verosimilitud[Indice])

### Parte III c ####
#Insertar modelo Kaplan-Meier:
km <- survfit(Surv(time_day)~ 1, data=data)


legenda = c("Kaplan Meier", "Cox", "Exponencial",
            "Weibull",'Lognormal','Gamma')#, 
colores = c("black","orange", "green", "red",'blue','purple')#, 

ggplot(km)+
  geom_step(aes(km$time,km$surv,colour = "Kaplan Meier"))+
  geom_line(data=S_wei,aes(x = S_wei$time,
                           y = S_wei$surv,colour = "Weibull" ))+
  scale_colour_manual("", 
                      breaks = legenda,
                      values =colores) +
  xlab("Tiempo en días") +
  scale_y_continuous("Survival Probability", limits = c(0,1)) + 
  labs(title="Gráfica comparativa Kaplan Meier - Weibull")



#Test general de comparación (entre curvas de sobrevivencia):
#z=diferencia/error estándar=(S_o - S_1)/SQRT(sigma^2_{S_o}-sigma^2_{S_1})  para comparación de sobrevivencias
#z=diferencia/error estándar=(t_o - t_1)/SQRT(sigma^2_{t_o}-sigma^2_{t_1})  para comparación de percentiles
#Idea: en vez de percentiles utilizar los cuartiles




#Parte III d (Elegimos paso 2º opción 2)
#Ajustar un modelo de regresión de Cox:

cox <- coxph(Surv(time_day)~ as.factor(model)  + as.factor(smart_3_f) + smart_5_normalized_mean + smart_7_normalized_mean + smart_192_normalized_mean  + smart_194_normalized_mean + smart_197_normalized_mean + smart_198_normalized_mean + smart_3_normalized_delta_skew + smart_7_normalized_delta_skew + smart_193_normalized_delta_skew,data = data)
Verosimilitud_cox<-logLik(cox)
S_cox <- survfit(cox)


#Gráfico comparativo
## Kaplan-Meier estimator without grouping
km <- survfit(Surv(time_day)~ 1, data=data)

## Weibull
S_wei<-survfit(Surv(predict(modelo_wei))~1, data=data)

## Add legends
legenda = c("Kaplan Meier", "Cox", "Exponencial",
            "Weibull",'Lognormal','Gamma')#, "Log normal"
colores = c("black","orange", "green", "red",'blue','purple')#, "blue"

ggplot(km)+
  geom_step(aes(km$time,km$surv,colour = "Kaplan Meier"))+
  geom_line(data = S_cox, aes(S_cox$time, S_cox$surv, colour="Cox"))+
  geom_line(data=S_wei,aes(x = S_wei$time,
                           y = S_wei$surv,colour = "Weibull" ))+
  scale_colour_manual("", 
                      breaks = legenda,
                      values =colores) +
  xlab("Tiempo en días") +
  scale_y_continuous("Survival Probability", limits = c(0,1)) + 
  labs(title="Gráfica comparativa")

# "Desempate" entre decisión por el medio gráfico y métrico:
# métrica AIC
AIC(modelo_wei)
AIC(cox)

#Interpretación de los coeficientes de regresión
## Riesgo relativo HR = exp(ß)

B_cox = cox$coefficients
l=length(B_cox)
B_cox_f = B_cox[1:4]
B_cox_n = B_cox[5:l]

#Gráfico variables numéricas
Com_Betas<- data.frame(
  'Nombre de los coeficientes'=names(B_cox_n),
  'Coeficientes del modelo Cox'=unname(B_cox_n),
  'Coeficientes del modelo weibull'=unname(B_wei_n)
)

Com_Betas$Coeficientes.del.modelo.Cox=sapply(
  Com_Betas$Coeficientes.del.modelo.Cox,
  function(x) ifelse(is.na(x),0,exp(x) ))

Com_Betas$Coeficientes.del.modelo.weibull=sapply(
  Com_Betas$Coeficientes.del.modelo.weibull,
  function(x) ifelse(is.na(x),0,exp(x) ))

## Add legends
legenda = c( "Cox", "Weibull")
colores = c("orange", "red")


ggplot(Com_Betas)+
  geom_point(aes(Com_Betas$Coeficientes.del.modelo.Cox
                 ,Com_Betas$Nombre.de.los.coeficientes
                 ,colour = "Cox"))+
  geom_point(aes(Com_Betas$Coeficientes.del.modelo.weibull
                 ,Com_Betas$Nombre.de.los.coeficientes
                 ,colour = "Weibull"))+
  
    scale_colour_manual("", 
                      breaks = legenda,
                      values =colores) +
  xlab("Tasa de riesgo") +
  ylab("Variables") + 
  labs(title="Grafica de coeficientes variables numéricas")

#Gráfico variables categóricas
Com_Betas_f<- data.frame(
  'Nombre de los coeficientes'=names(B_cox_f),
  'Coeficientes del modelo Cox'=unname(B_cox_f),
  'Coeficientes del modelo weibull'=unname(B_wei_f)
)

Com_Betas_f$Coeficientes.del.modelo.Cox=sapply(
  Com_Betas_f$Coeficientes.del.modelo.Cox,
  function(x) ifelse(is.na(x),0,exp(x)**(-1)))

Com_Betas_f$Coeficientes.del.modelo.weibull=sapply(
  Com_Betas_f$Coeficientes.del.modelo.weibull,
  function(x) ifelse(is.na(x),0,exp(x)**(-1) ))

## Add legends
legenda = c( "Cox", "Weibull")
colores = c("orange","red")

ggplot(Com_Betas_f)+
  geom_point(aes(Com_Betas_f$Coeficientes.del.modelo.Cox
                 ,Com_Betas_f$Nombre.de.los.coeficientes
                 ,colour = "Cox"))+
  geom_point(aes(Com_Betas_f$Coeficientes.del.modelo.weibull
                 ,Com_Betas_f$Nombre.de.los.coeficientes
                 ,colour = "Weibull"))+
  
  scale_colour_manual("", 
                      breaks = legenda,
                      values =colores) +
  xlab("Tasa de riesgo") +
  ylab("Variables") + 
  labs(title="Grafica de coeficientes variables categóricas")

#Gráfico variables categóricas con análisis cox -> exp(beta)
Com_Betas_f<- data.frame(
  'Nombre de los coeficientes'=names(B_cox_f),
  'Coeficientes del modelo Cox'=unname(B_cox_f),
  'Coeficientes del modelo weibull'=unname(B_wei_f)
)

Com_Betas_f$Coeficientes.del.modelo.Cox=sapply(
  Com_Betas_f$Coeficientes.del.modelo.Cox,
  function(x) ifelse(is.na(x),0,exp(x)))

Com_Betas_f$Coeficientes.del.modelo.weibull=sapply(
  Com_Betas_f$Coeficientes.del.modelo.weibull,
  function(x) ifelse(is.na(x),0,exp(x)**(-1) ))

## Add legends
legenda = c( "Cox", "Weibull")
colores = c("orange","red")

ggplot(Com_Betas_f)+
  geom_point(aes(Com_Betas_f$Coeficientes.del.modelo.Cox
                 ,Com_Betas_f$Nombre.de.los.coeficientes
                 ,colour = "Cox"))+
  geom_point(aes(Com_Betas_f$Coeficientes.del.modelo.weibull
                 ,Com_Betas_f$Nombre.de.los.coeficientes
                 ,colour = "Weibull"))+
  
  scale_colour_manual("", 
                      breaks = legenda,
                      values =colores) +
  xlab("Tasa de riesgo") +
  ylab("Variables") + 
  labs(title="Grafica de coeficientes variables categóricas")


