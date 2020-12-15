library(dplyr)
library(data.table)
library(survival)
library(ggplot2)
library(dplyr)
library(ggfortify)

#Lectura de datos
#data<-fread("/Users/antoniaindaorlandi/Desktop/Análisis Predictivo/Prueba 2/data_fact_smart.csv")
data<-fread("data_fact_smart.csv")

data<-rename(data,model=model_Bin)

data$V1<-NULL


### Parte II a ####
#Análisis bivariado con el tiempo de vida
#Función de sobrevivencia vía Kaplan-Meier

#model
km_m <- survfit(Surv(time_day)~ as.factor(model), data=data)

autoplot(km_m)

#smart_1_normalized
km_1 <- survfit(Surv(time_day)~as.factor(smart_1_normalized), data=data)

autoplot(km_1)

#smart_3_normalized
km_3 <- survfit(Surv(time_day)~as.factor(smart_3_normalized), data=data)

autoplot(km_3)

#smart_5_normalized
km_5 <- survfit(Surv(time_day)~as.factor(smart_5_normalized), data=data)

autoplot(km_5)

#smart_7_normalized
km_7 <- survfit(Surv(time_day)~as.factor(smart_7_normalized), data=data)

autoplot(km_7)

#smart_192_normalized
km_192 <- survfit(Surv(time_day)~as.factor(smart_192_normalized), data=data)

autoplot(km_192)

#smart_193_normalized
km_193 <- survfit(Surv(time_day)~as.factor(smart_193_normalized), data=data)

autoplot(km_193)

#smart_194_normalized
km_194 <- survfit(Surv(time_day)~as.factor(smart_194_normalized), data=data)

autoplot(km_194)

#smart_197_normalized
km_197 <- survfit(Surv(time_day)~as.factor(smart_197_normalized), data=data)

autoplot(km_197)

#smart_198_normalized
km_198 <- survfit(Surv(time_day)~as.factor(smart_198_normalized), data=data)

autoplot(km_198)


### Parte II b ####
#Test de comparación entre curvas
#Test Log-rank: para detectar diferencias al final de la curva (compara curvas de dos grupos… buscar test génerico para más de dos curvas)
#Test de Wilcoxon: para detectar diferencias al inicio de la curva

### Parte II c ####
#Inspección gráfica del supuesto de riesgos proporcionales
#Método 1: log{-log(S)} vs log(t)

# col=c("black", "red","green" ,"cyan")

plot(km_1, col=c("black", "red","green" ,"cyan"), fun="cloglog")
title(main='Comparación de riesgos proporcionales',
      sub='Smart_1_normalized',
      xlab = 'log(tiempo)' , ylab='log{-log(S)}')


plot(km_3, col=c("black", "red","green" ,"cyan"), fun="cloglog")+
title(main='Comparación de riesgos proporcionales',
      sub='Smart_3_normalized',
      xlab = 'log(tiempo)' , ylab='log{-log(S)}')


plot(km_5, col=c("black", "red"), fun="cloglog")+
title(main='Comparación de riesgos proporcionales',
      sub='Smart_5_normalized',
      xlab = 'log(tiempo)' , ylab='log{-log(S)}')

plot(km_7, col=c("black", "red","green" ), fun="cloglog")
title(main='Comparación de riesgos proporcionales',
      sub='Smart_7_normalized',
      xlab = 'log(tiempo)' , ylab='log{-log(S)}')


plot(km_192, col=c("black", "red"), fun="cloglog")+
  title(main='Comparación de riesgos proporcionales',
        sub='Smart_192_normalized',
        xlab = 'log(tiempo)' , ylab='log{-log(S)}')
  

plot(km_193, col=c("black", "red","green" ,"cyan"), fun="cloglog")
title(main='Comparación de riesgos proporcionales',
      sub='Smart_193_normalized',
      xlab = 'log(tiempo)' , ylab='log{-log(S)}')


plot(km_194, col=c("black", "red","green" ,"cyan"), fun="cloglog")+
  title(main='Comparación de riesgos proporcionales',
        sub='Smart_194_normalized',
        xlab = 'log(tiempo)' , ylab='log{-log(S)}')


plot(km_197, col=c("black", "red","green" ,"cyan"), fun="cloglog")+
  title(main='Comparación de riesgos proporcionales',
        sub='Smart_197_normalized',
        xlab = 'log(tiempo)' , ylab='log{-log(S)}')


plot(km_198, col=c("black", "red","green" ,"cyan"), fun="cloglog")+
  title(main='Comparación de riesgos proporcionales',
        sub='Smart_198_normalized',
        xlab = 'log(tiempo)' , ylab='log{-log(S)}')

plot(km_m, col=c("black", "red","green" ,"cyan"), fun="cloglog")+
title(main='Comparación de riesgos proporcionales', sub='Model', xlab = 'log(tiempo)' , ylab='log{-log(S)}')

# Método 2: log(HR) vs t       HR: riesgo relativo

# Buscar la función

#plot( km_m, col=c("black", "red","green" ,"cyan"),fun="kfhaekg") 

