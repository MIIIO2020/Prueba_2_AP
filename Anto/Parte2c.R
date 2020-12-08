library(dplyr)
library(data.table)
library(survival)
library(ggplot2)
library(dplyr)
library(ggfortify)

data = fread("/Users/antoniaindaorlandi/Desktop/Análisis Predictivo/Prueba 2/data_model.csv")

#Estimación de la función de sobrevivencia vía Kaplan-Meier
km_1 <- survfit(Surv(time_day)~as.factor(smart_1_normalized), data=data)
km_m <- survfit(Surv(time_day)~ as.factor(model), data=data)

autoplot(km_1)
autoplot(km_m)
#Inspección gráfica del supuesto de riesgos proporcionales
#Método 1: log{-log(S)} vs log(t)
plot(km_1, col=c("black", "red"), fun="cloglog")
plot(km_m, col=c("black", "red"), fun="cloglog")

#Método 2: log(HR) vs t       HR: riesgo relativo
#plot(km_1, col=c("green","cyan",fun="kfhaekg")) Buscar la función
