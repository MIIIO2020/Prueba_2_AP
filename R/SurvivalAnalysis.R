#install.packages("ranger")
#install.packages("rms")

library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(rms)

data(veteran)
head(veteran)

# The variables in veteran are:
# * trt: 1=standard 2=test
# * celltype: 1=squamous, 2=small cell, 3=adeno, 4=large
# * time: survival time in days
# * status: censoring status
# * karno: Karnofsky performance score (100=good)
# * diagtime: months from diagnosis to randomization
# * age: in years
# * prior: prior therapy 0=no, 10=yes

# Surv(time, status): Objeto que construye e interpreta tiempos de sobrevivencia

# Estimaci?n de la funci?n de Sobrevivncia v?a Kaplan-meier
### KM sin tomar en cuenta variables####
km <- with(veteran, Surv(time, status))
head(km,80)

km_fit <- survfit(Surv(time, status) ~ 1, data=veteran)
summary(km_fit, times = c(1,30,60,90*(1:10)))

autoplot(km_fit)



### Estimación de S(t), tomando en cuenta la variable trt####

km_trt_fit <- survfit(Surv(time, status) ~ trt, data=veteran)

autoplot(km_trt_fit)

###Estimación de S(t), tomando en cuenta la variable edad ####
# se segrega segun su rango etario.
# Podriamos utilizar el smart 9

vet <- mutate(veteran, AG = ifelse((age < 60), "LT60", "OV60"),
              AG = factor(AG),
              trt = factor(trt,labels=c("standard","test")),
              prior = factor(prior,labels=c("N0","Yes")))

km_AG_fit <- survfit(Surv(time, status) ~ AG, data=vet)
autoplot(km_AG_fit)

# Verificaci?n de riesgos proporcionales

plot(km_trt_fit, col=c("black", "red"), fun="cloglog")
plot(km_AG_fit, col=c("black", "red"), fun="cloglog")

# test de comparaci?n de grupos

# Test de logRank
survdiff(Surv(time, status) ~ as.factor(trt) , veteran)

# Test de Peto
survdiff(Surv(time, status) ~ as.factor(trt) , veteran,rho=1)

# H0: no existe dif entre los grupos vs Ha: existe diferencia
# p-valor (p<<0.05) sea peque?o para rechazar H0

# Test de logRank
survdiff(Surv(time, status) ~ AG, data=vet)
# Test de Peto
survdiff(Surv(time, status) ~ AG, data=vet,rho=1)

# H0: no existe dif entre los grupos vs Ha: existe diferencia
# p-valor (p<<0.05) sea peque?o para rechazar H0


## Treatment variable (rx)
#survplot(fit  = km_AG_fit,
#         conf = c("none","bands","bars")[1],
#         xlab = "",
#         label.curves = list(keys = "lines"),  # legend instead of direct label
#         loglog   = TRUE,                      # log(-log Survival) plot
#         logt     = TRUE,                      # log time
#)


# Ajuste de modelos param?tricos de regresi?n

# Modelo Exponencial
survreg(Surv(time, status) ~ as.factor(trt) + as.factor(celltype) +karno+ diagtime + age + as.factor(prior), veteran, dist='exponential')
survreg(Surv(time, status) ~ as.factor(trt) + as.factor(celltype) +karno+ diagtime + age + as.factor(prior), veteran, dist='weibull',scale=1)


# interpretaci?n de coeficientes y resultados
exp(-0.2195653067)
exp(-0.8202447458)-1
exp(-0.0494816350)-1



# Modelo Weibull
survreg(Surv(time, status) ~ as.factor(trt) + as.factor(celltype) +karno+ diagtime + age + as.factor(prior), veteran, dist='weibull')

# Docimar H0: gamma =1 vs Ha: gamma \neq 1