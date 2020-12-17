library(dplyr)
library(data.table)
library(survival)
library(flexsurv)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(rms)
library(Hmisc) #Cosas rolf
library(remotes) #Cosas rolf
library(stats)

#Lectura de datos
#df=fread("/Users/antoniaindaorlandi/Desktop/Análisis Predictivo/Prueba 2/data_smart_mean_delta_skew.csv")
df=fread("data/data_smart_mean_delta_skew.csv")



### comienza parte 2 a y b ####

  # En esta parte 2 a se estructuran
  # las variables como tipo categorica.
  # Ademas se plotean los km y se calculan
  # los test de diferencias entre curvas


### KM ####

  #S(t) general
  km_fit <- survfit(Surv(time_day) ~ 1, data=df)
  summary(km_fit, time_day = c(1,30,60,90*(1:10)))
  autoplot(km_fit)

  rm(km_fit)


### KM-Model####
  # veo como la variable model posee influencia en el time_day
  ggplot(data = df, aes(y = model, x = time_day))+geom_point()

  #Histograma de los model
  ggplot(data=df, aes(y=model) ) + 
    geom_bar(orientation="y")

  # Se obs que el modelo ST40000DM000
  # es el con una mayor frecuencia
  # es por lo que se creará una variable dummy 
  # par identificar pertenencia a este modelo

  model_df <- mutate(df, model_F = ifelse((model == 'ST4000DM000'), "ST40000DM000", "other"),
                     model = factor(model_F),
                     time_day = time_day)

  df$model_Bin=model_df$model

  rm(model_df)

km_model <- survfit(Surv(time_day) ~ as.factor(model_Bin), data=df)
#summary(km_fit, time_day = c(1,30,60,90*(1:10)))

autoplot(km_model)
summary(km_model, times = c(1,30,60,90*(1:10)))

rm(km_model)

#Vewmos segun LogRank si las curvas son dif en el final
#With rho = 0 this is the log-rank 
log_rang_model=survdiff(Surv(time_day) ~ as.factor(model_Bin) , df)


#with rho = 1 it is equivalent to the Peto & Peto
#modification of the Gehan-Wilcoxon test.
Peto_test_model=survdiff(Surv(time_day) ~ as.factor(model_Bin) , df, rho = 1)

### Función de Test de comparación de curvas ####
Test_curv<-function(log_rang,Peto_test){
  
  log_rang=pchisq(log_rang$chisq, length(log_rang$n)-1, lower.tail = FALSE)
  Peto_test=pchisq(Peto_test$chisq, length(Peto_test$n)-1, lower.tail = FALSE)
  
  
  cat("log_rang P-value: ",log_rang
      ,'\n =>',ifelse(log_rang<0.05,
                      " Hay evidencia para rechazar Ho",
                      " Se conserva Ho, No hay evidencia para rechazar Ho" ),"\n")
  
  cat('\n')
  
  cat('Peto_test P-value: ', Peto_test
      ,'\n =>',ifelse(Peto_test<0.05,
                      " Hay evidencia para rechazar Ho",
                      " Se conserva Ho, No hay evidencia para rechazar Ho"),'\n' )
  cat('\n','Ho:= las curvas son iguales','\n')
}
Test_curv(log_rang_model,Peto_test_model)



### Curvas de KM smart####

#Creamos una copia para ajustar una a 
#una las var smart sin afectar a df
df.smart=copy(df)
df.smart$model=NULL


### Recogemos las columnas smart ####
#Esto es para hacer el analisis bi 
#variado de forma automatica

Colums_df=colnames(df.smart)
Colums_df
#l=length(Colums_df) #ya no se necesita l

Colums_df=Colums_df[ 3:11 ]

#Colums_df=Colums_df[!Colums_df %in% "model_Bin"]
Colums_df





### KM smart_1_normalized_mean_mean ####

i=Colums_df[1]
X=(df.smart[[i]])
length(X)
Y= X[!is.na(X)]
length(Y)
X=sapply(X, function(x) ifelse(is.na(x),mean(Y),x ) )
hist(X)
D=quantile(X, prob=c(0,0.25,0.5,0.75,1))
l=length(D[])
for (j in c(2:l)) {
  D[j]<-0.00001+D[j]
}
rm(j,l,Y)
D



df.smart$smart_1_f <- cut(X , breaks = D, 
                                   labels = c('0.25','0.5','0.75','1'), right = FALSE)


km_fit <- survfit(Surv(time_day) ~ as.factor(smart_1_f), data=df.smart)

#summary(km_fit, time_day = c(1,30,60,90*(1:10)))

autoplot(km_fit)


#Vewmos segun LogRank si las curvas son dif en el final
#With rho = 0 this is the log-rank 
log_rang_1=survdiff(Surv(time_day) ~ as.factor(smart_1_f) , df.smart)

#with rho = 1 it is equivalent to the Peto & Peto
#modification of the Gehan-Wilcoxon test.
Peto_test_1=survdiff(Surv(time_day) ~ as.factor(smart_1_f) , df.smart, rho = 1)

#survdiff(Surv(time_day) ~ as.factor(smart_1_normalized_mean_mean_mean), data=df.smart,rho = 1)

Test_curv(log_rang_1,Peto_test_1)



#_____________________________________________________________________________________



# km_fit <- survfit(Surv(time_day,model!='other') ~ as.factor(smart_1_normalized_mean_mean_Q), data=df.smart)
# #as.factor(smart_1_normalized_mean_mean_Q)
# #summary(km_fit, time_day = c(1,30,60,90*(1:10)))
# 
# autoplot(km_fit)



### smart 3 i->2####

#i='smart_3_normalized_mean_mean_mean'
i=Colums_df[2]

X=(df.smart[[i]])
length(X)
Y= X[!is.na(X)]
length(Y)
X=sapply(X, function(x) ifelse(is.na(x),mean(Y),x ) )
hist(X)

D=quantile(X, prob=c(0,0.25,0.5,0.75,1))
l=length(D[])
for (j in c(2:l)) {
  D[j]<-0.00001+D[j]
}
rm(j,l,Y)
D


df.smart$smart_3_f <- cut(X , breaks = D, 
                                   labels = c('0.25','0.5','0.75','1')
                                   , right = FALSE)


km_fit <- survfit(Surv(time_day) ~ as.factor(smart_3_f), data=df.smart)

#summary(km_fit, time_day = c(1,30,60,90*(1:10)))

autoplot(km_fit)


#Vewmos segun LogRank si las curvas son dif en el final
#With rho = 0 this is the log-rank 
log_rang_3=survdiff(Surv(time_day) ~ as.factor(smart_3_f) , df.smart)

#with rho = 1 it is equivalent to the Peto & Peto
#modification of the Gehan-Wilcoxon test.
Peto_test_3=survdiff(Surv(time_day) ~ as.factor(smart_3_f) , df.smart, rho = 1)

#survdiff(Surv(time_day) ~ as.factor(smart_1_normalized_mean_mean), data=df.smart,rho = 1)

Test_curv(log_rang_3,Peto_test_3)



# survreg(Surv(time_day) ~ as.factor(Qartere) , df.smart, dist='exponential')
# 
# exp(-0.5466061)+exp(-0.3250286)+
#   
# exp(-0.6194902)






### smart 5 i->3####

i=Colums_df[3]
i
#i='smart_5_normalized_mean_mean_mean'

X=(df.smart[[i]])
length(X)
Y= X[!is.na(X)]
length(Y)
X=sapply(X, function(x) ifelse(is.na(x),mean(Y),x ) )
hist(X)

D=quantile(X, prob=c(0,0.75,1))
l=length(D[])
for (i in c(2:l)) {
  D[i]<-0.00001+D[i]
}
D


df.smart$smart_5_f <- cut(X , breaks = D, 
                                   labels = c('0.75','1')
                                   , right = FALSE)


km_fit <- survfit(Surv(time_day) ~ as.factor(smart_5_f), data=df.smart)
#summary(km_fit, time_day = c(1,30,60,90*(1:10)))

autoplot(km_fit)

#survdiff(Surv(time_day) ~ as.factor(smart_5_f), data=df.smart, rho = 1)


#Vewmos segun LogRank si las curvas son dif en el final
#With rho = 0 this is the log-rank 
log_rang_5=survdiff(Surv(time_day) ~ as.factor(smart_5_f) , df.smart)

#with rho = 1 it is equivalent to the Peto & Peto
#modification of the Gehan-Wilcoxon test.
Peto_test_5=survdiff(Surv(time_day) ~ as.factor(smart_5_f) , df.smart, rho = 1)

#survdiff(Surv(time_day) ~ as.factor(smart_1_normalized_mean_mean), data=df.smart,rho = 1)

Test_curv(log_rang_5,Peto_test_5)
# survreg(Surv(time_day) ~  smart_5_normalized_mean_mean , df, dist='exponential')
# 
# exp(0.002491316)

### smart 7 i->4####

i=Colums_df[4]
i

X=(df.smart[[i]])
length(X)
Y= X[!is.na(X)]
length(Y)
X=sapply(X, function(x) ifelse(is.na(x),mean(Y),x ) )
hist(X)

D=quantile(X, prob=c(0,0.25,0.75,1))
l=length(D[])
for (i in c(2:l)) {
  D[i]<-0.00001+D[i]
}
D

#etiquetamos la particion
df.smart$smart_7_f <- cut(X , breaks = D, 
                                   labels = c('0.25','0.75','1')
                                   , right = FALSE)


km_fit <- survfit(Surv(time_day) ~ as.factor(smart_7_f), data=df.smart)
#summary(km_fit, time_day = c(1,30,60,90*(1:10)))

autoplot(km_fit)

# survdiff(Surv(time_day) ~ as.factor(smart_7_normalized_mean_mean), data=df.smart, rho = 1)


#Vewmos segun LogRank si las curvas son dif en el final
#With rho = 0 this is the log-rank 
log_rang_7=survdiff(Surv(time_day) ~ as.factor(smart_7_f) , df.smart)

#with rho = 1 it is equivalent to the Peto & Peto
#modification of the Gehan-Wilcoxon test.
Peto_test_7=survdiff(Surv(time_day) ~ as.factor(smart_7_f) , df.smart, rho = 1)

#survdiff(Surv(time_day) ~ as.factor(smart_1_normalized_mean_mean), data=df.smart,rho = 1)

Test_curv(log_rang_7,Peto_test_7)

### smart 192 i->5####


i=Colums_df[5]
i

X=(df.smart[[i]])
length(X)
Y= X[!is.na(X)]
length(Y)
X=sapply(X, function(x) ifelse(is.na(x),mean(Y),x ) )
hist(X)

D=quantile(X, prob=c(0,0.75,1))
l=length(D[])
for (i in c(2:l)) {
  D[i]<-0.00001+D[i]
}
D


#etiquetamos la particion
df.smart$smart_192_f <- cut(X , breaks = D, 
                                     labels = c('0.75','1')
                                     , right = FALSE)


km_fit <- survfit(Surv(time_day) ~ as.factor(smart_192_f), data=df.smart)
#summary(km_fit, time_day = c(1,30,60,90*(1:10)))

autoplot(km_fit)

#survdiff(Surv(time_day) ~ as.factor(smart_192_f), data=df.smart, rho = 1)


#Vewmos segun LogRank si las curvas son dif en el final
#With rho = 0 this is the log-rank 
log_rang_192=survdiff(Surv(time_day) ~ as.factor(smart_192_f) , df.smart)

#with rho = 1 it is equivalent to the Peto & Peto
#modification of the Gehan-Wilcoxon test.
Peto_test_192=survdiff(Surv(time_day) ~ as.factor(smart_192_f) , df.smart, rho = 1)

#survdiff(Surv(time_day) ~ as.factor(smart_1_normalized_mean_mean), data=df.smart,rho = 1)

Test_curv(log_rang_192,Peto_test_192)

### smart 193 i->6####


i=Colums_df[6]
i

X=(df.smart[[i]])
length(X)
Y= X[!is.na(X)]
length(Y)
X=sapply(X, function(x) ifelse(is.na(x),mean(Y),x ) )
hist(X)

D=quantile(X, prob=c(0,0.25,0.75,1))
l=length(D[])
for (i in c(2:l)) {
  D[i]<-0.00001+D[i]
}
D


#etiquetamos la particion
df.smart$smart_193_f <- cut(X , breaks = D, 
                                     labels = c('0.25','0.75','1')
                                     , right = FALSE)


km_fit <- survfit(Surv(time_day) ~ as.factor(smart_193_f), data=df.smart)
#summary(km_fit, time_day = c(1,30,60,90*(1:10)))

autoplot(km_fit)

#survdiff(Surv(time_day) ~ as.factor(Qartere), data=df.smart, rho = 1)

#Vewmos segun LogRank si las curvas son dif en el final
#With rho = 0 this is the log-rank 
log_rang_193=survdiff(Surv(time_day) ~ as.factor(smart_193_f) , df.smart)

#with rho = 1 it is equivalent to the Peto & Peto
#modification of the Gehan-Wilcoxon test.
Peto_test_193=survdiff(Surv(time_day) ~ as.factor(smart_193_f) , df.smart, rho = 1)

#survdiff(Surv(time_day) ~ as.factor(smart_1_normalized_mean_mean), data=df.smart,rho = 1)

Test_curv(log_rang_193,Peto_test_193)

### smart 194 i->7####


i=Colums_df[7]
i

X=(df.smart[[i]])
length(X)
Y= X[!is.na(X)]
length(Y)
X=sapply(X, function(x) ifelse(is.na(x),mean(Y),x ) )
hist(X)

D=quantile(X, prob=c(0,0.25,0.75,1))
l=length(D[])
for (i in c(2:l)) {
  D[i]<-0.00001+D[i]
}
D


#etiquetamos la particion
df.smart$smart_194_f <- cut(X , breaks = D, 
                                     labels = c('0.25','0.75','1')
                                     , right = FALSE)


km_fit <- survfit(Surv(time_day) ~ as.factor(smart_194_f), data=df.smart)
#summary(km_fit, time_day = c(1,30,60,90*(1:10)))

autoplot(km_fit)


#Vewmos segun LogRank si las curvas son dif en el final
#With rho = 0 this is the log-rank 
log_rang_194=survdiff(Surv(time_day) ~ as.factor(smart_194_f) , df.smart)

#with rho = 1 it is equivalent to the Peto & Peto
#modification of the Gehan-Wilcoxon test.
Peto_test_194=survdiff(Surv(time_day) ~ as.factor(smart_194_f) , df.smart, rho = 1)

#survdiff(Surv(time_day) ~ as.factor(smart_1_normalized_mean_mean), data=df.smart,rho = 1)

Test_curv(log_rang_194,Peto_test_194)


### smart 197 i->8####


i=Colums_df[8]
i

X=(df.smart[[i]])
length(X)
Y= X[!is.na(X)]
length(Y)
X=sapply(X, function(x) ifelse(is.na(x),mean(Y),x ) )
hist(X)

D=quantile(X, prob=c(0,0.75,1))
l=length(D[])
for (i in c(2:l)) {
  D[i]<-0.00001+D[i]
}
D


#etiquetamos la particion
df.smart$smart_197_f <- cut(X , breaks = D, 
                                     labels = c('0.75','1')
                                     , right = FALSE)


km_fit <- survfit(Surv(time_day) ~ as.factor(smart_197_f), data=df.smart)
#summary(km_fit, time_day = c(1,30,60,90*(1:10)))

autoplot(km_fit)

#survdiff(Surv(time_day) ~ as.factor(Qartere), data=df.smart, rho = 1)


#Vewmos segun LogRank si las curvas son dif en el final
#With rho = 0 this is the log-rank 
log_rang_197=survdiff(Surv(time_day) ~ as.factor(smart_197_f) , df.smart)

#with rho = 1 it is equivalent to the Peto & Peto
#modification of the Gehan-Wilcoxon test.
Peto_test_197=survdiff(Surv(time_day) ~ as.factor(smart_197_f) , df.smart, rho = 1)

#survdiff(Surv(time_day) ~ as.factor(smart_1_normalized_mean_mean), data=df.smart,rho = 1)

Test_curv(log_rang_197,Peto_test_197)





### smart 198 i->9####


i=Colums_df[9]
i

X=(df.smart[[i]])
length(X)
Y= X[!is.na(X)]
length(Y)
X=sapply(X, function(x) ifelse(is.na(x),mean(Y),x ) )
hist(X)

#(0,0.25,0.75,1))
D=quantile(X, prob=c(0,0.01,0.96,1))
l=length(D[])
for (i in c(2:l)) {
  D[i]<-0.00001+D[i]
}
D
#sum(X<100)/length(X)

#df.smart$smart_198_normalized_mean_mean =df$smart_198_normalized_mean_mean 

#etiquetamos la particion
df.smart$smart_198_f <- cut(X , breaks = D, 
                                     labels = c('0.01','0.96','1')
                                     , right = FALSE)


km_fit <- survfit(Surv(time_day) ~ as.factor(smart_198_f), data=df.smart)
#summary(km_fit, time_day = c(1,30,60,90*(1:10)))

autoplot(km_fit)

#survdiff(Surv(time_day) ~ as.factor(smart_198_normalized_mean_mean), data=df.smart, rho = 1)


#Vewmos segun LogRank si las curvas son dif en el final
#With rho = 0 this is the log-rank 
log_rang_198=survdiff(Surv(time_day) ~ as.factor(smart_198_f) , df.smart)

#with rho = 1 it is equivalent to the Peto & Peto
#modification of the Gehan-Wilcoxon test.
Peto_test_198=survdiff(Surv(time_day) ~ as.factor(smart_198_f) , df.smart, rho = 1)

#survdiff(Surv(time_day) ~ as.factor(smart_1_normalized_mean_mean), data=df.smart,rho = 1)

Test_curv(log_rang_198,Peto_test_198)


###Comienza analisis de las columnas smart skews ####
### Recogemos las columnas smart skews ####
#Esto es para hacer el analisis bi 
#variado de forma automatica

Colums_df=colnames(df.smart)
Colums_df

Colums_df=Colums_df[ 12:20 ]

Colums_df


### KM smart_1_normalized ####

i=Colums_df[1]
i
X=(df.smart[[i]])
length(X)
hist(X)

Y= X[!is.na(X)]
length(Y)

X=sapply(X, function(x) ifelse(is.na(x),mean(Y),x ) )
hist(X)




ggplot(data = df.smart, aes(y = time_day,
                            x = X))+
  geom_point()






D=quantile(X, prob=c(0,0.5,0.75,1))
D
l=length(D[])
for (j in c(2:l)) {
  D[j]<-0.00001+D[j]
}
D


df.smart$Categoric <- cut(X , breaks = D, 
                          labels = c('0.5','0.75','1'), right = FALSE)


km_fit <- survfit(Surv(time_day) ~ as.factor(Categoric), data=df.smart)



autoplot(km_fit)


#Vewmos segun LogRank si las curvas son dif en el final
#With rho = 0 this is the log-rank 
log_rang=survdiff(Surv(time_day) ~ as.factor(Categoric) , df.smart)

#with rho = 1 it is equivalent to the Peto & Peto
#modification of the Gehan-Wilcoxon test.
Peto_test=survdiff(Surv(time_day) ~ as.factor(Categoric) , df.smart, rho = 1)


Test_curv(log_rang,Peto_test)


### smart 3 i->2####

i=Colums_df[2]
i
#i='smart_3_normalized_delta_skew'

X=(df.smart[[i]])
length(X)
hist(X)
Y= X[!is.na(X)]
length(Y)
X=sapply(X, function(x) ifelse(is.na(x),mean(Y),x ) )
hist(X)

D=quantile(X, prob=c(0,0.75,1))
D
l=length(D[])
for (i in c(2:l)) {
  D[i]<-0.00001+D[i]
}
D


df.smart$Categoric <- cut(X , breaks = D, 
                          labels = c('0.75','1')
                          , right = FALSE)

df.smart$skew_3_f<-df.smart$Categoric

km_fit <- survfit(Surv(time_day) ~ as.factor(Categoric), data=df.smart)



autoplot(km_fit)

#Vewmos segun LogRank si las curvas son dif en el final
#With rho = 0 this is the log-rank 
log_rang=survdiff(Surv(time_day) ~ as.factor(Categoric) , df.smart)

#with rho = 1 it is equivalent to the Peto & Peto
#modification of the Gehan-Wilcoxon test.
Peto_test=survdiff(Surv(time_day) ~ as.factor(Categoric) , df.smart, rho = 1)


Test_curv(log_rang,Peto_test)



### smart 5 i->3####

i=Colums_df[3]
i
#i='smart_5_normalized'

X=(df.smart[[i]])
length(X)
hist(X)
Y= X[!is.na(X)]
length(Y)
X=sapply(X, function(x) ifelse(is.na(x),mean(Y),x ) )
hist(X)

D=quantile(X, prob=c(0,0.2,1))
D
l=length(D[])
for (i in c(2:l)) {
  D[i]<-0.00001+D[i]
}
D


df.smart$Categoric <- cut(X , breaks = D, 
                          labels = c('0.20','1')
                          , right = FALSE)


km_fit <- survfit(Surv(time_day) ~ as.factor(Categoric), data=df.smart)


autoplot(km_fit)

#Vewmos segun LogRank si las curvas son dif en el final
#With rho = 0 this is the log-rank 
log_rang=survdiff(Surv(time_day) ~ as.factor(Categoric) , df.smart)

#with rho = 1 it is equivalent to the Peto & Peto
#modification of the Gehan-Wilcoxon test.
Peto_test=survdiff(Surv(time_day) ~ as.factor(Categoric) , df.smart, rho = 1)

Test_curv(log_rang,Peto_test)

### smart 7 i->4####

i=Colums_df[4]

#i='smart_7_normalized_delta_skew'

X=(df.smart[[i]])
length(X)
Y= X[!is.na(X)]
length(Y)
X=sapply(X, function(x) ifelse(is.na(x),mean(Y),x ) )
hist(X)

D=quantile(X, prob=c(0,0.80,1))
l=length(D[])
for (i in c(2:l)) {
  D[i]<-0.00001+D[i]
}
D

#etiquetamos la particion

df.smart$Categoric <- cut(X , breaks = D, 
                          labels = c('0.80','1')
                          , right = FALSE)

df.smart$skew_7_f<-df.smart$Categoric

km_fit <- survfit(Surv(time_day) ~ as.factor(Categoric), data=df.smart)
#summary(km_fit, time_day = c(1,30,60,90*(1:10)))

autoplot(km_fit)

#Vewmos segun LogRank si las curvas son dif en el final
#With rho = 0 this is the log-rank 
log_rang=survdiff(Surv(time_day) ~ as.factor(Categoric) , df.smart)

#with rho = 1 it is equivalent to the Peto & Peto
#modification of the Gehan-Wilcoxon test.
Peto_test=survdiff(Surv(time_day) ~ as.factor(Categoric) , df.smart, rho = 1)

Test_curv(log_rang,Peto_test)


### smart 192 i->5####


i=Colums_df[5]
i

X=(df.smart[[i]])
length(X)
Y= X[!is.na(X)]
length(Y)
X=sapply(X, function(x) ifelse(is.na(x),mean(Y),x ) )
hist(X)

D=quantile(X, prob=c(0,0.25,1))
l=length(D[])
for (i in c(2:l)) {
  D[i]<-0.00001+D[i]
}
D


#etiquetamos la particion
df.smart$Categoric <- cut(X , breaks = D, 
                          labels = c('0.25','1')
                          , right = FALSE)


km_fit <- survfit(Surv(time_day) ~ as.factor(Categoric), data=df.smart)
#summary(km_fit, time_day = c(1,30,60,90*(1:10)))

autoplot(km_fit)

survdiff(Surv(time_day) ~ as.factor(Categoric), data=df.smart, rho = 1)


#Vewmos segun LogRank si las curvas son dif en el final
#With rho = 0 this is the log-rank 
log_rang=survdiff(Surv(time_day) ~ as.factor(Categoric) , df.smart)

#with rho = 1 it is equivalent to the Peto & Peto
#modification of the Gehan-Wilcoxon test.
Peto_test=survdiff(Surv(time_day) ~ as.factor(Categoric) , df.smart, rho = 1)


Test_curv(log_rang,Peto_test)

### smart 193 i->6####


i=Colums_df[6]

#i='smart_193_normalized_delta_skew'
#i

X=(df.smart[[i]])
length(X)
Y= X[!is.na(X)]
length(Y)
X=sapply(X, function(x) ifelse(is.na(x),mean(Y),x ) )
hist(X)

D=quantile(X, prob=c(0,0.75,1))
l=length(D[])
for (i in c(2:l)) {
  D[i]<-0.00001+D[i]
}
D


#etiquetamos la particion
df.smart$Categoric <- cut(X , breaks = D, 
                          labels = c('0.75','1')
                          , right = FALSE)

df.smart$skew_193_f<-df.smart$Categoric

km_fit <- survfit(Surv(time_day) ~ as.factor(Categoric), data=df.smart)
#summary(km_fit, time_day = c(1,30,60,90*(1:10)))

autoplot(km_fit)

#survdiff(Surv(time_day) ~ as.factor(Qartere), data=df.smart, rho = 1)

#Vewmos segun LogRank si las curvas son dif en el final
#With rho = 0 this is the log-rank 
log_rang=survdiff(Surv(time_day) ~ as.factor(Categoric) , df.smart)

#with rho = 1 it is equivalent to the Peto & Peto
#modification of the Gehan-Wilcoxon test.
Peto_test=survdiff(Surv(time_day) ~ as.factor(Categoric) , df.smart, rho = 1)

#survdiff(Surv(time_day) ~ as.factor(smart_1_normalized), data=df.smart,rho = 1)

Test_curv(log_rang,Peto_test)

### smart 194 i->7####


i=Colums_df[7]
i

X=(df.smart[[i]])
length(X)
Y= X[!is.na(X)]
length(Y)
X=sapply(X, function(x) ifelse(is.na(x),mean(Y),x ) )
hist(X)

D=quantile(X, prob=c(0,0.25,0.5,0.75,1))
l=length(D[])
for (i in c(2:l)) {
  D[i]<-0.00001+D[i]
}
D


#etiquetamos la particion
df.smart$Categoric <- cut(X , breaks = D, 
                          labels = c('0.25','0.5','0.75','1')
                          , right = FALSE)


km_fit <- survfit(Surv(time_day) ~ as.factor(Categoric), data=df.smart)
#summary(km_fit, time_day = c(1,30,60,90*(1:10)))

autoplot(km_fit)


#Vewmos segun LogRank si las curvas son dif en el final
#With rho = 0 this is the log-rank 
log_rang=survdiff(Surv(time_day) ~ as.factor(Categoric) , df.smart)

#with rho = 1 it is equivalent to the Peto & Peto
#modification of the Gehan-Wilcoxon test.
Peto_test=survdiff(Surv(time_day) ~ as.factor(Categoric) , df.smart, rho = 1)


Test_curv(log_rang,Peto_test)


### smart 197 i->8####


i=Colums_df[8]
i

X=(df.smart[[i]])
length(X)
Y= X[!is.na(X)]
length(Y)
X=sapply(X, function(x) ifelse(is.na(x),mean(Y),x ) )
hist(X)

D=quantile(X, prob=c(0,0.25,1))
l=length(D[])
for (i in c(2:l)) {
  D[i]<-0.00001+D[i]
}
D


#etiquetamos la particion
df.smart$Categoric <- cut(X , breaks = D, 
                          labels = c('0.25','1')
                          , right = FALSE)


km_fit <- survfit(Surv(time_day) ~ as.factor(Categoric), data=df.smart)
#summary(km_fit, time_day = c(1,30,60,90*(1:10)))

autoplot(km_fit)

#survdiff(Surv(time_day) ~ as.factor(Qartere), data=df.smart, rho = 1)


#Vewmos segun LogRank si las curvas son dif en el final
#With rho = 0 this is the log-rank 
log_rang=survdiff(Surv(time_day) ~ as.factor(Categoric) , df.smart)

#with rho = 1 it is equivalent to the Peto & Peto
#modification of the Gehan-Wilcoxon test.
Peto_test=survdiff(Surv(time_day) ~ as.factor(Categoric) , df.smart, rho = 1)


Test_curv(log_rang,Peto_test)

### smart 198 i->9####


i=Colums_df[9]
i

X=(df.smart[[i]])
length(X)
Y= X[!is.na(X)]
length(Y)
X=sapply(X, function(x) ifelse(is.na(x),mean(Y),x ) )
hist(X)

#(0,0.25,0.75,1))
D=quantile(X, prob=c(0,0.25,1))
l=length(D[])
for (i in c(2:l)) {
  D[i]<-0.00001+D[i]
}
D

#etiquetamos la particion
df.smart$Categoric <- cut(X , breaks = D, 
                          labels = c('0.25','1')
                          , right = FALSE)


km_fit <- survfit(Surv(time_day) ~ as.factor(Categoric), data=df.smart)
#summary(km_fit, time_day = c(1,30,60,90*(1:10)))

autoplot(km_fit)

#Vewmos segun LogRank si las curvas son dif en el final
#With rho = 0 this is the log-rank 
log_rang=survdiff(Surv(time_day) ~ as.factor(smart_198_normalized) , df.smart)

#with rho = 1 it is equivalent to the Peto & Peto
#modification of the Gehan-Wilcoxon test.
Peto_test=survdiff(Surv(time_day) ~ as.factor(smart_198_normalized) , df.smart, rho = 1)


Test_curv(log_rang,Peto_test)


### Termino Km ####

df.smart$Categoric=NULL
rm(i,j,l,X,Y,D,Colums_df,km_fit,log_rang,Peto_test)
#____________________________________________________



### eliminación de los skews que no se usarán####

#Solo nos quedamos con las variables skews que
#pasaron por la parte 2 a y b
Colums_df=colnames(df.smart)
Colums_df

Colums_df=Colums_df[ 12:20 ]
Colums_df

#i=as.character(i)
a=df.smart
l=length(Colums_df)
for(i in c(1:l) ){
  s=Colums_df[i]
  if(i!=2 &&i!=4  && i!=6 ){
    a[[s]]=NULL
    }
}



#rellenamos los valores Nas de las variables 
#delta skews que usaremos 



#Variables skew seleccionadas (3, 7 y 193)
Colums_df=colnames(df.smart)
Colums_df=colnames(a)
Colums_df

Colums_df=Colums_df[ 12:14 ]
Colums_df

l=length(Colums_df)
for(n in c(1:l)){
i=Colums_df[n]
X=(df.smart[[i]])
length(X)
Y= X[!is.na(X)]
length(Y)
X=sapply(X, function(x) ifelse(is.na(x),mean(Y),x ) )
a[,i]=X
}

### Realizar los graficos log log  para los skews####

km_skew_3 <- survfit(Surv(time_day)~ skew_3_f
                     , data=df.smart)


plot(km_skew_3, col=c("black", "red","green" ,"cyan"),
     fun="cloglog") + title(main='Comparación de riesgos proporcionales',
      sub='km_skew_3', xlab = 'log(tiempo)' , ylab='log{-log(S)}')

km_skew_7 <- survfit(Surv(time_day)~ skew_7_f, data=df.smart)


plot(km_skew_7, col=c("black", "red","green" ,"cyan","blue"), fun="cloglog") + title(main='Comparación de riesgos proporcionales',
                                                                              sub='km_skew_7', xlab = 'log(tiempo)' , ylab='log{-log(S)}')


#i=skew193

km_skew_193 <- survfit(Surv(time_day)~ skew_193_f, data=df.smart)


plot(km_skew_193, col=c("black", "red","green" ,"cyan","blue"), fun="cloglog") + title(main='Comparación de riesgos proporcionales',
                                                                                     sub='km_skew_193', xlab = 'log(tiempo)' , ylab='log{-log(S)}')


write.csv(df.smart,file = "data_smart_skew.csv")

### Aquí comienza otro codigo  Parte 2####
  #utilizado para apreciar de nuevo
  #Las curvas de Km y visualizar las graficas
  #de loglog

data<-fread("data_smart_skew.csv")

data<-rename(data,model=model_Bin)

data$V1<-NULL


### Parte II a ####
#Análisis bivariado con el tiempo de vida
#Función de sobrevivencia vía Kaplan-Meier

#model
km_m <- survfit(Surv(time_day)~ as.factor(model), data=data)

autoplot(km_m)

#smart_1_normalized_mean
km_1 <- survfit(Surv(time_day)~as.factor(smart_1_f), data=data)

autoplot(km_1)

#smart_3_normalized_mean
km_3 <- survfit(Surv(time_day)~as.factor(smart_3_f), data=data)

autoplot(km_3)

#smart_5_normalized_mean
km_5 <- survfit(Surv(time_day)~as.factor(smart_5_f), data=data)

autoplot(km_5)

#smart_7_normalized_mean
km_7 <- survfit(Surv(time_day)~as.factor(smart_7_f), data=data)

autoplot(km_7)

#smart_192_normalized_mean
km_192 <- survfit(Surv(time_day)~as.factor(smart_192_f), data=data)

autoplot(km_192)

#smart_193_normalized_mean
km_193 <- survfit(Surv(time_day)~as.factor(smart_193_f), data=data)

autoplot(km_193)

#smart_194_normalized_mean
km_194 <- survfit(Surv(time_day)~as.factor(smart_194_f), data=data)

autoplot(km_194)

#smart_197_normalized_mean
km_197 <- survfit(Surv(time_day)~as.factor(smart_197_f), data=data)

autoplot(km_197)

#smart_198_normalized_mean
km_198 <- survfit(Surv(time_day)~as.factor(smart_198_f), data=data)

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
      sub='Smart_1_normalized_mean',
      xlab = 'log(tiempo)' , ylab='log{-log(S)}')


plot(km_3, col=c("black", "red","green" ,"cyan"), fun="cloglog")+
  title(main='Comparación de riesgos proporcionales',
        sub='Smart_3_normalized_mean',
        xlab = 'log(tiempo)' , ylab='log{-log(S)}')


plot(km_5, col=c("black", "red"), fun="cloglog")+
  title(main='Comparación de riesgos proporcionales',
        sub='Smart_5_normalized_mean',
        xlab = 'log(tiempo)' , ylab='log{-log(S)}')

plot(km_7, col=c("black", "red","green" ), fun="cloglog")
title(main='Comparación de riesgos proporcionales',
      sub='Smart_7_normalized_mean',
      xlab = 'log(tiempo)' , ylab='log{-log(S)}')


plot(km_192, col=c("black", "red"), fun="cloglog")+
  title(main='Comparación de riesgos proporcionales',
        sub='Smart_192_normalized_mean',
        xlab = 'log(tiempo)' , ylab='log{-log(S)}')


plot(km_193, col=c("black", "red","green" ,"cyan"), fun="cloglog")
title(main='Comparación de riesgos proporcionales',
      sub='Smart_193_normalized_mean',
      xlab = 'log(tiempo)' , ylab='log{-log(S)}')


plot(km_194, col=c("black", "red","green" ,"cyan"), fun="cloglog")+
  title(main='Comparación de riesgos proporcionales',
        sub='Smart_194_normalized_mean',
        xlab = 'log(tiempo)' , ylab='log{-log(S)}')


plot(km_197, col=c("black", "red","green" ,"cyan"), fun="cloglog")+
  title(main='Comparación de riesgos proporcionales',
        sub='Smart_197_normalized_mean',
        xlab = 'log(tiempo)' , ylab='log{-log(S)}')


plot(km_198, col=c("black", "red","green" ,"cyan"), fun="cloglog")+
  title(main='Comparación de riesgos proporcionales',
        sub='Smart_198_normalized_mean',
        xlab = 'log(tiempo)' , ylab='log{-log(S)}')

plot(km_m, col=c("black", "red","green" ,"cyan"), fun="cloglog")+
  title(main='Comparación de riesgos proporcionales', sub='Model', xlab = 'log(tiempo)' , ylab='log{-log(S)}')

