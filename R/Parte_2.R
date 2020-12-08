#library(ggplot2)
library(dataQualityR)
#library(Amelia)
library(ggfortify)
library(gridExtra)
library(grid)
library(alluvial)
library(RColorBrewer)
library(corrplot)
library(Matrix)
library(dplyr)
library(MASS)
library(caret)
library(glmnet)
library(BLR)
library(lubridate)
library(data.table)
library(DMwR)
library(dplyr)
library(labelVector)

library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(rms)



data=fread('data/data_smart.csv')


### Comenzamos a ajustar la data ####
data$serial_number=as.factor(data$serial_number)
data$model=as.factor(data$model)


### Na Analisis y eliminacion####
df=data[data$failure=='1',]
df$failure=NULL
df$vida_util_restante_int=NULL
df$vida_util_restante=NULL



NAs = as.data.frame(sapply(df, function(x) sum(is.na(x))))
colnames(NAs) = c("Count_NAs")
NAs$Feature = rownames(NAs)                           
NAs = NAs[which(NAs$Count_NAs != 0 & NAs$Feature != "time_days"),]  

ggplot(NAs, aes(x=reorder(Feature,Count_NAs), y=Count_NAs)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
rm(NAs)
# smart 2 no sirve
# Posee muchisimos Na Por lo cual 
#eliminamos todos los smart con muchos Na


#Se encontraron variables SMART con muchisimos Na,
#por lo que se decide eliminarlas 
data$smart_196_normalized=NULL
data$smart_200_normalized=NULL
data$smart_195_normalized=NULL
data$smart_2_normalized=NULL
data$smart_8_normalized=NULL
data$smart_254_normalized=NULL

#Resulta que solo hay 4 items que no poseen valores en las variables smart 
#smart_192_normalized
#smart_193_normalized

# Verificamos si la capacidad es un factor de diferencia
sd(data$capacity_bytes)
## Como la desviación estanadr es de 0, 
#los datos no varian. por lo cual se descarta este factor
data$capacity_bytes =NULL

### Vemos los valores cuando fallan los items ####
# data$failure=as.factor(data$failure) Resulta que no filtra como factor

#data frame con la información de los productos al momento de fallar
df=data[data$failure=='1',]
df$failure=NULL
df$vida_util_restante_int=NULL
df$vida_util_restante=NULL
df$vida_util=NULL
df$vida_util_days=NULL
df$end_date=NULL
df$date=NULL
df$serial_number=NULL
df$serial_number=NULL


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
ggplot(df, aes(y=model) ) + 
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


#Vewmos segun LogRank si las curvas son dif en el final
#With rho = 0 this is the log-rank 
log_rang=survdiff(Surv(time_day) ~ as.factor(model_Bin) , df)
log_rang=pchisq(log_rang$chisq, length(log_rang$n)-1, lower.tail = FALSE)



#with rho = 1 it is equivalent to the Peto & Peto
#modification of the Gehan-Wilcoxon test.
Peto_test=survdiff(Surv(time_day) ~ as.factor(model_Bin) , df, rho = 1)
Peto_test=pchisq(Peto_test$chisq, length(Peto_test$n)-1, lower.tail = FALSE)

 
cat("log_rang P-value: ",log_rang
    ,ifelse(log_rang<0.05,
            " Hay evidencia para rechazar Ho",
            " Se conserva Ho") )

cat('Peto_test P-value: ', Peto_test
    ,ifelse(Peto_test<0.05,
            " Hay evidencia para rechazar Ho",
            " Se conserva Ho") )

survreg(Surv(time_day) ~ as.factor(model_Bin) , df, dist='exponential')

exp(-0.7309016)
### Curvas de KM smart####

  #Creamos una copia para ajustar una a 
  #una las var smart sin afectar a df
  df.smart=copy(df)
  df.smart$model=NULL


### Recogemos las columnas smart ####
  #Esto es para hacer el analisis bi 
  #variado de forma automatica

Colums_df=colnames(df.smart)
Colums_df<
l=length(Colums_df)

Colums_df=Colums_df[ 2:l ]

Colums_df=Colums_df[!Colums_df %in% "time_day"]
Colums_df


### KM por medio de  quantile #####


### KM smart_1_normalized ####

i='smart_1_normalized'


X=(df.smart[,..i])

hist(X[[i]])

D=quantile(X[[i]], prob=c(0,0.25,0.5,0.75,1))
l=length(D[])
for (j in c(2:l)) {
  D[j]<-0.00001+D[j]
}
D


df.smart$smart_1_normalized_Q <- cut(X[[i]] , breaks = D, 
                     labels = c('0.25','0.5','0.75','1'), right = FALSE)


km_fit <- survfit(Surv(time_day) ~ as.factor(smart_1_normalized_Q), data=df.smart)

#summary(km_fit, time_day = c(1,30,60,90*(1:10)))

autoplot(km_fit)

survdiff(Surv(time_day) ~ as.factor(smart_1_normalized), data=df.smart,rho = 1)

#_____________________________________________________________________________________



# km_fit <- survfit(Surv(time_day,model!='other') ~ as.factor(smart_1_normalized_Q), data=df.smart)
# #as.factor(smart_1_normalized_Q)
# #summary(km_fit, time_day = c(1,30,60,90*(1:10)))
# 
# autoplot(km_fit)



### smart 3 i->2####

i=Colums_df[2]
i
#i='smart_3_normalized'

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


df.smart$Qartere <- cut(X , breaks = D, 
                    labels = c('0.25','0.5','0.75','1')
                    , right = FALSE)


km_fit <- survfit(Surv(time_day) ~ as.factor(Qartere), data=df.smart)

#summary(km_fit, time_day = c(1,30,60,90*(1:10)))

autoplot(km_fit)

survdiff(Surv(time_day) ~ as.factor(smart_1_normalized_Q), data=df.smart)

### smart 5 i->3####

i=Colums_df[3]
i
#i='smart_5_normalized'

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


df.smart$Qartere <- cut(X , breaks = D, 
                        labels = c('0.75','1')
                        , right = FALSE)


km_fit <- survfit(Surv(time_day) ~ as.factor(Qartere), data=df.smart)
#summary(km_fit, time_day = c(1,30,60,90*(1:10)))

autoplot(km_fit)

survdiff(Surv(time_day) ~ as.factor(Qartere), data=df.smart, rho = 1)


survreg(Surv(time_day) ~  smart_5_normalized , df, dist='exponential')

exp(0.002491316)

### smart 7 i->4####

i=Colums_df[4]
i
#i='smart_5_normalized'
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
df.smart$Qartere <- cut(X , breaks = D, 
                        labels = c('0.25','0.75','1')
                        , right = FALSE)


  km_fit <- survfit(Surv(time_day) ~ as.factor(Qartere), data=df.smart)
#summary(km_fit, time_day = c(1,30,60,90*(1:10)))

autoplot(km_fit)

survdiff(Surv(time_day) ~ as.factor(Qartere), data=df.smart, rho = 1)

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
df.smart$Qartere <- cut(X , breaks = D, 
                        labels = c('0.75','1')
                        , right = FALSE)


km_fit <- survfit(Surv(time_day) ~ as.factor(Qartere), data=df.smart)
#summary(km_fit, time_day = c(1,30,60,90*(1:10)))

autoplot(km_fit)

survdiff(Surv(time_day) ~ as.factor(Qartere), data=df.smart, rho = 1)



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
df.smart$Qartere <- cut(X , breaks = D, 
                        labels = c('0.25','0.75','1')
                        , right = FALSE)


km_fit <- survfit(Surv(time_day) ~ as.factor(Qartere), data=df.smart)
#summary(km_fit, time_day = c(1,30,60,90*(1:10)))

autoplot(km_fit)

survdiff(Surv(time_day) ~ as.factor(Qartere), data=df.smart, rho = 1)


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
df.smart$Qartere <- cut(X , breaks = D, 
                        labels = c('0.25','0.75','1')
                        , right = FALSE)


km_fit <- survfit(Surv(time_day) ~ as.factor(Qartere), data=df.smart)
#summary(km_fit, time_day = c(1,30,60,90*(1:10)))

autoplot(km_fit)

survdiff(Surv(time_day) ~ as.factor(Qartere), data=df.smart, rho = 1)

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
df.smart$Qartere <- cut(X , breaks = D, 
                        labels = c('0.75','1')
                        , right = FALSE)


km_fit <- survfit(Surv(time_day) ~ as.factor(Qartere), data=df.smart)
#summary(km_fit, time_day = c(1,30,60,90*(1:10)))

autoplot(km_fit)

survdiff(Surv(time_day) ~ as.factor(Qartere), data=df.smart, rho = 1)



### smart 198 i->9####


i=Colums_df[9]
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
df.smart$Qartere <- cut(X , breaks = D, 
                        labels = c('0.75','1')
                        , right = FALSE)


km_fit <- survfit(Surv(time_day) ~ as.factor(Qartere), data=df.smart)
#summary(km_fit, time_day = c(1,30,60,90*(1:10)))

autoplot(km_fit)

survdiff(Surv(time_day) ~ as.factor(Qartere), data=df.smart, rho = 1)
