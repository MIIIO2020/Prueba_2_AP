library(ggplot2)
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


### Na Analisis####

NAs = as.data.frame(sapply(df, function(x) sum(is.na(x))))
colnames(NAs) = c("Count_NAs")
NAs$Feature = rownames(NAs)                           
NAs = NAs[which(NAs$Count_NAs != 0 & NAs$Feature != "time_days"),]  

ggplot(NAs, aes(x=reorder(Feature,Count_NAs), y=Count_NAs)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
+ xlab("Features")

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

df=data[data$failure=='1',]
df$failure=NULL
df$vida_util_restante_int=NULL
df$vida_util_restante=NULL

### Plot####
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
              model_F = factor(model_F),
              time_day = time_day)

km_model <- survfit(Surv(time_day) ~ as.factor(Quarter), data=df.smart)
#summary(km_fit, time_day = c(1,30,60,90*(1:10)))

autoplot(km_model)








### Curvas de KM####




#S(t) general
km_fit <- survfit(Surv(time_day) ~ 1, data=df)
s=summary(km_fit, time_day = c(1,30,60,90*(1:10)))
autoplot(km_fit)


#KM de modelo
km_fit <- survfit(Surv(time_day,model) ~ 1, data=df)
summary(km_fit, time_day = c(1,30,60,90*(1:10)))

autoplot(km_fit)


km_fit <- survfit(Surv(time_day,smart_1_normalized) ~ 1, data=df)
#summary(km_fit, time_day = c(1,30,60,90*(1:10)))
autoplot(km_fit)

hist(df$smart_1_normalized)

### Función de SMART Analitic####
    # smart 2 no sirve
    # Posee muchisimos Na Por lo cual 
    #eliminamos todos los smart con muchos Na



df.smart=copy(df)
### Recogemos las columnas ####
  #Esto es para hacer el analisis bi 
  #variado de forma automatica

Colums_df=colnames(df.smart)
Colums_df
#length(Colums_df)

Colums_df=Colums_df[6:21]
Colums_df=Colums_df[!Colums_df %in% "time_day"]

### KM por medio de  quantile #####


i=Colums_df[4]

#i='smart_193_normalized'

df.smart$capacity_bytes=as.numeric(df.smart$capacity_bytes)
X=(df.smart[,..i])

sd(X[[i]])

X=lapply(X, function(x) ifelse(is.na(x),0,x ) )
hist(X[[i]])

D=quantile(X[[i]], prob=c(0,0.25,0.5,0.75,1))


df.smart$Quarter <- cut(X[[i]] , breaks = D, 
                     labels = c('0.25','0.5','0.75','1'), right = FALSE)


km_fit <- survfit(Surv(time_day) ~ as.factor(Quarter), data=df.smart)

#summary(km_fit, time_day = c(1,30,60,90*(1:10)))

autoplot(km_fit)
#smart 2 no sirve lo anterior

                
# tiene muchos Na.


###Visualizar cuanstos Na tiene cada columna####

j=Colums_df[2]

Y=df.smart[,..j]
sum(sapply(Y[[j]], function(x) ifelse(is.na(x),1,0 ) ))

Y[[j]]=sapply(Y[[j]], function(x) ifelse(is.na(x),0,1 ) )
length(Y)
sum(Q)

Q=Y[is.na(x) ,  ]

