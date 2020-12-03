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
# data$failure=as.factor(data$failure)

df=data[data$failure=='1',]


km_fit <- survfit(Surv(time_day) ~ 1, data=df)
s=summary(km_fit, time_day = c(1,30,60,90*(1:10)))
autoplot(km_fit)



km_fit <- survfit(Surv(time_day,model) ~ 1, data=df)
summary(km_fit, time_day = c(1,30,60,90*(1:10)))

autoplot(km_fit)


km_fit <- survfit(Surv(time_day,smart_1_normalized) ~ 1, data=df)
#summary(km_fit, time_day = c(1,30,60,90*(1:10)))
autoplot(km_fit)

hist(df$smart_2_normalized)

### Función de SMART Analitic####
#smart 2 no sirve
# Posee muchisimos Na



df.smart=copy(df)
############################
Colums_df=colnames(df.smart)
Colums_df
#length(Colums_df)
Colums_df=Colums_df[6:21]
Colums_df=Colums_df[!Colums_df %in% "time_day"]

###########################################


i=Colums_df[3]

X=df.smart[,..i]

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



