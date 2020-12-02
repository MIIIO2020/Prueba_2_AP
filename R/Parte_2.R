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


### Función de SMART Analitic####
df.smart=df
Colums_df=colnames(df.smart)
Colums_df
#length(Colums_df)
Colums_df=Colums_df[6:21]
Colums_df=Colums_df[!Colums_df %in% "time_day"]

s=c(Colums_df[1])
#smart_1_normalized
D=quantile(df.smart$smart_1_normalized, prob=c(0,0.25,0.5,0.75,1))

df.smart$Quarter <- cut(df.smart$smart_1_normalized, breaks = D, 
                     labels = c('0.25','0.5','0.75','1'), right = FALSE)


km_fit <- survfit(Surv(time_day) ~ as.factor(Quarter), data=df.smart)
#summary(km_fit, time_day = c(1,30,60,90*(1:10)))
autoplot(km_fit)

