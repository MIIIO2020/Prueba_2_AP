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


data=fread('data/data_smart_mean_delta_skew.csv')

Funcion_no_usada<- function(){
      NAs = as.data.frame(sapply(df, function(x) sum(is.na(x))))
      colnames(NAs) = c("Count_NAs")
      NAs$Feature = rownames(NAs)                           
      NAs = NAs[which(NAs$Count_NAs != 0 & NAs$Feature != "time_days"),]  

      ggplot(NAs, aes(x=reorder(Feature,Count_NAs), y=Count_NAs)) +
        geom_bar(stat="identity") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      #rm(NAs)


      i='smart_192_normalized_mean'
      X=(df[[i]])
      length(X)
      Y= X[!is.na(X)]
      length(Y)
      df[[i]]=sapply(X, function(x) ifelse(is.na(x),mean(Y),x ) )

      i='smart_193_normalized_mean'
      X=(df[[i]])
      length(X)
      Y= X[!is.na(X)]
      length(Y)
      df[[i]]=sapply(X, function(x) ifelse(is.na(x),mean(Y),x ) )


      i='smart_193_normalized_delta_skew'
      X=(df[[i]])
      length(X)
      Y= X[!is.na(X)]
      length(Y)
      df[[i]]=sapply(X, function(x) ifelse(is.na(x),mean(Y),x ) )




      Colums_df=colnames(df)
      Colums_df
      l=length(Colums_df)

      Colums_df=Colums_df[ 4:l ]

      #Colums_df=Colums_df[!Colums_df %in% "model_Bin"]#
      Colums_df


      df.Nas<-data.frame()
      i=0
      #df.Nas[,'Features']
      for (c in Colums_df){
        i=1+i
        X=(df[[c]])
        length(X)
        Y= X[!is.na(X)]
        length(Y)
        c=as.character(c)
        #df.Nas[[c]]=sapply(X, function(x) ifelse(is.na(x),mean(c),x ) )
        #df.Nas[c,'df.Nas']=(sd(Y)/mean(Y))
        df.Nas[i,'Features']=c
        df.Nas[i,'covarianza']=(sd(Y)/mean(Y))
      }
      dsd=df.Nas[ df.Nas$covarianza<1,]
      #dsd=dsd[df.Nas$covarianza>0,]
      ggplot(data=dsd, aes(x=dsd$covarianza, y=dsd$Features))+geom_point()


      NAs = as.data.frame(
        sapply(df, function(x) if(!is.na(x))sd(x))
          )
        
      colnames(NAs) = c("Count_NAs")
      NAs$Feature = rownames(NAs)                           
      NAs = NAs[which(NAs$Count_NAs != 0 & NAs$Feature != "time_days"),]  

      ggplot(NAs, aes(x=reorder(Feature,Count_NAs), y=Count_NAs)) +
        geom_bar(stat="identity") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      }
      Funcion_no_usada()




### KM ####
df=data#[data$failure=='1',]
df$serial_number=NULL





ggplot(data = df, aes(y = time_day,
                      x = smart_1_normalized_delta_skew))+
  geom_point()

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

### Curvas de KM smart skew####

  #Creamos una copia para ajustar una a 
  #una las var smart sin afectar a df
  df.smart=copy(df)
  df.smart$model=NULL


### Recogemos las columnas smart skews ####
  #Esto es para hacer el analisis bi 
  #variado de forma automatica

  Colums_df=colnames(df.smart)
  Colums_df
  l=length(Colums_df)

  Colums_df=Colums_df[ 12:l ]

  #Colums_df=Colums_df[ 3:11 ]
  #Colums_df=Colums_df[!Colums_df %in% "model_Bin"]#
  Colums_df


### KM smart_1_normalized ####

  i=Colums_df[1]

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





  #D=quantile(X, prob=c(0,0.25,0.5,0.75,1))
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

  #summary(km_fit, time_day = c(1,30,60,90*(1:10)))

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
  #i='smart_3_normalized'

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
  i

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
  df.smart$Categoric <- cut(X , breaks = D, 
                          labels = c('0.75','1')
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
  df_skwews<-df.smart[,.(
    time_day=time_day,
    smart_3_normalized=smart_3_normalized,
    smart_7_normalized=smart_7_normalized,
    smart_193_normalized=smart_193_normalized
  )
  ]



#write.csv(df.smart,file = "data_fact_skews.csv")
