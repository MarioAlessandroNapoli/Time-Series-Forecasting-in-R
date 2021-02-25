library(plotly)
library(lubridate)
library(tidyverse)
library(stringr)
library(forecast)
library(fpp)
library(urca)
library(keras)
library(KFAS)
library(xts)

preprocessing <- function(df){
  df %>% group_by(DATA) %>% count() %>% filter(n != 24)
  df %>% filter(DATA == '2019-03-31')
  #df %>% filter(DATA == '2020-03-29')
  
  #Aggiungiamo le 2 ore mancanti nelle rispettive posizioni
  ind1 <- which(df$DATA=='2019-03-31' & df$Ora == 2)
  df <- rbind(df[1:ind1,], 
              list('2019-03-31',3,3039997),
              df[-(1:ind1),])
  
  ind2 <- which(df$DATA=='2020-03-29' & df$Ora == 2)
  df <- rbind(df[1:ind2,], 
              list('2020-03-29',3,2329514),
              df[-(1:ind2),])
  #df %>%
  #  mutate(time = str_c(DATA,Ora,sep = ":")) %>% 
  #  mutate(time = time %>% ymd_h()) -> df
  
  #Check date
  
  dateInDf <- unique(as.Date(df$DATA))
  dateReali <- seq(as.Date(first(df$DATA)), as.Date(last(df$DATA)), by="days")
  
  #Manca un giorno:
  as.Date(setdiff(dateReali,dateInDf))
  
  #plotInd1 <- which(df$DATA=='2020-05-30' & df$Ora == 1)
  #plotInd2 <- which(df$DATA=='2020-06-01' & df$Ora == 24)
  #plot(ts(df[plotInd1:plotInd2,]$VALORE))
  
  #Imputiamo i dati del giorno mancante come media dei due giorni adiacenti
  
  mean <- (df %>% filter(DATA == '2020-05-30') %>% pull(VALORE) + df %>% filter(DATA == '2020-06-01') %>% pull(VALORE) ) /2
  toAdd <- df %>% filter(DATA == '2020-05-30')
  toAdd$VALORE <- mean
  toAdd$DATA <- '2020-05-31'
  
  ind3 <- which(df$DATA=='2020-05-30' & df$Ora == 24)
  
  df <- rbind(df[1:ind3,], 
              toAdd,
              df[-(1:ind3),])
  
  #plotInd1 <- which(df$DATA=='2020-05-30' & df$Ora == 1)
  #plotInd2 <- which(df$DATA=='2020-06-01' & df$Ora == 24)
  #plot(ts(df[plotInd1:plotInd2,]$VALORE))
  return(df)
}

plotSeries <- function(series, other='none', third='none'){
  col1 <- '0eb9cc'
  col2 <- 'cc440e'
  if(class(third) == 'character'){
    if(class(other) == 'character'){
      if(other=='MA'){
        plot_ly(x = time(series)
                , y = as.vector(series), type="scatter", mode = 'lines', 
                line=list(color=col1, width=3), name = 'Time Series') %>% 
          add_lines(x=time(series),
                    y=stats::filter(as.vector(series), rep(1/7,7),sides = 1),
                    line=list(color=col2, width=1.5), name="Weekly MA") %>%
          layout(yaxis = list(range = c(min(series),max(series))))
      }
      else if(other=='none'){
          plot_ly(x = time(series)
                  , y = as.vector(series), type="scatter", mode = 'lines', 
                  line=list(color=col1, width=3), name = 'Time Series')
      }
      else if(other=='all'){
        plot_ly(x = time(series), y = as.vector(series), type="scatter", mode = 'lines', 
                line=list(color=col1, width=2.5), name = 'Time Series')                       %>% 
          add_lines(x=time(series), y=stats::filter(series, rep(1/7,7),sides = 1),
                    line=list(color='cca30e', width=2), name="Weekly MA")            %>%
          add_lines(x=time(series), y=stats::filter(series, rep(1/30,30),sides = 1),
                    line=list(color='cc0e63', width=1), name="Monthly MA")             %>%
          layout(yaxis = list(range = c(min(series),max(series))))
      }
    }
    else{
      plot_ly(x = time(series)
              , y = as.vector(series), type="scatter", mode = 'lines', 
              line=list(color=col1, width=3), name = 'Time Series') %>% 
        add_lines(x=time(series),
                  y=as.vector(other),
                  line=list(color=col2, width=1.5), name="Series 2") %>%
        layout(yaxis = list(range = c(min(series),max(series))))
    }
  }
  else{
    plot_ly(x = time(series), y = as.vector(series), type="scatter", mode = 'lines', 
            line=list(color='0eb9cc88', width=2.5), name = 'Time Series')                       %>% 
      add_lines(x=time(series), y=as.vector(other),
                line=list(color='cf0056', width=2), name="Series 2")            %>%
      add_lines(x=time(series), y=as.vector(third),
                line=list(color='42cf00', width=2), name="Series 3")             %>%
      layout(yaxis = list(range = c(min(series),max(series))))
  }
}

split_One_Step <- function(sequence, n_steps){
  X <- list()
  Y <- list()
  for( i in 1:(length(sequence)-n_steps)){
    end <- i + n_steps - 1
    X <- c(X, list(sequence[i:end] ) )
    Y <- c(Y,sequence[(end+1)])
  }
  return(list(X,Y))
}

mean <- c()
sd <- c()

normTS <- function(series){
  mean <<- mean(series)
  sd <<- sd(series)
  series <- (series - mean) / sd
  return(series)
}

denormTS <- function(series){
  return(series*sd+mean)
}

getMAE <- function(test,pred){
  return(as.integer(mean(abs(exp(as.vector(denormTS(test))) - exp(denormTS(pred))))))
}

df = read.csv('C://Users//Mario//Desktop//a//UNI//Time Series//Esame TSA//TrainingSet.csv',sep = ';')
covidData = read.csv('C://Users//Mario//Desktop//a//UNI//Time Series//Esame TSA//covidData.csv',sep = ',')
covidData$data <- as.Date(covidData$data)
df <- preprocessing(df)

d <- density(df$VALORE)
plot(d, type="n", main="Valore")
polygon(d, col="lightgray", border="gray")

df %>% mutate(VALORE = log(df$VALORE)) -> df

d <- density(df$VALORE)
plot(d, type="n", main="Valore")
polygon(d, col="lightgray", border="gray")

#Analizziamo TS
plotSeries(myts, 'all')
#TEST DICKEY-FULLER pre diff
{
  res <- c()
  pv <- c()
  for(hour in 1:24){
    data <- df %>% filter(Ora == hour)
    myts <- xts(data$VALORE, as.Date(data$DATA))
    myts <- normTS(myts)
    #diff = diff.xts(myts)
    #diff[1] <- 0
    res <- c(res,adf.test(myts,'stationary')$statistic)
    pv <- c(pv,adf.test(myts,'stationary')$p.value)
  }
  cat(which.max(as.vector(res)),': ',
  max(res),'\n',
  which.min(as.vector(res)),': ',
  min(as.vector(res)),'\n')
}
#TEST DICKEY-FULLER con diff
{
  res <- c()
  pv <- c()
  for(hour in 1:24){
    data <- df %>% filter(Ora == hour)
    myts <- xts(data$VALORE, as.Date(data$DATA))
    myts <- normTS(myts)
    diff = diff.xts(myts)
    diff[1] <- 0
    res <- c(res,adf.test(diff,'stationary')$statistic)
    pv <- c(pv,adf.test(diff,'stationary')$p.value)
  }
  cat(which.max(as.vector(res)),': ',
  max(res),'\n',
  which.min(as.vector(res)),': ',
  min(as.vector(res)),'\n')
}

#torniamo analisi ore 20



data <- df %>% filter(Ora == 20)
myts <- xts(data$VALORE, as.Date(data$DATA))
plotSeries(myts, 'all')
#Normalization
myts <- normTS(myts)

ggAcf(diff,lag.max = 50)
ggPacf(diff, lag.max = 50)

n <- length(myts)
winLen <- as.integer(n * .2)

train <- myts[1:(n-winLen)]
test <- myts[(n-winLen+1):n]

plotSeries(train)
plotSeries(test)

getArimaMAE <- function(p,d,q,P,D,Q){
  mod <- Arima(train, c(p,d,q), 
               seasonal = list(order = c(P,D,Q), period = 7))
  pred <- forecast(mod,winLen)$mean
  cat('(',p,',',d,',',q,'),(',P,',',D,',',Q,'):',getMAE(test,pred),'\n')
  return(invisible(pred))
}

getArimaMAE(1,1,1,1,1,1) #3
getArimaMAE(2,1,2,1,1,0) #2
bestArima <- getArimaMAE(1,1,1,1,1,0) #1

plotSeries(test,bestArima)

meanArima <- c()
for(hour in 1:24){
  cat(hour)
  data <- df %>% filter(Ora == hour)
  myts <- xts(data$VALORE, as.Date(data$DATA))
  myts <- normTS(myts)
  
  meanArima<- c(meanArima,getMAE(test,getArimaMAE(1,1,1,1,1,0))) #2
}
mean(meanArima)


getSinArimaMAE <- function(num_sin,p=1,d=1,q=1,P=1,D=1,Q=0){
  tempo <- 1:n
  vj <- 1:num_sin
  freq <- outer(tempo, vj) * 2 * pi / 365
  x <- cbind(cos(freq), sin(freq))
  colnames(x) <- c(paste0("cos", vj), paste0("sin",vj))
  trainReg <- x[1:(n-winLen),]
  testReg <- x[(n-winLen+1):n,]
  
  mod <- Arima(train, c(p,d,q), 
                    list(order = c(P,D,Q), period = 7),
                    xreg = trainReg)
  pred <- forecast(mod,winLen,x = testReg)$mean
  cat('(',p,',',d,',',q,'),(',P,',',D,',',Q,'),',num_sin,'sin:',getMAE(test,pred),'\n')
  return(invisible(mod$fitted))
}

meanArima <- c()
for(hour in 1:24){
  cat(hour)
  data <- df %>% filter(Ora == hour)
  myts <- xts(data$VALORE, as.Date(data$DATA))
  myts <- normTS(myts)
  
  meanArima<- c(meanArima,getMAE(train,getSinArimaMAE(8))) #2
}
mean(meanArima)

#BEST SARIMA TRIG
data <- df %>% filter(Ora == 20)
myts <- xts(data$VALORE, as.Date(data$DATA))
myts <- normTS(myts)

bestTrigArima <- getSinArimaMAE(8)  #1

plotSeries(test,bestTrigArima)




#BRUTE-FORCING PER CAPIRE DUMMY MIGLIORE
#Provo tutte le dummy per tutte le 24 serie storiche e faccio media

## P.S. LE DUMMY PEGGIORANO IL RISULTATO DA NON UTILIZZARE

getSinDumArimaMAE <- function(var,num_sin=4,p=1,d=1,q=1,P=1,D=1,Q=0){
  tempo <- 1:n
  vj <- 1:8
  freq <- outer(tempo, vj) * 2 * pi / 365
  x <- cbind(cos(freq), sin(freq))
  colnames(x) <- c(paste0("cos", vj), paste0("sin",vj))
  trainReg <- x[1:(n-winLen),]
  testReg <- x[(n-winLen+1):n,]
  
  dummies <- as.vector(as.data.frame(covidData %>%filter(data >= "2018-09-01", data<"2020-09-01"))[,var])
  zeros <- rep(0,(n-190))

  covidDum <- c(zeros,dummies)
  
  trainRegDum <- cbind(trainReg, 
                       covidDum[1:(n-winLen)])
  testRegDum <- cbind(testReg, 
                      covidDum[(n-winLen+1):n])
  
  mod <- Arima(train, c(1,1,1), 
               list(order = c(1,1,1), period = 7),
               xreg = trainRegDum)
  pred <- forecast(mod,winLen,x = testRegDum)$mean
  cat(getMAE(test,pred),':   ',names(covidData)[var],'\n')
  return(invisible(pred))
}
###SKIP
{
  var_covid <- c(3,4,5,6,7,8,9,10,11,14,15)
  names(covidData)[var_covid]
  
  {
    tempo <- 1:n
    vj <- 1:8
    freq <- outer(tempo, vj) * 2 * pi / 365
    x <- cbind(cos(freq), sin(freq))
    colnames(x) <- c(paste0("cos", vj), paste0("sin",vj))
    trainReg <- x[1:(n-winLen),]
    testReg <- x[(n-winLen+1):n,]
    
    
    var_covid <- c(3,4,5,6,7,8,9,14)
    names(covidData)[var_covid]
    
    MAEforVar <- c()
    for(var in var_covid){
      cat(var)
      dummies <- as.vector(as.data.frame(covidData %>%filter(data >= "2018-09-01", data<"2020-09-01"))[,var])
      zeros <- rep(0,(n-190))
      
      covidDum <- c(zeros,dummies)
      
      trainRegDum <- cbind(trainReg, 
                           covidDum[1:(n-winLen)])
      testRegDum <- cbind(testReg, 
                          covidDum[(n-winLen+1):n])
      MAEvals <- c()
      for( hour in 1:24){
        cat('.')
        data <- df %>% filter(Ora == hour)
        myts <- xts(data$VALORE, as.Date(data$DATA))
        
        myts <- normTS(myts)
        
        train <- myts[1:(n-winLen)]
        test <- myts[(n-winLen+1):n]
        
        plotSeries(test)
        
        
        mod <- Arima(train, c(1,1,1), 
                     list(order = c(1,1,0), period = 7),
                     xreg = trainRegDum)
        pred <- forecast(mod,winLen,x = testRegDum)$mean
        MAEvals <- c(MAEvals,getMAE(test,pred))
      }
      MAEforVar <- c(MAEforVar,mean(MAEvals))
    }
  }
  plotSeries(test,pred)
  
  bestVar <- which.min(MAEforVar)
  cat('La miglior Dummy è ',names(covidData)[bestVar],': ',MAEforVar[bestVar],'\n')

}

bestTrigWithDum <- getSinDumArimaMAE(bestVar)

plotSeries(test,bestTrigArima,bestTrigWithDum)
#Modello finale SARIMAX (1,1,1)(1,1,1) 8 sinusoidi, NO dummy

#############

#########
###UCM###
#########
{
  data <- df %>% filter(Ora == 20)
  myts <- xts(data$VALORE, as.Date(data$DATA))
  myts <- normTS(myts)
  train <- myts[1:(n-winLen)]
  test <- myts[(n-winLen+1):n]
  
  yna <- myts["2018-08-30/"]
  test[1]
  yna["2020-04-08/"] <- NA
  plotSeries(test)
  
  # pars[1] log-var(eta) pars[2] log-var(zeta) pars[3] log-var(omega7)
  # pars[4] log-var(omega365) pars[5] log-var(eps)
  updt1 <- function(pars, model, nh) {
    model$Q[1, 1, 1] <- exp(pars[1])
    model$Q[2, 2, 1] <- exp(pars[2])
    model$Q[3, 3, 1] <- exp(pars[3])
    diag(model$Q[4:(3 + 2*nh), 4:(3 + 2*nh), 1]) <- exp(pars[4])
    model$H[1, 1, 1] <- exp(pars[5])
    model
  }
  
  nh <- 8
  
  ucm1 <- SSModel(as.numeric(yna) ~ SSMtrend(2,list(NA,NA)) + 
                    SSMseasonal(7, NA) +
                    SSMseasonal(365, NA, "trig", harmonics = 1:nh),
                  H = NA)
  
  vy <- as.numeric(var(yna, na.rm = T))
  
  # - per stabilità, non usiamo condizioni diffuse
  ucm1$P1inf[] <- 0
  ucm1$a1[1] <- mean(yna, na.rm = TRUE)
  diag(ucm1$P1) <- vy
  
  ucmfit1 <- fitSSM(ucm1,
                    log(c(vy/1000, vy/1000, vy/1000, vy/10000, vy/10)),
                    updt1, update_args = list(nh = nh))
  
  ucmfit1$optim.out$convergence
  par <- ucmfit1$optim.out$par
  exp(par)
  
  kfs1 <- KFS(ucmfit1$model, smoothing = c("state","signal","disturbance"))
  
  #TREND
  plot(kfs1$alphahat[,'level'])
  #WEEKLY
  plot(kfs1$alphahat[100:150, "sea_dummy1"], type = "l",
       main = "weekly seasonal component")
  # STAG 365
  plot(rowSums(kfs1$alphahat[, seq(9, 8 + 2*nh, 2)]), type = "l",
       main = "Yearly seasonal component")
  
  #PRED MULTI STEP
  pred_multi_step <- kfs1$muhat[is.na(yna)]
  #PRED ONE STEP AHEAD
  ucm10 <- ucmfit1$model
  ucm10$y[] <- myts["2018-08-30/"]
  kfs10 <- KFS(ucm10, filtering = "signal")
  pred_one_step <- kfs10$m[is.na(yna)]
  pred_one_step_train <- kfs10$m[!is.na(yna)]
  
  cat(getMAE(as.numeric(test),as.numeric(pred_multi_step)),
  getMAE(test,pred_one_step_train))
  
  plotSeries(test,pred_multi_step,pred_one_step)
}


# CALCOLO MAE MEDIO PER TUTTE LE 24 TS, mediando i risultati, sia multi-step che one step
{
  MAEforHour <- c()
  MAEforHourMul <- c()
  cat('Ora  Multi-Step  One-Step\n')
  for(hour in 1:24){
    
    data <- df %>% filter(Ora == hour)
    myts <- xts(data$VALORE, as.Date(data$DATA))
    myts <- normTS(myts)
    train <- myts[1:(n-winLen)]
    test <- myts[(n-winLen+1):n]
    
    
    yna <- myts["2018-08-30/"]
    yna["2020-04-08/"] <- NA
    
    nh <- 4
    
    ucm1 <- SSModel(as.numeric(yna) ~ SSMtrend(2,list(NA,NA)) + 
                      SSMseasonal(7, NA) +
                      SSMseasonal(365, NA, "trig", harmonics = 1:nh),
                    H = NA)
    
    vy <- as.numeric(var(yna, na.rm = T))
    
    # - per stabilità, non usiamo condizioni diffuse
    ucm1$P1inf[] <- 0
    ucm1$a1[1] <- mean(yna, na.rm = TRUE)
    diag(ucm1$P1) <- vy
    
    ucmfit1 <- fitSSM(ucm1,
                      log(c(vy/200, vy/1000, vy/1000, vy/10000, vy/10)),
                      updt1, update_args = list(nh = nh))
    
    ucmfit1$optim.out$convergence
    par <- ucmfit1$optim.out$par
    exp(par)
    
    #MULTI STEP
    pred_multi_step <- kfs1$muhat[is.na(yna)]
    pred_multi_step_train <- kfs1$muhat[!is.na(yna)]
    #PRED ONE STEP AHEAD
    kfs1 <- KFS(ucmfit1$model, smoothing = c("state","signal","disturbance"))
    
    ucm10 <- ucmfit1$model
    ucm10$y[] <- myts["2018-08-30/"]
    kfs10 <- KFS(ucm10, filtering = "signal")
    pred_one_step <- kfs10$m[is.na(yna)]
    
    pred_one_step_train <- kfs10$m[!is.na(yna)]
      
    cat(hour,': ',getMAE(test,pred_multi_step),'\t',getMAE(test,pred_one_step),'\n')
    MAEforHour <- c(MAEforHour,getMAE(train,pred_one_step_train))
    MAEforHourMul <- c(MAEforHourMul,getMAE(train,pred_multi_step_train))
  }
  cat('\nMean MAE:\nOne-Step:\t',mean(MAEforHour),
  '\nMulti-Step:\t',mean(MAEforHourMul))
}


# one-step molto meglio, performance peggiore nella fascia 12-18,

# Provando il ciclo precedente con 4,8 o 12 sinusoidi su tutte le 24 ts, si ottengono i seguenti MAE per ONE-STEP

# MAE MEDIO ONE-STEP
# 12 trig:  210489
# 8  trig:  209165
# 4  trig:  207338

# 4 sinusoidi si confermano un'ottima scelta anche per gli UCM

# Test esaustivo UCM con DUMMIES come visto in precedenza per gli Arima
{
  var_covid <- c(3,4,5,6,7,8,9)
  MAEforVar <- c()
  for(var in var_covid){
    cat(var)
    dummies <- as.vector(as.data.frame(covidData %>%filter(data >= "2018-09-01", data<"2020-09-01"))[,var])
    zeros <- rep(0,(n-190))
    
    covidDum <- c(zeros,dummies)
    
    MAEforHour <- c()
    
    for(hour in 1:24){
      cat('.')
      data <- df %>% filter(Ora == hour)
      myts <- xts(log(data$VALORE), as.Date(data$DATA))
      myts <- normTS(myts)
      train <- myts[1:(n-winLen)]
      test <- myts[(n-winLen+1):n]
      
      nh <- 4
      
      ucm1 <- SSModel(as.numeric(yna) ~ SSMtrend(2,list(NA,NA)) + 
                        SSMseasonal(7, NA) +
                        SSMseasonal(365, NA, "trig", harmonics = 1:nh)+
                        covidDum,
                      H = NA,
                      data = as.data.frame(covidDum))
      
      vy <- as.numeric(var(yna, na.rm = T))
      
      # - per stabilità, non usiamo condizioni diffuse
      ucm1$P1inf[] <- 0
      ucm1$a1[1] <- mean(yna, na.rm = TRUE)
      diag(ucm1$P1) <- vy
      
      ucmfit1 <- fitSSM(ucm1,
                        log(c(vy/1000, vy/1000, vy/1000, vy/10000, vy/10)),
                        updt1, update_args = list(nh = nh))
      
      ucmfit1$optim.out$convergence
      par <- ucmfit1$optim.out$par
      exp(par)
      
      kfs1 <- KFS(ucmfit1$model, smoothing = c("state","signal","disturbance"))
      
      
      #PRED MULTI STEP
      pred_multi_step <- kfs1$muhat[is.na(yna)]
      #PRED ONE STEP AHEAD
      ucm10 <- ucmfit1$model
      ucm10$y[] <- myts["2018-08-30/"]
      kfs10 <- KFS(ucm10, filtering = "signal")
      pred_one_step <- kfs10$m[is.na(yna)]
      
      MAEforHour <- c(MAEforHour,getMAE(test,pred_one_step))
      }
      MAEforVar <- c(MAEforVar,mean(MAEforHour))
      cat('\n')
    }
}


# Miglior MAE medio con DUMMY 202689, per gli UCM, MAE scende di circa il 2%,
# per semplicità evitiamo quindi di complicare il modello a fronte di un guadagno così basso
# Miglior Modello Trend+doppia seasonality 7/365, 4 sinusoidi no dummy

# ML
# LSTM
#####FUNZIONA MALE, meglio implementazione su Python

MAEforHour <- c()
for(hour in 1:24){
  data <- df %>% filter(Ora == hour)
  myts <- xts(log(data$VALORE), as.Date(data$DATA))
  myts <- normTS(myts)
  train <- myts[1:(n-winLen)]
  test <- myts[(n-winLen+1):n]
  
  k <- length(test)
  windowLenght <- 120
  n_steps <- 21
  
  data <- split_One_Step(as.vector(myts),n_steps)
  X <- data[[1]] %>% k_reshape(c(length(data[[1]]),n_steps,1))
  Y <- data[[2]] %>% k_reshape(c(length(data[[2]]),1,1))
  
  model <- keras_model_sequential() %>% 
    layer_gru(units = 200, input_shape = c(n_steps,1)) %>%
    layer_dense(100) %>%
    layer_dense(1) %>%
    compile(optimizer = optimizer_adam(), loss='mae')
  
  keras::fit(model,x = X,y = Y, epochs = 125, batch_size = 128, verbose = 0)
  
  raw_data <- as.vector(myts)
  for( i in 1:k){
    x_input <- tail(raw_data,n_steps) %>% k_reshape(c(1,n_steps,1))
    yhat <- model %>% predict(x_input)
    raw_data <- c(raw_data,yhat)
  }
  
  predML <- tail(raw_data,k)
  
  
  cat(hour,': ',getMAE(test,predML,normPar),'\n')
  MAEforHour <- c(MAEforHour,getMAE(test,predML))
}


plotSeries(test,predML)

mean(MAEforHour)



####CODICE PER PRODURRE OUTPUT FINALE

getArima <- function(myts){
  #ARIMA CON SINUSOIDI
  tempo <- 1:792
  vj <- 1:3
  freq <- outer(tempo, vj) * 2 * pi / 365
  x <- cbind(cos(freq), sin(freq))
  colnames(x) <- c(paste0("cos", vj), paste0("sin",vj))
  trainReg <- x[1:731,]
  testReg <- x[732:792,]
  length(dummies)
  dummies <- as.vector(as.data.frame(covidData %>%filter(data >= "2018-09-01", data<="2020-10-31"))[,6])
  zeros <- rep(0,(n-190))
  
  covidDum <- c(zeros,dummies)
  
  trainRegDum <- cbind(trainReg, 
                       covidDum[1:n])
  testRegDum <- cbind(testReg, 
                      covidDum[n:n+61])
  
  modArima <- Arima(myts, c(1,1,1), 
                    list(order = c(1,1,0), period = 7),
                    xreg = trainReg)
  
  predArima <- forecast(modArima,61,x = testReg)
  bestArima <- as.vector(predArima$mean)
  return(bestArima)
}

getUCM   <- function(myts){
  NAs <- xts(as.numeric(rep(NA,61)),seq(last(time(myts))+1,last(time(myts))+61,'days'))
  yna <- rbind(myts,NAs)
  updt1 <- function(pars, model, nh) {
    model$Q[1, 1, 1] <- exp(pars[1])
    model$Q[2, 2, 1] <- exp(pars[2])
    model$Q[3, 3, 1] <- exp(pars[3])
    diag(model$Q[4:(3 + 2*nh), 4:(3 + 2*nh), 1]) <- exp(pars[4])
    model$H[1, 1, 1] <- exp(pars[5])
    model
  }
  nh <- 4
  ucm1 <- SSModel(as.numeric(yna) ~ SSMtrend(2,list(NA,NA)) + 
                    SSMseasonal(7, NA) +
                    SSMseasonal(365, NA, "trig", harmonics = 1:nh),
                  H = NA)
  vy <- as.numeric(var(yna, na.rm = T))
  ucm1$P1inf[] <- 0
  ucm1$a1[1] <- mean(yna, na.rm = TRUE)
  diag(ucm1$P1) <- vy
  ucmfit1 <- fitSSM(ucm1,
                    log(c(vy/200, vy/1000, vy/1000, vy/10000, vy/10)),
                    updt1, update_args = list(nh = nh))
  
  kfs1 <- KFS(ucmfit1$model, smoothing = c("state","signal","disturbance"))
  
  ucm10 <- ucmfit1$model
  ucm10$y[] <- yna["2018-08-30/"]
  kfs10 <- KFS(ucm10, filtering = "signal")
  bestUCM <- kfs10$m[is.na(yna)]
  return(bestUCM)
}

getML    <- function(myts){
  n <- length(myts)
  k <- 61
  windowLenght <- 120
  n_steps <- 21
  
  raw_data <- myts
  data <- split_One_Step(raw_data,n_steps)
  X <- data[[1]] %>% k_reshape(c(length(data[[1]]),n_steps,1))
  Y <- data[[2]] %>% k_reshape(c(length(data[[2]]),1,1))
  
  model <- keras_model_sequential() %>% 
    layer_gru(units = 200, input_shape = c(n_steps,1)) %>%
    layer_dense(1) %>%
    compile(optimizer = optimizer_adam(), loss='mae')
  
  keras::fit(model,x = X,y = Y, epochs = 125, batch_size = 128, verbose = 0)
  
  predData <- as.vector(tail(raw_data,n_steps) )
  for( i in 1:k){
    x_input <- tail(predData,n_steps) %>% k_reshape(c(1,n_steps,1))
    yhat <- model %>% predict(x_input)
    predData <- c(predData,yhat)
  }
  bestML <- tail(predData,k)
  return(bestML)
}


getPredictions <- function(df, orario){
  data <- df %>% filter(Ora == orario)
  myts <- xts(data$VALORE, as.Date(data$DATA))
  #Normalization
  myts <- normTS(myts)
  ##Models Output
  bestArima <- exp(denormTS(getArima(myts)))
  bestUCM <- exp(denormTS(getUCM(myts)))
  bestML <-c()
  #bestML <- exp(denormTS(getML(myts),normPars))
  
  return(list(bestArima, bestUCM, bestML))
}

finalPred <- c()

for(i in 1:24){
  finalPred <- c(finalPred, list(getPredictions(df,i)))
  print(i)
}

finalArima <- c()
finalUCM   <- c()
finalML    <- c()

for(i in 1:61){
  for(j in 1:24){
    finalArima <- c(finalArima,finalPred[[j]][[1]][i]  )
    finalUCM <- c(finalUCM,finalPred[[j]][[2]][i]  )
    finalML <- c(finalML,finalPred[[j]][[3]][i]  )
  }
}

dates <-rep(seq(as.Date('2020-09-01'),as.Date('2020-10-31'),'days'), each = 24)
hours <-rep(1:24,61)

finalDF <- data.frame(dates,hours,as.integer(finalArima),as.integer(finalUCM))#,as.integer(finalML))
colnames(finalDF) <- c('Data','Ora','ARIMA','UCM')#,'ML')

write.table(x = finalDF,file = 'SDMTSA_860571_3.csv', row.names = FALSE, quote = FALSE, sep=';')

