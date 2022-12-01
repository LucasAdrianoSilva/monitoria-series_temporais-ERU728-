##Aula 4
#ESTACIONARIEDADE, FAC E FACP

#Importando a serie
gasoline_df<-read.table("gasoline_df.csv",sep = ";", header = T)[,-1]
gasoline_ts <- ts(gasoline_df, start=c(2013,01,01), freq=12)  #transformacao em series temporais

#plotando a serie
plot(gasoline_ts)

#lembrando que,para encontrar a tend,tem-se a funcao ma
library(forecast)
trend_gasoline = ma(gasoline_ts, order = 12, centre = T)
plot(gasoline_ts)
lines(trend_gasoline)
plot(trend_gasoline)


#Pacotes necessarios
library(urca)

#Testando a existencia de raiz unitaria
gasoline_test <- ur.df(y=gasoline_ts, type = "trend", lags=10, selectlags = "AIC")
summary(gasoline_test)
#Existe raiz unit ,logo:
gasoline_test<- ur.df(y=diff(gasoline_ts,differences = 1),
                   type = "trend", lags=10, selectlags = "AIC")
summary(gasoline_test)
#Pelo resultado,rejeita-se a H0,logo a serie é estacionaria em primeira diferença
#Por consequencia,trata-se de uma serie estacionaria de ordem 1

#Testando o modelo sem tendencia,mas com drift(intercepto)
gasoline_test<-ur.df(y=diff(gasoline_ts,differences = 1),
                  type= "drift", lags=10, selectlags = "AIC")
summary(gasoline_test)

#Testando o modelo none
gasoline_test<-ur.df(y=diff(gasoline_ts,differences = 1),
                  type = "none", lags = 10, selectlags = "AIC")
summary(gasoline_test)

#RESULTADO: a nossa serie é I(1)


#FAC E FACP
par(mfrow=c(1,2))

acf(gasoline_ts)
pacf(gasoline_ts)
acf(diff(gasoline_ts, differences = 1))
pacf(diff(gasoline_ts, differences = 1))

#Estimando um ARIMA
#ARIMA(3,1,3)
reg<-arima(gasoline_ts, order = c(3,1,3))

#Grafico ACF e PACF para os residuos
par(mfrow=c(1,2))
acf(reg$residuals)
pacf(reg$residuals)

#Teste Ljung-box
lj.test<-Box.test(
  x = reg$residuals,
  lag = 10,
  type = "Ljung-Box", #p+q+1
  fitdf = 7
)

print(lj.test)
#Não se rejeitou H0

#Calculando os criterios de informacao AIC e BIC
aic.reg<-AIC(reg) %>% print
bic.reg<-BIC(reg) %>% print


###############################
#EXTRA (Tendencia-outro metodo)
###############################

##Encontrando a tendencia
#modelo de tendencia linear
t <- time(gasoline_ts) #extraindo o tempo como a variável explicativa da estrutura de dados da série temporal
reg0 <- lm(gasoline_ts~t, data = gasoline_ts) #modelo de tendencia linear
plot(gasoline_ts, main = "Time Series with Linear Trend")
points(t,predict.lm(reg0),type='l',col='red')
#
summary(reg0)

#modelo de tendencia quadratica
t <- time(gasoline_ts) #extraindo o tempo como a variável explicativa da estrutura de dados da série temporal
reg1 <- lm(gasoline_ts~t +I(t^2),data = gasoline_ts) #modelol de tendencia quadratica
plot(gasoline_ts, main = "Time Series with Quadratic Trend")
points(t,predict.lm(reg1),type='l',col='red')
#
summary(reg1)

#modelo de tendencia exponencial
t <- time(gasoline_ts) #extraindo o tempo como a variável explicativa da estrutura de dados da série temporal
reg2 <- lm(log(gasoline_ts)~t,data = gasoline_ts) #modelol de tendencia quadratica
plot(log(gasoline_ts), main = "Time Series with Quadratic Trend")
points(t,predict.lm(reg2),type='l',col='red')
#
summary(reg2)

#COMPARANDO OS MODELOS
install.packages("AICcmodavg")
library(AICcmodavg)

models <- list(reg0, reg1)

model.names <- c('reg0', 'reg1')

aictab(cand.set = models, modnames = model.names)
#A tendencia quadratica é a que se ajusta melhor com essa serie

#comparando modelos com variaveis diferentes
AIC(reg2)+2*sum(log(gasoline_ts)) #modelo exponencial
AIC(reg1) #modelo quadratico

#O melhor modelo é o quadratico
summary(reg1)

#Retirando a tendecia quadratica
t <- time(gasoline_ts)
lin.mod <- lm(gasoline_ts ~ t+I(t^2))
lin.trend <- lin.mod$fitted.values  #valores ajustados referem-se à tendência de tempo
quadr <- ts(lin.trend, start = c(2013, 1), frequency = 12)  #cria uma variável de série temporal para tendência
lin.cycle <- gasoline_ts - quadr
plot(lin.cycle)


###############################
###############################
