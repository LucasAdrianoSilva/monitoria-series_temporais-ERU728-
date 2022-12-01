##Aula 5
##Metodologia Box-Jenkins

#Processos autoregressivos de ordem 1 com diferentes valores de beta
Yt <- arima.sim(model=list(ar=0.1), n=200)
Zt <- arima.sim(model=list(ar=0.4), n=200)
Wt <- arima.sim(model=list(ar=0.6), n=200)
Xt <- arima.sim(model=list(ar=0.9), n=200)
#unindo cada processo numa mesma base
data <- cbind(Yt, Zt, Wt, Xt)
#preparando pra plotar os graficos (2 rows and 2 cols)
par(mfrow=c(2,2))
#plotando os graficos dos processos AR
for(i in 1:4){plot(data[,i], xlab='', ylab='')}


##Metodologia Box-Jenkins

#A modelagem Box-Jenkins se divide basicamente na:
#identificação
#estimação
#diagnóstico
#previsão

##Principais pacotes utilizados
#graficos mais complexos
library(ggplot2)
#recurso extra de um grafico gerado por ggplot2
library(ggthemes)
#métodos pra exibir e analisar previsões de séries univariadas
library(forecast)
#recurso extra do ggplot2(plot de varios graficos)
library(easyGgplot2)

#importando a serie da divida bruta do Brasil (2006 a 2016)
dbgg<-ts(read.csv2('arquivo.csv',header=T,sep=';',dec=',')[,2],
           start=c(2006,12),freq=12)
#explorando a nossa serie
head(dbgg,1)
tail(dbgg)

#grafico simples
autoplot(dbgg)
autoplot(dbgg)+geom_line(size=.8, colour='darkred')

#testando a estacion(teste ADF)
library(urca)
dbgg_test <- ur.df(y=diff(dbgg), type = "trend", lags=10, selectlags = "AIC")
summary(dbgg_test)

#diferenciando a nossa serie que é I(1)
dif.dbgg <- diff(dbgg)

#FAC e FACP da serie diferenciada
g1 <- ggAcf(dif.dbgg)
g2 <- ggPacf(dif.dbgg)
ggplot2.multiplot(g1, g2, cols=2)
ggtsdisplay(dif.dbgg)

#Modelo Arima
modelo01 <- Arima(dbgg, order=c(1,1,3))

#comparando a serie original com o modelo estimado
debt <- ts.intersect(dbgg, fitted(modelo01))
colnames(debt) <- c('Série Original', 'Modelo ARIMA')
autoplot(debt)+theme(legend.position="bottom")+labs(colour='')+
  xlab('')+ylab('')

#Plotando o residuo do modelo
autoplot(resid(modelo01))

#Testes com os residuos
#teste de autocorrelacao
Box.test(resid(modelo01), fitdf=1)
#teste de normalidade
library(tseries)
jarque.bera.test(resid(modelo01))

#Previsao
#Forecast
fauto <- forecast(modelo01, h=1, level=c(50, 75, 95))
#h é o num de periodos
#level sao os intervalos de previsao
forecast(fauto, h=2) #fazendo a previsao p/18 meses

#plotando a previsao
g1 <- autoplot(fauto)+
  ggtitle('Forecast do Auto Arima')+
  theme_economist() #pacote ggthemes

ggplot2.multiplot(g1, cols=1)

ggplot2.multiplot(g1)



#

