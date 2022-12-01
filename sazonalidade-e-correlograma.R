#SAZONALIDADE E CORRELOGRAMA

#IMPORTANDO A BASE
install.packages("fpp")
library(fpp)
data(ausbeer)
timeserie_beer = tail(head(ausbeer, 70),64)

#
#Plotando o gráfico
plot(timeserie_beer)
#
plot(timeserie_beer,main='Beer',
     ylab='Produção', xlab = 'Ano',
     bty='l',col='red',lty=1)
grid(col='darkgrey',
     lwd=2)

#IDENTIFICANDO SE HÁ SAZONALIDADE
#P/isso criar dummies sazonais e rodar a regressao
#numero de dummies depende da periodicidade da serie
Q <- ordered(cycle(timeserie_beer)) #ciclo fornece as posições no ciclo de cada observação
ausbeer.reg <- lm(timeserie_beer~Q)
#obtendo o resultado da regressao
summary(ausbeer.reg)
#Observar o p-valor do teste F, hipótese nula de que não existe sazonalide

#
#Observando os resíduos da regressão,série dessazonalizada e componente sazonal
ausbeer.des <- ts(resid(ausbeer.reg),
                   start = 1956, freq=4)
ausbeer.hat <- ts(fitted(ausbeer.reg),
                   start=1956, freq=4)
par(mfrow=c(1,2))
plot(ausbeer.des)
plot(ausbeer.hat)
##OBS:
#série dessazonalizada começa em níveis negativos
#isso acontece pq ao subtrair o valor estimado da série original
#estamos tbm retirando a sua média

#Então,é indicado normalizar a série adicionando a sua média
ausbeer.desn <- ausbeer.des + mean(fitted(ausbeer.reg))

#Gerando o grafico da serie original e serie dessazonalizada
par(mfrow=c(1,1))
plot(timeserie_beer,
     main='',
     xlab='Ano', ylab='',
     col='blue',
     bty='l')
par(new=TRUE)
plot(ausbeer.desn,
     axes=F, ann=F,
     col='red',lty=2)
legend('topleft',
       c('Produção', 'Produção dessazonalizada'),
       col=c('blue', 'red'), lty=1:2,
       bty='n')
grid(col='darkgrey')

#Na medida que a magnitude aumenta,a sazonalidade permanece constante
#isso é uma indicacao de que o melhor modelo é o aditivo

#Suavizando a série temporal usando a média móvel centrada
#p/encontrar a tendencia
install.packages("forecast")
library(forecast)
trend_beer = ma(timeserie_beer, order = 4, centre = T)
plot(timeserie_beer)
lines(trend_beer)
plot(trend_beer)

#Removendo o valor estimado (a tendencia)
#p/resultar em uma nova série temporal que expõe a sazonalidade
detrend_beer = timeserie_beer - trend_beer
plot(detrend_beer)

#A partir da série temporal sem tendência, calcula-se a sazonalidade média
#somando a sazonalidade e dividindo pelo período de sazonalidade
m_beer = t(matrix(data = detrend_beer, nrow = 4)) #c/o uso de matriz
seasonal_beer = colMeans(m_beer, na.rm = T)
plot(as.ts(rep(seasonal_beer,16)))

#Examinando o ruído aleatorio restante(modelo aditivo)
random_beer = timeserie_beer - trend_beer - seasonal_beer
plot(random_beer)

#Recompondo a serie
recomposed_beer = trend_beer+seasonal_beer+random_beer
plot(recomposed_beer) #serie original de novo

#
#Decompondo a serie em diferentes componentes
ts_beer = ts(timeserie_beer, frequency = 4)
decompose_beer = decompose(ts_beer, "additive")

plot(as.ts(decompose_beer$seasonal))
plot(as.ts(decompose_beer$trend))
plot(as.ts(decompose_beer$random))
plot(decompose_beer)


#

#REPETINDO O PROCEDIMENTO ANTERIOR EM OUTRA BASE
#Base de dados do preço da gasolina

#Importando a serie
gasoline_df<-read.table("gasoline_df.csv",sep = ";", header = T)[,-1]
gasoline_ts <- ts(gasoline_df, start=c(2013,01,01), freq=12)  #transformacao em series temporais

#Plotando gráfico
plot(gasoline_ts)
#
plot(gasoline_ts,main='Gasolina',
     ylab='Preço', xlab = 'Ano',
     bty='l',col='red',lty=1)
grid(col='darkgrey',
     lwd=2)

#
#IDENTIFICANDO A SAZONALIDADE
Q <- ordered(cycle(gasoline_ts))
gasoline.reg <- lm(gasoline_ts~Q)
#
summary(gasoline.reg)

#Observando os resíduos da regressão,série dessazonalizada e componente sazonal
gasoline.des <- ts(resid(gasoline.reg),
              start = 1991, freq=1)
gasoline.hat <- ts(fitted(gasoline.reg),
              start=1991, freq=1)
par(mfrow=c(1,2))
plot(gasoline.des)
plot(gasoline.hat)
#
#Normalizacao
gasoline.desn <- gasoline.des + mean(fitted(gasoline.reg))

#
par(mfrow=c(1,1))
plot(gasoline_ts,
     main='',
     xlab='Ano', ylab='',
     col='blue',
     bty='l')
par(new=TRUE)
plot(gasoline.desn,
     axes=F, ann=F,
     col='red',lty=2)
legend('topleft',
       c('Preço da Gasolina', 'Preço da Gasolina dessazonalizado'),
       col=c('blue', 'red'), lty=1:2,
       bty='n')
grid(col='darkgrey')



#Suavizando a série temporal usando a média móvel centrada
#p/encontrar a tendencia
install.packages("forecast")
library(forecast)
trend_gasoline = ma(gasoline_ts, order = 12, centre = T)
plot(gasoline_ts)
lines(trend_gasoline)
plot(trend_gasoline)

#Considerando o modelo multiplicativo(apenas como suposicao)
#Pq?
#Na medida que a série temporal aumenta em magnitude,a variação sazonal tbm aumenta
detrend_gasoline = gasoline_ts / trend_gasoline
plot(detrend_gasoline)

#A partir da série temporal sem tendência, calcula-se a sazonalidade média
#somando a sazonalidade e dividindo pelo período de sazonalidade
m_air = t(matrix(data = detrend_gasoline, nrow = 12))
seasonal_gasoline = colMeans(m_air, na.rm = T)
plot(as.ts(rep(seasonal_gasoline,12)))

#Examinando o ruído aleatorio restante(modelo multiplicativo)
random_gasoline = gasoline_ts / (trend_gasoline * seasonal_gasoline)
plot(random_gasoline)

#Recompondo a serie
recomposed_gasoline = trend_gasoline*seasonal_gasoline*random_gasoline
plot(recomposed_gasoline)

#Decompondo a serie
decompose_gasoline = decompose(gasoline_ts, "multiplicative")

plot(as.ts(decompose_gasoline$seasonal))
plot(as.ts(decompose_gasoline$trend))
plot(as.ts(decompose_gasoline$random))
plot(decompose_gasoline)


#######################

#CORRELOGRAMA
#função ggAcf() do pacote forecast
ggAcf(ausbeer)
ggAcf(gasoline_ts)
#A linha tracejada azul indica onde(em que período) a autocorrelacao
#é significativamente diferente de zero

#gerando uma serie de tempo aleatoria
aleat <- ts(rnorm(50))
ggAcf(aleat)
