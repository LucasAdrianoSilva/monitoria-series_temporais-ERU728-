#

######Importacao de dados e analise de tendencia######

####PARTE1) Importacao de dados

#importando a serie
gasoline_df<-read.table("gasoline_df.csv",sep = ";", header = T)[,-1]
gasoline_df <- read.table("gasoline_df.txt", h = T)[,-1]
#
library(readxl)
gasoline_df <- read_excel(file.choose(), sheet = 1)

#serie temporal (ts)
gasoline_ts <- ts(gasoline_df, start=c(2013,01,01), freq=12)  #transformacao em series temporais
#importando diretamente
gasoline_ts<-ts(read.csv2('gasoline_df.csv',header=T,sep=';',dec='.'),
           start=c(2013,01),freq=12)
#

#teste com e sem ts
plot(gasoline_df)
plot(gasoline_ts)
#


####PARTE2) Analise de tendencia

##PARTE2.1) Encontrando a tendencia
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

#comparando modelos com variaveis diferentes
AIC(reg2)+2*sum(log(gasoline_ts)) #modelo exponencial
AIC(reg1) #modelo quadratico

#O melhor modelo é o quadratico
summary(reg1)
qq=gasoline_ts - reg1

#Retirando a tendecia quadratica
t <- time(gasoline_ts)
lin.mod <- lm(gasoline_ts ~ t+I(t^2))
lin.trend <- lin.mod$fitted.values  #valores ajustados referem-se à tendência de tempo
quadr <- ts(lin.trend, start = c(2013, 1), frequency = 12)  #cria uma variável de série temporal para tendência
lin.cycle <- gasoline_ts - quadr
plot(lin.cycle)


#adicionando um componente sazonal
month <- as.factor(cycle(gasoline_ts))
reg1 <- lm(gasoline_ts~t + I(t^2)+I(t^3) + month)
par(mfrow=c(1,1))
plot(gasoline_ts, main = "Time Series with Trend and Season ")
points(t,predict.lm(reg1),type='l',col='red')

#Alguns testes
#testando o ajuste do modelo aos dados
print(paste("r-squared = ", summary(reg1)$r.squared))
#testando a normalidade dos residuos
shapiro.test(reg1$residuals)


##PARTE2.2) Filtrando a serie

#carregando a base (PIB Africa do Sul)
library(devtools)
devtools::install_github("KevinKotze/tsm")
install.packages("mFilter", repos = "https://cran.rstudio.com/",
                 dependencies = TRUE)

library(tsm)
library(mFilter)


dat <- sarb_quarter$KBP6006D
dat.tmp <- log(na.omit(dat))
gdp <- ts(dat.tmp, start = c(1960, 1), frequency = 4)

plot(gdp) #plotar o grafico

#estimando uma tendencia linear
lin.mod <- lm(gdp ~ time(gdp))
lin.trend <- lin.mod$fitted.values  #valores ajustados referem-se à tendência de tempo
linear <- ts(lin.trend, start = c(1960, 1), frequency = 4)  #cria uma variável de série temporal para tendência
lin.cycle <- gdp - linear  #ciclo é a diferença entre os dados e a tendência linear

par(mfrow = c(1, 2), mar = c(2.2, 2.2, 1, 1), cex = 0.8)  #plota dois gráficos lado a lado
plot.ts(gdp, ylab = "")  #plotar a primeira serie
lines(linear, col = "red")  #inclusao da tendencia
legend("topleft", legend = c("data", "trend"), lty = 1,
       col = c("black", "red"), bty = "n")
plot.ts(lin.cycle, ylab = "")  #plotar o ciclo
legend("topright", legend = c("cycle"), lty = 1, col = c("black"),
       bty = "n")


#retirando a tendencia (filtrodeHODRICK)
hp.decom <- hpfilter(gdp, freq = 1600, type = "lambda")

par(mfrow = c(1, 2), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot.ts(gdp, ylab = "")  # plotar as series
lines(hp.decom$trend, col = "red")  # incluir HP trend
legend("topleft", legend = c("data", "HPtrend"), lty = 1,
       col = c("black", "red"), bty = "n")
plot.ts(hp.decom$cycle, ylab = "")  # plotar o ciclo
legend("bottomleft", legend = c("HPcycle"), lty = 1, col = c("black"),
       bty = "n")

#tendencia linear e serie hp
comb <- ts.union(lin.cycle, hp.decom$cycle)

par(mfrow = c(1, 1), mar = c(2.2, 2.2, 2, 1), cex = 0.8)
plot.ts(comb, ylab = "", plot.type = "single", col = c("blue",
                                                       "red"))
legend("topright", legend = c("linear", "hp-filter"), lty = 1, col = c("blue",
                                                                         "red"), bty = "n")

##PARTE2.3) Identificando as diferentes tendencias
set.seed(777)
n <- 100
time <- c(1:n)
x0 <- arima.sim(list(order = c(1, 0, 0), ar = 0.5), n = n, n.start = 100, sd = 0.5)
x1 <- 2*time/n + x0
x2 <- 2*(time/n)^0.5 + x0
x3 <- 0.5*(time - n/2)/n - 6*((time - n/2)/n)^2 + x0
X <- as.data.frame(cbind(x0, x1, x2, x3))

install.packages("funtimes")
library(funtimes)

#testando se nao há tendencia (H0:Nao tem tend, H1:Tem tend linear)
notrend_test(x0)

#testando se há tendencia linear
apply(X[,-1], 2, function(x) notrend_test(x)$p.value)
apply(X, 2, function(x) notrend_test(x)$p.value)

#testando tendencia monotonica
apply(X, 2, function(x) notrend_test(x, test = "MK")$p.value)

#testando qualquer tendencia (H0:Nao tem tend, H1:Tem qualquer tend)
apply(X, 2, function(x) notrend_test(x, test = "WAVK",
                                     factor.length = "adaptive.selection")$p.value)


#testando uma forma parametrica especifica da tendencia
notrend_test(x0, test = "WAVK", factor.length = "adaptive.selection") # WAVK with sieve bootstrap

wavk_test(x0 ~ 0, factor.length = "adaptive.selection") # WAVK with hybrid bootstrap

wavk_test(x0 ~ t, factor.length = "adaptive.selection")

#
apply(X[,-1], 2, function(x) wavk_test(x ~ t, factor.length = "adaptive.selection")$p.value)

#
wavk_test(x3 ~ poly(t, 2), factor.length = "adaptive.selection", out = TRUE)


##2.4.Extra
#estimando uma tendencia linear
x0 <-X[,1]
x1 <-X[,2]
x2 <-X[,3]
x3 <-X[,4]

lin.mod <- lm(x1~ time(x1))
lin.trend <- lin.mod$fitted.values  # fitted values pertain to time trend
linear <- ts(lin.trend)  # create a time series variable for trend
lin.cycle <- x1 - linear  # cycle is the difference between the data and linear trend

par(mfrow = c(1, 2), mar = c(2.2, 2.2, 1, 1), cex = 0.8)  # plot two graphs side by side and squash margins
plot.ts(x1, ylab = "")  # first plot time series
lines(linear, col = "red")  # include lines over the plot
legend("topleft", legend = c("data", "trend"), lty = 1,
       col = c("black", "red"), bty = "n")
plot.ts(lin.cycle, ylab = "")  # second plot for cycle
legend("topright", legend = c("cycle"), lty = 1, col = c("black"),
       bty = "n")


################################################
