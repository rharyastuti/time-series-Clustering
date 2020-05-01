# ===========================================================
# Download stock from Yahoo Finance 
# ===========================================================
library(quantmod)
symbols <- c('ACES', 'MNCN.JK', 'ANTM', 'ELSA.JK', 'PTBA.JK', 'PGAS.JK', 'JSMR.JK',
             'EXCL.JK', 'TLKM.JK', 'GIAA.JK', 'INDF.JK', 'KAEF.JK', 'KLBF.JK',
             'ADHI.JK', 'ASRI.JK')
start <- as.Date("2018-08-01")
until <- as.Date("2019-01-31")

# Grab data, selecting only the Adjusted close price.
stocks <- lapply(symbols, function(symbol) {
  adjusted <- getSymbols(symbol, from = start, to = until, auto.assign = FALSE)[, 6]
  names(adjusted) <- symbol
  adjusted
}
)

# ===========================================================
# Save as xts format
# ===========================================================
stocks <- do.call(merge.xts, stocks)
View(stocks)
class(stocks) #"xts" "zoo"

# ===========================================================
# Save as csv format 
# ===========================================================
write.csv(as.data.frame(stocks),
          "D:/RH/2 AKADEMIK S2/Semester 2/STK691 Kapita Selekta Statistika Terapan/Tugas UTS/saham.csv")

# ===========================================================
# Replace NA value
# ===========================================================
stocks[,1] <- replace(stocks[,1], is.na(stocks[,1]), mean(stocks[,1], na.rm = T))
stocks[,2] <- replace(stocks[,2], is.na(stocks[,2]), mean(stocks[,2], na.rm = T))
stocks[,3] <- replace(stocks[,3], is.na(stocks[,3]), mean(stocks[,3], na.rm = T))
is.na(stocks[,4])
is.na(stocks[,5])
is.na(stocks[,6])
is.na(stocks[,7])
is.na(stocks[,8])
is.na(stocks[,9])
is.na(stocks[,10])
is.na(stocks[,11])
is.na(stocks[,12])
is.na(stocks[,13])
is.na(stocks[,14])
is.na(stocks[,15])
View(stocks)

# ===========================================================
# Explore the stocks 
# ===========================================================
# Stack the stocks
library(zoo)
temp = data.frame(index(stocks), stack(as.data.frame(coredata(stocks))))
View(temp)
colnames(temp) <- c("date", "value", "symbol")
class(temp) #"data.frame"

write.csv(temp, "D:/RH/2 AKADEMIK S2/Semester 2/STK691 Kapita Selekta Statistika Terapan/Tugas UTS/stacking.csv")

# Plotting stocks
library(ggplot2)
library(reshape2)
jpeg(paste0("D:/RH/2 AKADEMIK S2/Semester 2/STK691 Kapita Selekta Statistika Terapan/Tugas UTS/"
            , "plot_saham.jpg"))
plot.new()
ggplot(temp, aes(x=date, y=value)) + 
  geom_line() +
  facet_wrap(~symbol, scales = "free_y", nrow = 5, ncol = 4) +
  theme(legend.position="none",
        axis.text.x = element_text(color = "black", size = 7),
        axis.text.y = element_text(color = "black", size = 7),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  title(main = "Saham Terindeks Sektoral Industri", cex.main=1)
dev.off()

boxplot(as.matrix(stocks), cex.axis=0.75,
        main="Saham Terindeks Sektoral Industri")

# ===========================================================
# Transformasi Ln
# ===========================================================
lnstocks <- log(stocks)
write.csv(lnstocks,
          "D:/RH/2 AKADEMIK S2/Semester 2/STK691 Kapita Selekta Statistika Terapan/Tugas UTS/lnstocks.csv")
View(lnstocks)

# Stack lnstocks
lntemp = data.frame(index(lnstocks), stack(as.data.frame(coredata(lnstocks))))
View(lntemp)
colnames(lntemp) <- c("date", "value", "symbol")

# Plotting stocks yang telah ditansformasi ln
plot.new()
ggplot(lntemp, aes(x=date, y=value)) + 
  geom_line() +
  facet_wrap(~symbol, scales = "free_y", nrow = 5, ncol = 4) +
  theme(legend.position="none",
        axis.text.x = element_text(color = "black", size = 7),
        axis.text.y = element_text(color = "black", size = 7),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  title(main = "Saham Terindeks Sektoral Industri", cex.main=1)

boxplot(as.matrix(lnstocks), cex.axis=0.75,
        main="Ln(Saham Terindeks Sektoral Industri)")

# ===========================================================
# Training and Testing
# ===========================================================
trainstocks <- as.data.frame(lnstocks)[which(index(lnstocks)=="2018-08-01"):which(index(lnstocks)=="2018-12-31"),]
View(trainstocks)
teststocks <- as.data.frame(lnstocks)[which(index(lnstocks)=="2019-01-01"):which(index(lnstocks)=="2019-01-31"),]
View(teststocks)

# ===========================================================
# Check Stationarity (training data)
# ===========================================================
library(tseries)
adf.test(trainstocks[,1]) #p-value = 0.4497 #tak stasioner
adf.test(trainstocks[,2]) #p-value = 0.3726 #tak stasioner
adf.test(trainstocks[,3]) #p-value = 0.4993 #tak stasioner
adf.test(trainstocks[,4]) #p-value = 0.1866 #tak stasioner
adf.test(trainstocks[,5]) #p-value = 0.08512 #tak stasioner
adf.test(trainstocks[,6]) #p-value = 0.2062 #tak stasioner
adf.test(trainstocks[,7]) #p-value = 0.3942 #tak stasioner
adf.test(trainstocks[,8]) #p-value = 0.4205 #tak stasioner
adf.test(trainstocks[,9]) #p-value = 0.2708 #tak stasioner
adf.test(trainstocks[,10]) #p-value = 0.99 #tak stasioner
adf.test(trainstocks[,11]) #p-value = 0.957 #tak stasioner
adf.test(trainstocks[,12]) #p-value = 0.01 #stasioner
adf.test(trainstocks[,13]) #p-value = 0.1938 #tak stasioner
adf.test(trainstocks[,14]) #p-value = 0.6492 #tak stasioner
adf.test(trainstocks[,15]) #p-value = 0.55 #tak stasioner

# ===========================================================
# Identifikasi Model ARIMA Individu
# ===========================================================
library(forecast)
library(TSA)
library(TTR)

#ACES
acf(diff(trainstocks[,1], differences = 1), lag.max = 20) #(0,1,1)
pacf(diff(trainstocks[,1], differences = 1), lag.max = 20) #(1,1,0) (2,1,0)
eacf(diff(trainstocks[,1], differences = 1)) #(2,1,1) 

#MNCN.JK
acf(diff(trainstocks[,2], differences = 1), lag.max = 20) #(0,1,4)
pacf(diff(trainstocks[,2], differences = 1), lag.max = 20) #(4,1,0)
eacf(diff(trainstocks[,2], differences = 1)) #(2,1,1) (3,1,1)

#'ANTM'
acf(diff(trainstocks[,3], differences = 1), lag.max = 20) #(0,1,1)
pacf(diff(trainstocks[,3], differences = 1), lag.max = 20) #(1,1,0)
eacf(diff(trainstocks[,3], differences = 1))

#'ELSA.JK' 
acf(diff(trainstocks[,4], differences = 1), lag.max = 20) 
pacf(diff(trainstocks[,4], differences = 1), lag.max = 20)
eacf(diff(trainstocks[,4], differences = 1)) #(1,1,1) 

#'PTBA.JK' 
acf(diff(trainstocks[,5], differences = 1), lag.max = 20) 
pacf(diff(trainstocks[,5], differences = 1), lag.max = 20) 
eacf(diff(trainstocks[,5], differences = 1)) #(1,1,1) (2,1,1)

#'PGAS.JK'
acf(diff(trainstocks[,6], differences = 1), lag.max = 20) #(0,1,5)
pacf(diff(trainstocks[,6], differences = 1), lag.max = 20) #(4,1,0)
eacf(diff(trainstocks[,6], differences = 1))

#'JSMR.JK'
acf(diff(trainstocks[,7], differences = 1), lag.max = 20) 
pacf(diff(trainstocks[,7], differences = 1), lag.max = 20) #(1,1,0)
eacf(diff(trainstocks[,7], differences = 1)) #(1,1,1) (3,1,1)

#'EXCL.JK'
acf(diff(trainstocks[,8], differences = 1), lag.max = 20) #(0,1,5)
pacf(diff(trainstocks[,8], differences = 1), lag.max = 20) #(5,1,0)
eacf(diff(trainstocks[,8], differences = 1)) #(1,1,1)

#'TLKM.JK'
acf(diff(trainstocks[,9], differences = 1), lag.max = 20)  #(0,1,1)
pacf(diff(trainstocks[,9], differences = 1), lag.max = 20) #(1,1,0)
eacf(diff(trainstocks[,9], differences = 1)) #(0,1,2) (1,1,1)

#'GIAA.JK'
acf(diff(trainstocks[,10], differences = 1), lag.max = 20)  
pacf(diff(trainstocks[,10], differences = 1), lag.max = 20) 
eacf(diff(trainstocks[,10], differences = 1)) #(1,1,1) 

#'INDF.JK' 
acf(diff(trainstocks[,11], differences = 1), lag.max = 20)  
pacf(diff(trainstocks[,11], differences = 1), lag.max = 20) 
eacf(diff(trainstocks[,11], differences = 1)) #(1,1,1) (2,1,1)

#'KAEF.JK' 
acf(trainstocks[,12], lag.max = 20)  
pacf(trainstocks[,12], lag.max = 20) #(1,0,0)
eacf(trainstocks[,12]) 

#'KLBF.JK'
acf(diff(trainstocks[,13], differences = 1), lag.max = 20)  
pacf(diff(trainstocks[,13], differences = 1), lag.max = 20) 
eacf(diff(trainstocks[,13], differences = 1)) #(1,1,1) 

#'ADHI.JK' 
acf(diff(trainstocks[,14], differences = 1), lag.max = 20) #(0,1,1)
pacf(diff(trainstocks[,14], differences = 1), lag.max = 20) #(1,1,0)
eacf(diff(trainstocks[,14], differences = 1)) #(1,1,1) (2,1,1)

#'ASRI.JK'
acf(diff(trainstocks[,15], differences = 1), lag.max = 20)  
pacf(diff(trainstocks[,15], differences = 1), lag.max = 20) 
eacf(diff(trainstocks[,15], differences = 1)) #(1,1,1) 

# ===========================================================
# Pemilihan Model Terbaik untuk tiap saham
# ===========================================================
#ACES
arima(trainstocks[,1], order = c(0,1,1), method = "ML") #aic = -532.32 #LL = 267.16
arima(trainstocks[,1], order = c(1,1,0), method = "ML") #aic = -529.01
arima(trainstocks[,1], order = c(2,1,0), method = "ML") #aic = -530.67
arima(trainstocks[,1], order = c(2,1,1), method = "ML") #aic = -528.86 #LL = 267.43

#MNCN.JK
arima(trainstocks[,2], order = c(0,1,4), method = "ML") #aic = -520.92
arima(trainstocks[,2], order = c(4,1,0), method = "ML") #aic = -521.33
arima(trainstocks[,2], order = c(2,1,1), method = "ML") #aic = -519.35
arima(trainstocks[,2], order = c(3,1,1), method = "ML") #aic = -518.98

#ANTM
arima(trainstocks[,3], order = c(0,1,1), method = "ML") #aic = -541.84
arima(trainstocks[,3], order = c(1,1,0), method = "ML") #aic = -541.5

#ELSA.JK
arima(trainstocks[,4], order = c(1,1,1), method = "ML") #aic = -479.63

#PTBA.JK
arima(trainstocks[,5], order = c(1,1,1), method = "ML") #aic = -452.07
arima(trainstocks[,5], order = c(2,1,1), method = "ML") #aic = -446.9

#PGAS.JK
arima(trainstocks[,6], order = c(0,1,5), method = "ML") #aic = -456.5
arima(trainstocks[,6], order = c(4,1,0), method = "ML") #aic = -457.16

#JSMR.JK
arima(trainstocks[,7], order = c(1,1,1), method = "ML") #aic = -545.94
arima(trainstocks[,7], order = c(1,1,0), method = "ML") #aic = -546.3
arima(trainstocks[,7], order = c(3,1,1), method = "ML") #aic = -542.28

#EXCL.JK
arima(trainstocks[,8], order = c(0,1,5), method = "ML") #aic = -473.83
arima(trainstocks[,8], order = c(5,1,0), method = "ML") #aic = -438.14
arima(trainstocks[,8], order = c(1,1,1), method = "ML") #aic = -438.03

#TLKM.JK
arima(trainstocks[,9], order = c(0,1,1), method = "ML") #aic = -536.09
arima(trainstocks[,9], order = c(1,1,0), method = "ML") #aic = -536.09
arima(trainstocks[,9], order = c(0,1,2), method = "ML") #aic = -541.67
arima(trainstocks[,9], order = c(1,1,1), method = "ML") #aic = -534.09

#GIAA.JK
arima(trainstocks[,10], order = c(1,1,1), method = "ML") #aic = -442.75

#INDF.JK
arima(trainstocks[,11], order = c(1,1,1), method = "ML") #aic = -528.17
arima(trainstocks[,11], order = c(2,1,1), method = "ML") #aic = -527.61

#KAEF.JK
arima(trainstocks[,12], order = c(1,0,0), method = "ML") #aic = -524.74

#KLBF.JK
arima(trainstocks[,13], order = c(1,1,1), method = "ML") #aic = -499.44

#ADHI.JK
arima(trainstocks[,14], order = c(0,1,1), method = "ML") #aic = -515.18
arima(trainstocks[,14], order = c(1,1,0), method = "ML") #aic = -515.04
arima(trainstocks[,14], order = c(1,1,1), method = "ML") #aic = -513.21
arima(trainstocks[,14], order = c(2,1,1), method = "ML") #aic = -511.32

#ASRI.JK
arima(trainstocks[,15], order = c(1,1,1), method = "ML") #aic = -512.93

# ===========================================================
# Clustering Times Series
# ===========================================================
# Convert from xts object to a matrix (since xts not supported as input for TSclust)
# Also need to transpose because diss() expects data to be along rows.
stocks1 <- t(as.matrix(lnstocks))

# ===========================================================
# Clustering dengan Package "TSclust"
# ===========================================================
# diss = CORRELATION
library(TSclust)
d1 <- diss(stocks1, "COR")
summary(d1)
sort(rowMeans(as.matrix(d1)))
c1 <- hclust(d1, method = "average")
plot(c1)
rect.hclust(c1, k=5)

# diss = FRECHET
d2 <- diss(stocks1, "FRECHET")
summary(d2)
sort(rowMeans(as.matrix(d2)))
c2 <- hclust(d2)
plot(c2)
rect.hclust(c2, k=5)

# diss = DTWARP
d3 <- diss(stocks1, "DTWARP")
summary(d3)
sort(rowMeans(as.matrix(d3)))
c3 <- hclust(d3)
plot(c3)
rect.hclust(c3, k=5)

# ===========================================================
# Modelling (ARIMA) per Cluster
# ===========================================================
## Setelah didapatkan gerombol saham :
#1. hitung rata-rata (seluruh) saham di tiap gerombol (dengan excel)
#2. masukkan ke R sebanyak k kolom (k=banyak gerombol) yg merupakan rata-rata
#3. buat model ARIMA untuk tiap gerombol dg data rataan tadi

# Gerombol Berdasarkan "COR"
gcor <- read.csv("D:/RH/2 AKADEMIK S2/Semester 2/STK691 Kapita Selekta Statistika Terapan/Tugas UTS/gcor.csv",
                header = T)
traingcor <- gcor[1:109,]
testgcor <- gcor[110:nrow(gcor),]

# Gerombol Berdasarkan "DTW"
gdtw <- read.csv("D:/RH/2 AKADEMIK S2/Semester 2/STK691 Kapita Selekta Statistika Terapan/Tugas UTS/gdtw.csv",
                 header = T)
traingdtw <- gdtw[1:109,]
testgdtw <- gdtw[110:nrow(gdtw),]

# Plot time series
plot.new()
par(mfrow=c(3,2))
plot.ts(ts(traingcor[,1]), ylab="ln(saham gerombol 1)",
        main="Gerombol 1 (Cor)", cex.main=1)
plot.ts(ts(traingcor[,2]), ylab="ln(saham gerombol 2)",
        main="Gerombol 2 (Cor)", cex.main=1)
plot.ts(ts(traingcor[,3]), ylab="ln(saham gerombol 3)",
        main="Gerombol 3 (Cor)", cex.main=1)
plot.ts(ts(traingcor[,4]), ylab="ln(saham gerombol 4)",
        main="Gerombol 4 (Cor)", cex.main=1)
plot.ts(ts(traingcor[,5]), ylab="ln(saham gerombol 5)",
        main="Gerombol 5 (Cor)", cex.main=1)

plot.new()
par(mfrow=c(3,2))
plot.ts(ts(traingdtw[,1]), ylab="ln(saham gerombol 1)",
        main="Gerombol 1 (Frechet & DTWarp)", cex.main=1)
plot.ts(ts(traingdtw[,2]), ylab="ln(saham gerombol 2)",
        main="Gerombol 2 (Frechet & DTWarp)", cex.main=1)
plot.ts(ts(traingdtw[,3]), ylab="ln(saham gerombol 3)",
        main="Gerombol 3 (Frechet & DTWarp)", cex.main=1)
plot.ts(ts(traingdtw[,4]), ylab="ln(saham gerombol 4)",
        main="Gerombol 4 (Frechet & DTWarp)", cex.main=1)
plot.ts(ts(traingdtw[,5]), ylab="ln(saham gerombol 5)",
        main="Gerombol 5 (Frechet & DTWarp)", cex.main=1)

# Check Stationarity
adf.test(ts(traingcor[,1])) #p-value = 0.01
adf.test(ts(traingcor[,2])) #p-value = 0.4993 #tidak stasioner
adf.test(ts(traingcor[,3])) #p-value = 0.01122
adf.test(ts(traingcor[,4])) #p-value = 0.8889 #tidak stasioner
adf.test(ts(traingcor[,5])) #p-value = 0.07261 #tidak stasioner

adf.test(ts(traingdtw[,1])) #p-value = 0.4497 #tidak stasioner
adf.test(ts(traingdtw[,2])) #p-value = 0.3665 #tidak stasioner
adf.test(ts(traingdtw[,3])) #p-value = 0.03374
adf.test(ts(traingdtw[,4])) #p-value = 0.3886 #tidak stasioner
adf.test(ts(traingdtw[,5])) #p-value = 0.4217 #tidak stasioner

# ===========================================================
# Identifikasi Model ARIMA Per Cluster
# ===========================================================
# method = 'COR'
acf(ts(traingcor[,1]), lag.max = 20) 
pacf(ts(traingcor[,1]), lag.max = 20) #1,0,0
eacf(ts(traingcor[,1]))

acf(diff(ts(traingcor[,2]), differences = 1), lag.max = 20) #(0,1,1)
pacf(diff(ts(traingcor[,2]), differences = 1), lag.max = 20) #(1,1,0)
eacf(diff(ts(traingcor[,2]), differences = 1)) 

acf(ts(traingcor[,3]), lag.max = 20)
pacf(ts(traingcor[,3]), lag.max = 20) #(1,1,0) 
eacf(ts(traingcor[,3])) 

acf(diff(ts(traingcor[,4]), differences = 1), lag.max = 20)
pacf(diff(ts(traingcor[,4]), differences = 1), lag.max = 20) 
eacf(diff(ts(traingcor[,4]), differences = 1)) #(1,1,1) (3,1,1)

acf(diff(ts(traingcor[,5]), differences = 1), lag.max = 20) #(0,1,5)
pacf(diff(ts(traingcor[,5]), differences = 1), lag.max = 20) #(5,1,0)
eacf(diff(ts(traingcor[,5]), differences = 1))

# method = 'DTW'
acf(diff(ts(traingdtw[,1]), differences = 1), lag.max = 20) #(0,1,1)
pacf(diff(ts(traingdtw[,1]), differences = 1), lag.max = 20) #(1,1,0) (2,1,0)
eacf(diff(ts(traingdtw[,1]), differences = 1)) #(2,1,1)

acf(diff(ts(traingdtw[,2]), differences = 1), lag.max = 20) #(0,1,5)
pacf(diff(ts(traingdtw[,2]), differences = 1), lag.max = 20) #(5,1,0)
eacf(diff(ts(traingdtw[,2]), differences = 1)) 

acf(ts(traingdtw[,3]), lag.max = 20)
pacf(ts(traingdtw[,3]), lag.max = 20) #(1,0,0) 
eacf(ts(traingdtw[,3])) 

acf(diff(ts(traingdtw[,4]), differences = 1), lag.max = 20) 
pacf(diff(ts(traingdtw[,4]), differences = 1), lag.max = 20)
eacf(diff(ts(traingdtw[,4]), differences = 1)) #(1,1,1) (2,1,1)

acf(diff(ts(traingdtw[,5]), differences = 1), lag.max = 20) 
pacf(diff(ts(traingdtw[,5]), differences = 1), lag.max = 20)
eacf(diff(ts(traingdtw[,5]), differences = 1)) #(1,1,1) (3,1,2)

# ===========================================================
# Pemilihan Model Terbaik untuk tiap gerombol
# ===========================================================
# method = 'COR'
arima(traingcor[,1], order = c(1,0,0), method = "ML") #aic = -579.31

arima(traingcor[,2], order = c(0,1,1), method = "ML") #aic = -541.84
arima(traingcor[,2], order = c(1,1,0), method = "ML") #aic = -541.5

arima(traingcor[,3], order = c(1,1,0), method = "ML") #aic = -584.94

arima(traingcor[,4], order = c(1,1,1), method = "ML") #aic = -584.68
arima(traingcor[,4], order = c(3,1,1), method = "ML") #aic = -581.59

arima(traingcor[,5], order = c(0,1,5), method = "ML") #aic = -527.94
arima(traingcor[,5], order = c(5,1,0), method = "ML") #aic = -526.09

# method = 'DTW'
arima(traingdtw[,1], order = c(0,1,1), method = "ML") #aic = -532.32
arima(traingdtw[,1], order = c(1,1,0), method = "ML") #aic = -529.01
arima(traingdtw[,1], order = c(2,1,0), method = "ML") #aic = -530.67

arima(traingdtw[,2], order = c(0,1,5), method = "ML") #aic = -542.87
arima(traingdtw[,2], order = c(5,1,0), method = "ML") #aic = -544.11

arima(traingdtw[,3], order = c(1,0,0), method = "ML") #aic = -610.02

arima(traingdtw[,4], order = c(1,1,0), method = "ML") #aic = -595.14
arima(traingdtw[,4], order = c(2,1,0), method = "ML") #aic = -593.49

arima(traingdtw[,5], order = c(1,1,1), method = "ML") #aic = -594.98
arima(traingdtw[,5], order = c(3,1,2), method = "ML") #aic = -591.55

# ===========================================================
# Peramalan dengan Model secara Individu 
# ===========================================================
fi1 <- forecast(trainstocks[,1],
                model = arima(trainstocks[,1], order = c(0,1,1), method = "ML"),
                h = nrow(teststocks))

fi2 <- forecast(trainstocks[,2],
                model = arima(trainstocks[,2], order = c(4,1,0), method = "ML"),
                h = nrow(teststocks))

fi3 <- forecast(trainstocks[,3],
                model = arima(trainstocks[,3], order = c(0,1,1), method = "ML"),
                h = nrow(teststocks))

fi4 <- forecast(trainstocks[,4],
                model = arima(trainstocks[,4], order = c(1,1,1), method = "ML"),
                h = nrow(teststocks))

fi5 <- forecast(trainstocks[,5],
                model = arima(trainstocks[,5], order = c(1,1,1), method = "ML"),
                h = nrow(teststocks))

fi6 <- forecast(trainstocks[,6],
                model = arima(trainstocks[,6], order = c(4,1,0), method = "ML"),
                h = nrow(teststocks))

fi7 <- forecast(trainstocks[,7],
                model = arima(trainstocks[,7], order = c(1,1,0), method = "ML"),
                h = nrow(teststocks))

fi8 <- forecast(trainstocks[,8],
                model = arima(trainstocks[,8], order = c(0,1,5), method = "ML"),
                h = nrow(teststocks))

fi9 <- forecast(trainstocks[,9],
                model = arima(trainstocks[,9], order = c(0,1,2), method = "ML"),
                h = nrow(teststocks))

fi10 <- forecast(trainstocks[,10],
                 model = arima(trainstocks[,10], order = c(1,1,1), method = "ML"),
                 h = nrow(teststocks))

fi11 <- forecast(trainstocks[,11],
                 model = arima(trainstocks[,11], order = c(1,1,1), method = "ML"),
                 h = nrow(teststocks))

fi12 <- forecast(trainstocks[,12],
                 model = arima(trainstocks[,12], order = c(1,0,0), method = "ML"),
                 h = nrow(teststocks))

fi13 <- forecast(trainstocks[,13],
                 model = arima(trainstocks[,13], order = c(1,1,1), method = "ML"),
                 h = nrow(teststocks))

fi14 <- forecast(trainstocks[,14],
                 model = arima(trainstocks[,14], order = c(0,1,1), method = "ML"),
                 h = nrow(teststocks))

fi15 <- forecast(trainstocks[,15],
                 model = arima(trainstocks[,15], order = c(1,1,1), method = "ML"),
                 h = nrow(teststocks))

# ===========================================================
# Peramalan dengan Model secara Gerombol
# ===========================================================
fg1 <- forecast(trainstocks[,1],
                model = arima(trainstocks[,1], order = c(1,0,0), method = "ML"),
                h = nrow(teststocks))

fg2 <- forecast(trainstocks[,2],
                model = arima(trainstocks[,2], order = c(0,1,5), method = "ML"),
                h = nrow(teststocks))

fg3 <- forecast(trainstocks[,3],
                model = arima(trainstocks[,3], order = c(1,1,1), method = "ML"),
                h = nrow(teststocks))

fg4 <- forecast(trainstocks[,4],
                model = arima(trainstocks[,4], order = c(1,1,1), method = "ML"),
                h = nrow(teststocks))

fg5 <- forecast(trainstocks[,5],
                model = arima(trainstocks[,5], order = c(1,1,0), method = "ML"),
                h = nrow(teststocks))

fg6 <- forecast(trainstocks[,6],
                model = arima(trainstocks[,6], order = c(0,1,1), method = "ML"),
                h = nrow(teststocks))

fg7 <- forecast(trainstocks[,7],
                model = arima(trainstocks[,7], order = c(1,1,0), method = "ML"),
                h = nrow(teststocks))

fg8 <- forecast(trainstocks[,8],
                model = arima(trainstocks[,8], order = c(0,1,1), method = "ML"),
                h = nrow(teststocks))

fg9 <- forecast(trainstocks[,9],
                model = arima(trainstocks[,9], order = c(1,1,0), method = "ML"),
                h = nrow(teststocks))

fg10 <- forecast(trainstocks[,10],
                 model = arima(trainstocks[,10], order = c(1,1,1), method = "ML"),
                 h = nrow(teststocks))

fg11 <- forecast(trainstocks[,11],
                 model = arima(trainstocks[,11], order = c(1,1,0), method = "ML"),
                 h = nrow(teststocks))

fg12 <- forecast(trainstocks[,12],
                 model = arima(trainstocks[,12], order = c(0,1,1), method = "ML"),
                 h = nrow(teststocks))

fg13 <- forecast(trainstocks[,13],
                 model = arima(trainstocks[,13], order = c(0,1,5), method = "ML"),
                 h = nrow(teststocks))

fg14 <- forecast(trainstocks[,14],
                 model = arima(trainstocks[,14], order = c(0,1,5), method = "ML"),
                 h = nrow(teststocks))

fg15 <- forecast(trainstocks[,15],
                 model = arima(trainstocks[,15], order = c(1,1,1), method = "ML"),
                 h = nrow(teststocks))

# ===========================================================
# Validasi Model
# ===========================================================
# Forecast Error
ei1 <- teststocks[,1] - as.data.frame(fi1)[,1]
ei2 <- teststocks[,2] - as.data.frame(fi2)[,1]
ei3 <- teststocks[,3] - as.data.frame(fi3)[,1]
ei4 <- teststocks[,4] - as.data.frame(fi4)[,1]
ei5 <- teststocks[,5] - as.data.frame(fi5)[,1]
ei6 <- teststocks[,6] - as.data.frame(fi6)[,1]
ei7 <- teststocks[,7] - as.data.frame(fi7)[,1]
ei8 <- teststocks[,8] - as.data.frame(fi8)[,1]
ei9 <- teststocks[,9] - as.data.frame(fi9)[,1]
ei10 <- teststocks[,10] - as.data.frame(fi10)[,1]
ei11 <- teststocks[,11] - as.data.frame(fi11)[,1]
ei12 <- teststocks[,12] - as.data.frame(fi12)[,1]
ei13 <- teststocks[,13] - as.data.frame(fi13)[,1]
ei14 <- teststocks[,14] - as.data.frame(fi14)[,1]
ei15 <- teststocks[,15] - as.data.frame(fi15)[,1]

eg1 <- teststocks[,1] - as.data.frame(fg1)[,1]
eg2 <- teststocks[,2] - as.data.frame(fg2)[,1]
eg3 <- teststocks[,3] - as.data.frame(fg3)[,1]
eg4 <- teststocks[,4] - as.data.frame(fg4)[,1]
eg5 <- teststocks[,5] - as.data.frame(fg5)[,1]
eg6 <- teststocks[,6] - as.data.frame(fg6)[,1]
eg7 <- teststocks[,7] - as.data.frame(fg7)[,1]
eg8 <- teststocks[,8] - as.data.frame(fg8)[,1]
eg9 <- teststocks[,9] - as.data.frame(fg9)[,1]
eg10 <- teststocks[,10] - as.data.frame(fg10)[,1]
eg11 <- teststocks[,11] - as.data.frame(fg11)[,1]
eg12 <- teststocks[,12] - as.data.frame(fg12)[,1]
eg13 <- teststocks[,13] - as.data.frame(fg13)[,1]
eg14 <- teststocks[,14] - as.data.frame(fg14)[,1]
eg15 <- teststocks[,15] - as.data.frame(fg15)[,1]

#MAPE
mapei1 <- mean(abs(ei1)/teststocks[,1])*100 
mapei2 <- mean(abs(ei2)/teststocks[,2])*100 
mapei3 <- mean(abs(ei3)/teststocks[,3])*100 
mapei4 <- mean(abs(ei4)/teststocks[,4])*100 
mapei5 <- mean(abs(ei5)/teststocks[,5])*100 
mapei6 <- mean(abs(ei6)/teststocks[,6])*100 
mapei7 <- mean(abs(ei7)/teststocks[,7])*100 
mapei8 <- mean(abs(ei8)/teststocks[,8])*100 
mapei9 <- mean(abs(ei9)/teststocks[,9])*100 
mapei10 <- mean(abs(ei10)/teststocks[,10])*100 
mapei11 <- mean(abs(ei11)/teststocks[,11])*100 
mapei12 <- mean(abs(ei12)/teststocks[,12])*100 
mapei13 <- mean(abs(ei13)/teststocks[,13])*100 
mapei14 <- mean(abs(ei14)/teststocks[,14])*100 
mapei15 <- mean(abs(ei15)/teststocks[,15])*100 

mapeg1 <- mean(abs(eg1)/teststocks[,1])*100 
mapeg2 <- mean(abs(eg2)/teststocks[,2])*100 
mapeg3 <- mean(abs(eg3)/teststocks[,3])*100 
mapeg4 <- mean(abs(eg4)/teststocks[,4])*100 
mapeg5 <- mean(abs(eg5)/teststocks[,5])*100 
mapeg6 <- mean(abs(eg6)/teststocks[,6])*100 
mapeg7 <- mean(abs(eg7)/teststocks[,7])*100 
mapeg8 <- mean(abs(eg8)/teststocks[,8])*100 
mapeg9 <- mean(abs(eg9)/teststocks[,9])*100 
mapeg10 <- mean(abs(eg10)/teststocks[,10])*100 
mapeg11 <- mean(abs(eg11)/teststocks[,11])*100 
mapeg12 <- mean(abs(eg12)/teststocks[,12])*100 
mapeg13 <- mean(abs(eg13)/teststocks[,13])*100 
mapeg14 <- mean(abs(eg14)/teststocks[,14])*100 
mapeg15 <- mean(abs(eg15)/teststocks[,15])*100 

rbind(mapei1,
      mapei2,
      mapei3,
      mapei4,
      mapei5,
      mapei6,
      mapei7,
      mapei8,
      mapei9,
      mapei10,
      mapei11,
      mapei12,
      mapei13,
      mapei14,
      mapei15)

rbind(mapeg1,
      mapeg2,
      mapeg3,
      mapeg4,
      mapeg5,
      mapeg6,
      mapeg7,
      mapeg8,
      mapeg9,
      mapeg10,
      mapeg11,
      mapeg12,
      mapeg13,
      mapeg14,
      mapeg15)

