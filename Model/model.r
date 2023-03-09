library(readxl)
library(ggplot2)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)
library(dplyr)
library(forecast)


fulldata2 <- read_excel("C:/Users/Madhawa Hulangamuwa/Desktop/Prediction_Models/Data Sets/INDIA_FDI.xlsx")
str(fulldata2)

fdtime2 = ts(ARIMA_DATA2$FDI, start = c(1980), end = c(2020), frequency = 1)

View(fulldata2)
class(fulldata2)

#### ARIMA MODEL
ARIMA_DATA2 <- data.frame(fulldata2[,9:9])
row.names(ARIMA_DATA2) <- fulldata2$Year

# Check for white noise
Box.test(ARIMA_DATA2$FDI, lag = 24,fitdf = 0, type = "Lj")

# Checking for Stationarity
adf.test(ARIMA_DATA2[,"FDI"])

# Checking for autocorrelation - 
acf(ARIMA_DATA2[,"FDI"],lag.max = 20, main = "India")
pacf(ARIMA_DATA2[,"FDI"],lag.max = 20, main = "India")

# Model

model2 = auto.arima(fdtime2, ic = "aic", trace=TRUE)


# Train and Test Data

train <- ARIMA_DATA[1:34,]
head(train)
test <- ARIMA_DATA[35:41,]
head(test)
tail(test)

