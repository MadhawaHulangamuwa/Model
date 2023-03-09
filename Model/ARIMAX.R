library(readxl)
library(ggplot2)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)
library(dplyr)
library(forecast)

fulldata <- read_excel("C:/Users/Madhawa Hulangamuwa/Desktop/Prediction_Models/New.xlsx")
summary(fulldata)
View(fulldata)
str(fulldata)

data <- ts(fulldata, start = 1980, frequency = 1)

# Check for white noise
Box.test(fulldata$FD_FD_IX, lag = 24,fitdf = 0, type = "Lj")
Box.test(fulldata$FD_FM_IX, lag = 24,fitdf = 0, type = "Lj")
Box.test(fulldata$FD_FI_IX, lag = 24,fitdf = 0, type = "Lj")

# determining optimal number of referencing using forecast package
ndiffs(x=data[,"FD_FMA_IX"])
ndiffs(x=data[,"FD_FME_IX"])
ndiffs(x=data[,"FD_FMD_IX"])
ndiffs(x=data[,"FD_FIA_IX"])
ndiffs(x=data[,"FD_FIE_IX"])
ndiffs(x=data[,"FD_FID_IX"])
ndiffs(x=data[,"FD_FD_IX"])
# ndiffs(x=data)

# Differencing 
data = diff(data,lag=1)

# Checking for autocorrelation

acf(data[,"FD_FMA_IX"])
acf(data[,"FD_FME_IX"])
acf(data[,"FD_FMD_IX"])
acf(data[,"FD_FIA_IX"])
acf(data[,"FD_FIE_IX"])
acf(data[,"FD_FID_IX"])

pacf(data[,"FD_FMA_IX"])
pacf(data[,"FD_FME_IX"])
pacf(data[,"FD_FMD_IX"])
pacf(data[,"FD_FIA_IX"])
pacf(data[,"FD_FIE_IX"])
pacf(data[,"FD_FID_IX"])


#FD_FMA_IX <- ts(data$FD_FMA_IX, start = c(1980), frequency = 1)
#FD_FI_IX <- ts(data$FD_FI_IX, start = c(1980), frequency = 1)
#FD_FM_IX <- ts(data$FD_FM_IX, start = c(1980), frequency = 1)

ts_plot(data[,"FD_FMA_IX"])
ts_plot(data[,"FD_FME_IX"])
ts_plot(data[,"FD_FMD_IX"])
ts_plot(data[,"FD_FIA_IX"])
ts_plot(data[,"FD_FIE_IX"])
ts_plot(data[,"FD_FID_IX"])

# Checking for Stationarity

adf.test(data[,"FD_FMA_IX"])
adf.test(data[,"FD_FME_IX"])
adf.test(data[,"FD_FMD_IX"])
adf.test(data[,"FD_FIA_IX"])
adf.test(data[,"FD_FIE_IX"])
adf.test(data[,"FD_FID_IX"])

# Model
# Train and Test Data

train <- fulldata[1:34,]
head(train)
test <- fulldata[35:41,]
head(test)
tail(test)


#ts_train <- ts(train, start = c(1980), frequency = 1)
#ts_test <- ts(test, start = c(2014), frequency = 1)



# # xreg_train <- cbind(FD_FMA_IX = train[,"FD_FMA_IX"], 
#                     FD_FME_IX = train[,"FD_FME_IX"],
#                     FD_FMD_IX = train[,"FD_FMD_IX"],
#                     FD_FMA_IX = train[,"FD_FIA_IX"], 
#                     FD_FME_IX = train[,"FD_FIE_IX"],
#                     FD_FMD_IX = train[,"FD_FID_IX"])

xreg_train1 <- cbind(FD_FI_IX = train[,"FD_FI_IX"])
xreg_train2 <- cbind(FD_FM_IX = train[,"FD_FM_IX"])
xreg_train3 <- cbind(FD_FM_IX = train[,"FD_FM_IX"],
                     FD_FI_IX = train[,"FD_FI_IX"])
                   

xreg_test1 <- cbind(FD_FI_IX = train[,"FD_FI_IX"])
xreg_test2 <- cbind(FD_FM_IX = train[,"FD_FM_IX"])
xreg_test3 <- cbind(FD_FM_IX = train[,"FD_FM_IX"],
                    FD_FI_IX = train[,"FD_FI_IX"])
             


fit_ARIMAX <- auto.arima(train[,"FD_FD_IX"],xreg = as.matrix(xreg_train1))
summary(fit_ARIMAX)

checkresiduals(fit_ARIMAX)

forecast_ARIMAX <- forecast(fit_ARIMAX, xreg = as.matrix(xreg_test1))
summary(forecast_ARIMAX)

autoplot(forecast_ARIMAX)+xlab("Year") +ylab("") + ggtitle("Financial Development Index")

p1 <- cbind("Regression Errors" = residuals(fit_ARIMAX,type = "Regression"),"ARIMA Errors" = residuals(fit_ARIMAX,type = "innovation")) %>% autoplot(facets = TRUE)

plot <- plot_ly() %>% add_line(x = as.POSIXct((test$Year, origin="2014"), y = test$FD_FD_IX, color =I("red"), name = "Actual", maker=list(mode = 'lines')) %>%
                      add_line(x = fore., y = test$FD_FD_IX, color =I("red"), name = "Actual", maker=list(mode = 'lines'))

?add_lines
