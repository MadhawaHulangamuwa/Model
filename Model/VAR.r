library(readxl)
library(ggplot2)
library(TSstudio)
library(forecast)
library(tidyverse)
library(dplyr)
library(tseries)
library(forecast)
library(bruceR)
library(aTSA)
library(urca)
library(plotly)
library(vars)


fulldata <- read_excel("C:/Users/Madhawa Hulangamuwa/Desktop/Forecasting_Models/Data Sets/SRILANKA_FDI.xlsx")
#dataset <- read_excel(file.choose())
str(fulldata)
View(fulldata)


data1 <- data.frame(fulldata[,3:9])
data2 <- as_tibble(data1)


FDI <- ts(fulldata$FDI, start = "1980", frequency = 1)
FMA <- ts(fulldata$FMA, start = "1980", frequency = 1)
FMD <- ts(fulldata$FMD, start = "1980", frequency = 1)
FIA <- ts(fulldata$FIA, start = "1980", frequency = 1)
FID <- ts(fulldata$FID, start = "1980", frequency = 1)
FME <- ts(fulldata$FME, start = "1980", frequency = 1)
FIE <- ts(fulldata$FIE, start = "1980", frequency = 1)

FDI <- log(FDI)
adf.test(FDI)

ts_plot(FDI2,Xtitle = "Year",Ytitle = "Value", title = "Financial Development Index - FDI", Xgrid = TRUE, Ygrid = TRUE )
ts_plot(FMA, Xtitle = "Year",Ytitle = "Value", title = "Financial Market Accessibility Index - FMA", Xgrid = TRUE, Ygrid = TRUE )
ts_plot(FMD, Xtitle = "Year",Ytitle = "Value", title = "Financial Market Depth Index - FMD", Xgrid = TRUE, Ygrid = TRUE )
ts_plot(FME, Xtitle = "Year",Ytitle = "Value", title = "Financial Market Efficiency Index - FME", Xgrid = TRUE, Ygrid = TRUE )
ts_plot(FIA, Xtitle = "Year",Ytitle = "Value", title = "Financial Institution Accessibility Index - FIA", Xgrid = TRUE, Ygrid = TRUE )
ts_plot(FID, Xtitle = "Year",Ytitle = "Value", title = "Financial Institution Depth Index - FID", Xgrid = TRUE, Ygrid = TRUE )
ts_plot(FIE, Xtitle = "Year",Ytitle = "Value", title = "Financial Institution Efficiency Index - FIE", Xgrid = TRUE, Ygrid = TRUE )

subplot(p1, p2, p3, p4, nrows = 4)


adf.test(FDI) # p-value for time series is greater than 0.05 = non stationary 
FDI2 = diff(FDI, differences = 1)
FDI3 = diff(FDI, differences = 2)
adf.test(FDI2)
adf.test(FDI3)

adf.test(FMD) # p-value for time series is greater than 0.05 = non stationary 
FMD2 = diff(FMD, differences = 1)
FMD3 = diff(FMD, differences = 2)
adf.test(FMD2)
adf.test(FMD3)

adf.test(FME) # p-value for time series is greater than 0.05 = non stationary 
FME2 = diff(FME, differences = 1)
adf.test(FME2)

adf.test(FMA) # p-value for time series is greater than 0.05 = non stationary 
FMA2 = diff(FMA, differences = 1)
adf.test(FMA2)

adf.test(FID) # p-value for time series is greater than 0.05 = non stationary 
FID2 = diff(FID, differences = 1)
adf.test(FID2)

adf.test(FIE) # p-value for time series is greater than 0.05 = non stationary 
FIE2 = diff(FIE, differences = 1)
adf.test(FIE2)

adf.test(FIA) # p-value for time series is greater than 0.05 = non stationary 
FIA2 = diff(FIA, differences = 2)
adf.test(FIA2)


corrplot(cor(data2), type = "upper", method = "number")
cov(data2, method = "spearman")

### Gragner Causality test 

## Null: FMA(independent) does not cuases FDI(dependent), 
##       FMA would be independent variable
##       FDI would be dependent variable

lagselect1 <- VARselect(FDI3, lag.max = 10, type = "const",season = NULL, exogen = NULL)
lagselect1$selection



grangertest(FMD3 ~ FDI3, order = 7) # p value > 0.05 , reject null hypothesis.FDI is useful for predicting the FMA

grangertest(FME2 ~ FDI2, order = 1)
grangertest(FMA2 ~ FDI2, order = 1)
grangertest(FID2 ~ FDI2, order = 1)
grangertest(FIE2 ~ FDI2, order = 1)
grangertest(FIA2 ~ FDI3, order = 1)


### Lag order of FDI

lagselect <- VARselect(FDI2, lag.max = 10, type = "const",season = NULL, exogen = NULL)
lagselect$selection


v_all <- cbind(FDI2,FMD2)
colnames(v_all) <- cbind("FDI","FMD")

lagselect1 <- VARselect(v_all, lag.max = 10, type = "const",season = NULL, exogen = NULL)
lagselect1$selection

model1 <- VAR(FDI2, p = 9, type = "const",season = NULL, exogen = NULL, lag.max = 10,ic = c("AIC", "HQ", "SC", "FPE"))
summary(model1)




v_all <- cbind(FDI2,FMD2,FME2,FMA2,FID2,FIE2,FIA2)
colnames(v_all) <- cbind("FDI","FMD","FME","FMA","FID","FIE","FIA") 
lagselect_all <- VARselect(v_all, lag.max = 20, type = "const",season = NULL, exogen = NULL)
lagselect_all$selection

v_x <- cbind(FMD,FME,FMA,FID,FIE,FIA)
colnames(v_x) <- cbind("FMD","FME","FMA","FID","FIE","FIA")
lagselect_x <- VARselect(v_x, lag.max = 20, type = "const",season = NULL, exogen = NULL)
lagselect_x$selection

v_y <- cbind(FID2)
lagselect_y <- VARselect(v_y, lag.max = 20, type = "const",season = NULL, exogen = NULL)
lagselect_y$selection


varmodel <- VAR(v_all, p = 3, type = "const",season = NULL, exogen = NULL, lag.max = 10,ic = c("AIC", "HQ", "SC", "FPE"))
summary(varmodel)
granger_causality(varmodel, var.y = v_y, var.x = v_x, test = c("F", "Chisq"), file = NULL, check.dropped = FALSE )




