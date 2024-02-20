install.packages("xlsx")
install.packages("corrplot")
install.packages("tidyverse")
install.packages("datarium")
install.packages("qqplotr")
install.packages("ggplot2")
install.packages("moments")
install.packages("car")
library(moments)
library(qqplotr)
library(ggplot2)
library(datarium)
library(tidyverse)
library(corrplot)
library(xlsx)
library(dplyr)
library(car)
GDP_data = read.xlsx("WDI.xlsx", 1)
GDP_data = GDP_data[ -c(1,4) ] #removing unnecessary columns
names(GDP_data) = c('Country','Year','GDP_growth','Inflation','Gini','Education','FDI_inflows','Taxes','Unemployment','Consumption',' Urban_Population','Imports',"Political Stability")
GDP_data[GDP_data == ".."] = NA #changing missing values to NA  
GDP_data = na.omit(GDP_data) # removing NA values

i = c(5,8) #picking columns 
GDP_data[ , i] <- apply(GDP_data[ , i], 2,            # changing character columns to numeric
                        function(x) as.numeric(as.character(x)))
High = GDP_data[is.element(GDP_data$Country,c('GBR','USA','DEU','FRA','ITA')),]


High <- High %>%
  add_column(Group = "High") #creating groups

Low = GDP_data[is.element(GDP_data$Country,c('ESP','BGR','TUR','CHE','NLD')),]
Low <- Low %>%
  add_column(Group = "Low")

data = rbind(High,Low) # creating a new table with the groups split


#EDA
boxplot(GDP_data$GDP_growth ~ GDP_data$Country, main=("GDP growth across 10 years"))
boxplot(GDP_data$Inflation ~ GDP_data$Country, main=("Inflation rates across 10 years"))
boxplot(GDP_data$Gini ~ GDP_data$Country, main = ("Gini index across the past 10 years"))
boxplot(GDP_data$Education ~ GDP_data$Country, main = ("Education expenditure as a measure of GDP"))
boxplot(GDP_data$Inflation ~ GDP_data$Country, title = "Inflation rates across 10 years")

hist(GDP_data$FDI_inflows)
hist(GDP_data$Unemployment)
hist(GDP_data$Taxes)
hist(GDP_data$Imports)


summary(GDP_data)
boxplot(High$GDP_growth)
boxplot(Low$GDP_growth)
#4.1
GDP_data = GDP_data[ -c(2) ]
GDP_data %>% 
  group_by(Country) %>%
  summarise_all("mean")

GDP_data %>% 
  group_by(Country) %>%
  summarise_all("median")

GDP_data %>% 
  group_by(Country) %>%
  summarise_all("skewness")

GDP_data %>% 
  group_by(Country) %>%
  summarise_all("kurtosis")


#4.2
cor(GDP_data$Inflation,GDP_data$`GDP_growth`, method='spearman')
cor(GDP_data$Education,GDP_data$`GDP_growth`, method='spearman')
cor(GDP_data$Taxes,GDP_data$`GDP_growth`, method='spearman')
cor(GDP_data$Unemployment,GDP_data$`GDP_growth`, method='spearman')
cor(GDP_data$`Political Stability`,GDP_data$`GDP_growth`, method='spearman')
cor(GDP_data$`FDI inflows`,GDP_data$GDP_growth,method='spearman')

GDP_data = GDP_data[ -c(1) ]

round(cor(GDP_data),digits =2)

corrplot(cor(GDP_data), method='circle',type="upper",diag=FALSE)


plot(GDP_data$Taxes,GDP_data$`GDP growth`)
#4.3

ggplot(mapping = aes(sample = GDP_data$`GDP_growth`)) +stat_qq_point(size = 2,color = "blue") +stat_qq_line(color="orange") +xlab("Theoretical") + ylab("Sample")
shapiro.test(GDP_data$`GDP_growth`) #checking normality
shapiro.test(log(GDP_data$`GDP_growth`))
shapiro.test(sqrt(GDP_data$`GDP_growth`))
shapiro.test(GDP_data$`GDP_growth`^(1/3))

data = data[ -c(1,2) ]
summary(data)
data$Group <- as.factor(data$Group)

summary(data)

wilcox.test(data$GDP_growth, mu=2, alternative = "less")


wilcox.test(data$GDP_growth ~ data$Group, data=data)

#4.4
install.packages("lmtest")
library(lmtest)
plot(data$GDP_growth ~ data$Consumption)

Regression1 = lm((data$GDP_growth) ~ data$Consumption)
summary(Regression1)
plot(Regression1,1) #checking for residual independence
plot(Regression1,2) #checking for normality 
plot(Regression1,3) #checking for homoscedasticity 
bptest(Regression1)



Regression2 = lm(((data$GDP_growth^1/3)) ~ data$Consumption + data$Imports)
summary(Regression2)

plot(data$GDP_growth ~ data$`Imports`)
plot(Regression2,1)
plot(Regression2,2)
plot(Regression2,3)
vif(Regression2)

#4.5
install.packages("TTR")
install.packages("forecast")
install.packages("tseries")
library(tseries)
library(forecast)
library(TTR)
mean_GDP = aggregate(GDP_data$GDP_growth, list(GDP_data$Year), FUN=mean) #creating a variable which aggregates and calculates mean
names(mean_GDP) = c("Year","GDP growth") #renaming columns
mean_GDP = mean_GDP[ -c(1) ]#removing year column
timeseries = ts(mean_GDP,start=2010) #creating timeseries
plot.ts(timeseries)
SMA3 = SMA(timeseries, n =2)
plot(SMA3)
adf.test(timeseries) #testing whether the time series is stationary

GDP_forecasts = HoltWinters(timeseries, gamma = FALSE) #creating exponential smoothing time series
GDP_forecasts
plot(GDP_forecasts)
GDP_forecasts2 = forecast(GDP_forecasts,h=5) #creating a forecast for the next 5 years
plot(GDP_forecasts2)

acf(GDP_forecasts2$residuals, lag.max=5, na.action = na.pass)
Box.test(GDP_forecasts2$residuals, lag=5, type="Ljung-Box")

plot.ts(GDP_forecasts2$residuals) # make time series plot

plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}


GDP_forecasts2$residuals <-
  GDP_forecasts2$residuals[!is.na(GDP_forecasts2$residuals)]
plotForecastErrors(GDP_forecasts2$residuals) # make a histogram

timeseriesdiff = diff(timeseries)
timeseriesdiff2 = diff(timeseriesdiff) #differencing timeseries
plot(timeseriesdiff2)

acf(timeseriesdiff2, lag.max=20)
acf(timeseriesdiff2, lag.max=20, plot=FALSE)

pacf(timeseriesdiff2, lag.max=20)
pacf(timeseriesdiff2, lag.max=20, plot=FALSE)

timeseriesarima <- arima(timeseriesdiff2, order=c(0,2,1))  #creating new timeseries
timeseriesarima_forecast =  forecast(timeseriesarima, h=5) #creating forecast for next 5 years
plot(timeseriesarima_forecast)

acf(timeseriesarima_forecast$residuals, lag.max=20)
Box.test(timeseriesarima_forecast$residuals, lag=5, type="Ljung-Box")

timeseriesarima_forecast$residuals <-
 timeseriesarima_forecast$residuals[!is.na(timeseriesarima_forecast$residuals)]
plotForecastErrors(timeseriesarima_forecast$residuals) # make a histogram

