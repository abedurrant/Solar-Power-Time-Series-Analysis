# Needed Packages
library(astsa)
library(forecast)
library(lubridate)
library(tidyverse)
library(dplyr)

# Read in Data
kwh <- read.csv("KWH.csv",header=T)


# Time Series Plots
kwh.ts = ts(kwh$kwh, start = c(2017,1), frequency = 365)
plot.ts(kwh.ts)
acf(kwh.ts)
pacf(kwh.ts)
auto.arima(kwh.ts)
#ts.model = sarima(kwh$kwh, p=1, d=0, q=0)
ts.model.a = Arima(kwh$kwh, order = c(1,0,0), seasonal = list(order=c(0,1,0),period=365),include.drift = TRUE)
acf2(ts.model.a$residuals, max.lag = 365)
summary(ts.model.a)

# Using Sarima to get some predictions
future_preds <- sarima.for(kwh.ts, p=1, d=0, q=0, D=1, S=365, n.ahead = 365)

#Linear model for comparison
kwh$Date <- as.Date(kwh$Date)
basic.lm = lm(kwh~Date, data = kwh)
summary(basic.lm)

#EDA on year
ggplot(kwh, aes(x=year,y=kwh)) + geom_point() + geom_smooth(method = 'lm', se = FALSE)

#Max, min, median each year
kwh$year <- year(kwh$Date)

kwh
by_year <- kwh %>% group_by(year)
by_year %>% summarise(
  yearmeans = mean(kwh),
  yearmax = max(kwh),
  yearmin = min(kwh),
  yearmed = median(kwh),
  yearq3 = quantile(kwh, .75),
  yearq1 = quantile(kwh, .25),
  yeartop10 = quantile(kwh, .9),
  yearbottom10 = quantile(kwh, .1)
)
mean(future_preds$pred)
median(future_preds$pred)
quantile(future_preds$pred, .75)
quantile(future_preds$pred, .25)


# Anova on year means to check for yearly differences
kwh$year <- as.factor(kwh$year)
year.lm = lm(kwh~year, data = kwh)
year.anova <- aov(year.lm)
year.anova$coefficients
confint(year.lm)
summary(year.anova)

# The estimated effect of year on kwh
kwh$year <- as.numeric(kwh$year)
year.lm = lm(kwh~year, data = kwh)
confint(year.lm)

# Cross Validation on model to judge predictions
n.cv = 12
rpmse <- rep(x=NA, times=n.cv)
bias <- rep(x=NA, times=n.cv)
for(i in 1:n.cv) {
  train = kwh[1:(730+i*30),]
  test = kwh[(731+i*30):1095,]
  train.model = Arima(train$kwh, order = c(1,0,0), seasonal = list(order=c(0,1,0),period=365),include.drift = TRUE)
  our_preds = forecast(train.model, h=335)
  rpmse[i] <- sqrt(mean((our_preds$fitted - test$kwh)^2))
  bias[i]<- mean(our_preds$fitted - test$kwh)
}

mean(rpmse)
mean(bias)

mvspec(kwh.ts)
