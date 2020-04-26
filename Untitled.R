#########################################################################################
# IMPORTING AND PRE-PROCESSING

# Import Data as revenue

# check the quotes while importing to get 2 columns
class(revenue$V2)   

# chopping off the useless quotes at 2 positions
library(tidyr)
revenue <- separate(revenue, col = V2, 
                    sep = c(2, -3), 
                    into = c("rest", "data", "rest2"))

# all the relevant data is in column "data"
head(revenue)

# class is still a character (with some missing data)
class(revenue$data)

# conversion to time series
revenue <- scan()
myts <- ts(as.numeric(revenue),
           start = c(1992, 1), frequency = 12)

# data is still not clean (outliers and NAs)
summary(myts)

# all in one cleaning tool
library(forecast)
myts <- tsclean(myts)

# check the data
summary(myts)

plot(myts)
#########################################################################################



#########################################################################################
# PRELIMINARY ANALYSIS

# time plot
library(ggplot2)
autoplot(myts) + 
  ggtitle("US Retail Sales") +
  ylab("Millions of Dollars") +
  xlab("Time")

plot(decompose(myts))

# data has a strong trend
# investigate transformation

# take the first diffrence to remove the trend from the data
d_1_myts <- diff(myts)

# time plot of diffrence data
autoplot(d_1_myts) +
  ggtitle("TIME PLOT: Change in US Retail Sales") +
  ylab("Millions of Dollars") +
  xlab("Time")

# series appears stationary, use to investigate seasonality
ggseasonplot(d_1_myts) +
  ggtitle("SEASONAL PLOT: Change in US Retail Sales") +
  ylab("Millions of Dollars") +
  xlab("Months")

# another seasonal plot, seasonal subseries plot
ggsubseriesplot(d_1_myts) +
  ggtitle("SEASONAL PLOT: Change in US Retail Sales") +
  ylab("Millions of Dollars") +
  xlab("Months")
#########################################################################################



#########################################################################################
# FORECASTING

# ets
fit_ets <- ets(myts)
print(summary(fit_ets))
checkresiduals(fit_ets)

# ARIMA
fit_arima <- auto.arima(myts, d = 1, D = 1, stepwise = F, approximation = F, trace = T)
print(summary(fit_arima))
checkresiduals(fit_arima)

# forecast ets
fcast_ets<- forecast(fit_ets, h = 2*12)
autoplot(fcast_ets, include = 180)
# forecast arima
fcast_arima <- forecast(fit_arima, h = 2*12)
autoplot(fcast_arima, include = 180)
#########################################################################################



#########################################################################################
## CROSS VALIDATION OF ETS AND ARIMA
fit_ets <- ets(myts)
fit_arima <- auto.arima(myts, 
                        d = 1, 
                        D = 1, 
                        stepwise = F, 
                        approximation = F, 
                        trace = T)


forecastets = 
  function(x, h) 
  {
    forecast(ets(x), h = h)
  }

forecastarima = 
  function(x, h) 
  {
    forecast(auto.arima(x), stepwise = T, approximation = F, h=h)
  }

etserror = tsCV(myts, forecastets, h=1)
arimaerror = tsCV(myts, forecastarima, h=1)

mean(etserror^2, na.rm=TRUE)
mean(arimaerror^2, na.rm=TRUE)
#########################################################################################



#########################################################################################
# SET UP NEURAL NETWORK MODEL
mynnetar <- nnetar(myts)

# forecasting 3 years with the model
nnetforecast <- forecast(mynnetar, h = 36,
                         PI = T)
library(ggplot2)
autoplot(nnetforecast)

# interactive dygraph

# data we need for the graph
data <- nnetforecast$x
lower <- nnetforecast$lower[,2]
upper <- nnetforecast$upper[,2]
pforecast <- nnetforecast$mean

mydata <- cbind(data, lower, upper,
                pforecast)

library(dygraphs)

dygraph(data = mydata, main = "US Retail Sales") %>% 
  dyRangeSelector() %>% 
  dySeries(name = "data", label = "Revenue Data") %>%
  dySeries(c("lower","pforecast","upper"), label = "Revenue Forecast") %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
  dyAxis("y", label = "Millions of Dollars") %>%
  dyHighlight(highlightCircleSize = 5,
              highlightSeriesOpts = list(strokeWidth = 2)) %>%
  dyOptions(axisLineColor = "navy", gridLineColor = "grey") %>%
  dyAnnotation("2008-1-22", text = "HC", tooltip = "Housing Crisis", attachAtBottom = T)
#########################################################################################