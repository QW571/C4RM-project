# Covid Case
# Author: Qianyao Wu & Jiatong Liu


### Data Preparation ----
library(readr)
library(sqldf)
library(ggplot2)

Covids <- read_csv("NYCovidCases.csv")
CovidRecord <- sqldf('select "Date" as Date, 
                     "New Cases" as NewCases, 
                     "Percent of Total Fully Vaccinated Pop with Booster Dose (%)" as VaccinatedRate, 
                     "Average Temperature (°F)" as AverageTemperature from Covids')

# Convert character data to dates
CovidRecord$Date = as.Date(CovidRecord$Date)
CovidRecord


### Show Data ----
# Basic line plots with points
ggplot(data = CovidRecord, mapping = aes(x=Date, y=NewCases, group=1)) +
  geom_line()+
  geom_point()+
  xlab('Month')+
  ylab('NewCases')

ggplot(data = CovidRecord, mapping = aes(x=Date, y=VaccinatedRate, group=1)) +
  geom_line(linetype = "dashed")+
  xlab('Month')+
  ylab('Vaccinated Rate (%)')

ggplot(data = CovidRecord, mapping = aes(x=Date, y=AverageTemperature, group=1)) +
  geom_line(colour = "blue")+
  xlab('Month')+
  ylab('Average Temperature (°F)')


### Build models ----
#### Method 1: Linear model ----
fit1 <- lm(NewCases ~ AverageTemperature + VaccinatedRate, CovidRecord)
summary(fit1)

#### Method 2: Polynomial model ----
fit2_1 <- lm(CovidRecord$NewCases ~ poly(CovidRecord$VaccinatedRate, 10))
summary(fit2_1)

fit2_2 <- lm(CovidRecord$NewCases ~ poly(CovidRecord$AverageTemperature, 10))
summary(fit2_2)

#### Method 3: Time series model ----
# Exponential smoothing state space model
# ETS(Error, Trend, Seasonal)
library(forecast)

# ANN
fit3 <- ets(CovidRecord$NewCases, model="ANN")
summary(fit3)

forecast(fit3,100)
plot(forecast(fit3,100), 
     xlab="Time",
     ylab="Covid Cases")

# AAN
fit4 <- ets(CovidRecord$NewCases, model="AAN")
summary(fit4)

forecast(fit4,100)
plot(forecast(fit4,100), 
     xlab="Time",
     ylab="Covid Cases")

# ZZZ
fit5 <- ets(CovidRecord$NewCases, model="ZZZ")
summary(fit5)

forecast(fit5,100)
plot(forecast(fit5,100), 
     xlab="Time",
     ylab="Covid Cases")






