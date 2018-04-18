#Cameron Moyle

#Predict Monthly Energy Consumption for the next two years

#DATA

# US residential energy consumption
# http://www.eia.gov/totalenergy/data/monthly/index.cfm#consumption
# Accessed 1/16/18

data1<-read.csv("http://www.eia.gov/totalenergy/data/browser/csv.cfm?tbl=T02.01")
# subset to TERCBUS Total Energy Consumed by the Residential Sector
data2<-subset(data1,MSN=="TERCBUS")
# subset to "your lifetime"
data3<-subset(data2,data2$YYYYMM>199100)
# remove yearly total (coded "month 13", every 13th obs)
data4<-subset(data3,data3$YYYYMM%%100 != 13)
energy<-data4$Value

tail(energy)

T<-length(energy)

plot(1:T, energy , type = 'b', xlab = "YYYYMM", ylab = "TERCBUS (Trillion Btu)",
     main = "Monthly Energy Consumption by the Residential Sector")

# The plot indicates a monthly pattern for energy consumption, which is likely attributed to varying 
# uses of air conditioning and heating during variable weather months. Businesses don't use heating or AC 
# if the weather is nice to save money.

library(astsa)

#Estimate Parameters
energy.out <- sarima(energy,1,1,1,1,1,1,12)
# Compute Predictions
energy.future <- sarima.for(energy,n.ahead=27,1,1,1,1,1,1,12)

#Compute 95% prediction intervals
L <- energy.future$pred - 2 * energy.future$se
U <- energy.future$pred + 2 * energy.future$se

# table of predictions and predictions intervals
cbind(energy.future$pred,L,U)

# Graphic
plot(1:T, energy, type='b', xlab="Months", ylab = "TERCBUS (Trillion Btu)",
     main = "Prediction of Next 27 Months for Montly Energy Consumption",
     xlim = c(300, 350) )
lines(322:348, energy.future$pred, type = 'b', pch = 19, col = "red")
lines(322:348, L, col = 'grey', lty = 2)
lines(322:348, U, col = 'grey', lty = 2)

# Research Task: Predict Future Energy Consumption Values for the next 27 months
# Data Features: Time Series, seasonal correlation observed in past is expected to continue 
# each month for the next 2 years

# Analysis weakness: No explanatory variables to tell us 'why'

#Challenge: 
# Using the same source (http://www.eia.gov/totalenergy/data/monthly/index.cfm#consumption)
# but to predict the next two years of energy consumption in the transportation sector using
# the SARIMA model.
