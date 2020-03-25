library(readxl)
library(readr)
library(ggplot2)
library(data.table)
library(tseries)
library(xts)
library(forecast)
library(gridExtra)

source("Scripts/Import_Data.R")


View(EU_Data)

#Setting up EU Data (currently brocken)
l <- nrow(EU_Data)
EU_Data_ts <- matrix(data = NA, nrow = l)
dates <- seq(as.Date("2020-01-22"),length=l,by="days")
EU_Data_ts$Date <- dates
EU_Data_ts$Index <- xts(c(1:l), order.by = dates)
EU_Data_ts$Germany <- xts(EU_Data$Germany_Germany, order.by = dates)
EU_Data_ts$Austria <- xts(EU_Data$Austria_Austria, order.by = dates)

#Setting up German Data---------------------------------------------------------------------------
l <- nrow(WW_Data)
dates <- seq(as.Date("2020-01-22"),length=l,by="days")

Germany_ts <- xts(WW_Data$Germany_Germany, order.by = dates)
names(Germany_ts) <- c("Data")
Germany_ts$Index <- xts(c(1:l), order.by = dates)

#For checking if the last index is right
View(Germany_ts)
tail(Germany_ts, 7)

#Model Data

phases <- c(4, 16, 14, 17, 7, 5)
Germany_ts_3pModel <- xts(WW_Data$Germany_Germany, order.by = dates)
names(Germany_ts_3pModel) <- c("Data")
Germany_ts_3pModel$Index <- xts(c(1:l), order.by = dates)
Germany_ts_3pModel$p1 <- xts(c(rep(0, phases[1]), rep(1, phases[2]), rep(0, phases[3]), rep(0, phases[4]), rep(0, phases[5]), rep(0, phases[6])), order.by = dates)
Germany_ts_3pModel$p2 <- xts(c(rep(0, phases[1]), rep(0, phases[2]), rep(1, phases[3]), rep(0, phases[4]), rep(0, phases[5]), rep(0, phases[6])), order.by = dates)
Germany_ts_3pModel$p3 <- xts(c(rep(0, phases[1]), rep(0, phases[2]), rep(0, phases[3]), rep(1, phases[4]), rep(0, phases[5]), rep(0, phases[6])), order.by = dates)
Germany_ts_3pModel$p4 <- xts(c(rep(0, phases[1]), rep(0, phases[2]), rep(0, phases[3]), rep(0, phases[4]), rep(1, phases[5]), rep(0, phases[6])), order.by = dates)
Germany_ts_3pModel$p5 <- xts(c(rep(0, phases[1]), rep(0, phases[2]), rep(0, phases[3]), rep(0, phases[4]), rep(0, phases[5]), rep(1, phases[6])), order.by = dates)
Germany_ts_3pModel$Index1 <- xts(Germany_ts_3pModel$Index * Germany_ts_3pModel$p1, order.by = dates)
Germany_ts_3pModel$Index2 <- xts(Germany_ts_3pModel$Index * Germany_ts_3pModel$p2, order.by = dates)
Germany_ts_3pModel$Index3 <- xts(Germany_ts_3pModel$Index * Germany_ts_3pModel$p3, order.by = dates)
Germany_ts_3pModel$Index4 <- xts(Germany_ts_3pModel$Index * Germany_ts_3pModel$p4, order.by = dates)
Germany_ts_3pModel$Index5 <- xts(Germany_ts_3pModel$Index * Germany_ts_3pModel$p5, order.by = dates)

# plot of data Germany---------------------------------------------------------------------------

Graphics <- list()

Graphics$Germ_ConfCases <- ggplot() +
  geom_line(aes(x = index(Germany_ts$Data), y = Germany_ts$Data)) +
  scale_x_date(minor_breaks = function(x) seq.Date(from = min(x), to = max(x), by = "days"), breaks = function(x) seq.Date(from = min(x), to = max(x), by = "14 days")) +
  labs(title = "Germany confirmed Corona cases",  x = "Time", y = "Confirmed Cases")
Graphics$Germ_ConfCases

Graphics$Germ_lnConfCases <- ggplot() +
  geom_line(aes(x = index(Germany_ts$Data), y = log(Germany_ts$Data))) +
  scale_x_date(minor_breaks = function(x) seq.Date(from = min(x), to = max(x), by = "days"), breaks = function(x) seq.Date(from = min(x), to = max(x), by = "14 days")) +
  labs(title = "Germany ln of confirmed Corona cases",  x = "Time", y = "ln(Confirmed Cases)")
Graphics$Germ_lnConfCases


ggsave("Graphics/Germany_Confirmed_Cases.png", width = 20, height = 20, units = "cm", dpi = 300, plot = arrangeGrob(Graphics$Germ_ConfCases, Graphics$Germ_lnConfCases, nrow = 2)) 

#Simple exponential Model (currently broken)---------------------------------------------------------------------------
expMod_germany <- lm(log(window(Germany_ts$Data, start = "2020-01-27")) ~ window(Germany_ts$Index, start = "2020-01-27"))
summary(expMod_germany)

ggplot() +
  geom_line(aes(x = index(EU_Data_ts$Germany), y = EU_Data_ts$Germany)) +
  geom_line(aes(x = index(window(EU_Data_ts$Germany, start = "2020-01-27")), y = exp(fitted(expMod_germany))), col = c("red")) +
  scale_x_date(minor_breaks = function(x) seq.Date(from = min(x), to = max(x), by = "days"), breaks = function(x) seq.Date(from = min(x), to = max(x), by = "14 days"))

ggplot() +
  geom_line(aes(x = index(EU_Data_ts$Germany), y = log(EU_Data_ts$Germany))) +
  geom_line(aes(x = index(window(EU_Data_ts$Germany, start = "2020-01-27")), y = fitted(expMod_germany)), col = c("red")) +
  scale_x_date(minor_breaks = function(x) seq.Date(from = min(x), to = max(x), by = "days"), breaks = function(x) seq.Date(from = min(x), to = max(x), by = "14 days"))


#Multi Model---------------------------------------------------------------------------
expMultiMod_germany <- lm(log(Data) ~ Index + p2 + Index2 + p3 + Index3 +p4 + Index4 + p5 + Index5, data = window(Germany_ts_3pModel, start = "2020-01-27"))

(s <- summary(expMultiMod_germany))
(growthFactor <- s$coefficients[2] + s$coefficients[10])
exp(growthFactor)
#verdopplungszeit
(log(2) / growthFactor)

#Log Plot
ggplot() +
  geom_line(aes(x = index(Germany_ts_3pModel$Data), y = log(Germany_ts_3pModel$Data))) +
  geom_line(aes(x = index(window(Germany_ts_3pModel$Data, start = "2020-01-27")), y = fitted(expMultiMod_germany)), col = c("red")) +
  scale_x_date(minor_breaks = function(x) seq.Date(from = min(x), to = max(x), by = "days"), breaks = function(x) seq.Date(from = min(x), to = max(x), by = "14 days")) +
  labs(title = "Germany ln of confirmed Corona cases",  x = "Time", y = "Confirmed Cases")

#Plot
ggplot() +
  geom_line(aes(x = index(Germany_ts_3pModel$Data), y = Germany_ts_3pModel$Data)) +
  geom_line(aes(x = index(window(Germany_ts_3pModel$Data, start = "2020-01-27")), y = exp(fitted(expMultiMod_germany))), col = c("red")) +
  scale_x_date(minor_breaks = function(x) seq.Date(from = min(x), to = max(x), by = "days"), breaks = function(x) seq.Date(from = min(x), to = max(x), by = "14 days")) +
  labs(title = "Germany confirmed Corona cases",  x = "Time", y = "Confirmed Cases")

#logPlot after 2020-02-21
ggplot() +
  geom_line(aes(x = index(Germany_ts_3pModel$Data), y = log(Germany_ts_3pModel$Data))) +
  geom_line(aes(x = index(window(Germany_ts_3pModel$Data, start = "2020-01-27")), y = fitted(expMultiMod_germany)), col = c("red")) +
  coord_cartesian(xlim = range(time(window(Germany_ts_3pModel$Data, start = c("2020-02-21"))))) +
  scale_x_date(minor_breaks = function(x) seq.Date(from = min(x), to = max(x), by = "days"), breaks = function(x) seq.Date(from = min(x), to = max(x), by = "14 days")) +
  labs(title = "Germany ln of confirmed Corona cases",  x = "Time", y = "ln(Confirmed Cases)")

#Resid Plot
ggplot() +
  geom_point(aes(x = index(window(Germany_ts_3pModel$Data, start = "2020-01-27")), y = expMultiMod_germany$residuals)) +
  geom_hline(yintercept = mean(expMultiMod_germany$residuals), col = c("red")) +
  coord_cartesian(xlim = range(time(window(Germany_ts_3pModel$Data, start = c("2020-02-21"))))) +
  labs(title = "Residuals",  x = "Time", y = "Residuals")

ggplot() +
  geom_density(aes(x = exp(expMultiMod_germany$residuals)))

ggplot() +
  geom_line(aes(x = index(Germany_ts_3pModel$Data), y = Germany_ts_3pModel$Data))

#Germany Forecast---------------------------------------------------------------------------
lastDate <- c((as.Date(index(last(Germany_ts$Data)))+1))
newDates <- seq(as.Date(lastDate), length=14, by="days")
Germany_ts_3pModel_NewData <- xts(c((l+1):(l+14)), order.by = newDates)
names(Germany_ts_3pModel_NewData) <- c("Index")
Germany_ts_3pModel_NewData$p1 <- xts(rep(0, 14), order.by = newDates)
Germany_ts_3pModel_NewData$p2 <- xts(rep(0, 14), order.by = newDates)
Germany_ts_3pModel_NewData$p3 <- xts(rep(0, 14), order.by = newDates)
Germany_ts_3pModel_NewData$p4 <- xts(rep(0, 14), order.by = newDates)
Germany_ts_3pModel_NewData$p5 <- xts(rep(1, 14), order.by = newDates)
Germany_ts_3pModel_NewData$Index1 <- xts(Germany_ts_3pModel_NewData$Index * Germany_ts_3pModel_NewData$p1, order.by = newDates)
Germany_ts_3pModel_NewData$Index2 <- xts(Germany_ts_3pModel_NewData$Index * Germany_ts_3pModel_NewData$p2, order.by = newDates)
Germany_ts_3pModel_NewData$Index3 <- xts(Germany_ts_3pModel_NewData$Index * Germany_ts_3pModel_NewData$p3, order.by = newDates)
Germany_ts_3pModel_NewData$Index4 <- xts(Germany_ts_3pModel_NewData$Index * Germany_ts_3pModel_NewData$p4, order.by = newDates)
Germany_ts_3pModel_NewData$Index5 <- xts(Germany_ts_3pModel_NewData$Index * Germany_ts_3pModel_NewData$p5, order.by = newDates)

Germany_ts_3pModel_NewData$Data <- xts(predict(expMultiMod_germany, newdata = Germany_ts_3pModel_NewData), order.by = newDates)

ggplot() +
  geom_line(aes(x = index(Germany_ts_3pModel$Data), y = Germany_ts_3pModel$Data)) +
  geom_line(aes(x = index(window(Germany_ts_3pModel$Data, start = "2020-01-27")), y = exp(fitted(expMultiMod_germany))), col = c("red")) +
  geom_line(aes(x = index(Germany_ts_3pModel_NewData$Data), y = exp(Germany_ts_3pModel_NewData$Data)), col = c("blue")) +
  scale_y_continuous(labels = function(x) format(x,big.mark = ",", scientific = FALSE)) +
  scale_x_date(minor_breaks = function(x) seq.Date(from = min(x), to = max(x), by = "days")) +
  labs(title = "German confirmed Corona Cases", x = "Time", y = "Infected")

#Fitting Gaus---------------------------------------------------------------------------

Germany_ts <- xts(EU_Data$Germany_Germany, order.by = dates)
names(Germany_ts) <- c("Data")
Germany_ts$Index <- xts(c(1:l), order.by = dates)
Germany_ts$x1 <- xts(c(rep(0, 4), rep(1, 16), rep(0, 36)), order.by = dates)
Germany_ts$p2 <- xts(c(rep(0, 5), rep(0, 15), rep(1, 14), rep(0, 22)), order.by = dates)
Germany_ts$p3 <- xts(c(rep(0, 5), rep(0, 15), rep(0, 14), rep(1, 22)), order.by = dates)
Germany_ts$Index1 <- xts(Germany_ts$Index * Germany_ts$p1, order.by = dates)
Germany_ts$Index2 <- xts(Germany_ts$Index * Germany_ts$p2, order.by = dates)
Germany_ts$Index3 <- xts(Germany_ts$Index * Germany_ts$p3, order.by = dates)
# plot of data Austria

ggplot() +
  geom_line(aes(x = index(EU_Data_ts$Austria), y = EU_Data_ts$Austria))

ggplot() +
  geom_line(aes(x = index(EU_Data_ts$Austria), y = log(EU_Data_ts$Austria)))




