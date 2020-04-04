library(readxl)
library(readr)
library(ggplot2)
library(data.table)
library(tseries)
library(xts)
library(forecast)
library(gridExtra)

source("Scripts/Import_Data.R")


#Setting up German Data---------------------------------------------------------------------------
l <- nrow(WW_Data)
dates <- seq(as.Date("2020-01-22"),length=l,by="days")

Germany_ts <- xts(WW_Data$Germany_Germany, order.by = dates)
names(Germany_ts) <- c("Data")
Germany_ts$Recov <- xts(WW_Data_Reco$Germany_Germany, order.by = dates)
Germany_ts$Index <- xts(c(1:l), order.by = dates)



#For checking if the last index is right
#View(Germany_ts)
tail(Germany_ts, 7)

#Model Data
phases <- c(4, 16, 14, 17, 7, 8, 7)
Germany_ts_3pModel <- xts(WW_Data$Germany_Germany, order.by = dates)
names(Germany_ts_3pModel) <- c("Data")
Germany_ts_3pModel$Index <- xts(c(1:l), order.by = dates)
Germany_ts_3pModel$p1 <- xts(c(rep(0, phases[1]), rep(1, phases[2]), rep(0, phases[3]), rep(0, phases[4]), rep(0, phases[5]), rep(0, phases[6]), rep(0, phases[7])), order.by = dates)
Germany_ts_3pModel$p2 <- xts(c(rep(0, phases[1]), rep(0, phases[2]), rep(1, phases[3]), rep(0, phases[4]), rep(0, phases[5]), rep(0, phases[6]), rep(0, phases[7])), order.by = dates)
Germany_ts_3pModel$p3 <- xts(c(rep(0, phases[1]), rep(0, phases[2]), rep(0, phases[3]), rep(1, phases[4]), rep(0, phases[5]), rep(0, phases[6]), rep(0, phases[7])), order.by = dates)
Germany_ts_3pModel$p4 <- xts(c(rep(0, phases[1]), rep(0, phases[2]), rep(0, phases[3]), rep(0, phases[4]), rep(1, phases[5]), rep(0, phases[6]), rep(0, phases[7])), order.by = dates)
Germany_ts_3pModel$p5 <- xts(c(rep(0, phases[1]), rep(0, phases[2]), rep(0, phases[3]), rep(0, phases[4]), rep(0, phases[5]), rep(1, phases[6]), rep(0, phases[7])), order.by = dates)
Germany_ts_3pModel$p6 <- xts(c(rep(0, phases[1]), rep(0, phases[2]), rep(0, phases[3]), rep(0, phases[4]), rep(0, phases[5]), rep(0, phases[6]), rep(1, phases[7])), order.by = dates)
Germany_ts_3pModel$Index1 <- xts(Germany_ts_3pModel$Index * Germany_ts_3pModel$p1, order.by = dates)
Germany_ts_3pModel$Index2 <- xts(Germany_ts_3pModel$Index * Germany_ts_3pModel$p2, order.by = dates)
Germany_ts_3pModel$Index3 <- xts(Germany_ts_3pModel$Index * Germany_ts_3pModel$p3, order.by = dates)
Germany_ts_3pModel$Index4 <- xts(Germany_ts_3pModel$Index * Germany_ts_3pModel$p4, order.by = dates)
Germany_ts_3pModel$Index5 <- xts(Germany_ts_3pModel$Index * Germany_ts_3pModel$p5, order.by = dates)
Germany_ts_3pModel$Index6 <- xts(Germany_ts_3pModel$Index * Germany_ts_3pModel$p6, order.by = dates)

# plot of data Germany---------------------------------------------------------------------------

Graphics <- list()

Graphics$Germ_ConfCases <- ggplot() +
  geom_line(aes(x = index(Germany_ts$Data), y = Germany_ts$Data)) +
  geom_line(aes(x = index(Germany_ts$Recov), y = Germany_ts$Recov), col = "blue") +
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
  geom_line(aes(x = index(Germany_ts$Data), y = Germany_ts$Data)) +
  geom_line(aes(x = index(window(Germany_ts$Data, start = "2020-01-27")), y = exp(fitted(expMod_germany))), col = c("red")) +
  scale_x_date(minor_breaks = function(x) seq.Date(from = min(x), to = max(x), by = "days"), breaks = function(x) seq.Date(from = min(x), to = max(x), by = "14 days"))

ggplot() +
  geom_line(aes(x = index(Germany_ts$Data), y = log(Germany_ts$Data))) +
  geom_line(aes(x = index(window(Germany_ts$Data, start = "2020-01-27")), y = fitted(expMod_germany)), col = c("red")) +
  scale_x_date(minor_breaks = function(x) seq.Date(from = min(x), to = max(x), by = "days"), breaks = function(x) seq.Date(from = min(x), to = max(x), by = "14 days"))


#Multi Model---------------------------------------------------------------------------
expMultiMod_germany <- lm(log(Data) ~ Index + p2 + Index2 + p3 + Index3 +p4 + Index4 + p5 + Index5 + p6 + Index6, data = window(Germany_ts_3pModel, start = "2020-01-27"))

(s <- summary(expMultiMod_germany))
(growthFactor <- s$coefficients[2] + s$coefficients[12])
exp(growthFactor)
#verdopplungszeit
(log(2) / growthFactor)

#Log Plot
ggplot() +
  geom_line(aes(x = index(Germany_ts_3pModel$Data), y = log(Germany_ts_3pModel$Data))) +
  geom_line(aes(x = index(window(Germany_ts_3pModel$Data, start = "2020-01-27")), y = fitted(expMultiMod_germany)), col = c("red")) +
  scale_x_date(minor_breaks = function(x) seq.Date(from = min(x), to = max(x), by = "days"), breaks = function(x) seq.Date(from = min(x), to = max(x), by = "14 days")) +
  scale_y_continuous(labels = function(x) format(x,big.mark = ",", scientific = FALSE)) +
  labs(title = "Germany ln of confirmed Corona cases",  x = "Time", y = "Confirmed Cases")

#Plot
ggplot() +
  geom_line(aes(x = index(Germany_ts_3pModel$Data), y = Germany_ts_3pModel$Data)) +
  geom_line(aes(x = index(window(Germany_ts_3pModel$Data, start = "2020-01-27")), y = exp(fitted(expMultiMod_germany))), col = c("red")) +
  scale_x_date(minor_breaks = function(x) seq.Date(from = min(x), to = max(x), by = "days"), breaks = function(x) seq.Date(from = min(x), to = max(x), by = "14 days")) +
  scale_y_continuous(labels = function(x) format(x,big.mark = ",", scientific = FALSE)) +
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
  geom_density(aes(x = expMultiMod_germany$residuals)) +
  geom_density(aes(x = rnorm(mean = mean(expMultiMod_germany$residuals, na.rm = TRUE), sd = sd(expMultiMod_germany$residuals, na.rm = TRUE), n = length(expMultiMod_germany$residuals))), col = "blue")

ggplot() +
  geom_line(aes(x = index(Germany_ts_3pModel$Data), y = Germany_ts_3pModel$Data))

#Testen der Verteilung auf NV
p.dens <- density(expMultiMod_germany$residuals)$y
p.dens.norm <- density(rnorm(mean = mean(expMultiMod_germany$residuals, na.rm = TRUE), sd = sd(expMultiMod_germany$residuals, na.rm = TRUE), n = length(expMultiMod_germany$residuals)))$y
chisq.test(x = p.dens, p = p.dens.norm, rescale.p = TRUE)

#Analysis of the last Week------------------------------------------------------------------------------------------
lw_model_exp <- lm(log(Data) ~ Index5, data = window(Germany_ts_3pModel, start = "2020-03-21"))
lw_model_lin <- lm(Data ~ Index5, data = window(Germany_ts_3pModel, start = "2020-03-21"))

summary(lw_model_exp)
summary(lw_model_lin)

BIC(lw_model_exp)
BIC(lw_model_lin)

AIC(lw_model_exp)
AIC(lw_model_lin)

ggplot() +
  geom_line(aes(x = index(Germany_ts_3pModel$Data), y = Germany_ts_3pModel$Data, color = "1")) +
  geom_line(aes(x = index(window(Germany_ts_3pModel$Data, start = "2020-03-21")), y = exp(fitted(lw_model_exp)), color = "2")) +
  geom_line(aes(x = index(window(Germany_ts_3pModel$Data, start = "2020-03-21")), y = fitted(lw_model_lin), color = "3")) +
  coord_cartesian(xlim = range(time(window(Germany_ts_3pModel$Data, start = c("2020-03-21"))))) +
  scale_x_date(minor_breaks = function(x) seq.Date(from = min(x), to = max(x), by = "days"), breaks = function(x) seq.Date(from = min(x), to = max(x), by = "14 days")) +
  labs(title = "Germany confirmed Corona cases",  x = "Time", y = "Confirmed Cases") +
  scale_color_manual(limits = c("1", "2", "3"), values = c("black", "red", "orange"), labels = c("Data", "Exp Model", "Lin Model"), name = "") +
  theme(legend.position = "bottom")



l <- 10
growth.est <- data.frame(growthrate = rep(NA, l), expGrowthrate = rep(NA, l), doubletime = rep(NA, l))
for(i in c(0:(l-1))){
  lw_model_exp <- lm(log(Data) ~ Index5, data = window(Germany_ts_3pModel, start = "2020-03-20", end = as.Date("2020-03-23")+i))
  growth.est$growthrate[i+1] <- summary(lw_model_exp)$coefficients[2]
  growth.est$expGrowthrate[i+1] <- exp(growth.est$growthrate[i+1])
  growth.est$doubletime[i+1] <- log(2) / growth.est$growthrate[i+1]
}

plot(growth.est$expGrowthrate, type = "l", xlab = c("Days"), ylab = c("Growthrate"))
plot(growth.est$doubletime, type = "l", xlab = c("Days"), ylab = c("Doubletime"))
lw_model_exp <- lm(log(Data) ~ Index5, data = window(Germany_ts_3pModel, start = "2020-03-20", end = as.Date("2020-03-26")-1))


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


#Recoverd Analysis----------------------------------------------------------------------

recov_lag_model <- lm(window(Germany_ts$Data, end = "2020-03-08") ~ window(lag(Germany_ts$Recov, -10), end = "2020-03-08") + 
                        window(lag(Germany_ts$Recov, -13), end = "2020-03-08") +
                        window(lag(Germany_ts$Recov, -14), end = "2020-03-08") + 
                        window(lag(Germany_ts$Recov, -15), end = "2020-03-08") + 
                        window(lag(Germany_ts$Recov, -16), end = "2020-03-08") + 
                        window(lag(Germany_ts$Recov, -18), end = "2020-03-08") + 
                        window(lag(Germany_ts$Recov, -20), end = "2020-03-08"))
summary(recov_lag_model)

Germany_ts_RecovModel <- Germany_ts
Germany_ts_RecovModel$Recov_l10 <- lag(Germany_ts$Recov, -15)
cor(na.trim(Germany_ts_RecovModel)$Data,na.trim(Germany_ts_RecovModel)$Recov_l10)


Graphics$Germ_ConfCases <- ggplot() +
  geom_line(aes(x = index(Germany_ts$Data), y = Germany_ts$Data)) +
  geom_line(aes(x = index(Germany_ts$Recov), y = lag(Germany_ts$Recov, -12)), col = "blue") +
  scale_x_date(minor_breaks = function(x) seq.Date(from = min(x), to = max(x), by = "days"), breaks = function(x) seq.Date(from = min(x), to = max(x), by = "14 days")) +
  labs(title = "Germany confirmed Corona cases",  x = "Time", y = "Confirmed Cases")
Graphics$Germ_ConfCases


na.trim.ts(cbind(Germany_ts$Data, lag(Germany_ts$Recov, 20)))








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

