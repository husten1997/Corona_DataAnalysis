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

Italy_ts <- xts(WW_Data$Italy_Italy, order.by = dates)
names(Italy_ts) <- c("Data")
Italy_ts$Index <- xts(c(1:l), order.by = dates)

#For checking if the last index is right
View(Italy_ts)
tail(Italy_ts, 7)

#Plots--------------------------------------------------------------------------------------------
ggplot() +
  geom_line(aes(x = index(Italy_ts$Data), y = Italy_ts$Data)) +
  geom_line(aes(x = index(Germany_ts$Data), y = Germany_ts$Data), col = "blue") +
  scale_x_date(minor_breaks = function(x) seq.Date(from = min(x), to = max(x), by = "days"), breaks = function(x) seq.Date(from = min(x), to = max(x), by = "14 days")) +
  scale_y_continuous(labels = function(x) format(x,big.mark = ",", scientific = FALSE)) +
  labs(title = "Italy confirmed Corona cases",  x = "Time", y = "Confirmed Cases")

View(cbind(Germany_ts, Italy_ts))
