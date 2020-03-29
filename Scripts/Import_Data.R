#New Data Import and Transformation

helper <- read_csv("Data/OriginalData/time_series_covid19_confirmed_global.csv")
date.index <- data.table(orig = names(helper)[-c(1:4)])
date.index$day <- as.vector(sapply(date.index$orig, FUN = function(x) strsplit(x, "/")[[1]][2]))
date.index$month <- as.vector(sapply(date.index$orig, FUN = function(x) strsplit(x, "/")[[1]][1]))
date.index$year <- as.numeric(as.vector(sapply(date.index$orig, FUN = function(x) strsplit(x, "/")[[1]][3]))) + 2000
date.index$index <- paste(date.index$year, date.index$month, date.index$day, sep = "/")
date.index$date <- as.Date(date.index$index)

helper$Country_Region <- mapply(x = helper$`Country/Region`,y = helper$`Province/State`, FUN = function(x, y) if(is.na(y)) paste(x, x, sep = "_") else paste(x, y, sep = "_"))
helper <- helper[, c(ncol(helper), c(1:(ncol(helper)-1)))]
helper$`Province/State` <- NULL
helper$`Country/Region` <- NULL
helper$Lat <- NULL
helper$Long <- NULL

WW_Data <- data.frame(t(helper[, -1]))
names(WW_Data) <- helper$Country_Region
WW_Data$Date <- date.index$date
WW_Data <- WW_Data[, c(ncol(WW_Data), c(1:(ncol(WW_Data)-1)))]

rm(helper)
rm(date.index)


helper <- read_csv("Data/OriginalData/time_series_covid19_recovered_global.csv")
date.index <- data.table(orig = names(helper)[-c(1:4)])
date.index$day <- as.vector(sapply(date.index$orig, FUN = function(x) strsplit(x, "/")[[1]][2]))
date.index$month <- as.vector(sapply(date.index$orig, FUN = function(x) strsplit(x, "/")[[1]][1]))
date.index$year <- as.numeric(as.vector(sapply(date.index$orig, FUN = function(x) strsplit(x, "/")[[1]][3]))) + 2000
date.index$index <- paste(date.index$year, date.index$month, date.index$day, sep = "/")
date.index$date <- as.Date(date.index$index)

helper$Country_Region <- mapply(x = helper$`Country/Region`,y = helper$`Province/State`, FUN = function(x, y) if(is.na(y)) paste(x, x, sep = "_") else paste(x, y, sep = "_"))
helper <- helper[, c(ncol(helper), c(1:(ncol(helper)-1)))]
helper$`Province/State` <- NULL
helper$`Country/Region` <- NULL
helper$Lat <- NULL
helper$Long <- NULL

WW_Data_Reco <- data.frame(t(helper[, -1]))
names(WW_Data_Reco) <- helper$Country_Region
WW_Data_Reco$Date <- date.index$date
WW_Data_Reco <- WW_Data_Reco[, c(ncol(WW_Data_Reco), c(1:(ncol(WW_Data_Reco)-1)))]

rm(helper)
rm(date.index)
