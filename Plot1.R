#==============================================================================
#Load the required packages

library(dplyr)
library(magrittr)
library(lubridate)
library(tidyverse)
library(reshape)


#==============================================================================
#importing the electricity consumption data

ec = read.table(file.choose(), header = TRUE, sep = ";")


#==============================================================================
#Create a copy of data and check the structure of data

ec_copy = ec
str(ec_copy)


#==============================================================================
#Extract data dated "1/2/2007" or "2/2/2007"

plot_data = subset(ec_copy, Date == "1/2/2007" | Date == "2/2/2007")


#=============================================================================
#Replace "?" as NA and omit them

plot_data[plot_data == "?"] = NA
nrow(plot_data)
plot_data = na.omit(plot_data)


#=============================================================================
#Combine Date and Time column to DateTime of POSIXlt format


plot_data$DateTime <- strptime(paste(plot_data$Date, plot_data$Time, sep=" "), "%d/%m/%Y %H:%M:%S")
plot_data = subset(plot_data, select = -c(Date, Time))



#============================================================================
#Convert the Required factor columns to numeric

factor_cols = names(plot_data)[sapply(plot_data, is.factor)]
plot_data[factor_cols] = as.numeric(as.matrix(plot_data[factor_cols]))
str(plot_data)


#============================================================================
#Histogram of Global_active_power

png("plot1.png", height = 480, width = 480)
with(plot_data, hist(Global_active_power, col = "red", xlab = "Global_active_power(kilowatts)",
                          main = "Global Active Power"))
dev.off()

