
library(tidyverse)
library(readxl)
library(shiny)
library(ggplot2)
library(vroom)
#library(shinyWidgets)

setwd("C:/Users/ramme/Downloads/us")

nyc <- vroom::vroom("KNYC.csv")

nyc$date <- as.Date(nyc$date, format = "%Y-%m-%d")

#ggplot(data=nyc, (aes(x=date, y=record_min_temp))) + geom_point() + 
#  geom_ribbon(aes(ymin = record_min_temp, ymax = record_max_temp),
#              fill = "grey", alpha = .4)+ labs(x = "Date", y = "Temperature")

#ggplot(data=nyc, (aes(x=date, y=record_min_temp))) + labs(x = "Year", y = "Temperature (?F)") + 
#  stat_smooth(se = TRUE) + geom_point(color = "gray40", alpha = .5)

#Probably best not to use the se

#How to plot to sets of points simultaneously
nyc %>% ggplot(aes(x=date)) + 
  geom_ribbon(aes(ymin = record_min_temp, ymax = record_max_temp),
              fill = "grey", alpha = .4)+ labs(x = "Date", y = "Temperature") +
  geom_ribbon(aes(ymin = average_min_temp, ymax = average_max_temp),
              fill = "grey", alpha = .8)+ labs(x = "Date", y = "Temperature") +
  geom_ribbon(aes(ymin = actual_min_temp, ymax = actual_max_temp),
              fill = "black", alpha = 0.6)+ labs(x = "Date", y = "Temperature")
