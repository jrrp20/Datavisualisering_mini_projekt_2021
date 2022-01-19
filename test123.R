library(tidyverse)
library(readxl)
library(shiny)
library(vroom)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
nyc <- vroom::vroom("us/KNYC.csv")
phx <- vroom::vroom("us/KPHX.csv")
sea <- vroom::vroom("us/KSEA.csv")

#make sure 'date' is recognized as dates:
#ny_weather$date <- as.Date(ny_weather$date, format = "%Y-%m-%d")

nyc$date <- as.Date(nyc$date, format = "%Y-%m-%d")
phx$date <- as.Date(phx$date, format = "%Y-%m-%d")
sea$date <- as.Date(sea$date, format = "%Y-%m-%d")

highlight_min_nyc <- nyc %>% 
  filter(actual_min_temp<=record_min_temp)
head(highlight_min_nyc)