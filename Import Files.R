#the object of this project is to see if Total phosphorus can be modeled by analytes that can be measured using sensors. 
# this script will import data


library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(tidyr)
library(stringr)
library(lubridate)
library(scales)
library(RColorBrewer)
library(viridis)
library(Hmisc)
library(ggpmisc)
library(ggrepel)
library(zoo)
library(dbhydroR)

#Import Data
s7_R_BK <- get_hydro(dbkey = "RQ465", date_min="2021-06-01",date_max=as.character(today()))  #DBHYDRO data for inflow to Cell 2B of STA34
STA_WQ_Data <- get_wq(station_id = c( "S7"),date_min = "2010-01-01", date_max = as.character(today()), test_name = c("PHOSPHATE, DISSOLVED AS P", "TOTAL SUSPENDED SOLIDS","PHOSPHATE, TOTAL AS P","PHOSPHATE, ORTHO AS P","TURBIDITY"))
