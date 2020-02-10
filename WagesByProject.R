#Author: James Cronan
#Created on: 10-Feb-2020
#Location: USDA, Forest Service, PNW Resarch Station, Pacific Wildland Fire Sciences Lab, 400 N 34th Street, Suite 201, Seattle, WA 98103
#Purpose: To divide hours downloaded from Google Calendar through GTimeReport by project billing code for contracted employees.

#Open Data
setwd("C:/Users/jcronan/Documents/GitHub/WageBilling/2018")
raw.data <- read.table("Time report 2018-01-01 - 2018-12-31.csv", header = F, fill = T, skip = 1, sep = ",")
str(raw.data)
head(raw.data)



