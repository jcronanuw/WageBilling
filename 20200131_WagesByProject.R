#Author: James Cronan
#Created on: 10-Feb-2020
#Location: USDA, Forest Service, PNW Resarch Station, Pacific Wildland Fire Sciences Lab, 400 N 34th Street, Suite 201, Seattle, WA 98103
#Purpose: To divide hours downloaded from Google Calendar through GTimeReport by project billing code for contracted employees.

#Reset functions
rm(list=ls())
dev.off()

#Libraries
library(tidyr)
library(dplyr)
library(stringr)
library(rlang)

#Open Data
setwd("C:/Users/jcronan/Documents/GitHub/WageBilling/2020")
raw.data <- read.table("Time report 2020-01-01 - 2020-01-31.csv", header = F, fill = T, skip = 1, sep = ",")

#Review Data
str(raw.data)
head(raw.data)

#Create new data frame with the following fields:
#1) Last name
#2) Project Code
#3) Date
#4) Duration

#Convert column with names and job codes into a character string
col2.1 <- as.character(raw.data$V2)

#Create a vector that lists possible last names.
last.names <- c("Cronan", "Darmody", "Hallet", "Lascheck", "Nemens", "Thoreson", "Tripodi")

#Create a vector store last names.
lastName.List <- character(length = length(col2.1))

#Check character strings for last names and sort out from other text.
for(i in 1:length(last.names))
  { 
  for(v in 1:length(col2.1))
       {
         if(is_empty(unlist(str_extract_all(col2.1[v], last.names[i]))) == T)
         {lastName.List[v] <- lastName.List[v]} else
         {lastName.List[v] <- unlist(str_extract_all(col2.1[v], last.names[i]))}
  }
}

#Create a vector that lists possible project codes.
project.codes <- c("ADM", "LVE", "3DF", "FLB")

#Create a vector store last names.
projectCode.List <- character(length = length(col2.1))

#Check character strings for last names and sort out from other text.
for(i in 1:length(project.codes))
{ 
  for(v in 1:length(col2.1))
  {
    if(is_empty(unlist(str_extract_all(col2.1[v], project.codes[i]))) == T)
    {projectCode.List[v] <- projectCode.List[v]} else
    {projectCode.List[v] <- unlist(str_extract_all(col2.1[v], project.codes[i]))}
  }
}

#Fix missing codes
#Create a list of incorrectly recorded project codes.
incorrect.codes <- c("3D", "Taylor", "Holiday", "FSB")

#Create correspnding list of correct codes.
correct.codes <- c("3DF", "WWC", "LVE", "FLB")

#Check character strings for incorrect codes and add correct codes to list.
for(i in 1:length(incorrect.codes))
{ 
  for(v in 1:length(col2.1))
  {
    if(is_empty(unlist(str_extract_all(col2.1[v], incorrect.codes[i]))) == T)
    {projectCode.List[v] <- projectCode.List[v]} else
    {projectCode.List[v] <- correct.codes[unlist(str_extract_all(col2.1[v], incorrect.codes[i])) == incorrect.codes]}
  }
}

#Calculate duration worked for each project by person


#Calculate duration
col3.1 <- strsplit(as.character(raw.data$V6), ":")
col3.1_hrs <- as.numeric(unlist(lapply(col3.1, function(x) x[1])))
col3.1_mins <- as.numeric(unlist(lapply(col3.1, function(x) x[2])))/60
duration <- round(col3.1_hrs + col3.1_mins,2)


#Create a data frame
billing <- data.frame(last_name = lastName.List, project_code = projectCode.List, date = raw.data$V4, 
                      start_time = raw.data$V5, duration = duration)

billing %>%
  group_by(last_name, project_code) %>%
  summarise(sum = sum(duration), n = n())


mapply(billing, sum())
summarize(billing$duration, sum(), by = billing$last_name)

tt <- as.character(billing$duration)
as.numeric(tt)






