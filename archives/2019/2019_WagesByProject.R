#Author: James Cronan
#Created on: 26-Oct-2020
#Location: USDA, Forest Service, PNW Resarch Station, Pacific Wildland Fire Sciences Lab, 400 N 34th Street, Suite 201, Seattle, WA 98103
#Purpose: To divide hours downloaded from Google Calendar through GTimeReport by project billing code for contracted employees.

#Reset functions
rm(list=ls())
dev.off()

#Libraries
library(dplyr)
library(tidyr)
library(stringr)
library(rlang)
#library(Rfast) - there is a bug if this library is open at the same time as dplyr. RStudio will hang if functions to view
#object details (e.g., head, length, str) are run while Rfast and dplyr are open at the same time. There is also significant
#CPU ~ 30%.

#Open Data
setwd("C:/Users/jcronan/Documents/GitHub/WageBilling/2019")
raw.data <- read.table("Time report 2019-01-01 - 2019-12-31.csv", header = F, fill = T, skip = 1, sep = ",", quote = "\"")
codes <- read.table("projectCodes_2019.csv", header = F, fill = T, skip = 1, sep = ",")

#Review Data
str(raw.data)
head(raw.data)

#Create new data frame with the following fields:
#1) Last name
#2) Project Code
#3) Date
#4) Duration

#Generate a vector that converts factor levels of dates into date values
date <- as.Date(raw.data$V4, format = "%m/%d/%Y")

#Generate a vector that converts dates into work weeks
week <- as.numeric(strftime(date, format = "%U"))

#Convert column with names and job codes into a character string
col2.1 <- as.character(raw.data$V2)

#Create a vector that lists possible last names.
last.names <- c("Cronan", "Restaino", "Eagle", "Lascheck", "Tripodi", "Thoreson", "Nemens", "Darmody", "Hallet")

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

#Show missing last names
test.names <- data.frame(raw = raw.data$V2, new = lastName.List)
test.names[test.names$new == "",]#enter all versions of incorrectly recorded last names below
#No incorrectly spelled last names, all entries with "" are events. These should be removed.

#Remove entries that are not staff time records.
remove_these_rows <- which(test.names$new == "",)
raw.data2 <- raw.data[-remove_these_rows,]
col2.2 <- as.character(raw.data2$V2)
lastName.List2 <- character(length = length(col2.2))
for(i in 1:length(last.names))
{ 
  for(v in 1:length(col2.2))
  {
    if(is_empty(unlist(str_extract_all(col2.2[v], last.names[i]))) == T)
    {lastName.List2[v] <- lastName.List2[v]} else
    {lastName.List2[v] <- unlist(str_extract_all(col2.2[v], last.names[i]))}
  }
}
test.names2 <- data.frame(raw = raw.data2$V2, new = lastName.List2)
tr <- test.names2[test.names2$new == "",]
tr #There should be no data here. If there are, then the scripts above did not do their job cleaning out calendar entries that are not hour entries.

#Generate a vector that converts factor levels of dates into date values
date2 <- as.Date(raw.data2$V4, format = "%m/%d/%Y")

#Generate a vector that converts dates into work weeks
week2 <- as.numeric(strftime(date2, format = "%U"))

#If there are rows with mispelled names fix them
if(nrow(tr) == 0)
{
  
} else
{
  #Fix missing last
  #Create a list of incorrectly recorded last names.
  incorrect.names <- c("incorrectly spelled name")
  
  #Create correspnding list of correct codes.
  #Number of elements should corresond with above vector.
  #Enter correctly spelled version of name or "DELETE" if entry is not associated with staff time and 
  #was not included in the "remove_these_rows vector above.
  correct.names <- c("Restaino", "Eagle", "DELETE", "DELETE")

  #Check character strings for incorrect codes and add correct codes to list.
  for(i in 1:length(incorrect.names))
    { 
    for(v in 1:length(col2.1))
      {
      if(is_empty(unlist(str_extract_all(col2.1[v], incorrect.names[i]))) == T)
      {lastName.List[v] <- lastName.List[v]} else
      {lastName.List[v] <- correct.names[unlist(str_extract_all(col2.1[v], incorrect.names[i])) == incorrect.names]}
    }
  }
}

#Show missing last names
test.names <- data.frame(raw = raw.data2$V2, new = lastName.List2)
test.names[test.names$new == "",]#there should not be any

#Delete rows that do not contain hours worked
rd_remove_bad_names <- raw.data2[!(lastName.List == "DELETE") == T,]
rd_remove_bad_names <- raw.data2

#Confirm that rows to be deleted were removed
#Lengths should be equal:
length(raw.data2[,1])
length(test.names[test.names$new == "DELETE",][,1])+length(rd_remove_bad_names$V2)

#Redo project by person list
col2.3 <- as.character(rd_remove_bad_names$V2)

#Create a vector that lists possible project codes.
project.codes <- c("ADM", "LVE", "3DF", "FLB", "JMV", "WWC", "FCM", "FCC", "TMS", "LEF", 
                   "UAV", "RBP", "MJO", "SMC", "PSP", "RUD", "EEF", "DUL", "ROG", "PFS")

#Create a vector store project codes.
projectCode.List <- character(length = length(col2.3))

#Check character strings for last names and sort out from other text.
for(i in 1:length(project.codes))
{ 
  for(v in 1:length(col2.3))
  {
    if(is_empty(unlist(str_extract_all(col2.3[v], project.codes[i]))) == T)
    {projectCode.List[v] <- projectCode.List[v]} else
    {projectCode.List[v] <- unlist(str_extract_all(col2.3[v], project.codes[i]))}
  }
}

#Identify billing codes that could not be identified.
test <- cbind(col2.3, projectCode.List)
test[test[,2] == "",]


#Fix missing codes
#Create a list of incorrectly recorded project codes.
incorrect.codes <- c("ROV", "Lab", "Field", "Lve", "Rx-410", "TRN", "Training", "Portland", "S/L", "Fire Refresher/WCT", "Utah", "PERSONAL DAY", 
                     "Sick Leave", "6th Fire Behavior and Fuels Conference", "Site Visit", "JVM", "Travel", "Work at Home")

#Create correspnding list of correct codes.
correct.codes <- c("ROG", "ADM", "ADM", "LVE", "ADM", "ADM", "ADM", "ADM", "LVE", "ADM", "FLB", "ADM", "LVE", "ADM", "ADM", "JMV", "ADM", "ADM")

#Check character strings for incorrect codes and add correct codes to list.
for(i in 1:length(incorrect.codes))
{ 
  for(v in 1:length(col2.2))
  {
    if(is_empty(unlist(str_extract_all(col2.2[v], incorrect.codes[i]))) == T)
    {projectCode.List[v] <- projectCode.List[v]} else
    {projectCode.List[v] <- correct.codes[unlist(str_extract_all(col2.2[v], incorrect.codes[i])) == incorrect.codes]}
  }
}

#Add the admin code (ADM) for all entries that do not list a code
projectCode.List[projectCode.List == ""] <- "ADM"

#Identify billing codes that could not be identified.
test <- cbind(col2.2, projectCode.List)
test[test[,2] == "",]

#Calculate duration
col3.1 <- strsplit(as.character(raw.data2$V6), ":")
col3.1_hrs <- as.numeric(unlist(lapply(col3.1, function(x) x[1])))
col3.1_mins <- as.numeric(unlist(lapply(col3.1, function(x) x[2])))/60
duration <- round(col3.1_hrs + col3.1_mins,2)


#Create a data frame
cal_d <- as.character(unlist(lapply(raw.data2$V4, function(x) x[1])))
s_time <- as.character(unlist(lapply(raw.data2$V5, function(x) x[1])))

billing <- data.frame(last_name = lastName.List2, 
                      project_code = projectCode.List, 
                      date = cal_d, 
                      start_time = s_time, 
                      duration = duration, 
                      week = week2, stringsAsFactors = F)

#Summarize hours by week to ID OT
billing_by_week <- as.data.frame(billing %>% 
                                 group_by(week, last_name) %>%
                                 summarise(sum = sum(duration)))

#Isolate OT hours
ot <- billing_by_week$sum - 40
ot[ot <= 0] <- 0

#Isolate base hours
base <- billing_by_week$sum
base[base > 40] <- 40

#Create a table that shows hours worked per week by budget code and by person. This is needed to identify
#the project code with the largest amount of hours which in turn will be used to assign overtime.
code.name <- expand.grid(project.codes, last.names)
lastName_vec <- as.character(unlist(lapply(code.name[,2], function(x) x[1])))
projectCode_vec <- as.character(unlist(lapply(code.name[,1], function(x) x[1])))
week_number <- sort(unique(week2))
time_by_week <- data.frame(lastName = lastName_vec, 
                           projectCode = projectCode_vec, 
                           matrix(data = NA, nrow = length(code.name[,1]), ncol = length(week_number)), 
                           stringsAsFactors = F)

#Summarise hours by day into a table that shows hours by week grouped by employee and project along rows and
#by columns that represent work weeks (Monday - Sunday)
for(a in 1:length(week_number))
{
  for(b in 1:length(time_by_week[,1]))
  {
    time_by_week[b,(a+2)] <- sum(billing$duration[billing$week == week_number[a]
                                                 & billing$last_name == time_by_week$lastName[b] 
                                                 & billing$project_code == time_by_week$projectCode[b]])
  }
}

######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
#                                                       NEMENS
#Deborah submitted most of her 2019 hours on a spreadhseet with a much different format than the Google hours spreadhseet.
#This section will format the info so it can be merged with the rest of the hours in the time_by_week data frame
#This is the most logical place to merge Deborah's hours for September-December.
nemens <- read.table("Nemens_2019_mod.csv", header = T, fill = T, skip = 0, sep = ",", quote = "\"")

#Generate a vector that converts factor levels of dates into date values
nemens.date <- as.Date(nemens$Date, format = "%m/%d/%Y")

#Generate a vector that converts dates into work weeks
nemens.week <- as.numeric(strftime(nemens.date, format = "%U"))

#Strip out project codes from column namnes
pcn.1 <- colnames(nemens)[2:length(nemens[1,])]
pcn.2 <- strsplit(pcn.1, "X")
pcn.3 <- vector()
for(i in 1:length(pcn.1))
{
  if(length(pcn.2[[i]]) > 1)
  {
    pcn.3[i] <- pcn.2[[i]][2]
  } else
  {
    pcn.3[i] <- pcn.2[[i]][1]
  }
}

#Merge Nemens 2019 additional data with everyone else's data.
nwl <- sort(unique(nemens.week))
for(i in nwl)
{
  for(z in 1:length(pcn.3))
  {
    time_by_week[time_by_week$lastName == "Nemens" & time_by_week$projectCode == pcn.3[z], 2+(i+1)] <- sum(nemens[nemens.week == i, 1+z], na.rm = T)
  }
}

#                                                       END
######################################################################################################################
######################################################################################################################
######################################################################################################################

#Calculate total hours worked per week and overtime hours worked per week.
#Calculate project codes billed each week (group by employee) and the number of hours attributed to them. Codes are
#ordered from project in order from most to least amount of hours followed by administrative codes (ADM and LVE).
#This is done in order to determine how to allocate overtime hours. Assign OT hours to project code with most amount
#of hours first and assign to codes in decreasing order. If OT hours still remain after assigning to billable
#codes overtime will be assigned to ADM and finally LVE. However, if OT is being assigned to LVE times were
#probably incorrectly entered in Google Calendar.
OT_Codes <- list()
OT_Code_Hours <- list()
Total_Hours <- vector(mode = 'numeric')
OT_Hours <- vector(mode = 'numeric')
for(b in 1:length(last.names))
  {
  for(c in 1:length(week_number))
    {
    unordered_codes <- time_by_week[,c(2,(2+c))][time_by_week$lastName == last.names[b],]
    ordered_codes <- unordered_codes[order(unordered_codes[,2], decreasing = T),]
    reduced_codes <- ordered_codes[ordered_codes$projectCode %in% c("ADM", "LVE") == F & ordered_codes[,2] > 0,]
    reordered_codes <- rbind(reduced_codes, 
                             ordered_codes[ordered_codes$projectCode %in% c("ADM", "LVE") == T & ordered_codes[,2] > 0,])
    OT_Codes[[length(OT_Codes) + 1]] <- reordered_codes[,1]
    OT_Code_Hours[[length(OT_Code_Hours) + 1]] <- reordered_codes[,2]
    Total_Hours[length(Total_Hours) + 1] <- sum(time_by_week[,(2+c)][time_by_week$lastName == last.names[b]])
    if(sum(time_by_week[,(2+c)][time_by_week$lastName == last.names[b]]) > 40)
    {
      OT_Hours[length(OT_Hours) + 1] <- sum(time_by_week[,(2+c)][time_by_week$lastName == last.names[b]]) - 40
    } else
    {
      OT_Hours[length(OT_Hours) + 1] <- 0
    }
  }
}

#Measure the number of projects billed for each week and identify week with largest number of projects billed.
code_length <- vector(mode = 'numeric')
for(i in 1:length(OT_Codes))
{
  code_length[length(code_length) + 1] <- length(OT_Codes[[i]])
}
max_codes <- max(code_length)

#Create a list of OT codes by week with the same number of elements for each week.
OT_Codes_FullCount <- list()
for(i in 1:length(OT_Codes))
  {
  OT_Codes_FullCount[[i]] <- c(OT_Codes[[i]], rep("--", (max_codes - length(OT_Codes[[i]]))))
  }
otcMatrix <- matrix(data = unlist(OT_Codes_FullCount),nrow = (length(last.names) * length(week_number)), ncol = max_codes, byrow = T)

#Create a corresponding list of OT hours by week with the same number of elements for each week.
OT_CodeHrs_FullCount <- list()
for(i in 1:length(OT_Code_Hours))
{
  OT_CodeHrs_FullCount[[i]] <- c(OT_Code_Hours[[i]], rep(0, (max_codes - length(OT_Code_Hours[[i]]))))
}
othMatrix <- matrix(data = unlist(OT_CodeHrs_FullCount),nrow = (length(last.names) * length(week_number)), ncol = max_codes, byrow = T)

#Create a data frame that shows details of hours worked grouped by employee and week.
#Table shows
#1: Total hours and overtime hours
#2: Project codes used that week
#3: Hours attributed to each project code
#4: Base (0-40) hours for each of the 20 project codes
weekXnames <- expand.grid(week_number,last.names)
row_stop <- length(last.names)*20
col_stop <- length(time_by_week[1,])
wage_table <- data.frame(LastName = weekXnames[,2], 
                         Week_No = weekXnames[,1], 
                         Total_Hrs = Total_Hours,
                         OT_Hours = OT_Hours,
                         otcMatrix,
                         othMatrix, 
                         as.vector(unlist(t(time_by_week[seq(1,row_stop,20),3:col_stop]))),
                         as.vector(unlist(t(time_by_week[seq(2,row_stop,20),3:col_stop]))),
                         as.vector(unlist(t(time_by_week[seq(3,row_stop,20),3:col_stop]))),
                         as.vector(unlist(t(time_by_week[seq(4,row_stop,20),3:col_stop]))),
                         as.vector(unlist(t(time_by_week[seq(5,row_stop,20),3:col_stop]))),
                         as.vector(unlist(t(time_by_week[seq(6,row_stop,20),3:col_stop]))),
                         as.vector(unlist(t(time_by_week[seq(7,row_stop,20),3:col_stop]))),
                         as.vector(unlist(t(time_by_week[seq(8,row_stop,20),3:col_stop]))),
                         as.vector(unlist(t(time_by_week[seq(9,row_stop,20),3:col_stop]))),
                         as.vector(unlist(t(time_by_week[seq(10,row_stop,20),3:col_stop]))),
                         as.vector(unlist(t(time_by_week[seq(11,row_stop,20),3:col_stop]))),
                         as.vector(unlist(t(time_by_week[seq(12,row_stop,20),3:col_stop]))),
                         as.vector(unlist(t(time_by_week[seq(13,row_stop,20),3:col_stop]))),
                         as.vector(unlist(t(time_by_week[seq(14,row_stop,20),3:col_stop]))),
                         as.vector(unlist(t(time_by_week[seq(15,row_stop,20),3:col_stop]))),
                         as.vector(unlist(t(time_by_week[seq(16,row_stop,20),3:col_stop]))),
                         as.vector(unlist(t(time_by_week[seq(17,row_stop,20),3:col_stop]))),
                         as.vector(unlist(t(time_by_week[seq(18,row_stop,20),3:col_stop]))),
                         as.vector(unlist(t(time_by_week[seq(19,row_stop,20),3:col_stop]))),
                         as.vector(unlist(t(time_by_week[seq(20,row_stop,20),3:col_stop]))))
names(wage_table)[5:((max_codes*2)+(5-1))] <- c(paste0("pc", seq(1,max_codes,1)),
                                                paste0("ph", seq(1,max_codes,1)))
base_cols <- (5+(max_codes*2)):(((5-1)+(max_codes*2))+length(project.codes))
names(wage_table)[base_cols] <- project.codes

#Create a matrix with employee/week (rows) and project codes (column to hold OT hours)
#This corresponds with the fourth section in wage_table.
rwt <- wage_table
ot_matrix <- matrix(nrow = (length(last.names) * length(week_number)), ncol = length(project.codes))
colnames(ot_matrix) <- project.codes

#Loops assigns overtime hours by project codes and runs for each project within a given week before advancing to the next week.
#OT hours are assigned to the project with the largest amount of hours first and remaining OT hours are assigned to subsequent
#projects in decreasing order of hours billed to project. Finally non-billable project codes (ADM and then LVE) are assigned
#OT hours if any remain.
for(i in 1:length(rwt[,1]))
{
  if(rwt$OT_Hours[i] == 0){} else
  {
    remainder <- rwt$OT_Hours[i]
    for(z in 1:max_codes)
      {
      ot_matrix[i, colnames(ot_matrix) == rwt[i,4+z]] <- min(rwt[i, colnames(rwt) == rwt[i,4+z]], rwt$OT_Hours[i], remainder)
      rwt[i, colnames(rwt) == rwt[i,4+z]] <- rwt[i, colnames(rwt) == rwt[i,4+z]] - ot_matrix[i, colnames(ot_matrix) == rwt[i,4+z]]
      remainder <- remainder - as.vector(ot_matrix[i, colnames(ot_matrix) == rwt[i,4+z]])
      if(remainder <= 0){
        break
        }
    }
    rm(remainder)
  }
  }

#Add table with overtime hours by project to wage_table
rwta <- cbind(rwt, ot_matrix)
ot_cols <- (dim(rwt)[2]+1):(dim(rwt)[2]+length(project.codes))
rwta[,ot_cols][is.na(rwta[,ot_cols]) == T] <- 0
check_base <- apply(rwta[,ot_cols], 1, sum)
check_ot <- apply(rwta[,ot_cols], 1, sum)
check_summary <- data.frame(base_hrs = (rwta$Total_Hrs-rwta$OT_Hours), check_base = check_base, ot_hrs = rwta$OT_Hours, check_ot = check_ot)
sort(unique(check_summary$base_hrs - check_summary$check_base))
sort(unique(check_summary$ot_hrs - check_summary$check_ot))
which(check_summary$base_hrs != check_summary$check_base)

#Multiply OT hours by 1.5
#For representation by wage type (base, OT, leave)
rwta_ot_type <- rwta[,c(ot_cols[1], ot_cols[3]:max(ot_cols))]
rwta_base_type <- rwta[,c(base_cols[1], base_cols[3]:max(base_cols))]
rwta_leave_type <- rwta[,c(base_cols[2],ot_cols[2])]

#For representation by PI
rwta_ot_pi <- rwta[,ot_cols] * 1.5
rwta_base_pi <- rwta[,base_cols]

#Calculate total hours per week (grouped by employee) with OT hours multiplied by 1.5
#This can be multiplied by the employees wage to determine cost per week.
wage_hours <- matrix(data = 0, nrow = length(rwta_base_pi[,1]), ncol = length(rwta_base_pi[1,]))
for(i in 1:length(rwta_ot_pi[1,]))
{
  wage_hours[,i] <- rwta_base_pi[,i] + rwta_ot_pi[,i]
}
colnames(wage_hours) <- colnames(rwta_base_pi)

#Summarize hours by owner, rather than project code.
owner_hours <- matrix(nrow = length(wage_hours[,1]), ncol = length(unique(codes$V3)))
owners <- sort(unique(codes$V3))
for(i in 1:length(unique(codes$V3)))
{
  colnos <- which(is.na(match(colnames(wage_hours), codes$V1[codes$V3 == owners[i]])) == F)
  colnos <- colnos[!is.na(colnos) == T]
  if(length(colnos) == 0)
  {
    owner_hours[,i] <- 0
  } else
  {
    if(length(colnos) == 1)
    {
      owner_hours[,i] <- wage_hours[,colnos]
    } else
    {
      owner_hours[,i] <- apply(wage_hours[,colnos], 1, sum)
    }
  }
  }

#Create a new data frame
owner_by_week <- cbind.data.frame(as.vector(rwta$LastName), owner_hours)
colnames(owner_by_week) <- c(colnames(rwta)[1], as.character(owners))

#Summarize hours worked by last name and budget owner.
owner_summary <- data.frame(owner_by_week %>%
                                group_by(LastName) %>%
                                summarise_all(list(sum)))

#Calculate base hours per week by employee
total_base <- apply(rwta_base_type, 1, sum)

#Calculate paid leave hours per week by employee
total_leave <- apply(rwta_leave_type, 1, sum)

#Calculate ot hours per week by employee
total_ot <- apply(rwta_ot_type, 1, sum)

#I need dates for week numbers
wn <- strftime(date, format = "%U")
wn1 <- sort(unique(wn))
wn2a1 <- as.Date(paste(2019, sort(unique(week2)), 7, sep="-"), "%Y-%U-%u")
wn2a2 <- wn2a1[!is.na(wn2a1)]
wn2b <- as.Date(paste(2020, 1, 7, sep="-"), "%Y-%U-%u")
wn2 <- c(wn2a2, wn2b)
wn3 <- substring(wn2,6)


#Barplot of hours by week for each UW field staff employee
dev.off()
par(mfrow=c(4,2), mar = c(4, 4, 4, 2))
cf <- 0.5
ln <- "Nemens"
barplot(cbind(total_leave[rwta$LastName == ln], total_base[rwta$LastName == ln], total_ot[rwta$LastName == ln]) ~ wn3, 
        las = 2, xlab ="", ylim = c(0,90), col = gray((1:3)/4), main = ln)
ln <- "Restaino"
barplot(cbind(total_leave[rwta$LastName == ln], total_base[rwta$LastName == ln], total_ot[rwta$LastName == ln]) ~ wn3, 
        las = 2, xlab ="", ylim = c(0,90), col = gray((1:3)/4), main = ln)
ln <- "Thoreson"
barplot(cbind(total_leave[rwta$LastName == ln], total_base[rwta$LastName == ln], total_ot[rwta$LastName == ln]) ~ wn3, 
        las = 2, xlab ="", ylim = c(0,90), col = gray((1:3)/4), main = ln)
ln <- "Hallet"
barplot(cbind(total_leave[rwta$LastName == ln], total_base[rwta$LastName == ln], total_ot[rwta$LastName == ln]) ~ wn3, 
        las = 2, xlab ="", ylim = c(0,90), col = gray((1:3)/4), main = ln)
ln <- "Tripodi"
barplot(cbind(total_leave[rwta$LastName == "Hallet"], total_base[rwta$LastName == "Hallet"], total_ot[rwta$LastName == "Hallet"]) ~ wn3, 
        las = 2, xlab ="", ylim = c(0,90), col = gray((1:3)/4), main = ln)
ln <- "Darmody"
barplot(cbind(total_leave[rwta$LastName == ln], total_base[rwta$LastName == ln], total_ot[rwta$LastName == ln]) ~ wn3, 
        las = 2, xlab ="", ylim = c(0,90), col = gray((1:3)/4), main = ln)
ln <- "Lascheck"
barplot(cbind(total_leave[rwta$LastName == ln], total_base[rwta$LastName == ln], total_ot[rwta$LastName == ln]) ~ wn3, 
        las = 2, xlab ="", ylim = c(0,90), col = gray((1:3)/4), main = ln)
#legend(1,80, c("Paid Leave", "Base", "OT"), fill = gray((1:3)/4))

#Save 
write.csv(owner_summary, file = "summary_byOwner_2019.csv")

#Calculate salary allocations as a percentage
total_hrs <- apply(owner_summary[,2:9], 1, sum)
percent_by_owner <- round(((owner_summary[,2:9]/total_hrs)*100),1)
row.names(percent_by_owner) <- owner_summary$LastName
percent_by_owner


