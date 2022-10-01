#Author: James Cronan
#Created on: 02-Feb-2021
#Location: USDA, Forest Service, PNW Resarch Station, Pacific Wildland Fire Sciences Lab, 400 N 34th Street, Suite 201, Seattle, WA 98103
#Purpose: To calculate billing hours for field staff.

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
library(googledrive)#used to access files in Google Drive
library(xlsx)
library(sjmisc)#str_find
library(readxl)#needed to import reporting_period file. Date format will be altered to default (mm/dd/yyyy) if csv file is used and needs to be in
#yyyy-mm-dd format for as.Date function to interpret

#Set working directory
setwd("C:/Users/jcronan/OneDrive - USDA/Documents/GitHub/WageBilling/input_files/temp_downloads/")

#Delete existing files from temp downloads folder
temp = list.files(pattern="*.csv")
if (file.exists(temp)) {
  unlink(temp)
  cat("The files are deleted")
}

#Drive authorization -- provide authorization to access Google Drive
drive_auth()
1#jcronan@uw.edu

#Boyd
#Copy link to file into an object
target <- drive_get("https://docs.google.com/spreadsheets/d/167Lo-bjcP_ErkpbdnRIDvZpgWzmtrCFT/edit#gid=1715091972")
#Download a copy of the fuel moisture file
drive_download(target, path = "C:/Users/jcronan/OneDrive - USDA/Documents/GitHub/WageBilling/input_files/temp_downloads/boyd.csv", overwrite = T)

#Cronan
#Copy link to file into an object
target <- drive_get("https://docs.google.com/spreadsheets/d/1uhUgwtkgGyNQDjj2l-CqigCDhCUqiveU/edit#gid=2107396445")
#Download a copy of the fuel moisture file
drive_download(target, path = "C:/Users/jcronan/OneDrive - USDA/Documents/GitHub/WageBilling/input_files/temp_downloads/cronan.csv", overwrite = T)

#Nemens
#Copy link to file into an object
target <- drive_get("https://docs.google.com/spreadsheets/d/16GjdxU5lGuD21wwAbd-feirqQmXAGG7C/edit#gid=198112230")
#Download a copy of the fuel moisture file
drive_download(target, path = "C:/Users/jcronan/OneDrive - USDA/Documents/GitHub/WageBilling/input_files/temp_downloads/nemens.csv", overwrite = T)

#Patterson
#Copy link to file into an object
target <- drive_get("https://docs.google.com/spreadsheets/d/1u1Eu6CI7uBB4G2RveEMoppvgkOFYie__/edit#gid=2107396445")
#Download a copy of the fuel moisture file
drive_download(target, path = "C:/Users/jcronan/OneDrive - USDA/Documents/GitHub/WageBilling/input_files/temp_downloads/patterson.csv", overwrite = T)

#Swan-Streepy
#Copy link to file into an object
target <- drive_get("https://docs.google.com/spreadsheets/d/1ivGAwCtjUPk-GfoFI33ryWItOEfXOTWj/edit#gid=1715091972")
#Download a copy of the fuel moisture file
drive_download(target, path = "C:/Users/jcronan/OneDrive - USDA/Documents/GitHub/WageBilling/input_files/temp_downloads/swan.csv", overwrite = T)

#Open files you just downloaded.
temp = list.files(pattern="*.csv")
temp_names <- substring(temp,1, nchar(temp)-4)
for (i in 1:length(temp)) 
  {
  assign(temp_names[i], read.xlsx(temp[i], sheetIndex = 1, startRow = 2, header = T))
}

file_names <- objects()
file_names <- file_names[!file_names %in% c("file_names", "temp", "i", "target", "temp_names")]

#Set working drive
setwd("C:/Users/jcronan/OneDrive - USDA/Documents/GitHub/WageBilling/input_files/")
codes <- read.table("project_codes.csv", header = T, fill = T, skip = 0, sep = ",")
period_dates_as_factors <- read.table("reporting_period.txt", header = T, fill = T, skip = 0, sep = ",")
period <- data.frame(employer = period_dates_as_factors$employer, 
                     start_date = as.Date(period_dates_as_factors$start_date),
                     end_date = as.Date(period_dates_as_factors$end_date))
staff_list <- read.table("staff_list.csv", header = T, fill = T, skip = 0, sep = ",")
staff_list <- staff_list[order(staff_list$last_name),]#needs to be ordered by last name or hours will be assigned to wrong person.
staff_wages <- read.table("staff_wages.csv", header = T, fill = T, skip = 0, sep = ",")
###########################################################################################################################################
###########################################################################################################################################
###########################################################################################################################################
###########################################################################################################################################
#
#                                                    UNIVERSITY OF WASHINGTON
#
empl <- "uw"
###########################################################################################################################################
###########################################################################################################################################
###########################################################################################################################################
###########################################################################################################################################

#Strip unused columns
start.col <- vector()
end.col <- vector()
staff <- list()
code_test_list <- vector()
for(i in 1:length(file_names))
{
  start.col[i] <- which(colnames(get(file_names[i])) %in% "Date")
  end.col[i] <- which(colnames(get(file_names[i])) %in% "Check.with.Jim.or.Deborah.before.adding.new.codes.")
  temp.1 <- get(file_names[i])[,-c(1:(start.col[i]-1),end.col[i]:length(colnames(get(file_names[i]))))]
  temp.2 <- temp.1[temp.1$Date >= period$start_date[period$employer == empl] & temp.1$Date <= period$end_date[period$employer == empl],]
    if(length(which(is.na(temp.2$Date)==T)) == 0)#if statement needed b/c if test result is zero then all valid dates will be dropped.
      {
      temp.3 <- temp.2
      } else
        {
          temp.3 <- temp.2[-which(is.na(temp.2$Date)==T),]
        }
  
  staff[[i]] <- temp.3 
  code_test_list <- c(code_test_list, colnames(temp.3)[colnames(temp.3) != "Date"])
}

#For some reason some columns may be imported as character values despite being classified as numeric in .csv file.
#This loop find them and converts them to numeric.
for(i in 1:length(file_names))
{
  for(a in 2:length(colnames(staff[[i]])))
  {
    if(is.numeric(staff[[i]][,a]) == T)
    {
      staff[[i]][,a] <- staff[[i]][,a]
    } else
    {
      staff[[i]][,a] <- as.numeric(staff[[i]][,a])
    }
  }
}

#Run script for University of Washington employees (work week = Monday - Sunday)
staff_uw <- staff[staff_list$employer == empl]
employees_uw <- staff_list$last_name[staff_list$employer == empl]

#Generate a data frame that uses time storage format used by previous years'scripts (so I don't have to re-write code
#that allocates OT hours to project.)
#rows = employee x project code
#cols = week number
if(length(unique(as.numeric(strftime(staff_uw[[1]]$Date, format = "%Y")))) == 1)
{
  week <- as.numeric(strftime(staff_uw[[1]]$Date, format = "%W"))
  week_number <- sort(unique(week))
    } else
  {
    week <- as.numeric(strftime(staff_uw[[1]]$Date, format = "%W"))
    week_number <- sort(unique(week))
    past_year_max <- max(week_number)#used in final section of this script.
    week[week == max(week_number)] <- min(week_number)
    week_number <- week_number[-which(week_number == max(week_number))]
    }
code_test_list <- sort(unique(code_test_list))
code_name_rows <- expand.grid(code_test_list, employees_uw)
time_by_week <- data.frame(lastName = as.character(code_name_rows$Var2), 
                           projectCode = as.character(code_name_rows$Var1), 
                           matrix(data = NA, nrow = length(code_name_rows$Var1), ncol = length(week_number)), 
                           stringsAsFactors = F)

#Summarize data into data frame.
for(i in 1:length(staff_uw))#6
  {
  for(a in 1:length(code_test_list))#16
    {
    for(b in 1:length(week_number))#6
      {
      temp.a <- colnames(staff_uw[[i]]) == code_test_list[a]
      if(length(temp.a[temp.a == T]) == 0)
      {
        time_by_week[time_by_week$lastName == employees_uw[i] & time_by_week$projectCode == code_test_list[a],b + 2] <- 0
      } else
      {
      time_by_week[time_by_week$lastName == employees_uw[i] & time_by_week$projectCode == code_test_list[a],b + 2] <- 
        sum(staff_uw[[i]][week == week_number[b], colnames(staff_uw[[i]]) == code_test_list[a]], na.rm = T)
      }
    }
  }
}

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
for(b in 1:length(employees_uw))
  {
  for(c in 1:length(week_number))
    {
    unordered_codes <- time_by_week[,c(2,(2+c))][time_by_week$lastName == employees_uw[b],]
    ordered_codes <- unordered_codes[order(unordered_codes[,2], decreasing = T),]
    reduced_codes <- ordered_codes[ordered_codes$projectCode %in% c("ADM", "LVE") == F & ordered_codes[,2] > 0,]
    reordered_codes <- rbind(reduced_codes, 
                             ordered_codes[ordered_codes$projectCode %in% c("ADM", "LVE") == T & ordered_codes[,2] > 0,])
    OT_Codes[[length(OT_Codes) + 1]] <- reordered_codes[,1]
    OT_Code_Hours[[length(OT_Code_Hours) + 1]] <- reordered_codes[,2]
    Total_Hours[length(Total_Hours) + 1] <- sum(time_by_week[,(2+c)][time_by_week$lastName == employees_uw[b]])
    if(sum(time_by_week[,(2+c)][time_by_week$lastName == employees_uw[b]]) > 40)
    {
      OT_Hours[length(OT_Hours) + 1] <- sum(time_by_week[,(2+c)][time_by_week$lastName == employees_uw[b]]) - 40
    } else
    {
      OT_Hours[length(OT_Hours) + 1] <- 0
    }
  }
}

#Measure the number of projects billed for each week and identify week with largest number of projects billed.
#code_length <- vector(mode = 'numeric')
#for(i in 1:length(OT_Codes))
#{
#  code_length[length(code_length) + 1] <- length(OT_Codes[[i]])
#}
#max_codes <- max(code_length)
max_codes <- length(code_test_list)

#Create a list of OT codes by week with the same number of elements for each week.
OT_Codes_FullCount <- list()
for(i in 1:length(OT_Codes))
  {
  OT_Codes_FullCount[[i]] <- c(OT_Codes[[i]], rep("--", (max_codes - length(OT_Codes[[i]]))))
  }
otcMatrix <- matrix(data = unlist(OT_Codes_FullCount),nrow = (length(employees_uw) * length(week_number)), ncol = max_codes, byrow = T)

#Create a corresponding list of OT hours by week with the same number of elements for each week.
OT_CodeHrs_FullCount <- list()
for(i in 1:length(OT_Code_Hours))
{
  OT_CodeHrs_FullCount[[i]] <- c(OT_Code_Hours[[i]], rep(0, (max_codes - length(OT_Code_Hours[[i]]))))
}
othMatrix <- matrix(data = unlist(OT_CodeHrs_FullCount),nrow = (length(employees_uw) * length(week_number)), ncol = max_codes, byrow = T)

#Create a data frame that shows details of hours worked grouped by employee and week.
#Table shows
#1: Total hours and overtime hours
#2: Project codes used that week
#3: Hours attributed to each project code
#4: Base (0-40) hours for each of the 20 project codes
weekXnames <- expand.grid(week_number, employees_uw)
row_stop <- length(employees_uw)*length(code_test_list)
col_stop <- length(time_by_week[1,])
collect_by_code <- matrix(data = NA, nrow = (length(employees_uw) * length(week_number)), ncol = length(code_test_list))
for(i in 1:length(code_test_list))
  {
  collect_by_code[,i] <- as.vector(unlist(t(time_by_week[time_by_week$projectCode == code_test_list[i],3:col_stop])))
  }
wage_table <- data.frame(LastName = weekXnames[,2], 
                         Week_No = weekXnames[,1], 
                         Total_Hrs = Total_Hours,
                         OT_Hours = OT_Hours,
                         otcMatrix,
                         othMatrix, 
                         collect_by_code
                         )
names(wage_table)[5:((max_codes*2)+(5-1))] <- c(paste0("pc", seq(1,max_codes,1)),
                                                paste0("ph", seq(1,max_codes,1)))
base_cols <- (5+(max_codes*2)):(((5-1)+(max_codes*2))+length(code_test_list))
names(wage_table)[base_cols] <- code_test_list

#Create a matrix with employee/week (rows) and project codes (column to hold OT hours)
#This corresponds with the fourth section in wage_table.
rwt <- wage_table
ot_matrix <- matrix(nrow = (length(employees_uw) * length(week_number)), ncol = length(code_test_list))
colnames(ot_matrix) <- code_test_list

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
ot_cols <- (dim(rwt)[2]+1):(dim(rwt)[2]+length(code_test_list))
rwta[,ot_cols][is.na(rwta[,ot_cols]) == T] <- 0
check_base <- apply(rwta[,ot_cols], 1, sum)
check_ot <- apply(rwta[,ot_cols], 1, sum)
check_summary <- data.frame(base_hrs = (rwta$Total_Hrs-rwta$OT_Hours), check_base = check_base, ot_hrs = rwta$OT_Hours, check_ot = check_ot)
sort(unique(check_summary$base_hrs - check_summary$check_base))
sort(unique(check_summary$ot_hrs - check_summary$check_ot))
which(check_summary$base_hrs != check_summary$check_base)

#Multiply OT hours by 1.5
#For representation by wage type (base, OT, leave)
#rwta_ot_type <- rwta[,c(ot_cols[1], ot_cols[3]:max(ot_cols))]
#rwta_base_type <- rwta[,c(base_cols[1], base_cols[3]:max(base_cols))]
#rwta_leave_type <- rwta[,c(base_cols[2],ot_cols[2])]

#For representation by PI
#rwta_ot_pi <- rwta[,ot_cols] * 1.5
#rwta_base_pi <- rwta[,base_cols]

#Calculate total hours per week (grouped by employee) with OT hours multiplied by 1.5
#This can be multiplied by the employees wage to determine cost per week.
#wage_hours <- matrix(data = 0, nrow = length(rwta_base_pi[,1]), ncol = length(rwta_base_pi[1,]))
#for(i in 1:length(rwta_ot_pi[1,]))
#{
#  wage_hours[,i] <- rwta_base_pi[,i] + rwta_ot_pi[,i]
#}
#colnames(wage_hours) <- colnames(rwta_base_pi)

#Save uw billing info
uw_detail <- rwta
#uw_wage_hrs <- wage_hours

###########################################################################################################################################
###########################################################################################################################################
###########################################################################################################################################
###########################################################################################################################################
#
#                                                    US FOREST SERVICE
#
empl <- "fs"
###########################################################################################################################################
###########################################################################################################################################
###########################################################################################################################################
###########################################################################################################################################

#Strip unused columns
start.col <- vector()
end.col <- vector()
staff <- list()
code_test_list <- vector()
for(i in 1:length(file_names))
{
  start.col[i] <- which(colnames(get(file_names[i])) %in% "Date")
  end.col[i] <- which(colnames(get(file_names[i])) %in% "Check.with.Jim.or.Deborah.before.adding.new.codes.")
  temp.1 <- get(file_names[i])[,-c(1:(start.col[i]-1),end.col[i]:length(colnames(get(file_names[i]))))]
  temp.2 <- temp.1[temp.1$Date >= period$start_date[period$employer == empl] & temp.1$Date <= period$end_date[period$employer == empl],]
  if(length(which(is.na(temp.2$Date)==T)) == 0)#if statement needed b/c if test result is zero then all valid dates will be dropped.
  {
    temp.3 <- temp.2
  } else
  {
    temp.3 <- temp.2[-which(is.na(temp.2$Date)==T),]
  }
  
  staff[[i]] <- temp.3
  code_test_list <- c(code_test_list, colnames(temp.3)[colnames(temp.3) != "Date"])
}

#For some reason some columns may be imported as character values despite being classified as numeric in .csv file.
#This loop find them and converts them to numeric.
for(i in 1:length(file_names))
{
  for(a in 2:length(colnames(staff[[i]])))
  {
    if(is.numeric(staff[[i]][,a]) == T)
    {
      staff[[i]][,a] <- staff[[i]][,a]
    } else
    {
      staff[[i]][,a] <- as.numeric(staff[[i]][,a])
    }
  }
}

#Run script for University of Washington employees (work week = Sunday - Saturday)
staff_fs <- staff[staff_list$employer == empl]
employees_fs <- staff_list$last_name[staff_list$employer == empl]

#Generate a data frame that uses time storage format used by previous years'scripts (so I don't have to re-write code
#that allocates OT hours to project.)
#rows = employee x project code
#cols = week number
if(length(unique(as.numeric(strftime(staff_fs[[1]]$Date, format = "%Y")))) == 1)
{
  week <- as.numeric(strftime(staff_fs[[1]]$Date, format = "%W"))
  week[seq(1,length(week),7)] <- week[seq(1,length(week),7)] + 1#adjust week number based on USFS work week, different than %W 
} else
{
  week <- as.numeric(strftime(staff_fs[[1]]$Date, format = "%W"))
  week[seq(1,length(week),7)] <- week[seq(1,length(week),7)] + 1#adjust week number based on USFS work week, different than %W 
  week[week %in% week[week > 10]] <- min(week_number)
}
code_test_list <- sort(unique(code_test_list))
code_name_rows <- expand.grid(code_test_list, employees_fs)
time_by_week <- data.frame(lastName = as.character(code_name_rows$Var2), 
                           projectCode = as.character(code_name_rows$Var1), 
                           matrix(data = NA, nrow = length(code_name_rows$Var1), ncol = length(week_number)), 
                           stringsAsFactors = F)

#Summarize data into data frame.
for(i in 1:length(staff_fs))
{
  for(a in 1:length(code_test_list))
  {
    for(b in 1:length(week_number))
    {
      temp.a <- colnames(staff_fs[[i]]) == code_test_list[a]
      if(length(temp.a[temp.a == T]) == 0)
      {
        time_by_week[time_by_week$lastName == employees_fs[i] & time_by_week$projectCode == code_test_list[a],b + 2] <- 0
      } else
      {
        time_by_week[time_by_week$lastName == employees_fs[i] & time_by_week$projectCode == code_test_list[a],b + 2] <- 
          sum(staff_fs[[i]][week == week_number[b], colnames(staff_fs[[i]]) == code_test_list[a]], na.rm = T)
      }
    }
  }
}

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
for(b in 1:length(employees_fs))
{
  for(c in 1:length(week_number))
  {
    unordered_codes <- time_by_week[,c(2,(2+c))][time_by_week$lastName == employees_fs[b],]
    ordered_codes <- unordered_codes[order(unordered_codes[,2], decreasing = T),]
    reduced_codes <- ordered_codes[ordered_codes$projectCode %in% c("ADM", "LVE") == F & ordered_codes[,2] > 0,]
    reordered_codes <- rbind(reduced_codes, 
                             ordered_codes[ordered_codes$projectCode %in% c("ADM", "LVE") == T & ordered_codes[,2] > 0,])
    OT_Codes[[length(OT_Codes) + 1]] <- reordered_codes[,1]
    OT_Code_Hours[[length(OT_Code_Hours) + 1]] <- reordered_codes[,2]
    Total_Hours[length(Total_Hours) + 1] <- sum(time_by_week[,(2+c)][time_by_week$lastName == employees_fs[b]])
    if(sum(time_by_week[,(2+c)][time_by_week$lastName == employees_fs[b]]) > 40)
    {
      OT_Hours[length(OT_Hours) + 1] <- sum(time_by_week[,(2+c)][time_by_week$lastName == employees_fs[b]]) - 40
    } else
    {
      OT_Hours[length(OT_Hours) + 1] <- 0
    }
  }
}

#Measure the number of projects billed for each week and identify week with largest number of projects billed.
#code_length <- vector(mode = 'numeric')
#for(i in 1:length(OT_Codes))
#{
#  code_length[length(code_length) + 1] <- length(OT_Codes[[i]])
#}
#max_codes <- max(code_length)
max_codes <- length(code_test_list)

#Create a list of OT codes by week with the same number of elements for each week.
OT_Codes_FullCount <- list()
for(i in 1:length(OT_Codes))
{
  OT_Codes_FullCount[[i]] <- c(OT_Codes[[i]], rep("--", (max_codes - length(OT_Codes[[i]]))))
}
otcMatrix <- matrix(data = unlist(OT_Codes_FullCount),nrow = (length(employees_fs) * length(week_number)), ncol = max_codes, byrow = T)

#Create a corresponding list of OT hours by week with the same number of elements for each week.
OT_CodeHrs_FullCount <- list()
for(i in 1:length(OT_Code_Hours))
{
  OT_CodeHrs_FullCount[[i]] <- c(OT_Code_Hours[[i]], rep(0, (max_codes - length(OT_Code_Hours[[i]]))))
}
othMatrix <- matrix(data = unlist(OT_CodeHrs_FullCount),nrow = (length(employees_fs) * length(week_number)), ncol = max_codes, byrow = T)

#Create a data frame that shows details of hours worked grouped by employee and week.
#Table shows
#1: Total hours and overtime hours
#2: Project codes used that week
#3: Hours attributed to each project code
#4: Base (0-40) hours for each of the 20 project codes
weekXnames <- expand.grid(week_number, employees_fs)
row_stop <- length(employees_fs)*length(code_test_list)
col_stop <- length(time_by_week[1,])
collect_by_code <- matrix(data = NA, nrow = (length(employees_fs) * length(week_number)), ncol = length(code_test_list))
for(i in 1:length(code_test_list))
{
  collect_by_code[,i] <- as.vector(unlist(t(time_by_week[time_by_week$projectCode == code_test_list[i],3:col_stop])))
}
wage_table <- data.frame(LastName = weekXnames[,2], 
                         Week_No = weekXnames[,1], 
                         Total_Hrs = Total_Hours,
                         OT_Hours = OT_Hours,
                         otcMatrix,
                         othMatrix, 
                         collect_by_code
)
names(wage_table)[5:((max_codes*2)+(5-1))] <- c(paste0("pc", seq(1,max_codes,1)),
                                                paste0("ph", seq(1,max_codes,1)))
base_cols <- (5+(max_codes*2)):(((5-1)+(max_codes*2))+length(code_test_list))
names(wage_table)[base_cols] <- code_test_list

#Create a matrix with employee/week (rows) and project codes (column to hold OT hours)
#This corresponds with the fourth section in wage_table.
rwt <- wage_table
ot_matrix <- matrix(nrow = (length(employees_fs) * length(week_number)), ncol = length(code_test_list))
colnames(ot_matrix) <- code_test_list

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
ot_cols <- (dim(rwt)[2]+1):(dim(rwt)[2]+length(code_test_list))
rwta[,ot_cols][is.na(rwta[,ot_cols]) == T] <- 0
check_base <- apply(rwta[,ot_cols], 1, sum)
check_ot <- apply(rwta[,ot_cols], 1, sum)
check_summary <- data.frame(base_hrs = (rwta$Total_Hrs-rwta$OT_Hours), check_base = check_base, ot_hrs = rwta$OT_Hours, check_ot = check_ot)
sort(unique(check_summary$base_hrs - check_summary$check_base))
sort(unique(check_summary$ot_hrs - check_summary$check_ot))
which(check_summary$base_hrs != check_summary$check_base)

#Multiply OT hours by 1.5
#For representation by wage type (base, OT, leave)
#rwta_ot_type <- rwta[,c(ot_cols[1], ot_cols[3]:max(ot_cols))]
#rwta_base_type <- rwta[,c(base_cols[1], base_cols[3]:max(base_cols))]
#rwta_leave_type <- rwta[,c(base_cols[2],ot_cols[2])]

#For representation by PI
#rwta_ot_pi <- rwta[,ot_cols] * 1.5
#rwta_base_pi <- rwta[,base_cols]

#Calculate total hours per week (grouped by employee) with OT hours multiplied by 1.5
#This can be multiplied by the employees wage to determine cost per week.
#wage_hours <- matrix(data = 0, nrow = length(rwta_base_pi[,1]), ncol = length(rwta_base_pi[1,]))
#for(i in 1:length(rwta_ot_pi[1,]))
#{
#  wage_hours[,i] <- rwta_base_pi[,i] + rwta_ot_pi[,i]
#}
#colnames(wage_hours) <- colnames(rwta_base_pi)

#Save uw billing info
fs_detail <- rwta
#fs_wage_hrs <- wage_hours

###########################################################################################################################################
###########################################################################################################################################
###########################################################################################################################################
###########################################################################################################################################

#Combine wage hours tables for FS and UW employees
#wage_hours <- rbind(fs_wage_hrs, uw_wage_hrs)
rwta <- rbind(fs_detail, uw_detail)

#Summarize hours by owner, rather than project code.
#owner_hours <- matrix(nrow = length(wage_hours[,1]), ncol = length(unique(codes$owner)))
#owners <- sort(unique(codes$owner))
#for(i in 1:length(unique(codes$owner)))
#{
#  colnos <- which(is.na(match(colnames(wage_hours), codes$code[codes$owner == owners[i]])) == F)
#  colnos <- colnos[!is.na(colnos) == T]
#  if(length(colnos) == 0)
#  {
#    owner_hours[,i] <- 0
#  } else
#  {
#    if(length(colnos) == 1)
#    {
#      owner_hours[,i] <- wage_hours[,colnos]
#    } else
#    {
#      owner_hours[,i] <- apply(wage_hours[,colnos], 1, sum)
#    }
#  }
#  }

#Create a new data frame
#owner_by_week <- cbind.data.frame(as.vector(rwta$LastName), owner_hours)
#colnames(owner_by_week) <- c(colnames(rwta)[1], as.character(owners))

#Summarize hours worked by last name and budget owner.
#owner_summary <- data.frame(owner_by_week %>%
#                                group_by(LastName) %>%
#                                summarise_all(list(sum)))

#Calculate base hours per week by employee
#total_base <- apply(rwta_base_type, 1, sum)

#Calculate paid leave hours per week by employee
#total_leave <- apply(rwta_leave_type, 1, sum)

#Calculate ot hours per week by employee
#total_ot <- apply(rwta_ot_type, 1, sum)

#Calculate raw hours (i.e, OT hours not multiplied by 1.5)
#Summarize hours by project code.

pis <- sort(unique(codes$pi))
flush.cols <- max(grep("ph", colnames(rwta), value = F))
start.base <- flush.cols + 1
end.base <- flush.cols + length(code_test_list)
start.ot <- flush.cols + (length(code_test_list)+1)
end.ot <- flush.cols + (length(code_test_list)*2)
base_rwta <- rwta[,start.base:end.base]
ot_rwta <- rwta[,start.ot:end.ot]
if(min(week_number) == 0)
{
  friday <- as.Date(paste(as.numeric(format(Sys.Date(), "%Y"))-1, past_year_max, 5, sep="-"), "%Y-%W-%w")
  friday <- c(friday, as.Date(paste(format(Sys.Date(), "%Y"), week_number[-1], 5, sep="-"), "%Y-%W-%w"))
  } else
    {
      friday <- as.Date(paste(format(Sys.Date(), "%Y"), week_number, 5, sep="-"), "%Y-%W-%w")
    }
colnames(base_rwta) <- paste("base", colnames(base_rwta), sep = "_")
colnames(ot_rwta) <- paste("ot", colnames(ot_rwta), sep = "_")
project_weekly <- data.frame(last_name = rwta$LastName, 
                         friday = rep(friday, length(unique(rwta$LastName))), 
                         base_hrs = rwta$Total_Hrs - rwta$OT_Hours, 
                         ot_hrs = rwta$OT_Hours, 
                         base_rwta, 
                         ot_rwta)

#Summarize hours by project investigator.
pi_base <- matrix(nrow = length(rwta[,1]), ncol = length(unique(codes$pi)))
pi_ot <- matrix(nrow = length(rwta[,1]), ncol = length(unique(codes$pi)))

for(i in 1:length(unique(codes$pi)))
{
  colnos <- which(is.na(match(colnames(rwta[,start.base:end.base]), codes$code[codes$pi == pis[i]])) == F)
  colnos <- colnos[!is.na(colnos) == T]
  if(length(colnos) == 0)
  {
    pi_base[,i] <- 0
  } else
  {
    if(length(colnos) == 1)
    {
      pi_base[,i] <- base_rwta[,colnos]
    } else
    {
      pi_base[,i] <- apply(base_rwta[,colnos], 1, sum)
    }
  }
}

for(i in 1:length(unique(codes$pi)))
{
  colnos <- which(is.na(match(colnames(rwta[start.ot:end.ot]), codes$code[codes$pi == pis[i]])) == F)
  colnos <- colnos[!is.na(colnos) == T]
  if(length(colnos) == 0)
  {
    pi_ot[,i] <- 0
  } else
  {
    if(length(colnos) == 1)
    {
      pi_ot[,i] <- ot_rwta[,colnos]
    } else
    {
      pi_ot[,i] <- apply(ot_rwta[,colnos], 1, sum)
    }
  }
}


#Create a new data frame
colnames(pi_base) <- paste("base", pis, sep = "_")
colnames(pi_ot) <- paste("ot", pis, sep = "_")
pi_weekly <- data.frame(last_name = rwta$LastName, 
                         friday = rep(friday, length(unique(rwta$LastName))), 
                         base_hrs = rwta$Total_Hrs, 
                         ot_hrs = rwta$OT_Hours, 
                         pi_base, 
                         pi_ot)

#Summarize hours worked by last name and pi.
pi_weekly_reduced_base <- data.frame(last_name = pi_weekly$last_name, pi_base)
pi_monthly_base <- data.frame(pi_weekly_reduced_base %>%
                              group_by(last_name) %>%
                              summarise_all(list(sum)))
pi_weekly_reduced_ot <- data.frame(last_name = pi_weekly$last_name, pi_ot)
pi_monthly_ot <- data.frame(pi_weekly_reduced_ot %>%
                                group_by(last_name) %>%
                                summarise_all(list(sum)))

#I need dates for week numbers
#I need dates for week numbers
#wn <- strftime(staff_fs[[1]]$Date, format = "%V")
#wn1 <- sort(unique(wn)) 
#as.Date(paste(2021, week_number+1, 0, sep="-"), "%Y-%W-%w")
#wn3 <- substring(wn2,6)

#Barplot of hours by week for each UW field staff_fs employee
#dev.off()
#par(mfrow=c(3,2), mar = c(4, 4, 4, 2))
#cf <- 0.5
#ln <- "nemens"
#barplot(cbind(total_leave[rwta$LastName == ln], total_base[rwta$LastName == ln], total_ot[rwta$LastName == ln]) ~ wn3, 
#        las = 2, xlab ="", ylim = c(0,90), col = gray((1:3)/4), main = ln)
#ln <- "thoreson"
#barplot(cbind(total_leave[rwta$LastName == ln], total_base[rwta$LastName == ln], total_ot[rwta$LastName == ln]) ~ wn3, 
#        las = 2, xlab ="", ylim = c(0,90), col = gray((1:3)/4), main = ln)
#ln <- "tripodi"
#barplot(cbind(total_leave[rwta$LastName == ln], total_base[rwta$LastName == ln], total_ot[rwta$LastName == ln]) ~ wn3, 
#        las = 2, xlab ="", ylim = c(0,90), col = gray((1:3)/4), main = ln)
#ln <- "darmody"
#barplot(cbind(total_leave[rwta$LastName == ln], total_base[rwta$LastName == ln], total_ot[rwta$LastName == ln]) ~ wn3, 
#        las = 2, xlab ="", ylim = c(0,90), col = gray((1:3)/4), main = ln)
#ln <- "cronan"
#barplot(cbind(total_leave[rwta$LastName == ln], total_base[rwta$LastName == ln], total_ot[rwta$LastName == ln]) ~ wn3, 
#        las = 2, xlab ="", ylim = c(0,90), col = gray((1:3)/4), main = ln)
#
#
#legend(50,80, c("Paid Leave", "Base", "OT"), fill = gray((1:3)/4))
#total_base_arch <- total_base
#total_leave_arch <- total_leave
#total_ot_arch <- total_ot

#Save data 
#Weekly by project
mid_month <- staff_uw[[1]]$Date[14]
year_name <- format(mid_month, "%Y")
month_name <- format(mid_month, "%B")
month_no <- substr(mid_month,6,7)
if(dir.exists(paste("C:/Users/jcronan/Box/FERA-UW/Admin/HR/Wage_Tracking/", as.numeric(format(Sys.Date(), "%Y")), sep = "")) == T)
  {
  setwd(paste("C:/Users/jcronan/Box/FERA-UW/Admin/HR/Wage_Tracking/", as.numeric(format(Sys.Date(), "%Y")), sep = ""))
  } else
    {
      setwd("C:/Users/jcronan/Box/FERA-UW/Admin/HR/Wage_Tracking/")
      year_dir <- paste(year_name)
      dir.create(year_dir)
      setwd(paste("C:/Users/jcronan/Box/FERA-UW/Admin/HR/Wage_Tracking/", format(Sys.Date(), "%Y"), sep = ""))
      }
sub_dir <- paste(year_name, "_", month_no, "_", month_name, sep = "")
dir.create(sub_dir)
setwd(paste("C:/Users/jcronan/Box/FERA-UW/Admin/HR/Wage_Tracking/", format(Sys.Date(), "%Y"), "/", sub_dir, sep = ""))
write.csv(project_weekly, file = paste(year_name, "_", month_name, "_salary_summarized_by_week_by_project.csv", sep = ""))
write.csv(pi_weekly, file = paste(year_name, "_", month_name, "_salary_summarized_by_week_by_pi.csv", sep = ""))
write.csv(pi_monthly_base, file = paste(year_name, "_", month_name, "_salary_summary_for_month_by_pi_base_hours.csv", sep = ""))
write.csv(pi_monthly_ot, file = paste(year_name, "_", month_name, "_salary_summary_for_month_by_pi_overtime_hours.csv", sep = ""))
write.csv(staff_list, file = paste(year_name, "_", month_name, "_lookup_table_staff_list.csv", sep = ""))
write.csv(period, file = paste(year_name, "_", month_name, "_lookup_table_reporting_period.csv", sep = ""))
write.csv(codes, file = paste(year_name, "_", month_name, "_lookup_table_project_codes.csv", sep = ""))

###################################################################################################################################
#END
###################################################################################################################################