Author: James Cronan
Organization: PNW Research Station, Seattle, WA
Date: March 3, 2021

Note: using a text file for the reporting period due to date formatting issues with
Excel files (i.e., .csv and .xlsx)

Why .csv files won't work
R script will not run unless date is formatted correctly (yyyy-mm-dd) in
reporting_period.csv. 
.csv files can revert back to the default Excel date format (mm/dd/yyyy) if that are not
saved correctly.
Basically the date needs to be formatted as needed in Excel:
"Number" from menu > "Number" tab in dialogue box > "Custom" option from drop down > set date format
Save file as an .xlsx
Save file as a .csv
Complete instructions:
https://admin.echo360.com/hc/en-us/articles/360035034372-Formatting-Dates-for-CSV-Imports

Why .xlsx files won't work
Cannot open .xlsx files with read.table() so another function readxl() is recommended.
This function open the table as a tibble with additional information the downstream 
code cannot interpret. using as.data.frame() fixes this, but dates are listed as POSIXct
instead of as.character which also interfers with downstream code.
Rather than trying to figure out how to convert POSIXct dates to characters
I opted to use .txt files.