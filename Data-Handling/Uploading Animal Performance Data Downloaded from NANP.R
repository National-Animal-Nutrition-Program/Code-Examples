##	Code for Uploading Animal Performance Data Downloaded from NANP Website

##	Updated: 10/13/2014

##	For Questions, please contact: rrwhite@vt.edu





##	Please Follow These Directions Carefully!

##	To run this code, you must have a copy of the XLConnect package in your 



##	Step 1. Set the working directory of R to the same file where you have saved your downloaded excel sheet.

setwd("C://Users/rrwhite/Documents/NANP/Modeling Committee/Datasets")



##	Step 2. Load the XLConnect Package

library(XLConnect)



##	Step 3. Ask R to import all data from the Excel File and label it 'wk'

wk <- loadWorkbook("NRC 2012 Swine Data.xlsx")



##	Step 4. Ask R to read the first sheet (ColumnDefs) from the Excel File as a dataframe and save it as 'ws'

ws <- readWorksheet(wk, sheet="ColumnDefs")



##	Step 5. Read the "Table" column of the dataframe and eliminate data that comes up as <NA>

wsnames <- ws$Table

wsnames <- wsnames[!is.na(wsnames)]

labs <- paste("Table", 1:length(wsnames), sep="")



##	Step 6. Read the remaining sheets and name then as sequential Tables

for(i in 1:length(wsnames)) { assign(labs[i], readWorksheet(wk, sheet=wsnames[i]))}



##	Example 1. Read the names of the rows from a particular table

names(Table1)



##	Example 2. Combine multiple columns from across tables to form a new table and run an analysis of variance

newTable <- data.frame(Table2$PubID, Table2$TrtID, Table2$TrtName, Table4$DMI, Table10$ADG)

summary(aov(Table10.ADG~Table4.DMI, data=newTable))
