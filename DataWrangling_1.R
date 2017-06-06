#Load packages
library(devtools)
library(dplyr)
library(tidyr)

# 0: Load the data in RStudio
mydata = read.csv("refine.csv", header=TRUE) 
View(mydata)

#1: Clean up brand names
#Approach 1-1: Identify the certain pattern in the cells. 
#Then, replace the pattern with corresponding requested name.
# Useful info of "Regular Expressions"
# https://rstudio-pubs-static.s3.amazonaws.com/74603_76cd14d5983f47408fdf0b323550b846.html

mydata$company <- gsub(".*s$", "philips", mydata$company,ignore.case = TRUE)
mydata$company <- gsub(".*[o|0]$", "akzo", mydata$company,ignore.case = TRUE)
mydata$company <- gsub(".*n .*", "van houten", mydata$company,ignore.case = TRUE)
mydata$company <- gsub("^u.*r$", "unilever", mydata$company,ignore.case = TRUE)

#Approach 1-2: Replace the data of the cell with requested name.
#mydata = read.csv("refine.csv", header=TRUE, stringsAsFactors=FALSE) 
#indices_philips <- grep("^[f|p|P].*", mydata$company)
#mydata$company <- replace (mydata$company,indices_philips, "philips")
#indices_akzo <- grep("^[k|K].*", mydata$company)
#mydata$company<- replace(mydata$company,indices_akon, "akzo")
#indices_van_houten <- grep("^[v|V].*", mydata$company)
#mydata$company<- replace(mydata$company,indices_van_houten, "van houten")
#indices_unilever <- grep("^[u|U].*", mydata$company)
#mydata$company<- replace(mydata$company,indices_unilever, "unilever")


# Approach 1-3: Create a loop to apply the function to the "company" column.
#mydata = read.csv("refine.csv", header=TRUE, stringsAsFactors=FALSE) 
#for (row in seq_along(mydata$company)) {
#  if (grepl("^[f|p|P].*", mydata[row,1])){
#    mydata[row,1] <- "philips"
#  } else if (grepl ("^[a|A]", mydata[row,1])){
#    mydata[row,1] <- "akzo"
#  } else if (grepl ("^[v|V].*",mydata[row,1])){
#    mydata[row,1] <- "van houten"
#  } else if (grepl ("^[u|U].*", mydata[row,1])) {
#    mydata[row,1] <- "unilever"
#  }
# }
# Note:  "^[f|p|P].*"= "^f+|^p+|^P+"

  
# 2: Separate product code and number
mydata<- separate(mydata, Product.code...number, c("product_code","product_number"), sep="-")

# 3: Add product categories
category <- function(x) {
  if (x=="p"){
      return("Smartphone") 
  }   else if (x=="v"){
      return("TV")
  }   else if (x=="x"){ 
      return("Laptop")
  }   else if (x=="q"){ 
      return("Tablet") 
  }
}
mydata <- mutate(mydata,category = sapply(product_code, category))

# 4: Add full address that concatenates the three address fields (address, city, country), 
mydata<- mutate( mydata, full_address = paste (address,",", city,",", country))
# The following method will remove the origianl columns (address, city, country).Then, unite these original columns in to a new column. 
# mydata<- unite (mydata, "full_address", address, city, country, sep=",")


# 5-1: Add four binary (1 or 0) columns for company: 
# company_philips, company_akzo, company_van_houten and company_unilever.
company_p <- function(x){ 
             if (x=="philips"){ 
                 return("1")} 
           else (return("0"))}
mydata<- mutate(mydata, company_philips= sapply(company, company_p))

company_a <- function(x){ 
             if (x=="akzo"){
                 return("1")} 
           else (return("0"))}
mydata<- mutate(mydata, company_akzo = sapply(company, company_a))

company_v <- function(x){ 
             if (x=="van houten"){
                 return("1")} 
           else (return("0"))}
mydata<- mutate(mydata, company_van_houten = sapply(company, company_v))

company_u <- function(x){
             if (x=="unilever"){
                 return("1")} 
           else (return("0"))}
mydata<- mutate(mydata, company_unilever   = sapply(company, company_u))

# 5-2: Add four binary (1 or 0) columns for product: 
# product_smartphone, product_tv, product_laptop and product_tablet.
product_p <- function(x){
             if (x=="p"){ 
                 return ("1")} 
           else (return ("0"))}
mydata<- mutate(mydata, product_smartphone = sapply(product_code, product_p))

product_v <- function(x){ 
             if (x=="v"){ 
                 return ("1")} 
           else (return ("0"))}
mydata<- mutate(mydata, product_tv = sapply(product_code, product_v))

product_x <- function(x){
             if (x=="x"){
                 return ("1")} 
           else (return ("0"))}
mydata<- mutate(mydata, product_laptop = sapply(product_code, product_x))

product_q <- function(x){
             if (x=="q"){ return ("1")} 
           else (return ("0"))}
mydata<- mutate(mydata, product_tablet = sapply(product_code, product_q))

# A better approach learned from "chiragpandya88" https://github.com/chiragpandya88/electronicstore/blob/master/refine.R
# 1. Use LogicalTest to test if mydata$company == "philips"
# 2. Return the logical results into a binary numeric dataset (TRUE =1 and FALSE=0)
# 3. Use this dataset to create the corresponding binary (1 or 0) columns
# 4. Ex: mydata <- mutate(mydata,company_philips = as.numeric((company == "philips")))

# Save cleaned data as CSV file
write.csv(mydata, "cleaned_data.csv")
 