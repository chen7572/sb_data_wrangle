# read in csv file & take a look at the file
companies <- read.csv("refine_original.csv")
head(companies)

# load dplyr library
install.packages("tidyverse")
library(plyr)
library(dplyr)
library(tidyr)
install.packages("dummies")
library(dummies)

# create a local data frame & view the data frame in two different ways
companies_raw <- tbl_df(companies)
glimpse(companies_raw)
View(companies_raw)

# clean up the company names 
clean_names <- companies_raw %>% 
  mutate(company = replace(company,grepl("^p|^P|^f",company),"philips"),
         company = replace(company,grepl("^a|^A",company),"akzo"),
         company = replace(company,grepl("^v|^V",company),"van houten"),
         company = replace(company,grepl("^u|^U",company),"unilever")) 

# separate product code and number
sep_names <- clean_names %>%  
  separate(Product.code...number,c("Product.code","number"),convert=TRUE)

# add product categories
product_cat <- data.frame(Product.code = c("p","v","x","q"),Product = c("Smartphone","TV","Laptop","Tablet"))  
updated_df <- left_join(sep_names,product_cat,by="Product.code")

# Question 1:
# Warning message:
# Column `Product.code` joining character vector and factor, coercing into character vector 

# Add full address for geocoding
updated_df <- updated_df %>% unite(full_address,address,city,country,sep=",")

# Create dummy variables for company and product category
# First, change the level names 
levels(updated_df$company) <- gsub("^p.*$|^f.*$","philips",levels(updated_df$company),ignore.case=TRUE)
levels(updated_df$company) <- gsub("^a.*$","akzo",levels(updated_df$company),ignore.case=TRUE)
levels(updated_df$company) <- gsub("^u.*$","unilever",levels(updated_df$company),ignore.case=TRUE)
levels(updated_df$company) <- gsub("^v.*$","van_houten",levels(updated_df$company),ignore.case=TRUE)

dummy1 <- dummy(updated_df$company,sep = "_")
dummy2 <- dummy(updated_df$Product,sep="_")

# Question 2: how to add dummy variables to categorical variable? 


