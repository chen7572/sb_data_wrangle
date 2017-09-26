# read in csv file & take a look at the file
companies <- read.csv("refine_original.csv",stringsAsFactors = FALSE)
head(companies)

# load library dplyr and dummies 
library(dplyr)
library(dummies)

# create a local data frame & view the data frame in two different ways
companies_raw <- tbl_df(companies)
glimpse(companies_raw)
View(companies_raw)

# clean up the company names
# step 1.take a look at all the names, including misspelled ones
unique(companies_raw$company)
# step 2. create a lookup table contains the misspelled names
# and the corresponding company name
lookup = data.frame(Company = unique(companies_raw$company), 
                    updated_nms = c(rep("philips",6),rep("akzo",5),
                                     rep("philips",2),rep("van_houten",3),
                                     rep("unilever",3)),
                    stringsAsFactors = FALSE)

# step 3. use the lookup table to replace company names
id = match(companies_raw$company,lookup$Company)
list = lookup$updated_nms[id]
clean_names <- companies_raw %>% 
               mutate(company = list)

# separate product code and number
sep_names <- clean_names %>%  
  separate(Product.code...number,c("Product_code","number"),convert=TRUE)

# add product categories
product_cat <- data.frame(Product_code = c("p","v","x","q"),
                          Product = c("Smartphone","TV","Laptop","Tablet"),
                          stringsAsFactors = FALSE)  
updated_df <- left_join(sep_names,product_cat,by="Product_code")

# Add full address for geocoding
updated_df <- updated_df %>% unite(full_address,address,city,country,sep=",")

# Create dummy variables for company and product category
# and saved as final_df. 
dummy1 <- dummy(updated_df$company,sep = "_")
dummy2 <- dummy(updated_df$Product,sep="_")
final_df <- cbind(updated_df,dummy1,dummy2)
