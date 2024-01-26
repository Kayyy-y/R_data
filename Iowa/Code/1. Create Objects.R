library(googlesheets4)
library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr)
library(multilinguer)



# -----------------------------------------------------------------------------------
# Account Verification
# -----------------------------------------------------------------------------------
# gs4_auth()
2



#----------------------------------------------------------------
# Read Data
#----------------------------------------------------------------

wdir <- 'https://docs.google.com/spreadsheets/d/1a1rbVgVJp6n6xd8r7Qxu1WTwo-9n1cnzd_7lk7FX6fM/'

wdir_inds <- 'https://docs.google.com/spreadsheets/d/1xZHyQzYe6uEus_39sGXcR9UXz_mrjxnxffrtHzMp3Kk/'

### Data;
setwd("./Data")
# data <- read.csv("data2019.csv")


inds_iowa <- read_sheet(wdir_inds,
                        sheet = 'inds_iowa',
                        col_names = T, na = '')



# Create Sales DF ------------------------------------------------

names(data)

Sales <- data %>% 
  select(1, 2, 3,10,12, 13, c(15:21)) %>% 
  mutate(City = ifelse(City == "", NA, City)) %>% 
  mutate(County = ifelse(County == "", NA, County))

names(Sales) <- c('Invoice_id', 'Date', 'Store_id', 'Category_id', 'Vendor_id','Item_id', 'Pack', 'Bottle.volume', 'Bottle.cost', 'Bottle.retail', 'Sold.bottle', 'Sold.cost', 'Sold.L')

Sales %>% head()



# Finish inds_region --------------------------------------------

inds_store %>% head()

inds_iowa %>% head()

data %>% 
  filter(!is.na(City) | !is.na(County)) %>% nrow() #12813482
  
data %>% 
  filter(str_equal(County, "")) %>% nrow()

data %>% 
  filter(!is.na(County)) %>% nrow()

data %>% 
  select(City, County) 
  

tmp_store <- data %>% 
  select(Store.Number, Store.Name, City, County) %>% 
  mutate(City = str_replace_all(City, "[:punct:]", "")) %>% 
  mutate(County = str_replace_all(County, " ", "")) %>% 
  mutate(City = str_replace_all(City, "[:punct:]", "")) %>% 
  mutate(County = str_replace_all(County, " ", "")) %>% 
  left_join(inds_iowa %>% 
              mutate(City = str_to_upper(City)) %>% 
              mutate(County = str_to_upper(County)) %>% 
              mutate(City = str_replace_all(City, "[:punct:]", "")) %>% 
              mutate(County = str_replace_all(County, " ", "")) %>% 
              mutate(City = str_replace_all(City, "[:punct:]", "")) %>% 
              mutate(County = str_replace_all(County, " ", "")), by=c('City', 'County')) %>%
  filter(is.na(region_id)) %>% 
  select(City, County, region_id) %>% 
  unique()

tmp_store %>% 
  write_sheet(wdir_inds, sheet = 'check_region')


# Rename inds_*--------------------------------------------------








