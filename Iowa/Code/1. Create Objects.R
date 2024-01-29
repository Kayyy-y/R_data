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

inds_category <- read_sheet(wdir_inds,
                        sheet = 'inds_category',
                        col_names = T, na = '')

inds_vendor <- read_sheet(wdir_inds,
                        sheet = 'inds_vendor',
                        col_names = T, na = '')

inds_item <- read_sheet(wdir_inds,
                        sheet = 'inds_item',
                        col_names = T, na = '')

inds_city <- read_sheet(wdir_inds,
                          sheet = 'inds_city',
                          col_names = T, na = '')

inds_county <- read_sheet(wdir_inds,
                        sheet = 'inds_county',
                        col_names = T, na = '')

inds_store <- read_sheet(wdir_inds,
                          sheet = 'inds_store',
                          col_names = T, na = '')


# Create Sales DF ------------------------------------------------

names(data)

Sales <- data %>% 
  select(1, 2, 3, 6, 10, 11, 13, 15, c(17:23)) %>% 
  mutate(City = ifelse(City == "", NA, City)) %>% 
  mutate(County = ifelse(County == "", NA, County))

names(Sales) <- c('Invoice_id', 'Date', 'Store_id', 'City', 'County', 'Category_id', 'Vendor_id','Item_id', 'Pack', 'Bottle.volume', 'Bottle.cost', 'Bottle.retail', 'Sold.bottle', 'Sold.cost', 'Sold.L')

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

Sales <- Sales %>% 
  select(-City, -County) 
  

# ready for JOIN inds_store - inds_iowa
tmp_iowa <- inds_iowa %>% 
  mutate(City = str_to_upper(City)) %>% 
  mutate(County = str_to_upper(County)) %>% 
  mutate(City = str_replace_all(City, "[:punct:]", "")) %>% 
  mutate(County = str_replace_all(County, "[:punct:]", "")) %>% 
  mutate(City = str_replace_all(City, "[:blank:]", "")) %>% 
  mutate(County = str_replace_all(County, "[:blank:]", ""))


tmp_store <- data %>% 
  select(Store.Number, Store.Name, City, County) %>% 
  mutate(City = str_replace_all(City, "[:punct:]", "")) %>% 
  mutate(County = str_replace_all(County, "[:punct:]", "")) %>% 
  mutate(City = str_replace_all(City, "[:blank:]", "")) %>% 
  mutate(County = str_replace_all(County, "[:blank:]", "")) %>% 
  left_join(tmp_iowa, by=c('City', 'County')) %>%
  filter(is.na(region_id)) %>% 
  select(City, County, region_id) %>% 
  unique()

# tmp_store %>%
#   write_sheet(wdir_inds, sheet = 'check_region')

chk_region <- read_sheet(wdir_inds,
                         sheet = 'check_region',
                         col_names = T, na = '')


chk_region %>% head()

chk_region <- chk_region %>%  
  filter(!is.na(region_id))

  
# CREATE region_id

inds_store_fin <- data %>% 
  select(Store.Number, City, County) %>%
  unique() %>% 
  mutate(City = str_replace_all(City, "[:punct:]", "")) %>% 
  mutate(County = str_replace_all(County, "[:punct:]", "")) %>% 
  mutate(City = str_replace_all(City, "[:blank:]", "")) %>% 
  mutate(County = str_replace_all(County, "[:blank:]", ""))

inds_store_fin %>% head()

inds_store1 <- inds_store_fin %>% 
  left_join(tmp_iowa, by=c('City', 'County')) %>% 
  filter(is.na(region_id)) %>% 
  select(-region_id) %>% 
  left_join(chk_region, by=c('City', 'County'))

inds_store2 <- inds_store_fin %>% 
  left_join(tmp_iowa, by=c('City', 'County')) %>% 
  filter(!is.na(region_id))

store_region_id <- inds_store1 %>%
  rbind(inds_store2) %>% 
  select(-City, -County) %>%
  unique() %>% 
  filter(!is.na(region_id)) %>% 
  arrange(Store.Number)


inds_store_fin <- inds_store %>% 
  left_join(store_region_id, by = c('Store.Number')) %>% 
  select(-City)


### OUTPUT
out.sheet <- gs4_get(wdir_inds)

# inds_store_fin %>%
#   arrange(Store.Number) %>%
#   write_sheet(out.sheet,
#               sheet = 'inds_store')

inds_store <- inds_store_fin
# rm(inds_store1, inds_store2, inds_store_fin)

# Rename inds_*--------------------------------------------------

names(inds_store) <- c('Store_id', 'Store', 'region_id')
names(inds_category) <- c('Catrgory_id', 'Category')
names(inds_vendor) <- c('Vendor_id', 'Vendor')
names(inds_item) <- c('Item_id', 'Item', 'Date')
names(inds_iowa) <- c('region_id', 'City', 'County')

Sales %>% names()
inds_store %>% names()


# inds_store -> EDA&Q2 작업 진행
# inds_store <- read_sheet(wdir_inds,
#                          sheet = 'inds_store',
#                          col_names = T, na = '')

inds_store %>% 
  select(Store_id) %>% unique() %>% nrow()


Sales <- Sales %>%
  left_join(inds_store %>% 
              select(-Store), by = 'Store_id') %>% 
  select(-City, -County) %>% 
  select(1, 2, 14, c(2: 13))


Sales %>% head()


rm(store_region_id, tmp_iowa, tmp_store, data, chk_region)


##### CHECK POINT START #####
