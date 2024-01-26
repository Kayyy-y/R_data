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
# data <- read.csv("Iowa_Liquor_Sales.csv")


#----------------------------------------------------------------
# Check Data
#----------------------------------------------------------------

## Default
data %>% names()
str(data)
dim(data)
data %>% summary()

## change type

data <- data %>% 
  mutate(Item.Number = as.integer(Item.Number))

data <- data %>%
  mutate(Date = as.Date(Date, "%m/%d/%Y"))

data <- data %>% 
  arrange(Date)

data %>% 
  select(Date) %>% summary()

data <- data %>% 
  mutate(year = format(Date, format="%Y"))

data <- data %>% 
  mutate(year = as.integer(year))

hist(data$year)

data$year %>% table()
2082059+2063763+2097796+2184483+2279893+2291276+2355558

## data after 2019
data2019 <- data[-(1:15354828) ,]
# write.csv(data2019, file = "data2019.csv", row.names = FALSE)
# rm(data)

###### CHECK POINT1 #######

## Default 2019
data2019 %>% names()
str(data2019)
data2019 %>% summary()

#NA check
table(is.na(data2019 %>% 
              select(c(1,4,5,6,7,8,10,12,14,16))))


### Category?
data2019 %>% 
  select(Category.Name) %>% 
  unique() %>% nrow() #51


## Col check
### PK Check
data2019 %>% 
  select(Invoice.Item.Number) %>% 
  unique() %>% nrow()

### duplicate check
data2019 %>% 
  select(Category, Category.Name, Vendor.Number, Vendor.Name, Item.Number, Item.Description, State.Bottle.Cost) %>% 
  unique() %>% nrow()

### Normalization Check
# Store -> Need Check
data2019 %>% 
  select(Store.Number) %>% unique() %>% nrow() #2465
data2019 %>% 
  select(Store.Name) %>% unique() %>% nrow() #2563
data2019 %>% 
  select(Store.Name, Store.Number) %>% unique() %>% nrow() #2656

# County
data2019 %>% 
  select(County) %>% unique() %>% nrow() #100(NA포함)

# Category
data2019 %>% 
  select(Category) %>% 
  filter(!is.na(Category)) %>% unique() %>% nrow() #61
data2019 %>% 
  select(Category.Name) %>% 
  filter(!is.na(Category.Name)) %>% unique() %>% nrow() #51

# Vendor -> Need Check
data2019 %>% 
  select(Vendor.Number) %>% unique() %>% nrow() #368
data2019 %>% 
  select(Vendor.Name) %>% unique() %>% nrow() #397

# Item 
data2019 %>% 
  select(Item.Number) %>% unique() %>% nrow() #8663
data2019 %>% 
  select(Item.Description) %>% unique() %>% nrow() #8152



#----------------------------------------------------------------
# Check Data in GSheet
#----------------------------------------------------------------

out.sheet <- gs4_get(wdir)

# data2019 %>%
#   select(Store.Number, Store.Name) %>%
#   unique() %>%
#   group_by(Store.Number) %>%
#   mutate(n = n()) %>%
#   filter(n > 1) %>%
#   left_join(data2019 %>%
#               select(Store.Number, Date) %>%
#               arrange(desc(Date)) %>%
#               distinct(Store.Number, .keep_all = T), by = "Store.Number") %>%
#   arrange(Store.Number, Date) %>%
#   sheet_write(out.sheet,
#               sheet = 'Check_Store2')

# data2019 %>%
#   select(Vendor.Number, Vendor.Name) %>%
#   unique() %>%
#   group_by(Vendor.Number) %>%
#   mutate(n = n()) %>%
#   filter(n > 1) %>%
#   left_join(data2019 %>%
#               select(Vendor.Number, Date) %>%
#               arrange(desc(Date)) %>%
#               distinct(Vendor.Number, .keep_all = T), by = "Vendor.Number") %>%
#   arrange(Vendor.Number, Date) %>%
#   sheet_write(out.sheet,
#               sheet = 'Check_Vendor2')



check_store <- read_sheet(wdir,
                       sheet = 'Check_Store',
                       col_names = T, na = '') %>% 
  unique() %>% 
  select(-4, -5) %>% 
  filter(check == 1)

check_vendor <- read_sheet(wdir,
                          sheet = 'Check_Vendor',
                          col_names = T, na = '') %>% 
  unique() %>% 
  select(-4, -5) %>% 
  filter(check == 1)

#----------------------------------------------------------------
# Processing Data
#----------------------------------------------------------------

data2019 <- data2019 %>% 
  mutate(Store.Number = ifelse(Store.Number == 2190, 4829, Store.Number))

#check change
data2019 %>% filter(Store.Number == 2190) %>% nrow()

### Change duplicated Store, Vendor Names
data2019 <- data2019 %>% 
  left_join(check_store, by = "Store.Number") %>% 
  mutate(Store.Name = ifelse(!is.na(Store.Name.y), Store.Name.y, Store.Name.x)) %>% 
  select(-Store.Name.x, -Store.Name.y) %>% 
  left_join(check_vendor, by = "Vendor.Number") %>% 
  mutate(Vendor.Name = ifelse(!is.na(Vendor.Name.y), Vendor.Name.y, Vendor.Name.x)) %>% 
  select(-Vendor.Name.x, -Vendor.Name.y) %>%
  select(-check.x, -check.y)


### count
data2019 %>% names()

data2019 %>% 
  select(Store.Number, Store.Name) %>% unique() %>% nrow() #2464

data2019 %>%  
  select(Vendor.Number, Vendor.Name) %>% unique() %>% nrow() #368


## Normalization
### Create inds_store
data2019 %>% 
  select(Store.Number, Store.Name, Address, City, Zip.Code, Store.Location) %>% 
  unique() %>% nrow() #10318 <-- NEED CHECK

### PK-Store
data2019 %>% 
  select(Store.Number) %>% 
  unique() %>% nrow() #2464

data2019 %>% 
  select(Store.Number, Store.Name) %>% unique() %>% nrow() #2464

#[ Address ]
check_address <- data2019 %>% 
  select(Store.Name, Address) %>% 
  filter(!str_equal(Address, "")) %>% 
  mutate(Address = str_to_upper(Address)) %>% 
  mutate(Address = str_replace_all(Address, "[:punct:]", "")) %>% 
  mutate(Address = str_replace_all(Address, "\n", "")) %>% 
  mutate(Address = str_replace_all(Address, c("ST$"="STREET", 
                                              "DR$" = "DRIVE", 
                                              "RD$" = "ROAD", 
                                              "AVE$" = "AVENUE",
                                              "HWY" = "HIGHWAY"))) %>% 
  unique() %>% 
  group_by(Store.Name) %>%
  mutate(n = n()) %>% 
  filter(n > 1) %>% 
  arrange(Store.Name)


# check_address %>%
#   sheet_write(out.sheet,
#               sheet = 'Check_address')

#### -> delete Address

# [Zip.Code ]
data2019 %>% 
  select(Store.Number, Zip.Code) %>% unique() %>% nrow() #2541

#### -> delete Zip.code

# [ City & County ]
data2019 %>% 
  select(Store.Number, City) %>% unique() %>% nrow() #2537

check_city <- data2019 %>% 
  select(Store.Number, City) %>% 
  unique() %>% 
  filter(!str_equal(City, "")) %>% 
  mutate(City = str_replace_all(City, " ", "")) %>% 
  unique() %>%  
  group_by(Store.Number) %>% 
  mutate(n = n()) %>% 
  filter(n > 1) %>% 
  arrange(Store.Number)

# check_city %>%
#   sheet_write(out.sheet,
#               sheet = 'Check_city')


data2019 %>% 
  select(City) %>% unique() %>% nrow() #473

temp_city <- data2019 %>% select(Store.Number, City, Date) %>% 
  filter(Store.Number == 5619)

table(temp_city$City)

temp_city %>% 
  select(-Store.Number) %>% 
  arrange(City, Date) %>%
  group_by(City) %>% slice(1:1)

# rm(temp_city)

#check city&county -> city 473개
data2019 %>% 
  select(City) %>% unique() %>% nrow()

data2019 %>% 
  select(City, County.Number) %>% 
  mutate(City = ifelse(City == "OTUMWA", "OTTUMWA", City)) %>% 
  unique() %>% 
  group_by(City) %>% 
  mutate(n = n()) %>% 
  filter(n > 1)


data2019 %>% 
  select(City, County.Number) %>% 
  mutate(City = ifelse(City, "OTUMWA", "OTTUMWA")) %>% 
  filter(!is.na(County.Number)) %>% 
  filter(!is.na(City)) %>% 
  unique() %>% 
  group_by(City) %>% 
  mutate(n = n()) %>% 
  filter(n > 1) %>% nrow()  #0!!!!


#### final 
check_add <- data2019 %>% 
  select(City, County.Number, County) %>% 
  filter(!str_equal(County.Number, "")) %>% 
  filter(!str_equal(City, "")) %>% 
  unique() %>% 
  mutate(City = str_replace_all(City, " ", "")) %>% 
  mutate(City = ifelse(City == "OTUMWA", "OTTUMWA", City)) %>% 
  unique() %>% 
  group_by(City) %>% 
  mutate(n = n()) %>% 
    filter(n > 1) %>% 
    arrange(City)

# check_add %>%
#   write_sheet(out.sheet,
#               sheet = 'check_add')


# rm(check_add)


inds_store <- data2019 %>% 
  select(Store.Number, Store.Name, City) %>% 
  filter(!str_equal(City, "")) %>%
  unique() %>% 
  mutate(City = str_replace_all(City, " ", "")) %>% 
  mutate(City = ifelse(City == "OTUMWA", "OTTUMWA", City)) %>% 
  mutate(City = ifelse(Store.Number == 5619, 
                       ifelse(City == "EVANSDALE", "ELKRUNHEIGHTS", City), City)) %>% 
  unique()



### Create inds_iowa (City + County)
# <- 외부 데이터 사용. 실제 Iowa city/county 데이터
# *세부사항 Notion EDA&Q.1 참조

inds_iowa <- read_sheet(wdir_inds,
                        sheet = 'inds_iowa',
                        col_names = T, na = '')

head(inds_iowa)

names(inds_iowa) <- c('City', 'County1', 'County2', 'County3')

inds_iowa <- inds_iowa %>% 
  pivot_longer(cols = starts_with("County"), names_to = "County") %>%
  filter(!is.na(value)) %>%
  mutate(County = value) %>%
  select(-value)

inds_iowa <- inds_iowa %>% 
  mutate(region_id = seq_len(nrow(inds_iowa))) %>% 
  select(region_id, City, County)



### Create inds_country
data2019 %>% select(County.Number) %>% unique() %>% nrow() #100

data2019 %>% select(County) %>% unique() %>% nrow() #99

data2019 %>% 
  select(County.Number, County) %>% 
  unique() %>% nrow() #199 <-- NEED CHECK

inds_county <- 
  data2019 %>% 
  select(County.Number, County) %>% 
  filter(!is.na(County.Number)) %>% 
  unique() %>%
  arrange(County.Number)



### Create inds_Vendor
data2019 %>% 
  select(Vendor.Number, Vendor.Name) %>% 
  unique() %>% nrow() #368

data2019 %>% 
  select(Vendor.Number) %>% unique() %>% nrow() #368

inds_vendor <- data2019 %>% 
  select(Vendor.Number, Vendor.Name) %>% 
  unique()



### Create inds_item
data2019 %>% 
  select(Item.Number, Item.Description) %>% 
  unique() %>% nrow() #9858

data2019 %>% 
  select(Item.Number) %>% unique() %>% nrow() #8663


inds_item <- data2019 %>% 
  select(Item.Number, Item.Description, Date) %>% 
  arrange(Item.Number, desc(Date)) %>%
  group_by(Item.Number) %>% slice(1:1)


### Create inds_category
data2019 %>% 
  select(Category) %>% 
  unique() %>% nrow() #62

data2019 %>% 
  select(Category, Category.Name) %>% 
  unique() %>% nrow() #62

inds_category <- data2019 %>% 
  select(Category, Category.Name) %>% 
  unique()


#### Print Inds ####
out.sheet2 <- gs4_get(wdir_inds)

# inds_county %>%
#   arrange(County.Number) %>% 
#   write_sheet(out.sheet2,
#               sheet = 'inds_county')

# inds_item %>%
#   arrange(Item.Number) %>% 
#   write_sheet(out.sheet2,
#               sheet = 'inds_item')

# inds_vendor %>%
#   arrange(Vendor.Number) %>%
#   write_sheet(out.sheet2,
#               sheet = 'inds_vendor')

# inds_store %>%
#   arrange(Store.Number) %>%
#   write_sheet(out.sheet2,
#               sheet = 'inds_store')

# inds_iowa %>%
#   arrange(region_id) %>%
#   write_sheet(out.sheet2,
#               sheet = 'inds_iowa')

# inds_category %>%
#   arrange(Category) %>%
#   write_sheet(out.sheet2,
#               sheet = 'inds_category')

