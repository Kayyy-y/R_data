library(googlesheets4)
library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr)
library(multilinguer)
# library(KoNLP)


# -----------------------------------------------------------------------------------
# Account Verification
# -----------------------------------------------------------------------------------
# gs4_auth()
2



#----------------------------------------------------------------
# Read Data
#----------------------------------------------------------------

wdir <- 'https://docs.google.com/spreadsheets/d/1a1rbVgVJp6n6xd8r7Qxu1WTwo-9n1cnzd_7lk7FX6fM/'

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

##### CHECK POINT2 #####


## Normalization
### Create Store
data2019 %>% 
  select(Store.Number, Store.Name, Address, City, Zip.Code, Store.Location) %>% 
  unique() %>% nrow() #10318 <-- NEED CHECK


### Create Country
data2019 %>% select(County.Number) %>% unique() %>% nrow()
data2019 %>% select(County) %>% unique() %>% nrow()
data2019 %>% 
  select(County.Number, County) %>% 
  unique() %>% nrow() #199 <-- NEED CHECK

inds_county <- 
  data2019 %>% 
  select(County.Number, County) %>% 
  filter(!is.na(County.Number)) %>% 
  unique() %>%
  arrange(County.Number)


### Create Vendor
data2019 %>% 
  select(Vendor.Number, Vendor.Name) %>% 
  unique() %>% nrow() #368

data2019 %>% 
  select(Vendor.Number) %>% unique() %>% nrow()


### Create Item
data2019 %>% 
  select(Item.Number, Item.Description) %>% 
  unique() %>% nrow() #9858

data2019 %>% 
  select(Item.Number) %>% unique() %>% nrow() #8663


data2019 %>% 
  select(Item.Number, Item.Description) %>% 
  unique() %>% 
  group_by(Item.Number) %>% 
  mutate(cnt = n()) %>% 
  group_by(cnt) %>% 
  summarise(n=n())
  

check_item <- data2019 %>% 
  select(Item.Number, Item.Description) %>% 
  unique() %>% 
  group_by(Item.Number) %>% 
  mutate(cnt = n()) %>% 
  arrange(cnt, Item.Number) %>% 
  select(cnt, Item.Number, Item.Description)

# check_item %>% 
#   filter(cnt > 1) %>% 
#   sheet_write(out.sheet, sheet = 'Check_item')

###????
check_item2 <- check_item %>%
  select(Item.Number, Item.Description) %>% unique() %>% 
  mutate(Description2 = str_replace_all(Item.Description,"[^a-zA-Z0-9]","")) %>% 
  mutate(Description2 = str_replace_all(Description2, "YR", "YEAR")) %>% 
  select(-Item.Description) %>%
  unique() %>% 
  group_by(Item.Number) %>% 
  mutate(cnt = n()) %>% 
  select(-Description2) %>% 
  left_join(check_item %>% select(Item.Number, Item.Description), by="Item.Number") %>% 
  head()


check_item2 %>%
  filter(cnt > 1) %>%
  sheet_write(out.sheet, sheet = 'Check_item')


mutate(data =
         str_replace_all(data,"<(/)?([a-zA-Z0-9]*)(\\s[a-zA-Z0-9]*=[^>]*)?(\\s)*(/)?>"," ")) %>%
  mutate(data =
           str_replace_all(data,"&nbsp"," ")) %>%
  mutate(data =
           gsub("\\*","",data)) %>%
  mutate(data =
           gsub("&gt;"," ",data)) %>%
  mutate(data =
           gsub("&lt;"," ", data)) %>%
  mutate(data =
           str_replace_all(data,"\t"," ")) %>%
  mutate(data =
           str_replace_all(data,"\\s+"," "))


### Create Invoice
data2019 %>% 
  select(Invoice.Item.Number, Date, Store.Number, County.Number, Category, Vendor.Number, Item.Number, Pack, State.Bottle.Retail, Bottles.Sold, Sale..Dollars., Volume.Sold..Liters.)

### 



