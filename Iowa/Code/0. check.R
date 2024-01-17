library(dplyr)


data <- read.csv("Iowa_Liquor_Sales.csv")


#----------------------------------------------------------------
# Check Data
#----------------------------------------------------------------

## Default
data %>% names()
str(data)
dim(data)
data %>% summary()

### Category?
data %>% 
  select(Category.Name) %>% 
  unique() %>% nrow()


## Col check
### PK Check
data %>% 
  select(Invoice.Item.Number) %>% 
  unique() %>% nrow()

### duplicate check
data %>% 
  select(Category, Category.Name, Vendor.Number, Vendor.Name, Item.Number, Item.Description, State.Bottle.Cost) %>% 
  unique() %>% nrow()

### Normalization Check
# Item
data %>% 
  select(Item.Number) %>% unique() %>% nrow() #7395
data %>% 
  select(Item.Description) %>% unique() %>% nrow() #5865
# Vendor
data %>% 
  select(Vendor.Number) %>% unique() %>% nrow() #272
data %>% 
  select(Vendor.Name) %>% unique() %>% nrow() #394
# Category
data %>% 
  select(Category) %>% 
  filter(!is.na(Category)) %>% unique() %>% nrow() #107
data %>% 
  select(Category.Name) %>% 
  filter(!is.na(Category.Name)) %>% unique() %>% nrow() #131
# 



## Normalization
### Create Store



### Create Country



### Create Vendor



### Create Item





### 



