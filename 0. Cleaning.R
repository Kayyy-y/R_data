library(googlesheets4)
library(tidyverse)
library(dplyr)
# library(xlsx)
# library(readxl)
library(lubridate)
library(stringr)



# -----------------------------------------------------------------------------------
# Account Verification
# -----------------------------------------------------------------------------------
gs4_auth()
1



# -----------------------------------------------------------------------------------
# Setting wdir
# -----------------------------------------------------------------------------------

### Data
wdir <- 'https://docs.google.com/spreadsheets/d/1tlAK3JjbI1LAPCHVcelPbQCKkI_kIH8CWFwga_s5osw/'

out.sheet <- gs4_get(wdir)

# -----------------------------------------------------------------------------------
# Read data
# -----------------------------------------------------------------------------------

### Data;
setwd("C:/Users/lenovo/Desktop/Game Recommendations on Steam/0. Data/0. Source")
game <- read.csv("games.csv")
recom <- read.csv("recommendations.csv")
users <- read.csv("users.csv")


### games
game %>% head()
game %>% names()

game <- game %>% 
  mutate(win = ifelse(win == "false", FALSE, TRUE)) %>% 
  mutate(mac = ifelse(mac == "false", FALSE, TRUE)) %>% 
  mutate(linux = ifelse(linux == "false", FALSE, TRUE)) %>% 
  mutate(steam_deck = ifelse(steam_deck == "false", FALSE, TRUE))

game_summary <- game %>% summary()


#check PK - OK
game %>% 
  select(app_id) %>% unique() %>% count()


### recom
recom %>% head()
recom %>% names()

recom_summary <- recom %>% summary()

#check PK - OK
recom %>% 
  select(review_id) %>% unique() %>% count()


### users
users %>% head()
users %>% names()

users_summary <- users %>% summary()

#check PK - OK
users %>% 
  select(user_id) %>% unique() %>% count()


# -----------------------------------------------------------------------------------
# Check
# -----------------------------------------------------------------------------------

recom %>% 
  left_join(users) %>% 
  filter(products == na) %>% count()

recom %>% select(user_id) %>% unique() %>% count()
recom %>% select(app_id) %>% unique() %>% count()



# -----------------------------------------------------------------------------------
# Output
# -----------------------------------------------------------------------------------

# data03 %>%
#   arrange(이름) %>%
#   select(-index1) %>%
#   write_sheet(out.sheet,
#               sheet = '8.수료생FW3')


data05 %>%
  arrange(이름) %>%
  filter(KEY != "노션 - 이력서 공유") %>%
  write_sheet(out.sheet,
              sheet = '6.수료생FW2')
