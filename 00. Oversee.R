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
# gs4_auth()
1


# -----------------------------------------------------------------------------------
# Read data
# -----------------------------------------------------------------------------------

### Data;
setwd("./0. Data/1. Cleaned_data")
game <- read.csv("games_edit.csv")
recom <- read.csv("recommendations_edit.csv")
users <- read.csv("users_edit.csv")


# -----------------------------------------------------------------------------------
# Check data - game
# -----------------------------------------------------------------------------------

game %>% select(rating) %>% unique() %>% count()

game %>% select(date_release, title) %>% arrange(date_release) %>% head()

game %>% select(date_release, title) %>% arrange(desc(date_release)) %>% head()


game %>% select(win) %>% table()
game %>% select(mac) %>% table()
game %>% select(linux) %>% table()


game %>% select(win, mac, linux) %>% table()

game %>% filter(win == FALSE & mac == FALSE & linux == FALSE) %>% 
  select(title, steam_deck) %>% 
  head()

game %>% 
  mutate(title = str_replace_all(tolower(title)," ", "")) %>%
  mutate(check = str_detect(title, "soudtrack")) %>% 
  select(check) %>% summary()
  filter(check == T) %>% count()

game %>% 
    mutate(title = str_replace_all(tolower(title)," ", "")) %>%
    filter(str_detect(title, "soundtrack")) %>% 
    count()

game %>% 
  mutate(title = str_replace_all(tolower(title)," ", "")) %>%
  filter(str_detect(title, "soundtrack")) %>% 
  filter(win == T | mac == T | linux == T) %>% 
  select(title, win, mac, linux, steam_deck) %>% count()


game %>% filter(win == FALSE & mac == FALSE & linux == FALSE) %>% 
  mutate(steam_deck = ifelse(steam_deck == "false" , FALSE, TRUE)) %>% 
  select(title, steam_deck) %>% summary()

game %>% filter(win == FALSE & mac == FALSE & linux == FALSE) %>% 
  mutate(title = str_replace_all(tolower(title)," ", "")) %>%
  filter(str_detect(title, "soundtrack")) %>% count()


table(game$rating)




# -----------------------------------------------------------------------------------
# Check Data - recom
# -----------------------------------------------------------------------------------

recom %>% 
  left_join(users) %>% 
  filter(products == na) %>% count()

recom %>% select(user_id) %>% unique() %>% count()
recom %>% select(app_id) %>% unique() %>% count()


recom %>% 
  filter(hours <= 0.5) %>% count()





# -----------------------------------------------------------------------------------
# Check Data - user
# -----------------------------------------------------------------------------------
