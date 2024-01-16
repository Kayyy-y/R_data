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
2



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
setwd("./0. Data/0. Source")
game <- read.csv("games.csv")
recom <- read.csv("recommendations.csv")
users <- read.csv("users.csv")

# -----------------------------------------------------------------------------------
# Check data - game
# -----------------------------------------------------------------------------------

### games
game %>% head()
game %>% names()
str(game)

game <- game %>%
  mutate(win = ifelse(win == "false", FALSE, TRUE)) %>%
  mutate(mac = ifelse(mac == "false", FALSE, TRUE)) %>%
  mutate(linux = ifelse(linux == "false", FALSE, TRUE)) %>%
  mutate(steam_deck = ifelse(steam_deck == "false", FALSE, TRUE)) %>%
  mutate(discount_per = as.double(discount)) %>%
  mutate(date_release = as.Date(date_release))

game_summary <- game %>% summary()


#check PK - OK
game %>% 
  select(app_id) %>% unique() %>% count()

# duplicated? - title(x)
game %>% 
  select(title) %>% 
  group_by(title) %>% 
  mutate(n = n()) %>% head(10)

# NA check
#NA check
table(is.na(game))


## mutate(discount)
# (old)         (new)
# discount -> discount_per
# (new col)
# discount or not -> discount

game <- game %>% 
  mutate(discount = ifelse(discount_per > 0, TRUE, FALSE))



# -----------------------------------------------------------------------------------
# Check data - recom
# -----------------------------------------------------------------------------------

### recom
recom %>% head()
recom %>% names()
str(recom)


recom <- recom %>%
  mutate(is_recommended = ifelse(is_recommended == "false", FALSE, TRUE)) %>%
  mutate(date = as.Date(date))

recom_summary <- recom %>% summary()

#check PK - OK
recom %>% 
  select(review_id) %>% unique() %>% count()

#duplicate check
recom %>%  
  select(user_id) %>% 
  group_by(user_id) %>% 
  mutate(n=n()) %>% 
  arrange(desc(n)) %>% unique() %>% head()

#NA check
table(is.na(recom))



# -----------------------------------------------------------------------------------
# Check data - users
# -----------------------------------------------------------------------------------

### users
users %>% head()
users %>% names()
str(users)

users_summary <- users %>% summary()

#check PK - OK
users %>% 
  select(user_id) %>% unique() %>% count()

#NA check
table(is.na(users))



# -----------------------------------------------------------------------------------
# Data Output
# -----------------------------------------------------------------------------------

setwd("..")
setwd("./1. Cleaned_data")
write.csv(game, "./games_edit.csv", row.names = FALSE)
write.csv(recom, "./recommendations_edit.csv", row.names = FALSE)
write.csv(users, "./users_edit.csv", row.names = FALSE)

