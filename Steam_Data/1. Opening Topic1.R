library(googlesheets4)
library(tidyverse)
library(dplyr)
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
# Opening
# -----------------------------------------------------------------------------------

# graph
recom.test <- recom %>% 
  filter(helpful != 0 | funny != 0) %>% select(2, 3)

rand_recom <- recom.test[sample(nrow(recom.test), size=400000), ]


pairs(rand_recom)

# Pearson
recom_h <- as.vector(rand_recom[,2])

recom_f <- as.vector(rand_recom[,3])

cor.test(recom_f, recom_h)


# boxplot
boxplot(rand_recom$funny)
boxplot(rand_recom$helpful)

rand_recom %>% 
  select(funny) %>% summary()

rand_recom %>% 
  select(helpful) %>% summary()


# -----------------------------------------------------------------------------------
# Topic 1 Steam에서 리뷰는 어떤 성격을 가질까?
# -----------------------------------------------------------------------------------



