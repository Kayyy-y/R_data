library(dplyr)
library(googlesheets4)
library(ggplot2)
library(hrbrthemes)


# -----------------------------------------------------------------------------------
# Account Verification
# -----------------------------------------------------------------------------------
# gs4_auth()
1


# -----------------------------------------------------------------------------------
# Read data
# -----------------------------------------------------------------------------------

wdir <- 'https://docs.google.com/spreadsheets/d/1tlAK3JjbI1LAPCHVcelPbQCKkI_kIH8CWFwga_s5osw/'

### Data;
setwd("./0. Data/1. Cleaned_data")
game <- read.csv("games_edit.csv")
recom <- read.csv("recommendations_edit.csv")

# -----------------------------------------------------------------------------------
# Topic 2 유저 총 평가(rating)별 양상은 어떨까?
# -----------------------------------------------------------------------------------

game_r <- game %>% 
  mutate(rating = ifelse(rating == "Overwhelmingly Positive", "Very Positive", rating)) %>% 
  mutate(rating = ifelse(rating == "Overwhelmingly Negative", "Very Negative", rating)) %>% 
  mutate(rating = ifelse(rating == "Positive", "Very Positive", rating)) %>% 
  mutate(rating = ifelse(rating == "Negative", "Very Negative", rating))

game_r %>% select(rating) %>% unique()


## Setting - inds

### Index Setting; Rating
inds.rating <- c('Very Negative', 'Mostly Negative', 'Mixed', 'Mostly Positive', 'Very Positive')

### Setting levels
game_r <- game_r |>
  mutate(inds.rating = factor(rating, levels = inds.rating)) |>
  mutate_at(
    vars(starts_with('inds')), as.numeric
  )


## 할인율(discount_per) ##

### discount X인 게임 filter
game_r_dis <- game_r %>% 
  filter(discount_per > 0)
game_r_dis %>% names()

game_r_dis <- game_r_dis %>% 
  mutate(ind_dis_per = cut(discount_per,
                           breaks = c(-1,9,19,29,39,49,59,69,79,89,99),
                           labels=(c(10,20,30,40,50,60,70,80,90,100)))) %>% 
  select(inds.rating, ind_dis_per)

# heat map matrix

heat_mat1 <- table(game_r_dis$inds.rating, game_r_dis$ind_dis_per)

inds.percent <- c(paste0(seq(10, 100,10),'%'))


heat_mat1_df <- as.data.frame(heat_mat1) %>% 
  rename(rating = Var1,
         dis_per = Var2) %>% 
  mutate(rating = factor(rating,levels = c(1,2,3,4,5),labels = inds.rating),
         dis_per = factor(dis_per,levels = c(seq(10,100,10)),labels = inds.percent))
  

ggplot(heat_mat1_df, aes(dis_per,  rating, fill= Freq)) + 
  geom_tile() +
  scale_fill_distiller(palette = "YlGnBu") +
  theme_ipsum() +
  geom_text(aes(dis_per,  rating, label=Freq), color = "white", size = 4)

rm(game_r_dis); rm(heat_mat1_df); rm(heat_mat1);





## reviewer 플레이 시간(hours) 평균 ##

### hours 0 이상인 recom filter
recom0 <- recom %>% 
  filter(hours > 0)

recom0_game <- recom0 %>% 
  group_by(app_id) %>% 
  mutate(avg_hours = mean(hours)) %>% 
  select(app_id, avg_hours) %>% 
  unique()

game_hour <- game_r %>%
  select(app_id,inds.rating) %>% 
  left_join(recom0_game %>% 
               select(app_id,avg_hours), by='app_id')

game_hour %>% names()

game_hour$avg_hours %>% summary()


game_hour <- game_hour %>% 
  filter(!is.na(avg_hours)) %>% 
  mutate(ind_hours = cut(avg_hours,
                           breaks = c(-1,4,9,14,19,24,499),
                           labels=(c(5,10,15,20,25,500)))) %>% 
  select(inds.rating, ind_hours)




heat_mat2 <- table(game_hour$inds.rating, game_hour$ind_hours)

total <- recom0_game %>% nrow()

heat_mat2_df <- as.data.frame(heat_mat2) %>% 
  rename(rating = Var1,
         avg_hours = Var2) %>% 
  mutate(rating = factor(rating,levels = c(1,2,3,4,5),labels = inds.rating)) %>% 
  mutate(per = paste0(round((Freq/total) * 100, digits = 2), '%','\n(',Freq,')'))


ggplot(heat_mat2_df, aes(avg_hours,  rating, fill= Freq)) + 
  geom_tile() +
  scale_fill_distiller(palette = "YlGnBu")+
  theme_ipsum()+
  geom_text(aes(avg_hours,  rating, label=per), color = "white", size = 4)


str(heat_mat2_df)





