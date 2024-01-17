library(dplyr)
library(googlesheets4)



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

# -----------------------------------------------------------------------------------
# Demographic
# -----------------------------------------------------------------------------------

## Summary with Industry
df <- data.frame(list = inds.rating) %>%
  left_join(game_r %>% select(rating) %>% table() %>% as.data.frame() %>%
              rename(list = rating,
                     n = Freq), by = 'list') %>%
  left_join(game_r %>% select(rating) %>% table() %>% prop.table() %>% as.data.frame() %>%
              rename(list = rating,
                     per = Freq), by = 'list')


df <- df %>%
  mutate(per = round(per * 100, digits = 1))

df

sum(df[[3]], na.rm = TRUE)

### Bind with All
# All
df_fin <- data.frame(matrix(nrow = 1, ncol = 3)) %>%
  rename(
    list = X1,
    n = X2,
    per = X3)

df_fin[1,] <- c("All", nrow(game_r), NA)

df_fin

# Bind

df_out <- df_fin %>%
  rbind(df)


df_out %>% write_sheet(wdir,
                           sheet = "check")

rm(df_ind_all); rm(df_emp_all); rm(df_inv_all);
rm(df_out_all);

# -----------------------------------------------------------------------------------
# Chi-squared test +
# -----------------------------------------------------------------------------------

data.G12 %>% select(invest) %>% table()
data.G22 %>% select(invest) %>% table()

df_out_G12 <- df_out_G12 %>%
  rename(n1 = n,
         per1 = per)

df_out_G22 <- df_out_G22 %>%
  rename(n2 = n,
         per2 = per)


df_test2 <- df_out_G12 %>%
  cbind(df_out_G22) %>%
  select(-3, -4, -6)

df_test2

df_test2 <- df_test2 %>%
  mutate(n1 = ifelse(is.na(n1), 0, n1)) %>%
  mutate(n2 = ifelse(is.na(n2), 0, n2))

df_test_ind2 <- df_test2[c(3:11),c(2:3)] %>%
  mutate(n1 = as.numeric(n1)) %>%
  mutate(n2 = as.numeric(n2))

df_test_inv2 <- df_test2[c(20:25),c(2:3)]%>%
  mutate(n1 = as.numeric(n1)) %>%
  mutate(n2 = as.numeric(n2))


#expected check
chisq_ind2 <- chisq.test(df_test_ind2)
data.frame(list = inds.industry2[c(2:10)]) %>%
  cbind(chisq_ind2$expected) %>% as.data.frame()


chisq_inv2 <- chisq.test(df_test_inv2)
data.frame(list = inds.invest2[c(2:7)]) %>%
  cbind(chisq_inv2$expected)



fisher.test(df_test_ind2)

fisher.test(df_test_inv2)




