library(dplyr)



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

# (recom) hours와 helpful의 상관 관계

recom_used <- recom %>% 
  filter(hours > 0)

test <- recom[sample(nrow(recom), size=400000), ]

test %>% names()

hours1 <- as.vector(test[,6])
helpful1 <- as.vector(test[,2])

cor.test(hours1, helpful1)

test_p <- test %>% select(2,6)

pairs(test_p)

test %>% summary()


# (game) positive_rate와 discount_per의 상관 관계

game %>% names()

pair2 <- game %>% select(8, 14)

str(pair2)

pairs(pair2)

pos_rate <- as.vector(game[,8])
dis_per <- as.vector(game[,14])

cor.test(pos_rate, dis_per)






