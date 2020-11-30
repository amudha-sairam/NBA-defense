library(tidyverse)
fg_train <- read_csv("data/nfl_fg_train.csv")
#1
fg_train <- fg_train %>% mutate(phat_all = mean(Success))
fg_train
#2
fg_train <- fg_train %>% 
  group_by(Kicker) %>%
  mutate(phat_kicker = mean(Success)) %>%
  ungroup()
fg_train
#3
fg_train <- fg_train %>% 
  mutate(Dist_10 = cut(Distance, breaks = seq(from = 10, to = 80, by = 10))) %>%
  group_by(Dist_10) %>%
  mutate(phat_dist_10 = mean(Success)) %>%
  ungroup()
fg_train

ggplot(data = fg_train) + geom_point(mapping = aes(x = Distance, y = phat_dist_10), alpha = 0.1)
#4




