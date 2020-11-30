library(tidyverse)
library(modelr)

data <- read_csv(file = "data/Final_Data.csv")

data
View(data)
def_stats <- select(data, c(RANK, TEAM, W, L, DEF_RTG, DREB, STL, BLK, SEASON, PLAY))
def_stats
arrange(def_stats, desc(SEASON))
def_stats
View(def_stats)

standardize <- function(x){
  mu <- mean(x, na.rm = TRUE)
  sigma <- sd(x, na.rm = TRUE)
  return( (x - mu)/sigma )
}

def_stats <- def_stats %>% 
  group_by(SEASON) %>%
  filter(PLAY == "reg",
         SEASON >= "2010-11") %>%
  mutate(Win_Percentage = W/(W+L),
         Standardize_DREB = standardize(DREB),
         Standardize_STL = standardize(STL),
         Standardize_BLK = standardize(BLK)
  )

# experiment
def_stats <- def_stats %>% 
  group_by(SEASON) %>%
  filter(PLAY == "reg",
         SEASON >= "2007-08") %>%
  mutate(Win_Percentage = W/(W+L),
         Standardize_DREB = standardize(DREB),
         Standardize_STL = standardize(STL),
         Standardize_BLK = standardize(BLK)
  )

def_stats
View(def_stats)



# low defensive rating is good

scatter_DREB <- ggplot(data = def_stats)
scatter_DREB <- scatter_DREB + geom_point(aes(x = DREB, y = Win_Percentage, color = TEAM, alpha = 0.1))
scatter_DREB
cor(def_stats[["DREB"]], def_stats[["Win_Percentage"]])

scatter_STL <- ggplot(data = def_stats)
scatter_STL <- scatter_STL + geom_point(aes(x = STL, y = Win_Percentage, color = TEAM, alpha = 0.1))
scatter_STL
cor(def_stats[["STL"]], def_stats[["Win_Percentage"]])

scatter_BLK <- ggplot(data = def_stats)
scatter_BLK <- scatter_BLK + geom_point(aes(x = BLK, y = Win_Percentage, color = TEAM, alpha = 0.1))
scatter_BLK
cor(def_stats[["BLK"]], def_stats[["Win_Percentage"]])

scatter_DEF_RTG <- ggplot(data = def_stats)
scatter_DEF_RTG <- scatter_DEF_RTG + geom_point(aes(x = DEF_RTG, y = Win_Percentage, color = TEAM, alpha = 0.1))
scatter_DEF_RTG
cor(def_stats[["DEF_RTG"]], def_stats[["Win_Percentage"]])


hist <- ggplot(data = def_stats)
hist <- hist + geom_histogram(aes(x = Standardize_DREB), binwidth = 0.25)
hist


cor(def_stats[["STL"]], def_stats[["Win_Percentage"]])
cor(def_stats[["BLK"]], def_stats[["Win_Percentage"]])
cor(def_stats[["DREB"]], def_stats[["Win_Percentage"]])
cor(def_stats[["DEF_RTG"]], def_stats[["Win_Percentage"]])

fit <- lm(Win_Percentage ~ STL + BLK + DREB + DEF_RTG, data = def_stats)
fit
fit[["coefficients"]]

def_stats <- def_stats %>% 
  add_predictions(fit) %>% 
  add_residuals(fit)
def_stats 
select(def_stats, TEAM, SEASON, Win_Percentage, pred, resid, PLAY)

scatter_DEF_RTG <- ggplot(data = def_stats)
scatter_DEF_RTG <- scatter_DEF_RTG + geom_point(aes(x = DEF_RTG, y = Win_Percentage, color = TEAM))
scatter_DEF_RTG <- scatter_DEF_RTG + geom_abline(intercept = 2.1869267503, slope = -0.0241879528, color = "red")
scatter_DEF_RTG1 <- scatter_DEF_RTG + geom_abline(intercept = 2.1869267503, slope = -0.0241879528, color = "red")

lm(Win_Percentage ~ DEF_RTG, data = def_stats)
ggplot(data = def_stats) +
  geom_abline(slope = -0.02621, intercept = 3.19987, color = "blue") +
  geom_point(aes(DEF_RTG,Win_Percentage))

lm(Win_Percentage ~ DREB, data = def_stats)
ggplot(data = def_stats) +
  geom_abline(slope = 0.0294, intercept =  -0.4447, color = "red") +
  geom_point(aes(DREB,Win_Percentage), alpha = 0.4)

lm(Win_Percentage ~ STL, data = def_stats)
ggplot(data = def_stats) +
  geom_abline(slope = 0.04326, intercept = 0.16757, color = "green") +
  geom_point(aes(STL,Win_Percentage), alpha = 0.4)

lm(Win_Percentage ~ BLK, data = def_stats)
ggplot(data = def_stats) +
  geom_abline(slope = 0.0558, intercept =  0.2272, color = "orange") +
  geom_point(aes(BLK,Win_Percentage), alpha = 0.4)

lm(Win_Percentage ~ DEF_RTG, data = def_stats)
ggplot(data = def_stats) +
  geom_abline(slope = -0.0316, intercept =  3.7888, color = "blue") +
  geom_point(aes(DEF_RTG,Win_Percentage), alpha = 0.4)

