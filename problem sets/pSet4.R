library(tidyverse)
library(Lahman)
Batting <- as_tibble(Batting)
Batting
View(Batting)
Batting <- Batting %>% replace_na(list(HBP = 0, SH = 0, SF = 0, IBB = 0))

#1
batting <- Batting %>% 
  mutate(PA = AB + BB + HBP + SH + SF,
         X1B = H - X2B - X3B - HR,
         uBB = BB - IBB,
         BA = H/AB,
         SLG = (X1B+2*X2B+3*X3B+4*HR)/AB,
         KP = (SO/PA),
         BBP = (BB/PA),
         OBP = ((H+BB+HBP)/(AB+BB+HBP+SF)),
         OPS = (OBP + SLG),
         wOBA = (0.687*uBB+0.718*HBP+0.881*X1B+1.256*X2B+1.594*X3B+2.065*HR)/(AB+uBB+SF+HBP)) %>%
  filter(lgID %in% c("AL", "NL") & PA >= 502.2) %>%
  select(playerID, yearID, teamID, lgID, PA, BA, OBP, OPS, wOBA)

View(batting)

#2
standardize <- function(x){
  mu <- mean(x, na.rm = TRUE)
  sigma <- sd(x, na.rm = TRUE)
  return ( (x-mu)/sigma )
}

batting <- 
  batting %>%
  mutate(zBA_all = standardize(BA), 
         zOBP_all = standardize(OBP), 
         zOPS_all = standardize(OPS), 
         zwOBA_all = standardize(wOBA))

batting %>% arrange(zBA_all, zOBP_all, zOPS_all, zwOBA_all)
View(batting)

batting %>% select(playerID, yearID, zBA_all) %>% arrange(zBA_all)

arrange(batting, zBA_all)
arrange(batting, desc(zBA_all))

# deerro and duffyhu

#3 
batting <- batting %>% group_by(yearID) %>% mutate(zBA_year = standardize(BA), zOBP_year = standardize(OBP), zOPS_year = standardize(OPS), zwOBA_year = standardize(wOBA))


#4
batting <- batting %>% ungroup() %>% group_by(yearID) %>% mutate(zBA_year_lg = standardize(BA), zOBP_year_lg = standardize(OBP), zOPS_year_lg = standardize(OPS), zwOBA_year_lg = standardize(wOBA))

#5
batting <- batting %>% ungroup() %>% mutate(HIST_ERA = case_when(1871 <= yearID & yearID <= 1892 ~ "Pioneer", 1893 <= yearID & yearID <= 1919 ~ "Spitball", 1920 <= yearID & yearID <= 1946 ~ "Landis", 1947 <= yearID & yearID <= 1968 ~ "Baby Boomer", 1969 <= yearID & yearID <= 1992 ~ "Artifical Turf", 1993 <= yearID ~ "Camden Yards")) %>% group_by(HIST_ERA) %>% mutate(zBA_hist = standardize(BA), zOBP_hist = standardize(OBP), zOPS_hist = standardize(OPS), zwOBA_hist = standardize(wOBA))

#6
batting <- batting %>% 
  group_by(HIST_ERA)

#CHECK THISSSSS

batting <- 
  batting %>%
  mutate(zBA_hist = standardize(zBA_all), 
         zOBP_hist = standardize(zOBP_all), 
         zOPS_hist = standardize(zOPS_all), 
         zwOBA_hist = standardize(zwOBA_all))

#7
batting <- batting %>% ungroup()

# part 2

#1
relative <- function(x) {
  med <- median(x, na.rm = TRUE)
  return(x/med)
}

#2
mlb_payrolls <- read_csv("data/mlb_payrolls.csv")
mlb_payrolls

#3
mlb_payrolls <- mlb_payrolls %>% 
 group_by(Year) %>%
  mutate(Relative_Payroll = relative(Team_Payroll))
mlb_payrolls

#4
ggplot(data = mlb_payrolls) + geom_point(mapping = aes(x = Relative_Payroll, y = Winning_Percentage))

#5
payroll_avg <- mlb_payrolls %>% 
  group_by(Year) %>%
  summarize(Team_Avg_Pay = mean(Team_Payroll), Relative_Avg_Pay = mean(Relative_Payroll))
payroll_avg

#6
ggplot(data = mlb_payrolls) + geom_point(mapping = aes(x = Year, y = Team_Payroll)) + 
  geom_line(data = payroll_avg, mapping = aes(x=Year, y = Team_Avg_Pay, col = 'red'))

ggplot(data = mlb_payrolls) + geom_point(mapping = aes(x = Year, y = Relative_Payroll)) + 
  geom_line(data = payroll_avg, mapping = aes(x=Year, y = Relative_Avg_Pay, col = 'red'))

#7
correlation <- mlb_payrolls %>%
  group_by(Year) %>%
  summarize(correlation = cor(Relative_Payroll, Winning_Percentage))
correlation 

#Challenge
batting_2014_2015 <-
batting %>%
filter(yearID %in% c(2014, 2015)) %>%
group_by(playerID) %>%
filter(n() == 2) %>% 
select(playerID, yearID, BA) %>%
arrange(playerID) %>%
save(file = "data/batting_2014_2015.RData")


