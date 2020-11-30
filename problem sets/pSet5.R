library(tidyverse)
#1
pitches <- read_csv("data/pitchfx_2015.csv")
pitches
#2
called_pitches <- pitches %>%
  filter(Description %in% c("Called Strike", "Ball")) %>%
  mutate(Call = case_when(Description == "Called Strike" ~ 1, Description == "Ball" ~ 0))
called_pitches
#3
g <- ggplot(data = called_pitches)
#4
g <- g + stat_summary_2d(mapping = aes(x = X, y = Z, z = Call))
g
#5
g <- g + scale_fill_distiller("P(Called Strike)", palette = "RdBu")
g
#6
g <- g + annotate("rect", xmin = -8.5, xmax = 8.5, ymin = 19, ymax = 41.5, alpha = 0, color = "black")
g
g <- g + theme_classic()
g
g <- g + theme(axis.title.x = element_blank())
g <- g + theme(axis.title.y = element_blank())
g <- g + labs(title = "Estimated Strike Zone")
g

# Part 2
#1
raw_boxscore <- read_csv("data/nba_boxscore.csv")
raw_boxscore
table(raw_boxscore["Tm"])
filter(raw_boxscore, Tm =="WSB")

#2
team_boxscore <- raw_boxscore %>%
  filter(Tm != "TOT") %>%
  group_by(Season, Tm) %>%
  mutate(FGM = sum(FGM), 
         FGA = sum(FGA), 
         TPM = sum(TPM), 
         TPA = sum(TPA), 
         FTM = sum(FTM), 
         FTA = sum(FTA),
         PTS = sum(PTS),
         FGP = FGM/FGA,
         TPP = TPM/TPA,
         FTP = FTM/FTA
  )
team_boxscore <- team_boxscore %>% ungroup()

#3
reduced_boxscore <- team_boxscore %>%
  filter(Tm %in% c("BOS", "CLE", "DAL", "DET", "GSW", "LAL", "MIA", "SAS")
         )

f <- ggplot(data = reduced_boxscore) + geom_point(aes(x = Season, y = TPP, color = Tm))
f


