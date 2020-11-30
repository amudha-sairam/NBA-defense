library(tidyverse)
hitting_qualified <- read_csv(file = "data/hitting_qualified.csv")
hitting_qualified
#2 1884 and 2015
arrange(hitting_qualified, yearID)
arrange(hitting_qualified, desc(yearID))
#3 1884 and 2015
summarize(hitting_qualified, yearID[1], yearID[12043])

#4 
hitting_qualified
hitting_qualified <- mutate(hitting_qualified, 
                            IBB = as.integer(IBB), 
                            HBP = as.integer(HBP), 
                            SH = as.integer(SH), 
                            SF = as.integer(SF), 
                            GIDP = as.integer(GIDP))

select(hitting_qualified, playerID, yearID, AB, IBB, HBP, SH, SF, GIDP)

#5
hitting_qualified <- replace_na(hitting_qualified, list(IBB = 0, HBP = 0, SH = 0, SF = 0, GIDP = 0))

#6
hitting_qualified <- mutate(hitting_qualified, X1B = H - X2B - X3B - HR)
hitting_qualified

#7
hitting_qualified <- mutate(hitting_qualified, uBB = BB - IBB)
hitting_qualified

#8
#walk percentage
hitting_qualified <- mutate(hitting_qualified, BBP = (BB/PA))
#strikeout percentage
hitting_qualified <- mutate(hitting_qualified, KP = (SO/PA))
#On-Base Percentage
hitting_qualified <- mutate(hitting_qualified, OBP = ((H+BB+HBP)/(AB+BB+HBP+SF)))
#slugging
hitting_qualified <- mutate(hitting_qualified, SLG = (X1B+2*X2B+3*X3B+4*HR)/AB)
#On-Base Plus Slugging
hitting_qualified <- mutate(hitting_qualified, OPS = OBP + SLG)
#weighted On-Base Average
hitting_qualified <- mutate(hitting_qualified, wOBA = (0.687*uBB+0.718*HBP+0.881*X1B+1.256*X2B+1.594*X3B+2.065*HR)/(AB+uBB+SF+HBP))

    # OR
hitting_qualified <- mutate(hitting_qualified,
                            BBP = (BB/PA),
                            KP = (SO/PA),
                            OBP = ((H+BB+HBP)/(AB+BB+HBP+SF)),
                            SLG = (X1B+2*X2B+3*X3B+4*HR)/AB,
                            OPS = (OBP + SLG),
                            wOBA = (0.687*uBB+0.718*HBP+0.881*X1B+1.256*X2B+1.594*X3B+2.065*HR)/(AB+uBB+SF+HBP)
                            )
hitting_qualified
#9 

hitting_qualified <- mutate(hitting_qualified, OBP_rating = case_when(
  OBP <= 0.29 ~ "Awful", 
  OBP <= 0.3 & OBP > 0.29 ~ "Poor",
  OBP <= 0.31 & OBP > 0.3 ~ "Below Average",
  OBP <= 0.32 & OBP > 0.31 ~ "Average",
  OBP <= 0.34 & OBP > 0.32 ~ "Above Average",
  OBP <=0.37 & OBP > 0.34 ~ "Great",
  OBP <= 0.39 & OBP > 0.37 ~ "Excellent"
))

select(hitting_qualified, playerID, OBP_rating)

#10 
tmp_batting <- filter(hitting_qualified, yearID >= 2000 & yearID <= 2015)
#OR
            tmp_batting <- filter(hitting_qualified, yearID %in% 2000:2015)
tmp_batting

#11
batting_recent <- select(filter(hitting_qualified, yearID >= 2000 & yearID <= 2015), playerID, yearID, teamID, lgID, BBP, KP, OBP, SLG, OPS, wOBA)
batting_recent

#12
ggplot(data = batting_recent) + geom_histogram(mapping = aes(x=BBP), bins = 50)




