teams <- c("Chicago Cubs", "Texas Rangers", "Washington Nationals", "Cleveland Indians", "Boston Red Sox", "Los Angeles Dodges", "Baltimore Orioles", "Toronto Blue Jays", "New York Mets", "San Francisco Giants", "Detroit Tigers", "St. Louis Cardinals", "Seattle Mariners", "New York Yankees", "Houston Astros", "Kansas City Royals", "Miami Marlins", "Pittsburgh Pirates", "Chicago White Sox", "Colorado Rockies", "Los Angeles Angels", "Milwaukee Brewers", "Philadeplphia Phillies", "Arizona Diamondbacks", "Oakland Athletics", "Atlanta Braves", "Tampa Bay Rays", "San Diego Padres", "Cincinnati Reds", "Minnesota Twins")
length(teams)

wins <- c(103, 95, 95, 94, 93, 91, 89, 89, 87, 87, 86, 86, 86, 84, 84, 81, 79, 78, 78, 75, 74, 73, 71, 69, 69, 68, 68, 68, 68, 59)
length(wins)

losses <- c(58, 67, 67, 67, 69, 71, 73, 73, 75, 75, 75, 76, 76, 78, 78, 81, 82, 83, 84, 87, 88, 89, 91, 93, 93, 93, 94, 94, 94, 103)
length(losses)

rs <- c(808, 765, 763, 777, 878, 725, 744, 759, 671, 715, 750, 779, 768, 680, 724, 675, 655, 729, 686, 845, 717, 671, 610, 752, 653, 649, 672, 686, 716, 722)
length(rs)

ra <- c(556, 757, 612, 676, 694, 638, 715, 666, 617, 631, 721, 712, 707, 702, 701, 712, 682, 758, 715, 860, 727, 733, 796, 890, 761, 779, 713, 770, 854, 889)
length(ra)

names(wins) <- teams
wins
names(losses) <- teams
losses
names(rs) <- teams
rs
names(ra) <- teams
ra

wp.actual <- wins/(wins+losses)
wp.actual

wp.pythag <- (rs^2)/((rs^2)+(ra^2))
wp.pythag

wp.resid <- wp.actual-wp.pythag
wp.resid

min(wp.pythag)
# Phillies had lowest wp.pythag
min(wp.actual)
# Twins had lowest wp.actual

max(wp.resid)
# Texas Rangers had highest wp.resid


