relative_payroll <- read_csv(file = "mlb_relative_payrolls.csv")

ggplot(data = relative_payroll) + geom_histogram(mapping = aes(x=Winning_Percentage), bins=5)
ggplot(data = relative_payroll) + geom_histogram(mapping = aes(x=Winning_Percentage), bins=50)

ggplot(data = relative_payroll) + geom_histogram(mapping = aes(x=Relative_Payroll), bins=5)
ggplot(data = relative_payroll) + geom_histogram(mapping = aes(x=Relative_Payroll), bins=25)

ggplot(data = relative_payroll) + geom_point(mapping = aes(x = Relative_Payroll, y = Winning_Percentage))

# below code will show how team payrolls have changed over time (it has slightly increased)
ggplot(data = relative_payroll) + geom_point(mapping = aes(x = Year, y = Team_Payroll))

# below code shows how relative payrolls have not really changed over the past 15 years (with a couple outliers)
ggplot(data = relative_payroll) + geom_point(mapping = aes(x = Year, y = Relative_Payroll))




