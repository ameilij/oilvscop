#Analysis of Correlation of International Price of Oil and Effect on Colombian Currency Exchange Rate
#September 5, 2015
#Ariel E. Meilij

library(ggplot2)
library(reshape2)
library(Hmisc)

###Simple Analysis of correlation between COP and WTI oil price

#Read and review file values
setwd("/home/user/project/main")
oilvscop = read.csv("oilvscop_datenumeric.csv")
oilvscop$date <- as.Date(oilvscop$date, origin = "1899-12-30")

summary(oilvscop)
names(oilvscop)

#First let's assess the behavior of WTI prices
ggplot(data=oilvscop, aes(x=date, y=wti, group=1)) + geom_line(color="gray") + labs(title="WTI Oil Price", x="Date", y="USD")

#Now let's assess the behavior of the USD/COP exchange rate
ggplot(data=oilvscop, aes(x=date, y=forex, group=1)) + geom_line(color="green") + labs(title="USD to COP Exchange Rate", x="Date", y="COP")

#Let's create the plot where we see both lines
oilvscop_long <- melt(oilvscop, id="date")
ggplot(data=oilvscop_long, aes(x=date, y=value, group=variable, colour=variable)) + geom_line() + labs(title="Correlation Oil Price vs. COP Forex", x="Date", y="USD")

# Check correlation with scatter plot - Best way to check for correlation!
ggplot(data=oilvscop, aes(wti, forex)) + geom_point(color="green") + 
xlab("Price of Barril USD") + ylab("Forex COP to USD")

# Simple correlation index
varcorr = cor(oilvscop_cor$wti, oilvscop_cor$forex)
cat("Correlation result among WTI and COP: ", varcorr)

# Let's see if we can fit into a regression model
fit <- lm(oilvscop$wti ~ oilvscop$forex)
fit
summary(fit)
