#Analysis of Correlation of International Price of Oil and Effect on Colombian Currency Exchange Rate
#September 5, 2015
#Ariel E. Meilij
#Second version with Brent oil prices

library(ggplot2)
library(reshape2)
library(Hmisc)

###Simple Analysis of correlation between COP and WTI oil price

#Read and review file values
setwd("/home/user/project/main")
oilvscop = read.csv("oilvscop_wti_brent_numeric.csv")
oilvscop$date <- as.Date(oilvscop$date, origin = "1899-12-30")

summary(oilvscop)
names(oilvscop)

# Check correlation with scatter plot - Best way to check for correlation!
ggplot(data=oilvscop, aes(brent, forex)) + geom_point(color="green") + 
xlab("Price of Barril USD") + ylab("Forex COP to USD")

# Simple correlation index
varcorr = cor(oilvscop$brent, oilvscop$forex)
cat("Correlation result among Brent and COP: ", varcorr)

# Let's see if we can fit into a regression model
fit <- lm(oilvscop$brent ~ oilvscop$forex)
fit
summary(fit)

