# AL Orieles Problem

# Source: http://espn.go.com/mlb/standings/_/season/2017

library(XML)
#Read in the data
orioles <- readHTMLTable("http://www.espn.com/mlb/standings/_/season/2017", header = FALSE, 
                         which = 1, colClasses = c('numeric', 'numeric', 'numeric', 'numeric', 'character',
                                                   'character', 'numeric', 'numeric', 'character', 'character',
                                                   'character'))
#Clean the data
orioles <- subset(orioles, !is.na(orioles[,1]))
orioles <- orioles[,c(1,9)]
colnames(orioles) <- c("wins", "run.diff")
orioles$division <- c("East", "East", "East", "East", "East", "Central", "Central", "Central", "Central", "Central",
                      "West", "West", "West", "West", "West")
#Make variable run.diff numerically formatted
orioles$run.diff <- as.numeric(gsub("[//+]", "", orioles$run.diff))

#EDA 
summary.stats <- aggregate(wins~division, FUN = mean, data = orioles)
summary.stats$winsSD <- aggregate(wins~division, FUN = sd, data = orioles)[,2]
summary.stats$run.diffMean <- aggregate(run.diff~division, FUN = mean, data = orioles)[,2]
summary.stats$run.diffSD <- aggregate(run.diff~division, FUN = sd, data = orioles)[,2]
colnames(summary.stats) <- c("Division", "Mean Wins", "Wins SD", "Mean Run Differential", "Run Differential SD")

library(gridExtra)
grid.table(summary.stats)

library(ggplot2)
qplot(run.diff, wins, data = orioles, color = division, xlab = "Run Differentials", ylab = "Wins", 
      main = "Run Differentials and Wins By Division")

#Analysis
#Make division a factor
orioles$division <- factor(orioles$division)
#Fit Model in R
out1.mlb <- lm(wins~division + run.diff, data = orioles, x = TRUE, y = TRUE)
out1.mlb$y
out1.mlb$x

#To specify the comparison case (y-intercept) as East
orioles$division <- relevel(orioles$division,"East")
out2.mlb <- lm(wins~division + run.diff, data = orioles, x = TRUE, y = TRUE)
summary(out2.mlb)
out2.mlb$y
out2.mlb$x
summary(out2.mlb)

#Interpret B3 = 0.08
#For one additional run scored or prevented we estimate an expected increase of .08 in wins holding division constant

#Model for Wins.hat:
#If East: Wins.hat = 81.1 + .08 run.diff

#If Central: Wins.hat = (81.1 + (-0.16)) + .08 run.diff = 80.9 + 0.08 run.diff

#If west: Wins.hat = 81.1 + 0.13 + .08 run.diff = 81.2 + .08 run.diff
#Graphic showing predicted model (3 lines)
plot(wins~run.diff,data=orioles,type='n')
points(wins~run.diff,data=subset(orioles,division=="East"), pch=19, col = "orange")
points(wins~run.diff,data=subset(orioles,division=="Central"), pch=19, col = "green")
points(wins~run.diff,data=subset(orioles,division=="West"), pch=19, col = "blue")
#east
abline(81.09691,0.082029, col = "orange")

#central
abline(81.09691 - 0.168041,0.082029, col = "green")

#west
abline(81.09691 + 0.138265,0.082029, col = "blue")

legend("bottomright", c("East","Central","West"),lty=1,col=c("orange","green","blue"))

#Test Ho: no difference between division afgter adjusting for run.diff
reduced.mlb2 <- lm(wins~run.diff, data = orioles)
anova(reduced.mlb2, out2.mlb)

#No statistically significant difference