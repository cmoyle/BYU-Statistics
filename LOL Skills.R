#LOL Skills 

#Data: 2017 Worlds
source('http://grimshawville.byu.edu/eSports2017.R')

#Create Train and Test
samp <- sample(1:3881, 3100, replace = FALSE)
lol.train <- all.players[samp,]
lol.test <- all.players[-samp,]

#EDA
#Kills
kills <- summary(lol.train$Kills)

#The typical professional league of legend player is expected to get about 2.6 kills per game. 50% of players score 
# between 1 and 4 kills but can get up to 17 kills.

#Deaths
deaths <- summary(lol.train$Deaths)

#Assists
assists <- summary(lol.train$Assists)

#Time
time <- summary(lol.train$Time)

library(gridExtra)
grid.table(round(rbind(kills, deaths, assists, time),2))

#Plot
par(mfrow=c(2,2))
boxplot(Kills~Win, data = lol.train)
boxplot(Deaths~Win, data = lol.train)
boxplot(Assists~Win, data = lol.train)
boxplot(Time~Win, data = lol.train)
par(mfrow=c(1,1))

#Response variable: win = 1 means player's team won game
#Model is log of odds ratio 
#ln[(p(win=1|kills,deaths,assists,time))/(p(win=0|kills,deaths,assists,time))] = bo + b1 Kills + b2 Deaths + b3 Assists +
# b4 Time (no epsillon)

#fit model:
out.lol <- glm(Win~Kills + Deaths + Assists + Time, data = lol.train, family = "binomial")
summary(out.lol)

#Beta1 interpretation: For each additional kill, we estimate an increase of 0.43 in log odds of winning, holding
# all other variables constant.
exp(0.43060)

#Better: For each additional kill, we estimate the odds of winning increase 1.53 times (55%) holding all else constant

#interpret change in odds
exp(coef(out.lol))[-1]

#interpret deaths: For each additional death, we estimate a 56% decrease in odds of winning, holding all else constant

#Plot: Y - P(win), x - Kills, holding all other factors at the median.

#Create X* has to have kills deaths assists time 
#kills : 0,1,2,3,...,10 (between Q3 and Max)
#Deaths : 2,2,2,2,2,2,2,2,2
#Assists : 6,6,6,6,6,6,6,6,6,6
#Time : 35,35,35,35,35,35,35,35

x.star <- data.frame(Kills=seq(0,10,length=100), Deaths = 2, Assists = 6, Time = 35)
plot(x.star$Kills, predict(out.lol, newdata = x.star, type = "response"),
     type = 'l', ylim = c(0,1),
     xlab = "Kills",
     ylab = "Probability of Winning (Holding All Else at Median)")

#Create interpretation and graph for all others

#interpret deaths: For each additional death, we estimate a 56% decrease in odds of winning, holding all else constant
x.star <- data.frame(Kills = 2, Deaths = seq(0,10,length=100), Assists = 6, Time = 35)
plot(x.star$Deaths, predict(out.lol, newdata = x.star, type = "response"),
     type = 'l', ylim = c(0,1),
     xlab = "Deaths",
     ylab = "Probability of Winning (Holding all Else at Median")


#Interpret Assists: For each additional assist, we estimate a 66% increase in odds of winning, holding all else constant
x.star <- data.frame(Kills = 2, Deaths = 2, Assists = seq(0,15,length=100), Time = 35)
plot(x.star$Assists, predict(out.lol, newdata = x.star, type = "response"),
     type = 'l', ylim = c(0,1),
     xlab = "Assists",
     ylab = "Probability of Winning (Holding all Else at Median")

#Interpret Time: For each additional minute played in a game, we estimate a 7% decrease in odds of winning, holding all
#else constant
#95% CI
exp(confint(out.lol))[-1,]

x.star <- data.frame(Kills = 2, Deaths = 2, Assists = 6, Time = seq(20,70,length = 100))
plot(x.star$Time, predict(out.lol, newdata = x.star, type = "response"),
     type = 'l', ylim = c(0,1),
     xlab = "Time Played (Minutes)",
     ylab = "Probability of Winning (Holding all Else at Median)")


#Test significance
summary(out.lol)

#LRT X^2 
reduced.LOL <- glm(Win~Kills + Deaths + Assists, data = lol.train, family = "binomial")
anova(reduced.LOL, out.lol, test = "Chisq")

# Prediction for Faker and Ambition
# Faker Prediction
predict(out.lol, newdata = data.frame(Kills=2,Deaths=3,Assists=8,Time=40), type = "response")
# Ambition Prediction
predict(out.lol, newdata = data.frame(Kills=2,Deaths=2,Assists=14,Time=40), type = "response")

# Faker CI
logit.Faker <- predict(out.lol, newdata = data.frame(Kills=2,Deaths=3,Assists=8,Time=40), type = "link", se.fit = TRUE)
logit.L <- logit.Faker$fit - 1.96*logit.Faker$se.fit
logit.U <- logit.Faker$fit + 1.96*logit.Faker$se.fit
phat.L.Faker <- 1/(1+exp(-logit.L))
phat.U.Faker <- 1/(1+exp(-logit.U))

#Ambition CI
logit.Ambition <- predict(out.lol, newdata = data.frame(Kills=2,Deaths=2,Assists=14,Time=40), type = "link", se.fit = TRUE)
logit.L <- logit.Ambition$fit - 1.96*logit.Ambition$se.fit
logit.U <- logit.Ambition$fit + 1.96*logit.Ambition$se.fit
phat.L.Ambition<- 1/(1+exp(-logit.L))
phat.U.Ambition <- 1/(1+exp(-logit.U))

#Construct ROC Curves
#This is a judgement on how well our model is classifying predicted probabilities
# Probs -> y.hat = {0,1}
#- misclassification
#-Sensitivity
#     proportion of true positives (said player would win, and they did win)
# -Specificity
#     proportion of true negatives (said player would lose, and they did lose)
#Choose a threshold (as the threshold increases we gain sensitivity but lose specificity)
# ROC plots sensitivity against 1 - specificity
# Ideal curve looks vertical at 0 and flattens at one. Worst curve is linear
# We need to choose different thresholds to show our curve. Theres a library for that though, so no worries.

#ROC Curve
library(ROCR) 
train.prediction <- prediction(predict(out.lol,type = "response"), lol.train$Win)
# tpr = sensitivity fpr = false positive rate
train.perf <- performance(train.prediction, measure = "tpr", x.measure = "fpr")
plot(train.perf, xlab = "1 - specificity", ylab = "Sensitivity", main = "ROC Curve")
abline(0,1,col="gray")

#We like AUC as a summary statistic (kind of like R^2)
# AUC = Area Under Curve (closer to one, better)

performance(train.prediction, measure = "auc")

# overlay the test ROC (out of sample validation)
test.prediction <- prediction(predict(out.lol,newdata =  lol.test, type = "response"), lol.test$Win)
test.perf <- performance(test.prediction, measure = "tpr", x.measure = "fpr")
plot(test.perf, col = "red", add = TRUE)

#Test data AUC
performance(test.prediction, measure = "auc")

