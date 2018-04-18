#Viewed 2017 Christmas Devotional (at least 500 unique visitors) and LDS Popn (# operating temples) 
 devotional <- read.table(header = TRUE, text = 
'State Viewed Temples
Alabama 0 1 
Alaska 0 1
Arizona 1 6
Arkansas 0 0
California 1 7
Colorado 1 2
Connecticut 0 1  
Delaware 0 0
Florida 1 2
Georgia 1 1
Hawaii 0 0
Idaho 1 5
Illinois 1 2
Indiana 1 1
Iowa 0 0
Kansas 0 0
Kentucky 0 1
Louisiana 0 1
Maine 0 0
Maryland 0 1
Massachusetts 0 1
Michigan 0 1
Minnesota 0 0
Mississippi 0 0
Missouri 1 0
Montana 0 1
Nebraska 0 0
Nevada 1 2
NewHampshire 0 0
NewJersey 0 0
NewMexico 1 1
NewYork 1 2
NorthCarolina 1 1
NorthDakota 0 0
Ohio 1 1
Oklahoma 0 1
Oregon 1 2
Pennsylvania 1 1
RhodeIsland 0 0
SouthCarolina 0 1
SouthDakota 0 0
Tennessee 1 2
Texas 1 4
Utah 1 17
Vermont 0 0
Virginia 1 0
Washington 1 3
WestVirginia 0 0
Wisconsin 0 0
Wyoming 1 1
')
#Fit Model using Logistic Regression
out.devotional <- glm(Viewed~Temples, data = devotional, family = 'binomial')
summary(out.devotional) 

#Increase of odds in viewing for one unit increase in temples
exp(2.4343)

#Confidence Interval
exp(confint(out.devotional))[-1,]

#LRT or X^2 test of Ho: B1 = 0
reduced.devotional <- glm(Viewed~+1, data = devotional, family = "binomial")
anova(reduced.devotional, out.devotional, test = "Chisq")

#ROC Curve
library(ROCR) 
devotional.prediction <- prediction(predict(out.devotional,type = "response"), devotional$Viewed)
# tpr = sensitivity fpr = false positive rate
devotional.perf <- performance(devotional.prediction, measure = "tpr", x.measure = "fpr")
plot(devotional.perf, xlab = "1 - specificity", ylab = "Sensitivity", main = "ROC Curve")
abline(0,1,col="gray")

#AUC
performance(devotional.prediction, measure = "auc")
##[1] 0.8741883

#Confidence Interval for North Carolina (1 temple)
logit.NC <- predict(out.devotional, newdata = data.frame(Temples = 1), type = "link", se.fit = TRUE)
logit.L <- logit.NC$fit - 1.96*logit.NC$se.fit
logit.U <- logit.NC$fit + 1.96*logit.NC$se.fit
phat.L.NC <- 1/(1+exp(-logit.L))
phat.U.NC <- 1/(1+exp(-logit.U))

