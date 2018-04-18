## STEM Data

source("http://grimshawville.byu.edu/STEMgetdata.R")

#EDA
boxplot(prevCalc~y, data=STEM, ylab = "prevCalc", xlab = "y")
boxplot(nST~y, data = STEM, ylab = "nST", xlab = "y")
boxplot(newMJ~y, data = STEM, ylab = "newMJ", xlab = "y")
boxplot(new.teach~y, data = STEM, ylab = "new.teach", xlab = "y")
boxplot(new.stu~y, data = STEM, ylab = "new.stu", xlab = "y")

table(STEM$y, STEM$prevCalc)
table(STEM$y, STEM$newMJ)
table(STEM$y, STEM$new.teach)
table(STEM$y, STEM$new.stu)
table(STEM$y, STEM$gender)

tail(STEM)

#Primarily, we are interested in adjusting for gender. We are worried about confounding factors.
#Categorical variables are prevCalc, gender, newMJ, we need them to be factors, specify a comparison case

# Declare Categorical explanatory variables as R class factor
STEM$gender <- factor(STEM$gender)
STEM$gender <- relevel(STEM$gender, ref = "1")
STEM$prevCalc <- factor(STEM$prevCalc)
STEM$prevCalc <- relevel(STEM$prevCalc, ref = "1")
STEM$newMJ <- factor(STEM$newMJ)
STEM$newMJ <- relevel(STEM$newMJ, ref = "1")

#Model
#logit(y=1)=logit(switchers)
# = bo + b1 nST + b2 new.teach + b3 new.stu 
#   + gender_i + prevCalc_j + newMJ_k

#Fit
out.STEM <- glm(y~nST+new.teach+new.stu+gender+prevCalc+newMJ, data = STEM, family = "binomial")
summary(out.STEM)

exp(coef(out.STEM))[-1]
#Interpretation for a non statistician
#We estimate that women are 1.428 times more likely to switch from the calculus sequence than men, holding all else constant.

#to get a picture, we will hold other factors at the best case scenario.
#all other factors at "best"
#men
x.star <- data.frame(gender = "1", nST = seq(2,99,length=100),
                     new.teach = 6, new.stu = 6, prevCalc = "1", newMJ = "1")  #"1" is because they are factors
plot(x.star$nST, predict(out.STEM, newdata=x.star, type = "response"),
     type = 'l', xlab = "Percentile of Standardized Test", ylab = "P(Switch from Calc Seq)",
     ylim = c(0,0.25))

#Women
x.star <- data.frame(gender = "2", nST = seq(2,99,length=100),
                     new.teach = 6, new.stu = 6, prevCalc = "1", newMJ = "1")
lines(x.star$nST,predict(out.STEM, newdata=x.star, type = "response"), col = "red")

#Is there a statistically significant difference between Men and Women holding all else constant?

#95% CI:
exp(confint(out.STEM))[-1,]

#Yes it is significant because it doesn't include 1. 

#z-test
summary(out.STEM)

#X^2 Test
red1.STEM <- glm(y~nST+new.teach+new.stu+prevCalc+newMJ, data = STEM, family = "binomial")
anova(red1.STEM, out.STEM, test = "Chisq")

#Is there a calculus prep effect?
red2.STEM <- glm(y~nST+new.teach+new.stu+gender+newMJ, data = STEM, family = "binomial")
anova(red2.STEM, out.STEM, test = "Chisq")

#No significant calc prep effect

#Predict or Classify 
#Identify people at risk and asking what can we do.
#What are the consquences of that classification

#ROC Curve
library(ROCR)
STEM.pred <- prediction(predict(out.STEM, type = "response"), STEM$y)
STEM.perf <- performance(STEM.pred, measure = "tpr", x.measure = "fpr")
plot(STEM.perf, xlab = "1 - Specificity", ylab = "Sensitivity", main = "ROC Curve")
abline(0,1, col = 'grey')

#AUC
performance(STEM.pred, measure = "auc")
# 0.7602573

