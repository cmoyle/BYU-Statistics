#Exam P Analysis

#Data

ExamP <- read.csv(header = TRUE, text = '
Student,ExamP,GPA
2,No Pass,3.42
3,No Pass,3.93
4,No Pass,3.41
5,No Pass,3.75
6,Passed,3.18
7,No Pass,2.76
8,No Pass,3.68
9,No Pass,3.11
10,Passed,3.77
11,No Pass,3.06
12,No Pass,3.89
13,No Pass,3.28
14,Passed,3.82
15,No Pass,2.8
16,No Pass,3.45
17,No Pass,3.75
18,No Pass,3.7
19,No Pass,2.61
20,No Pass,3.17
21,Passed,3.93
22,No Pass,2.94
23,No Pass,2.49
24,No Pass,3.77
25,No Pass,2.7
26,Passed,3.4
27,No Pass,3.87
28,No Pass,2.55
29,Passed,3.81
30,No Pass,1.92
31,No Pass,2.79
32,No Pass,3.42
33,Passed,3.91
34,No Pass,3.94
35,Passed,3.6
36,Passed,3.89
37,No Pass,3.66
38,No Pass,3.02
39,Passed,3.87
40,No Pass,3.92
41,No Pass,3.27
42,Passed,3.32
43,Passed,3.52
44,No Pass,3.75
45,No Pass,3.87
46,No Pass,2.67
')

#EDA
ExamP$low <- ExamP$GPA <= 3.5
ExamP$low[ExamP$low] = "Less than 3.5"
ExamP$low[ExamP$low == FALSE] = "Greater than 3.5"
library(gridExtra)
grid.table(round(t(prop.table(table(ExamP$ExamP, ExamP$low))),2))
boxplot(GPA ~ ExamP, data = ExamP, main = "GPA of Exam P Results")

t(prop.table(table(ExamP$ExamP, ExamP$low)))
#Dr. Grimshaw's table
prop.table(table(ExamP$GPA >= 3.5, ExamP$ExamP),margin = 1)

# response variable

ExamP$Pass <- ifelse(ExamP$ExamP == "Passed",1,0)

#Logistic Regression Model
# log(P(pass|GPA) / P(notpass|GPA)) = beta0 + beta1GPA

out.examp <- glm(Pass~GPA, data = ExamP, family = "binomial")
summary(out.examp)

#For a one unit increase in GPA, we estimate the expected log odds of passing increases by 2.256

#Better for non statisticians
#For a one unit increase (for example moving from a B to an A) we estimate the odds of passing increases 9.54 times. (exp2.256)

#Does GPA have a statistically significant effect on passing?
#Test Ho: B1 = 0
#We use a Z test, because it is normally distributed.
#Also use a Likelihood Ratio Test, but we don't have Sigma, so we will use CHI square test.
#95% CI on B1

#Summary gives Z values

#Formal Conclusion
#We reject Ho: B1 = 0 in favor of Ha: B1 != 0 at ALPHA = 0.05

#Informal Conclusion
#GPA has a statistically significant effect on passing (p value = 0.0329)

#LRT or X^2 test of Ho: Beta1 = 0

reduced.examp <- glm(Pass~+1, data = ExamP, family = "binomial")
anova(reduced.examp, out.examp, test = "Chisq")

##Informal Conclusion
#GPA has a statistically significant effect on passing (p value = 0.01055)

#95% CI on B1 (log odds)
confint(out.examp)

#95% CI on odds (exp(beta1))
exp(confint(out.examp))[-1,]

#For a one unit increase (for example moving from a B to an A) we estimate the odds of passing increases 9.54 times.
# 95% CI 1.6, 111.8 (needs to be compared to e(0) not just zero. Confindence interval is still good but watch for that)

#Predict Probability of passing for students with GPA 3.25 and 3.85
predict(out.examp, newdata=data.frame(GPA = c(3.25,3.85)),type="response")

#Create a graphic of the logistic regression model that uses GPA to model passing
#Exam P. The graphic must include the data, the estimated logistic regression
#model, and the 95% confidence interval for P[Pass Exam P | GPA = x].

#data
plot(Pass~GPA , data = ExamP, xlim = c(0,4), main = "Relationship Between GPA and Passing Exam P")

#predictive probability
xstar <- seq(0,4,length=100)
phat <- predict(out.examp, newdata = data.frame(GPA = xstar), type = "response")
lines(xstar, phat, col = 'red')

#Confidence Bands
logit.hat <- predict(out.examp, newdata = data.frame(GPA = xstar), type = "link", se.fit = TRUE)
logit.L <- logit.hat$fit - 1.96 * logit.hat$se.fit
logit.U <- logit.hat$fit + 1.96 * logit.hat$se.fit
phat.L <- 1 / (1+exp(-logit.L))
phat.U <- 1 / (1+exp(-logit.U))
lines(xstar,phat.L, lty = 2, col = 'gray')
lines(xstar,phat.U, lty = 2, col = 'gray')
