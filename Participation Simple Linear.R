# Cameron Moyle

#Test for statistically significant inheritance effect using simple linear regression

#Data

#Found in The Life, Letters and
#Labours of Francis Galton, which can be found on page 4 of http://galton.org/
#pearson/vol3/new/pearson-vol3a-ch14.pdf

#Create a data frame

peas <- read.csv(header = TRUE, sep = ' ', text = '
Parent(in) Offspring(in)
0.21 0.1726
0.20 0.170
0.19 0.1637
0.18 0.1640
0.17 0.1613
0.16 0.1617
0.15 0.1598')
names(peas) <- c("Parent", "Offspring")

#EDA

#Calculate Correlation Coefficient
cor(peas$Offspring, peas$Parent)
#[1] 0.9282826

#Plot to confirm it has the characteristics suggested by the correlation coefficient.
library(ggplot2)
qplot(Parent, Offspring, data = peas,
      geom = "point",
      xlab = "Diameter of Parent Pea",
      ylab = "Diameter of Offspring Pea")
#The plot does indicate a strong positive correlation in the data as the correlation coefficient suggests.

#Model: offspring = beta0 + beta1 parent + epsilon, epsilon~N(0, sigma2)

#Fit Model / Estimate Model Parameters
peas.out <- lm(Offspring~Parent, data = peas)

#Table of Estimates and Standard Errors
summary(peas.out)

#Interpretation
#The estimated slope is 0.21
#BETTER: For a one unit increase in parent sweet pea diameter we expect an estimated increase of 0.21 in 
#offspring sweet pea diameter.

#Scientific Evidence
#H0: Beta1 = 0
# t-test (wald) approach
# ANOVA F-test approach
#Conclusion based on that information
#  There is a statistically significant inheritance effect (pvalue = 0.0028)

#95% Confidence Interval on Beta1
confint(peas.out)
#                 2.5 %    97.5 %
#(Intercept) 0.1107302 0.1449269
#Parent      0.1105901 0.2994099

#Final Conclusion
#There is a statistically significant inheritance effect (pvalue = 0.0028)
#For a one unit increase in parent sweet pea diameter we expect an estimated increase of 0.21 in 
#offspring sweet pea diameter. (95% confidence interval: 0.1107, 0.3093)

#Graphic showing lines and uncertainty
qplot(Parent, Offspring, data = peas,
      geom = "smooth", formula = y~x, method="lm", se=TRUE,
      xlab = "Diameter of Parent Pea",
      ylab = "Diameter of Offspring Pea")
