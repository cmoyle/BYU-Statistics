# Methods HW for Poisson Regression
rocket <- read.table(header = TRUE, text = '
BadRings Temp
0 66
1 70
0 69
0 68
0 67
0 72
0 73
0 70
1 57
1 63
1 70
0 78
0 67
3 53
0 67
0 75
0 70
0 81
0 76
0 79
2 75
0 76
1 58
')

out.rocket <- glm(BadRings~Temp, data = rocket, family = "poisson")
summary(out.rocket)

#95% Confidence Interval
confint(out.rocket)

#Ho: Temperature has no effect.
summary(out.rocket)

#Predict O-ring failures for Jan 27 1986
logit.rocket <- predict(out.rocket, newdata = data.frame(Temp = 31), type = "link", se.fit = TRUE)
logit.L <- logit.rocket$fit - 1.96*logit.rocket$se.fit
logit.U <- logit.rocket$fit + 1.96*logit.rocket$se.fit
logit.fit <- logit.rocket$fit
fit2 <- out.rocket$family$linkinv(logit.fit)
upr2 <- out.rocket$family$linkinv(logit.U)
lwr2 <- out.rocket$family$linkinv(logit.L)

#Confidence Interval is really big. Not a lot of data around 31 degrees to compare too. Extrapolation is all we have.
c(fit2, lwr2, upr2)
