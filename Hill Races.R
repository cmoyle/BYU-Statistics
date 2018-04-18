hills <- read.table(header = TRUE, text = "
                    Race	Distance	Climb	Time
                    Greenmantle	2.5	650	16.083
                    Carnethy	6	2500	48.35
                    CraigDunain	6	900	33.65
                    BenRha	7.5	800	45.6
                    BenLomond	8	3070	62.267
                    Goatfell	8	2866	73.217
                    BensofJura	16	7500	204.617
                    Cairnpapple	6	800	36.367
                    Scolty	5	800	29.75
                    Traprain	6	650	39.75
                    LairigGhru	28	2100	192.667
                    Dollar	5	2000	43.05
                    Lomonds	9.5	2200	65
                    CairnTable	6	500	44.133
                    EildonTwo	4.5	1500	26.933
                    Cairngorm	10	3000	72.25
                    SevenHills	14	2200	98.417
                    KnockHill	3	350	78.65
                    BlackHill	4.5	1000	17.417
                    CreagBeag	5.5	600	32.567
                    KildconHill	3	300	15.95
                    MeallAnt-Suidhe	3.5	1500	27.9
                    HalfBenNevis	6	2200	47.633
                    CowHill	2	900	17.933
                    NBerwickLaw	3	600	18.683
                    CreagDubh	4	2000	26.217
                    Burnswark	6	800	34.433
                    LargoLaw	5	950	28.567
                    Criffel	6.5	1750	50.5
                    Acmony	5	500	20.95
                    BenNevis	10	4400	85.583
                    Knockfarrel	6	600	32.383
                    TwoBreweries	18	5200	170.25
                    Cockleroi	4.5	850	28.1
                    MoffatChase	20	5000	159.833
                    ")
plot(Time~Climb, data = hills, main = "Scatterplot of Climb Times")
odd.climb <- identify(x=hills$Climb, y=hills$Time)
plot(Time~Distance, data = hills, main = "Scatterplot of Distance Times")
odd.dist <- identify(x=hills$Distance, y = hills$Time)

odd.hills.dist <- hills[odd.dist,]
odd.hills.climb <- hills[odd.climb,]

#Regression Diagnostics

out.hills <- lm(Time~Distance+Climb, data = hills)

#Compute Leverage
leverage.hills <- lm.influence(out.hills)$hat
#example of homework question: What is the leverage for the BenNevis race?
subset(leverage.hills, hills$Race=="BenNevis")
#Not scary, but not negligable either

# Compute Cook's Distance
cd.hills <- cooks.distance(out.hills)
#What is cooks d for MoffitChase
subset(cd.hills,hills$Race=="MoffatChase")

#Compute R-Studentized Residuals
R.hills <- rstudent(out.hills)
subset(R.hills, hills$Race=="CairnTable")

#Validate Normality Assumption
hist(R.hills)

#ks test
ks.test(R.hills, "pnorm") #pnorm is R's CDF function for the normal dist. ks test is more of a diagnostic

#Analysis for LarigGhru
leverage.hills[11]
cd.hills[11]
plot(Time~Distance, data = hills, main = "Scatterplot of Distance Times")
points(hills$Distance[11], hills$Time[11], col = "red", pch = 19)
plot(Time~Climb, data = hills, main = "Scatterplot of Climb Times")
points(hills$Climb[11], hills$Time[11], col = 'red', pch=19)

#Very influential, bad influential

#Moffat Chase
leverage.hills[35]
cd.hills[35]
plot(Time~Distance, data = hills, main = "Scatterplot of Distance Times")
points(hills$Distance[35], hills$Time[35], col = "red", pch = 19)
plot(Time~Climb, data = hills, main = "Scatterplot of Climb Times")
points(hills$Climb[35], hills$Time[35], col = 'red', pch=19)

#Influential, good influential

#Bens of Jura

leverage.hills[7]
cd.hills[7]
plot(Time~Distance, data = hills, main = "Scatterplot of Distance Times")
points(hills$Distance[7], hills$Time[7], col = "red", pch = 19)
plot(Time~Climb, data = hills, main = "Scatterplot of Climb Times")
points(hills$Climb[7], hills$Time[7], col = 'red', pch=19)

#Influential, bad influential

#Outliers
#Kildcon Hill has a record time of 15.95 minutes, but the regression model predicts
#12.9762 minutes. Would you consider this observation an outlier? Justify your
#answer.

# We need to find the variance before we can decide. We can use R-studentized residuals for that.
# These are approx normal so we'll check for when it is > 2 or < -2 

#Is it an outlier?
subset(R.hills, hills$Race=="KildconHill") # no it is very common.

#Is cow hill an outlier?
subset(R.hills, hills$Race=="CowHill") #not an outlier

#Could I test that? 

#Test Ho: cowhill is not an outlier
# Resids are approx normal, we observed .314 so we could compute the following for a p-value
2*(1-pnorm(0.314))

#pvalue for Ho: KnockHill is not an outlier
subset(R.hills, hills$Race=="KnockHill") 
2*(1-pnorm(7.61)) #It's pretty clear that Knock hill was recorded incorrectly and is an outlier.