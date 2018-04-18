# Montgomery County Traffic Stops Data

stops.full<-read.csv("http://data.montgomerycountymd.gov/api/views/4mse-ku6q/rows.csv?accessType=DOWNLOAD",
                     header=TRUE,as.is=TRUE)

# subset to last year
last.year<-2017
stops.full$AutoYear<-as.numeric(stops.full$Year)
stops.full$Year<-as.numeric(substr(stops.full$Date.Of.Stop,7,10))
stops.last<-subset(stops.full,Year==last.year)
# delete the really big data set ... don't need to tie up the memory
rm(stops.full)

# Create Month and Hour variables
stops.last$Month<-as.numeric(substr(stops.last$Date.Of.Stop,1,2))
stops.last$Hour<-as.numeric(substr(stops.last$Time.Of.Stop,1,2))

# clean up dataset
stops.last$AutoState<-stops.last$State
stops.last$Out.of.State<-(stops.last$AutoState!="MD")

stops.last$Color<-as.character(stops.last$Color)
stops.last$Color[stops.last$Color %in% c("CAMOUFLAGE","CHROME","COPPER","CREAM","MULTICOLOR","N/A","PINK")]<-"OTHER"
stops.last$Color<-factor(stops.last$Color)

# other filters
stops.last<-subset(stops.last,Color != "N/A")
stops.last<-subset(stops.last,Color != "")
stops.last<-subset(stops.last,Gender != "U")
stops.last<-subset(stops.last,HAZMAT == "No")
stops.last<-subset(stops.last,AutoYear > 1990 & AutoYear < last.year+2)

# convert character variables to factors
stops.last$SubAgency<-factor(stops.last$SubAgency)
stops.last$Accident<-factor(stops.last$Accident)
stops.last$Belts<-factor(stops.last$Belts)
stops.last$Personal.Injury<-factor(stops.last$Personal.Injury)
stops.last$Property.Damage<-factor(stops.last$Property.Damage)
stops.last$Commercial.License<-factor(stops.last$Commercial.License)
stops.last$Commercial.Vehicle<-factor(stops.last$Commercial.Vehicle)
stops.last$Alcohol<-factor(stops.last$Alcohol)
stops.last$Work.Zone<-factor(stops.last$Work.Zone)
stops.last$Contributed.To.Accident<-factor(stops.last$Contributed.To.Accident)
stops.last$Race<-factor(stops.last$Race)
stops.last$Gender<-factor(stops.last$Gender)
stops.last$Out.of.State<-factor(stops.last$Out.of.State)


# Create dataset for Speeding
#  example: EXCEEDING MAXIMUM SPEED: 49 MPH IN A POSTED 40 MPH ZONE
speed.last1<-subset(stops.last,substr(Description,1,23)=="EXCEEDING MAXIMUM SPEED")
# difference between cited speed and posted speed limit
speed.last1$speed<-as.numeric(substr(speed.last1$Description,26,27))-as.numeric(substr(speed.last1$Description,45,46))
speed.last1<-subset(speed.last1,!is.na(speed))
#  example: EXCEEDING POSTED MAXIMUM SPEED LIMIT: 39 MPH IN A POSTED 30 MPH ZONE
speed.last2<-subset(stops.last,substr(Description,1,30)=="EXCEEDING POSTED MAXIMUM SPEED")
# difference between cited speed and posted speed limit
speed.last2$speed<-as.numeric(substr(speed.last2$Description,39,40))-as.numeric(substr(speed.last2$Description,58,59))
speed.last2<-subset(speed.last2,!is.na(speed))
# combine and subset to columns of interest
speed.last<-rbind(speed.last1,speed.last2)
speed.last<-speed.last[,c(4,9:12,14,16:18,24,28:30,36:38,40,41)]


# Create dataset for Ticket/Warning
ticket.last<-subset(stops.last,Violation.Type %in% c("Citation","Warning") )
ticket.last$Ticket<-factor(ticket.last$Violation.Type=="Citation")
# subset to columns of interest
ticket.last<-ticket.last[,c(4,9:12,14,17,18,24,28:30,36:38,40,41)]


# make a prediction at a new observation
#  note: grab an observation in the dataset that is very similar to my situation and change it
new.obs<-speed.last[25,]
new.obs$AutoYear<-2017
new.obs$Month<-8
new.obs$Hour<-18

hist(speed.last$speed)

# Create Train and Test 80% | 20%
# SRS without replacement
set.seed(12)
dim(speed.last)
train.rows <- sample(9773, 8000)
speed.train <- speed.last[train.rows,]
speed.test <- speed.last[-train.rows,]

# Validate similarity between train and test
summary(speed.train$speed)
summary(speed.test$speed)


# Grow a Random Forest on train
# Validate predictions on test

library(randomForest)
# fit model
out.speed<-randomForest(x = speed.train[,-18], y = speed.train$speed,
                        xtest = speed.test[,-18], ytest = speed.test$speed,  # Validates the model
                        replace = TRUE,                                      #Bootstrap samples for trees
                        keep.forest = TRUE,                     #Normally erases trees for memory
                        ntree = 100,                    #Number of trees we want (50-100 is good)
                        mtry = 5,                #Number of explanatory variables they get to look at.
                        nodesize = 25)           #The deepest that the trees will go

#Use the tree to make a prediction for new obs
predict(out.speed, newdata=new.obs)

#Prediction Performance
out.speed

# Compute RMSE
sqrt(49.01889) #Train
sqrt(50.37) #Test

# Model Insight (interpretation)
importance(out.speed)
varImpPlot(out.speed)

#Analysis weaknessess
# We lack an explanation for the predictions that we see, and we can't pinpoint which explanatory variable
# is responsible for our prediction because it may be prominent in one tree, and not in another. 

#Challenge

#We could look at the time of day and find a prediction for the time of day that you are likely to be pulled over.

