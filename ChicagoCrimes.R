#*********************************************
#     ANALYSIS OF CRIMES IN CHICAGO
#*********************************************


library(klaR)
library(usdm)
library(rpart)
library(e1071)
library(caret)
library(class)

#*********************************************PREPARING DATA**************************************************

#Volume of data is very high. So to make it managable, Getting data for only 2016 and 2017. 
Crimes2 <- subset(Crimes2, !Crimes2$Year == 2015)
#Other offense records arent very crutial for our prediction. SO we delete them and keep meaningful data. 
Crimes2 <- subset(Crimes2,!Crimes2$`Primary Type` == "OTHER OFFENSE")
View(ArrestD)
for (i in 1:ncol(Crimes))
{
  s= sum(is.na(Crimes[[i]]))
  print(c("Number of NA's in ",names(Crimes)[i], "is ", s))
  
}

#Location, Latitude, Longitude, X cordinate and Y cordinate have 23877 NAs
#Location Description has 1627 NAs

#23877 records do not have a location.
sum(is.na(Crimes$Location))/nrow(Crimes)
#This makes up for only 5% of the data. So we can delete this data. 

Crimes2 <- subset(Crimes2, !is.na(Crimes2$Location))
sum(is.na(Crimes2))

#Now we have 1086 NAs in the column location decsription. This isnt a column which is important for out prediction
#So we can ignore these NAs

Crimes2["Month"] <- substring(Crimes2$Date,1,2)
Crimes2["Day"] <- substring(Crimes2$Date,4,5)
Crimes2["Hour"] <- paste(substring(Crimes2$Date,12,13),substring(Crimes2$Date,21,22),sep = " ")
Crimes2["Mins"] <- substring(Crimes2$Date,15,16)

#Categorising the crimes in the file into INDEX and NON INDEX Crimes as given by the MCC (City of Chicago Municipal Code)
Crimes <- Crimes2
Crimes <- subset(Crimes, !Crimes$`Primary Type` == "CRIMINAL DAMAGE")
Crimes <- subset(Crimes, !Crimes$`Primary Type` == "CRIMINAL TRESPASS")
Crimes <- subset(Crimes, !Crimes$`Primary Type` == "DECEPTIVE PRACTICE")

Crimes["CrimeCategory"] <- NA

Crimes$CrimeCategory[intersect(grep("SIMPLE",Crimes$Description) , grep("ASSAULT",Crimes$`Primary Type`))] <- "NON-INDEX CRIME"
Crimes$CrimeCategory[intersect(grep("MIN",Crimes$Description) , grep("ASSAULT",Crimes$`Primary Type`)) ] <- "NON-INDEX CRIME"
Crimes$CrimeCategory[intersect(grep("AGG",Crimes$Description) , grep("ASSAULT",Crimes$`Primary Type`)) ] <- "INDEX CRIME"

Crimes$CrimeCategory[intersect(grep("SIMPLE",Crimes$Description) , grep("BATTERY",Crimes$`Primary Type`)) ] <-"NON-INDEX CRIME"
Crimes$CrimeCategory[intersect(grep("MIN",Crimes$Description) , grep("BATTERY",Crimes$`Primary Type`)) ] <- "NON-INDEX CRIME"
Crimes$CrimeCategory[intersect(grep("UNBORN",Crimes$Description) , grep("BATTERY",Crimes$`Primary Type`)) ] <- "NON-INDEX CRIME"
Crimes$CrimeCategory[intersect(grep("AGG",Crimes$Description) , grep("BATTERY",Crimes$`Primary Type`)) ] <- "INDEX CRIME"

Crimes$CrimeCategory[intersect(grep("NON",Crimes$`Primary Type`),grep("CRIMINAL",Crimes$`Primary Type`)) ] <- "NON-INDEX CRIME"

Crimes$CrimeCategory <- ifelse (Crimes$`Primary Type` == "BURGLARY", "INDEX CRIME",
                                ifelse  (Crimes$`Primary Type` == "CRIM SEXUAL ASSAULT", "INDEX CRIME", 
                                         ifelse  (Crimes$`Primary Type` == "ROBBERY", "INDEX CRIME",
                                                  ifelse  (Crimes$`Primary Type` == "THEFT","INDEX CRIME",
                                                           ifelse  (Crimes$`Primary Type` == "HOMICIDE", "INDEX CRIME", 
                                                                    ifelse  (Crimes$`Primary Type` == "MOTOR VEHICLE THEFT", "INDEX CRIME", 
                                                                             Crimes$CrimeCategory ))))))

Crimes$CrimeCategory <- ifelse  (Crimes$`Primary Type` == "OFFENSE INVOLVING CHILDREN","NON-INDEX CRIME",
                                 ifelse  (Crimes$`Primary Type` == "OBSCENITY", "NON-INDEX CRIME",
                                          ifelse  (Crimes$`Primary Type` == "LIQUOR LAW VIOLATION","NON-INDEX CRIME",
                                                   ifelse  (Crimes$`Primary Type` == "PROSTITUTION","NON-INDEX CRIME",
                                                            ifelse  (Crimes$`Primary Type` == "SEX OFFENSE", "NON-INDEX CRIME",
                                                                     ifelse  (Crimes$`Primary Type` == "GAMBLING","NON-INDEX CRIME",
                                                                              ifelse  (Crimes$`Primary Type` == "STALKING","NON-INDEX CRIME", 
                                                                                       ifelse  (Crimes$`Primary Type` == "WEAPONS VIOLATION", "NON-INDEX CRIME",  
                                                                                                ifelse  (Crimes$`Primary Type` == "DECEPTIVE PRACTICE","NON-INDEX CRIME", 
                                                                                                         ifelse  (Crimes$`Primary Type` == "CRIMINAL DAMAGE","NON-INDEX CRIME", 
                                                                                                                  ifelse  (Crimes$`Primary Type` == "NARCOTICS","NON-INDEX CRIME",
                                                                                                                           ifelse  (Crimes$`Primary Type` == "OTHER NARCOTIC VIOLATION","NON-INDEX CRIME",
                                                                                                                                    Crimes$CrimeCategory))))))))))))

Crimes$CrimeCategory <- ifelse  (Crimes$`Primary Type` == "CRIMINAL TRESPASS", "NON-INDEX CRIME", 
                                 ifelse  (Crimes$`Primary Type` == "HUMAN TRAFFICKING","NON-INDEX CRIME", 
                                          ifelse  (Crimes$`Primary Type` == "INTERFERENCE WITH PUBLIC OFFICER","NON-INDEX CRIME", 
                                                   ifelse  (Crimes$`Primary Type` == "INTIMIDATION","NON-INDEX CRIME", 
                                                            ifelse  (Crimes$`Primary Type` == "KIDNAPPING","NON-INDEX CRIME", 
                                                                     ifelse  (Crimes$`Primary Type` == "CONCEALED CARRY LICENSE VIOLATION","NON-INDEX CRIME",
                                                                              ifelse  (Crimes$`Primary Type` == "OTHER OFFENSE","NON-INDEX CRIME",
                                                                                       ifelse  (Crimes$`Primary Type` == "PUBLIC INDECENCY","NON-INDEX CRIME", 
                                                                                                ifelse  (Crimes$`Primary Type` == "PUBLIC PEACE VIOLATION","NON-INDEX CRIME",
                                                                                                         ifelse  (Crimes$`Primary Type` == "ARSON","NON-INDEX CRIME",
                                                                                                                  Crimes$CrimeCategory))))))))))
sum(is.na(Crimes$CrimeCategory))

barplot(table(Crimes$Hour,Crimes$CrimeCategory))

#Next, We delete the columns that we will only confuse the model and arent important in building the model

Crimes$ID <- NULL #ID column not needed
Crimes$IUCR <- NULL #ID column not needed
Crimes$`FBI Code` <- NULL #ID column not needed
Crimes$`Updated On` <- NULL
Crimes$`Primary Type` <- NULL #Since we have converted this column into CrimeCategroy which is out target varibale
Crimes$Description <- NULL #Same as above
Crimes$`Location Description` <- NULL
Crimes$`Case Number` <- NULL #ID column not needed
Crimes$Domestic <- NULL

write.table(Crimes,"CrimeDataCrimeType.csv",sep=",",row.names=TRUE)

#For this objective, to predict the probabilities of types of crimes, we dont need the arrest variable. 
#we will use it in the next objective when we have to predict whether the crime resulted in an arrest

Crimes$Arrest <- NULL

Crimes$Location <- NULL
#Location variable id just a concatenation of latitude and longitide which are already present as variables


str(Crimes)
Crimes$Month <- as.numeric(Crimes$Month) 
Crimes$Mins <- as.numeric(Crimes$Mins)
Crimes$Day <- as.numeric(Crimes$Day)
Crimes$Beat <- as.numeric(Crimes$Beat)
Crimes$District <- as.numeric(Crimes$District)

#Converting Hours into millitary time (1-24)
for (i in grep("PM",Crimes$Hour))
{
  Crimes$Hour[i] <- as.numeric(substring(Crimes$Hour[i],1,2))+12
}

for (i in grep("AM",Crimes$Hour))
{
  Crimes$Hour[i] <- as.numeric(substring(Crimes$Hour[i],1,2))
}

Crimes <- as.data.frame(Crimes)

#----Testing and comparing different machine learning algoithms 

#******************************************LOGISTIC REGRESSION****************************************************

vif(Crimes[-c(1,2,4,16,7,8)])
#Taking random sample fo 50% of the entire data since the volume of data is huge. 
sample.ind <- sample(2,nrow(Crimes),prob=c(0.5,0.5), replace = T)
CrimesSample <- Crimes[sample.ind ==1,]

#Deviding the data into train and test sets
t.ind <- sample(2,nrow(CrimesSample), prob=c(0.7,0.3), replace = T)
Trainsample <- CrimesSample[t.ind ==1,]
Testsample <- CrimesSample[t.ind ==2,]

#Making the Target variable quantitative
Trainsample$CrimeCategory[Trainsample$CrimeCategory == "INDEX CRIME"] <- 1
Trainsample$CrimeCategory[Trainsample$CrimeCategory == "NON-INDEX CRIME"] <- 0

Testsample$CrimeCategory[Testsample$CrimeCategory == "INDEX CRIME"] <- 1
Testsample$CrimeCategory[Testsample$CrimeCategory == "NON-INDEX CRIME"] <- 0


#Logistic regression models
lrformula1 <- relevel(factor(CrimeCategory),ref="0") ~ Beat + Ward + `Community Area` + Latitude + Longitude + Hour + Month +Day + Mins + Year
Crimes.logReg <- glm(lrformula1, data = Trainsample, family = "binomial")
summary(Crimes.logReg)
predict.lr1 <- predict(Crimes.logReg, newdata = Testsample, type = "response")

Predicted.lr <- rep(NA,nrow(Testsample))
Predicted.lr[predict.lr1 < 0.5] <- 0
Predicted.lr[predict.lr1 > 0.5] <- 1

actual.lr <- Testsample$CrimeCategory
CMlr1 <- confusionMatrix(Predicted.lr,actual.lr, positive = "1") 
#Accuracy = 61.49% ; Specificity = 24.93% : Sensitivity = 85.93%

#removing the variables that are not sugnificant : Beat, Day, Year
lrformula2 <- CrimeCategory ~  Ward + `Community Area` + Latitude + Longitude + Hour + Month + Mins
Crimes.logReg2 <- glm(lrformula2, data = Trainsample, family = "binomial")
summary(Crimes.logReg2)
predict.lr2 <- predict(Crimes.logReg2, newdata = Testsample, type = "response")

Predicted.lr2 <- rep(NA,nrow(Testsample))
Predicted.lr2[predict.lr2 < 0.5] <- 0
Predicted.lr2[predict.lr2 > 0.5] <- 1

actual.lr <- Testsample$CrimeCategory
CMlr2 <- confusionMatrix(Predicted.lr2,actual.lr,positive = "1")
#Accuracy = 61.51% ; Specificity = 24.86% : Sensitivity = 86.03%

lrformula3 <- CrimeCategory ~  Ward + `Community Area` + Latitude + Longitude + Hour + Month
Crimes.logReg3 <- glm(lrformula2, data = Trainsample, family = "binomial")
predict.lr3 <- predict(Crimes.logReg3, newdata = Testsample, type = "response")

#The accuracy isnt upto the mark. Therefore, we should check for the best threshold value 
#Using the ROC curve, we find the best threshold value for the model

library(ROCR)
prediction.obj1 <- prediction(predict.lr1,actual.lr)
perf1 = performance(prediction.obj1,"tpr","fpr")

prediction.obj2 <- prediction(predict.lr2,actual.lr)
perf2 = performance(prediction.obj2,"tpr","fpr")

prediction.obj3 <- prediction(predict.lr3,actual.lr)
perf3 = performance(prediction.obj3,"tpr","fpr")

plot(perf1,xlab="False Positive Rate (False Alarm)",ylab="True Positive Rate (Recall or Sensitivity)",
     main="ROC Curve", col = "red" )
plot(perf2, add = TRUE, col = "green")
plot(perf3, add = TRUE, col = "blue")

#For our model, we want both a high soecificity and a high sensitivity, so we use the ROC curve to find out the 
#best threshold

#Function to find the best cutoff
opt.cut.distance = function(perf, pred)
{
  cut.ind = mapply(FUN=function(x, y, p)
  {
    d = ((x - 0)^2 + (y-1)^2)^(1/2)
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]],cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}

print(opt.cut.distance(perf1, prediction.obj1))
#0.6
Predicted.lr[predict.lr1 < 0.6] <- 0
Predicted.lr[predict.lr1 > 0.6] <- 1
CMlr1 <- confusionMatrix(Predicted.lr,actual.lr, positive = "1")
#Accuracy : 58.81% Sensitivity : 58.4% Specificity = 59.41%

#The best threshold doesnt give high sensitivity and specificity values. Hence, we will have to look at other
#models for better performance






#*************************************************DECISION TREE***************************************************

ind <- sample(2,nrow(Crimes),prob=c(0.7,0.3),replace = TRUE)
TrainCrimes <- Crimes[ind == 1,]
TestCrimes <- Crimes[ind==2,]

#VARIABLE SELECTION
#Qualitative date variable and lattitude nd longitude information
formula1 <- CrimeCategory ~ Date + Block + Beat + Ward + `Community Area`+ Latitude + Longitude
Crime.tree <- rpart(formula1, data = Trainsample, method = "class")
printcp(Crime.tree)

actual.tree <- Testsample$CrimeCategory
Predicted.tree <- predict(Crime.tree, newdata = Testsample, type = "class")
CM <- confusionMatrix(Predicted.tree,actual.tree, positive = "1")
#Date is a categorical variable and it finds new set of values of date in the test set. 
#Therefore, date as a categorical varibale cannot be used. 

formula2 <- CrimeCategory ~ Year + Month + Day + Hour + Mins + Beat + Ward + `Community Area`+ Latitude + Longitude
Crime.tree2 <- rpart(formula2, data = Trainsample, method = "class")
printcp(Crime.tree)

actual.tree <- Testsample$CrimeCategory
Predicted.tree2 <- predict(Crime.tree2, newdata = Testsample, type = "class")
CM2 <- confusionMatrix(Predicted.tree2,actual.tree, positive = "1")
#Accuracy = 62.24 ; Sensitivity = 64.25 Specificity = 59.23

#Removing Year and Days
formula3 <- CrimeCategory ~  Month  + Hour + Mins + Beat + Ward + `Community Area`+ Latitude + Longitude
Crime.tree3 <- rpart(formula3, data = Trainsample, method = "class")
printcp(Crime.tree)

actual.tree <- Testsample$CrimeCategory
Predicted.tree3 <- predict(Crime.tree3, newdata = Testsample, type = "class")
CM3 <- confusionMatrix(Predicted.tree3,actual.tree, positive = "1")
#Accuracy = 62.24 ; Sensitivity = 64.25 Specificity = 59.23

#Removing Mins
formula4 <- CrimeCategory ~  Month  + Hour + Beat + Ward + `Community Area`+ Latitude + Longitude
Crime.tree4 <- rpart(formula4, data = Trainsample, method = "class", control = rpart.control(cp = 0.01))
printcp(Crime.tree)

actual.tree <- Testsample$CrimeCategory
Predicted.tree4 <- predict(Crime.tree4, newdata = Testsample, type = "class")
CM4 <- confusionMatrix(Predicted.tree4,actual.tree, positive = "1")
#Accuracy = 61.73% ; Sensitivity = 90.95% Specificity = 18.02%

#*********** DECISION TREE MODEL *****************************

#Using cross validation on the sample set to understand what complexity factor is giving the best performance

sequence = seq(0.1,1,0.1)
sequence = c(sequence, 0.1*sequence )
formulax <-CrimeCategory ~ Beat + Ward + `Community Area`+ Latitude + Longitude + Hour + Month

for (i in sequence)
{
  Crime.treeCV <- rpart(formulax, data = Trainsample, method = "class", control = rpart.control(cp = i))
  actual.tree <- Testsample$CrimeCategory
  predicted.treeCV <- predict(Crime.treeCV, newdata = Testsample, type = "class")
  CMcv = confusionMatrix(predicted.treeCV,actual.tree, positive = "1")
  #print(CMcv)
  print(c("CP = ",i,"; Accuracy =", CMcv$overall[1],"; Sensitivity =",CMcv$byClass[1],"; Specificity = ",CMcv$byClass[2]))
}
#Best results : Accuracy = 61.73% ; Sensitivity = 90.95% Specificity = 18.02% cp = 0.01

formulay <- CrimeCategory ~ Year + Month + Day + Hour + Mins + Beat + Ward + `Community Area`+ Latitude + Longitude

for (i in sequence)
{
  Crime.treeCV <- rpart(formulay, data = Trainsample, method = "class", control = rpart.control(cp = i))
  actual.tree <- Testsample$CrimeCategory
  predicted.treeCV <- predict(Crime.treeCV, newdata = Testsample, type = "class")
  CMcv = confusionMatrix(predicted.treeCV,actual.tree, positive = "1")
  #print(CMcv)
  print(c("CP = ",i,"; Accuracy =", CMcv$overall[1],"; Sensitivity =",CMcv$byClass[1],"; Specificity = ",CMcv$byClass[2]))
}
#Best results : Accuracy = 62.24 ; Sensitivity = 64.25 Specificity = 59.23: cp = 0.01





#*****************************************KNN*************************************************************
#Making target variable quantitative
TrainCrimes01 <- TrainCrimes
TestCrimes01 <- TestCrimes
TestCrimes01$CrimeCategory[TestCrimes01$CrimeCategory == "INDEX CRIME"] <- 1
TestCrimes01$CrimeCategory[TestCrimes01$CrimeCategory == "NON-INDEX CRIME"] <- 0

TrainCrimes01$CrimeCategory[TrainCrimes01$CrimeCategory == "INDEX CRIME"] <- 1
TrainCrimes01$CrimeCategory[TrainCrimes01$CrimeCategory == "NON-INDEX CRIME"] <- 0

#Checking for missing values
Trainsample <- subset(Trainsample, !is.na(Trainsample$Ward))
sum(is.na(Testsample))
sum(is.na(Trainsample))

#Conerting to numeric
str(TrainCrimes01)
TrainCrimes01$Hour <- as.numeric(TrainCrimes01$Hour)
TrainCrimes01$CrimeCategory <- as.numeric(TrainCrimes01$CrimeCategory)

TrainCrimes01$CrimeCategory <- as.numeric(TrainCrimes01$CrimeCategory)
TestCrimes01$CrimeCategory <- as.numeric(TestCrimes01$CrimeCategory)

#KNN model is being trained to run on the time and laditude and longitude information
#only time(Hour) of the day and latitude longitude information
crimes.knn <- knn(train=TrainCrimes01[-c(1,2,7,8,9,12,13,15)], test=TestCrimes01[-c(1,2,7,8,9,12,13,15)],
                  cl=as.factor(TrainCrimes01$CrimeCategory), k=1)
confusionMatrix(crimes.knn,TestCrimes01$CrimeCategory )
#Accuracy = 99.3% Sensitivity = 98.74% Specificity = 99.7%

#only time(Hour) of the day,  and month and latitude longitude information
crimes.knn2 <- knn(train=TrainCrimes01[-c(1,2,7,8,9,13,15)], test=TestCrimes01[-c(1,2,7,8,9,13,15)],
                   cl=as.factor(TrainCrimes01$CrimeCategory), k=1)
confusionMatrix(crimes.knn2,TestCrimes01$CrimeCategory )
#Accuracy = 92.1% Sensitivity = 88.29% Specificity = 94.7%

#trying to convert the data oh hours and minutes into minutes. so as to include the minutes information also 
#Eg, 09:35 will become (9*60)+35 mins
TrainCrimes01$Mins = (TrainCrimes01$Hour*60)+TrainCrimes01$Mins
TestCrimes01$Mins = (TestCrimes01$Hour*60)+TestCrimes01$Mins

View(TrainCrimes01)

#mins and latitude longitude
crimes.knn3 <- knn(train=TrainCrimes01[c(3,4,5,6,10,11,15)], test=TestCrimes01[c(3,4,5,6,10,11,15)],
                   cl=as.factor(TrainCrimes01$CrimeCategory), k=1)
confusionMatrix(crimes.knn3,TestCrimes01$CrimeCategory )
#Accuracy = 58.8% Sensitivity = 48.1% Specificity = 65.99%

#THEREFORE, OUT OF ALL THE KNN MODELS WE CHOSE THE ONE WITH ONLY TIME(HOUR) OF THE DAY AND LATITUDE AND LONGITUDE INFORMATION
#and POLICE DEPARTMENT INFORMATION
#Using cross validation to get the optimal value of k so as to avoid overfitting and get the best results


# ON ENTIRE DATA (Without the random sampling of data)
for (i in 1:10)
{
  crimes.knn <- knn(train=TrainCrimes01[-c(1,2,7,8,9,12,13,15)], test=TestCrimes01[-c(1,2,7,8,9,12,13,15)],
                    cl=as.factor(TrainCrimes01$CrimeCategory), k=i)
  CNKNN <- confusionMatrix(crimes.knn,TestCrimes01$CrimeCategory )
  print(c("K = ",i,"; Accuracy =", CNKNN$overall[1],"; Sensitivity =",CNKNN$byClass[1],"; Specificity = ",CNKNN$byClass[2]))
  
}

#*********** FINAL KNN MODEL ****************

#ON THE SAME SAMPLE OF THE DATA THAT WAS USED FOR DECISION TREES AND LOGISTICS REGRESSION (After the ransom sampling)
for (i in 1:1)
{
  crimes.knn <- knn(train=Trainsample[-c(1,2,7,8,9,12,13,15)], test=Testsample[-c(1,2,7,8,9,12,13,15)],
                    cl=as.factor(Trainsample$CrimeCategory), k=i)
  CNKNN <- confusionMatrix(crimes.knn,Testsample$CrimeCategory )
  print(c("K = ",i,"; Accuracy =", CNKNN$overall[1],"; Sensitivity =",CNKNN$byClass[1],"; Specificity = ",CNKNN$byClass[2]))
  
}

#K = 1 : Accuracy =" "0.9820 Sensitivity = 0.96921 Specificity = 0.9905
#K = 2 : Accuracy : 97.42% ; Sensitivity = 95.20% ; Spepecifity : 98.8%


#Testing KNN with only latitude and longitude information

crimes.knn <- knn(train=Trainsample[c(10,11,16)], test=Testsample[c(10,11,16)],
                  cl=as.factor(Trainsample$CrimeCategory), k=1)
CNKNN <- confusionMatrix(crimes.knn,Testsample$CrimeCategory )
print(c("K = ",1,"; Accuracy =", CNKNN$overall[1],"; Sensitivity =",CNKNN$byClass[1],"; Specificity = ",CNKNN$byClass[2]))

#100% accuracy ; 100% sensitivity ; 100% specificity


#-----------------------------------------------------------------------------------------------------------------
#                                         ***ARREST PREDICTION*****
#-----------------------------------------------------------------------------------------------------------------

#Data preparation

ArrestD$Arrest[ArrestD$Arrest == "TRUE"] <- 1
ArrestD$Arrest[ArrestD$Arrest == "FALSE"] <- 0

ArrestD["Hour"] <- paste(substring(Crimes2$Date,12,13),substring(Crimes2$Date,21,22),sep = " ")

for (i in 1:ncol(Arrest))
{
  s= sum(is.na(Arrest[[i]]))
  print(c("Number of NA's in ",names(Arrest)[i], "is ", s))
  
}

ArrestD <- as.data.frame(Arrest)
ArrestD <- subset(ArrestD, !is.na(ArrestD$Ward))

sum(is.na(ArrestD))

#Because the data volume is huge, we take a random sample of the data
sampleind <- sample(2,nrow(ArrestD),prob=c(0.5,0.5),replace = TRUE)
ArrestD <- ArrestD[sampleind ==1,]

ArrestD$Hour <- as.numeric(ArrestD$Hour)

#Deviding the data into test and train
ind <- sample(2,nrow(ArrestD),prob=c(0.7,0.3),replace = TRUE)
TrainArrest <- ArrestD[ind == 1,]
TestArrest <- ArrestD[ind==2,]


#------------------------------------K NEARESR NEIGHBOUR-------------------------------------------------------

arrest.knn <- knn(train=TrainArrest[c(4,6,7,10,11,12,14)], test=TestArrest[c(4,6,7,10,11,12,14)],
                  cl=as.factor(TrainArrest$Arrest), k=1)

CNKNN <- confusionMatrix(arrest.knn,TestArrest$Arrest )
print(c("K = ",1,"; Accuracy =", CNKNN$overall[1],"; Sensitivity =",CNKNN$byClass[1],"; Specificity = ",CNKNN$byClass[2]))

#Accuracy =" "0.727097661623109"   "; Sensitivity =0.831578547265194"  "; Specificity = " "0.298001873243834

arrest.knn2 <- knn(train=TrainArrest[c(4,6,7,11,12)], test=TestArrest[c(4,6,7,11,12)],
                   cl=as.factor(TrainArrest$Arrest), k=1)

CNKNN2 <- confusionMatrix(arrest.knn2,TestArrest$Arrest )
print(c("K = ",1,"; Accuracy =", CNKNN2$overall[1],"; Sensitivity =",CNKNN2$byClass[1],"; Specificity = ",CNKNN2$byClass[2]))

#----
arrest.knn2 <- knn(train=TrainArrest[c(11,12,16)], test=TestArrest[c(11,12,16)],
                   cl=as.factor(TrainArrest$Arrest), k=1)

CNKNN2 <- confusionMatrix(arrest.knn2,TestArrest$Arrest )
print(c("K = ",1,"; Accuracy =", CNKNN2$overall[1],"; Sensitivity =",CNKNN2$byClass[1],"; Specificity = ",CNKNN2$byClass[2]))


for (i in 1:25)
{
  arrest.knn2 <- knn(train=TrainArrest[c(4,6,7,11,12)], test=TestArrest[c(4,6,7,11,12)],
                     cl=as.factor(TrainArrest$Arrest), k=i)
  
  CNKNN2 <- confusionMatrix(arrest.knn2,TestArrest$Arrest )
  print(c("K = ",i,"; Accuracy =", CNKNN2$overall[1],"; Sensitivity =",CNKNN2$byClass[1],"; Specificity = ",CNKNN2$byClass[2]))
  
}

table(ArrestD$Arrest)[2]/table(ArrestD$Arrest)[1]+table(ArrestD$Arrest)[2]
42136/219003

#-----------------------------------LOGISTICE REGRESSIOn-------------------------------------------

#Using threshold = 0.4
lrformula <- relevel(factor(Arrest),ref="0") ~ Beat + Ward + `Community Area` + Latitude + Longitude 
Arrest.logReg <- glm(lrformula, data = TrainArrest, family = "binomial")
summary(Arrest.logReg)
predict.lr1 <- predict(Arrest.logReg, newdata = TestArrest, type = "response")

Predicted.lr <- rep(NA,nrow(TestArrest))
Predicted.lr[predict.lr1 < 0.4] <- 0
Predicted.lr[predict.lr1 > 0.4] <- 1

actual.lr <- TestArrest$Arrest
CMlr1 <- confusionMatrix(Predicted.lr,actual.lr, positive = "1")

#Accuracy : 0.8042  Sensitivity : 0.0000    Specificity : 1.0000   


#Using threshold = 0.2
lrformula <- relevel(factor(Arrest),ref="0") ~ Beat + Ward + `Community Area` + Latitude + Longitude 
Arrest.logReg <- glm(lrformula, data = TrainArrest, family = "binomial")
summary(Arrest.logReg)
predict.lr1 <- predict(Arrest.logReg, newdata = TestArrest, type = "response")

Predicted.lr <- rep(NA,nrow(TestArrest))
Predicted.lr[predict.lr1 < 0.2] <- 0
Predicted.lr[predict.lr1 > 0.2] <- 1

actual.lr <- TestArrest$Arrest
CMlr1 <- confusionMatrix(Predicted.lr,actual.lr, positive = "1")

#Accuracy : 0.6073   Sensitivity : 0.44630    Specificity : 0.64651      


