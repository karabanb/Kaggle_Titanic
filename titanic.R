###############################################################################

#### CALLING USEFULL LIBRARIES

library(caret)
library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)
###############################################################################

#### READING DATA 

train<-read.csv("train.csv", stringsAsFactors = F)
test<-read.csv("test.csv", stringsAsFactors = F)
test$Survived<-0

################################################################################

#### CLEANING DATA

combi<-rbind(train,test)

## extracting titles from passangeres' names ##

combi$Title<-sapply(combi$Name,FUN=function(x){strsplit(x,split = '[,.]')[[1]][2]})
combi$Title<-sub(' ','',combi$Title)
combi$Title[combi$PassengerId == 797] <- 'Mrs' # female doctor
combi$Title[combi$Title %in% c('Lady', 'the Countess', 'Mlle', 'Mee', 'Ms')] <- 'Miss'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Col', 'Jonkheer', 'Rev', 'Dr', 'Master')] <- 'Mr'
combi$Title[combi$Title %in% c('Dona')] <- 'Mrs'
combi$Title <- factor(combi$Title)


## caclulating family size
combi$FamilySize<-combi$SibSp + combi$Parch +1 

## extracting information about deck from Cabin variable
combi$Deck<-substr(combi$Cabin,1,1)
combi$Deck[combi$Deck==""]<-'Unknown'
str(combi)

## predicting missing age values
predicted.age<-rpart(Age~ Pclass + Sex  + SibSp + Parch + Fare + Embarked + Title + Tile_2 + FamilySize + Deck, data=combi[!is.na(combi$Age),], method="anova")
combi$Age_full<-predict(predicted.age, combi)

## filing missing Embarked values
in.training$Embarked[c(62,830)]<-"C"


############################## Building the models #########################################

## Acc: train: 0.84844, test: 0.79904
set.seed(1234)
in.training<-combi[1:891,]
in.testing<-combi[892:nrow(combi),]
tree_1<-rpart(Survived~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + Deck, data=in.training, method = "class")
predicted.test_1<-predict(tree_1,in.testing, type = "class")
predicted.train<-predict(tree_1,in.training, type = "class")

my_solution_1<-data.frame(PassengerID=test$PassengerId,Survived=predicted.test_1)
write.csv(my_solution_1,"tree_1_Titanic.csv",row.names = F)
conf.matrix_bench<-table(predicted.train,in.training$Survived)
print(conf.matrix_bench)


