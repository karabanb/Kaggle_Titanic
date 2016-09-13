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

#### CLEANING DATA SET

combi<-rbind(train,test)

## extracting titles from passangeres' names ##

# 1st manner
combi$Title<-sapply(combi$Name,FUN=function(x){strsplit(x,split = '[,.]')[[1]][2]})
combi$Title<-sub(' ','',combi$Title)
combi$Title[combi$PassengerId == 797] <- 'Mrs' # female doctor
combi$Title[combi$Title %in% c('Lady', 'the Countess', 'Mlle', 'Mee', 'Ms')] <- 'Miss'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Col', 'Jonkheer', 'Rev', 'Dr', 'Master')] <- 'Mr'
combi$Title[combi$Title %in% c('Dona')] <- 'Mrs'
combi$Title <- factor(combi$Title)

# 2nd manner
combi$Tile_2<-gsub('.*, |\\..*', '',combi$Name)
common_title <- c('Miss', 'Mrs', 'Mr','Master')
noble_title <- c('Don', 'Dona','Sir','the Countess', 'Lady', 'Jonkheer')

'%!in%' <- function(x,y)!('%in%'(x,y))  #define not in function

#Assign names to similar ones to avoid outliers
combi$Tile_2[combi$Tile_2 == 'Mlle']        <- 'Miss' 
combi$Tile_2[combi$Tile_2 == 'Ms']          <- 'Miss'
combi$Tile_2[combi$Tile_2 == 'Mme']         <- 'Mrs' 
combi$Tile_2[combi$Tile_2 == 'Capt']        <- 'Mr'
combi$Tile_2[combi$Tile_2 %!in% common_title && combi$Tile_2%!in% noble_title]  <- 'Rare Title'
combi$Tile_2[combi$Tile_2 %in% noble_title] <- 'Noble'
combi$Tile_2 <- as.factor(combi$Tile_2)

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
tree_1<-rpart(Survived~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + Tile_2 + FamilySize + Deck, data=in.training, method = "class")
predicted.test_1<-predict(tree_1,in.testing, type = "class")
predicted.train<-predict(tree_1,in.training, type = "class")

my_solution_1<-data.frame(PassengerID=test$PassengerId,Survived=predicted.test_1)
write.csv(my_solution_1,"tree_1_Titanic.csv",row.names = F)
conf.matrix_bench<-table(predicted.train,in.training$Survived)
print(conf.matrix_bench)

#  --Acc: train: 0.83950  test: 0.78469
tree_2<-rpart(Survived~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = in.training, method = "class")
predicted.test_2<-predict(tree_2, in.testing, type = "class")
predicted.train_2<-predict(tree_2, in.training, type="class")

my_solution<-data.frame(PassengerID=test$PassengerId, Survived=predicted.test_2)
write.csv(my_solution,file="tree_2_Titanic.csv", row.names = FALSE)

conf.matrix_2<-table(predicted.train_2,in.training$Survived)
print(conf.matrix_2)


## Tree with cp=0.005

#--Acc: 0.8518519 test: 0.78469

control<-rpart.control(cp=0.0058, minsplit = 1)
tree_3<-rpart(Survived~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + Tile_2 + FamilySize + Deck, data=in.training, method = "class", control = control)
predicted.train_3<-predict(tree_3, in.training, type = "class")
conf.matrix_3<-table(predicted.train_3, in.training$Survived)
print(conf.matrix_3)
printcp(tree_3)
print(conf.matric)

rpart.plot(tree_3, extra=1)
predicted_test_3<-predict(tree_3,in.testing, type="class")
my.solution_3<-data.frame(PassengerID=in.testing$PassengerId,Survived=predicted_test_3)
write.csv(my.solution_3,"tree_3_Titanic.csv", row.names = F)

