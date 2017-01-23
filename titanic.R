###############################################################################

#### CALLING USEFULL LIBRARIES

library(caret)
library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(C50)
library(smbinning)
###############################################################################

#### READING DATA 

train<-read.csv("train.csv", stringsAsFactors = F)
test<-read.csv("test.csv", stringsAsFactors = F)
test$Survived<-0

train.ix<-c(1:nrow(train))  # train index

################################################################################

#### CLEANING DATA SET

combi<-rbind(train,test)

## extracting titles from passangeres' names ##

combi$Title_2<-gsub('.*, |\\..*', '',combi$Name)
common_title <- c('Miss', 'Mrs', 'Mr','Master')
noble_title <- c('Don', 'Dona','Sir','the Countess', 'Lady', 'Jonkheer')

'%!in%' <- function(x,y)!('%in%'(x,y))  #define not in function

#Assign names to similar ones to avoid outliers
combi$Title_2[combi$Title_2 == 'Mlle']        <- 'Miss' 
combi$Title_2[combi$Title_2 == 'Ms']          <- 'Miss'
combi$Title_2[combi$Title_2 == 'Mme']         <- 'Mrs' 
combi$Title_2[combi$Title_2 == 'Capt']        <- 'Mr'
combi$Title_2[combi$Title_2 %!in% common_title && combi$Tile_2%!in% noble_title]  <- 'Rare Title'
combi$Title_2[combi$Title_2 %in% noble_title] <- 'Noble'
combi$Title_2 <- as.factor(combi$Title_2)

## caclulating family size
combi$FamilySize<-combi$SibSp + combi$Parch +1 

## extracting information about deck from Cabin variable
combi$Deck<-substr(combi$Cabin,1,1)
combi$Deck[combi$Deck==""]<-'Unknown'
str(combi)

## predicting missing age values
predicted.age<-rpart(Age~ Pclass + Sex  + SibSp + Parch + Fare + Embarked +  Title_2 + FamilySize + Deck, data=combi[!is.na(combi$Age),], method="anova")
combi$Age_full<-predict(predicted.age, combi)


## filing missing Embarked values
in.training$Embarked[c(62,830)]<-"C"

## filling missing Fare Value
combi$Fare[1044]<-mean(combi$Fare, na.rm=T)

## converting usefull variables into facotrs
combi$Survived.Factor<-as.factor(combi$Survived)
combi$Pclass<-as.factor(combi$Pclass)
combi$Sex<-as.factor(combi$Sex)
combi$Embarked<-as.factor(combi$Embarked)
combi$Deck<-as.factor(combi$Deck)

############################## Calulating WoE for ScoreCard ################################

#### Woe for Sex #####

result.sex<-smbinning.factor(combi[train.ix,], y="Survived", x="Sex")
smbinning.plot(result.sex, "WoE")
##### WoE for Age #####

result.age<-smbinning(train,y="Survived",x="Age",p=0.02)
combi<-smbinning.gen(combi,result.age,chrname = "AgeBinned")

#### WoE for Title

result.Title<-smbinning.factor(combi[train.ix,],y="Survived",x="Title_2")
#smbinning.plot(result.Title, option = "WoE")

#### Woe for PcClass
result.PcClass<-smbinning.factor(combi[train.ix,],y="Survived",x="Pclass")
smbinning.plot(result.PcClass,option="WoE")
############################## Building the models #########################################

## Acc: train: 0.8929, test: 0.78947
set.seed(1234)
in.training<-combi[1:891,]
in.testing<-combi[892:nrow(combi),]
tree_1<-rpart(Survived~ Pclass + Sex + Age+ SibSp + Parch + Fare + Embarked + Title_2 + FamilySize + Deck, data=in.training, method = "class")
predicted.test_1<-predict(tree_1,in.testing, type = "class")
predicted.train<-predict(tree_1,in.training, type = "class")

my_solution_1<-data.frame(PassengerID=test$PassengerId,Survived=predicted.test_1)
write.csv(my_solution_1,"tree_1_Titanic.csv",row.names = F)
conf.matrix_bench<-table(predicted.train,in.training$Survived)
print(conf.matrix_bench)

## Acc: train: 0.9614922, test: 0.77033

rf<-randomForest(Survived~ Pclass + Sex + Age_full + SibSp + Parch + Fare + Embarked + Title_2 + FamilySize, data=in.training, ntree=1000, importance=TRUE)
predict.rf<-predict(rf,in.training, type="class")
predicted.test.rf<-predict(rf,in.testing,type="class")
conf.matrix.rf<-table(predict.rf, in.training$Survived)
print(conf.matrix.rf)
print(conf.matrix_bench)

my_solution_2<-data.frame(PassengerID=test$PassengerId,Survived=predicted.test.rf)
write.csv(my_solution_2,"rf_Titanic.csv",row.names = F)

## C5.0 test: test: 0.77033

in.training<-combi[1:891,]
in.testing<-combi[892:nrow(combi),]
tree_c50<-C5.0.default(y=in.training$Survived, x=in.training[,-c(1,2,4,6,9,11,12)], trials=10)
predicted.test.c5.0<-predict.C5.0(tree_c50,in.testing[,-c(1,2,4,6,9,11,12)])

my_solution_3<-data.frame(PassengerID=test$PassengerId,Survived=predicted.test.c5.0)
write.csv(my_solution_3,"tree_c50_Titanic.csv",row.names = F)
