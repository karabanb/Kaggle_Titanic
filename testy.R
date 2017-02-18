


lr.fit<-glm(Survived~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title_2 + FamilySize + Deck,data=in.training, family = "binomial")

lr.predictions.test <- predict(lr.fit, in.testing, type="response")
lr.predictions.test <- round(lr.predictions.test)

lr.predictions.train<-predict(lr.fit, in.training, type = "response")
lr.predictions.train<-round(lr.predictions.train)

confusionMatrix(data=lr.predictions.train, reference = in.training$Survived, positive = '1')

my_solution_lr<-data.frame(PassengerID=test$PassengerId,Survived=lr.predictions.test)
write.csv(my_solution_lr,"lr_Titanic.csv",row.names = F)

control<-trainControl(method = "cv")
lr.fit.caret<-train(Survived~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title_2 + FamilySize + Deck, data=in.training, method="glm", trControl=control)

plot(varImp(lr.fit.caret, scale=FALSE))

lr.predictions.value<-in.training$Survived

predictions<-prediction(lr.predictions.train, lr.predictions.value)

par(mfrow=c(1,2))


############################# Creating folds ###########################
set.seed(1234)

partition<-createDataPartition(train$Survived,p=0.2, list = FALSE)

folds<-createFolds(GermanCredit$Class,k=5, list = FALSE)

in.training5<-GermanCredit[folds!=5,]
in.testing5<-GermanCredit[folds==5,]

in.training4<-GermanCredit[folds!=4,]
in.testing4<-GermanCredit[folds==4,]

in.training3<-GermanCredit[folds!=3,]
in.testing3<-GermanCredit[folds==3,]

in.training2<-GermanCredit[folds!=2,]
in.testing2<-GermanCredit[folds==2,]

in.training1<-GermanCredit[folds!=1,]
in.testing1<-GermanCredit[folds==1,]

##### fold 1 #######
fit.1<-glm(data=in.training1,Class~ Age+Amount+Duration+ResidenceDuration+InstallmentRatePercentage+ForeignWorker, family = binomial)
fit.1<-step(fit.1, direction = "both")
(fit1.summary<-summary(fit.1))
f1<-fit1.summary$coefficients[,4]

##### fold 2 ######

fit.2<-glm(data=in.training2,Class~ Age+Amount+Duration+ResidenceDuration+InstallmentRatePercentage+ForeignWorker, family = binomial)
fit.2<-step(fit.2, direction = "both")
(fit2.summary<-summary(fit.2))
f2<-fit2.summary$coefficients[,4]

##### fold 3 ######

fit.3<-glm(data=in.training3,Class~ Age+Amount+Duration+ResidenceDuration+InstallmentRatePercentage+ForeignWorker, family = binomial)
fit.3<-step(fit.3, direction = "both")
(fit3.summary<-summary(fit.3))
f3<-fit3.summary$coefficients[,4]

##### fold 4 ######

fit.4<-glm(data=in.training4,Class~ Age+Amount+Duration+ResidenceDuration+InstallmentRatePercentage+ForeignWorker, family = binomial)
fit.4<-step(fit.4, direction = "both")
(fit4.summary<-summary(fit.4))
f4<-fit4.summary$coefficients[,4]

##### fold 5 ######

fit.5<-glm(data=in.training5,Class~ Age+Amount+Duration+ResidenceDuration+InstallmentRatePercentage+ForeignWorker, family = binomial)
fit.5<-step(fit.5, direction = "both")
(fit5.summary<-summary(fit.5))
f5<-fit5.summary$coefficients[,4]
##### folds summary #####

n<-max(length(fit1.summary$coefficients[,4]),
       length(fit2.summary$coefficients[,4]),
       length(fit3.summary$coefficients[,4]),
       length(fit4.summary$coefficients[,4]),
       length(fit5.summary$coefficients[,4]))

length(fit1.summary$coefficients[,4])<-n
length(fit2.summary$coefficients[,4])<-n
length(fit3.summary$coefficients[,4])<-n
length(fit4.summary$coefficients[,4])<-n
length(fit5.summary$coefficients[,4])<-n


df<-cbind(fit1.summary$coefficients[,4],
          fit2.summary$coefficients[,4],
          fit3.summary$coefficients[,4],
          fit4.summary$coefficients[,4],
          fit5.summary$coefficients[,4])




x <- 1:2
y <- 1:10
n <- max(length(x), length(y))
length(x) <- n                      
class(fit5.summary$coefficients[,4])

x<-as.numeric(x)


