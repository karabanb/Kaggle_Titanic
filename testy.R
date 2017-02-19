


############################# Creating folds ###########################

library(caret)
data("GermanCredit")

set.seed(12345)

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

##### formula ######

formula<-"Class~Age+Amount+Duration+ResidenceDuration+InstallmentRatePercentage+ForeignWorker + Housing.Rent + Job.UnemployedUnskilled"

##### fold 1 #######
fit.1<-glm(data=in.training1,formula, family = binomial)
fit.1<-step(fit.1, direction = "both")
(fit1.summary<-summary(fit.1))
f1<-round(fit1.summary$coefficients[,4], digits = 2)

##### fold 2 ######

fit.2<-glm(data=in.training2,formula, family = binomial)
fit.2<-step(fit.2, direction = "both")
(fit2.summary<-summary(fit.2))
f2<-round(fit2.summary$coefficients[,4], digits = 2)

##### fold 3 ######

fit.3<-glm(data=in.training3,formula, family = binomial)
fit.3<-step(fit.3, direction = "both")
(fit3.summary<-summary(fit.3))
f3<-round(fit3.summary$coefficients[,4], digits = 2)

##### fold 4 ######

fit.4<-glm(data=in.training4,formula, family = binomial)
fit.4<-step(fit.4, direction = "both")
(fit4.summary<-summary(fit.4))
f4<-round(fit4.summary$coefficients[,4], digits = 2)

##### fold 5 ######

fit.5<-glm(data=in.training5,formula, family = binomial)
fit.5<-step(fit.5, direction = "both")
(fit5.summary<-summary(fit.5))
f5<-round(fit5.summary$coefficients[,4], digits = 2)

##### folds summary #####

#p_value<-c(f1,f2,f3,f4,f5)

n<-max(length(f1),
       length(f2),
       length(f3),
       length(f4),
       length(f5))

length(f1)<-n
length(f2)<-n
length(f3)<-n
length(f4)<-n
length(f5)<-n

(df<-cbind(f1,f2,f3,f4,f5))




