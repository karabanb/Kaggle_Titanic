############################# Creating folds ###########################

library(caret)
data("GermanCredit")

set.seed(123456)

folds<-createFolds(GermanCredit$Class,k=10, list = FALSE)

in.training10<-GermanCredit[folds!=10,]
in.testing10<-GermanCredit[folds==10,]

in.training9<-GermanCredit[folds!=9,]
in.testing9<-GermanCredit[folds==9,]

in.training8<-GermanCredit[folds!=8,]
in.testing8<-GermanCredit[folds==8,]

in.training7<-GermanCredit[folds!=7,]
in.testing7<-GermanCredit[folds==7,]

in.training6<-GermanCredit[folds!=6,]
in.testing6<-GermanCredit[folds==6,]

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

##### fold 6 ######

fit.6<-glm(data=in.training6,formula, family = binomial)
fit.6<-step(fit.6, direction = "both")
(fit6.summary<-summary(fit.6))
f6<-round(fit6.summary$coefficients[,4], digits = 2)

##### fold 7 ######

fit.7<-glm(data=in.training7,formula, family = binomial)
fit.7<-step(fit.7, direction = "both")
(fit7.summary<-summary(fit.7))
f7<-round(fit7.summary$coefficients[,4], digits = 2)

##### fold 8 ######

fit.8<-glm(data=in.training8,formula, family = binomial)
fit.8<-step(fit.8, direction = "both")
(fit8.summary<-summary(fit.8))
f8<-round(fit8.summary$coefficients[,4], digits = 2)

##### fold 9 ######

fit.9<-glm(data=in.training9,formula, family = binomial)
fit.9<-step(fit.9, direction = "both")
(fit9.summary<-summary(fit.9))
f9<-round(fit9.summary$coefficients[,4], digits = 2)

##### fold 10 ######

fit.10<-glm(data=in.training10,formula, family = binomial)
fit.10<-step(fit.10, direction = "both")
(fit10.summary<-summary(fit.10))
f10<-round(fit10.summary$coefficients[,4], digits = 2)


##### folds summary #####

pval_list<-list(f1=f1, f2=f2, f3=f3, f4=f4, f5=f5, f6=f6, f7=f7, f8=f8, f9=f9, f10=f10)

m<-0

for (i in 1:length(pval_list)){
  length(pval_list[[i]])->m[i]
}

n<-max(m)

for (i in 1:length(pval_list)){
  length(pval_list[[i]])<-n
}

(len<-as.list(sort(c(f1=length(f1),
             f2=length(f2),
             f3=length(f3),
             f4=length(f4),
             f5=length(f5),
             f6=length(f6),
             f7=length(f7),
             f8=length(f8),
             f9=length(f9),
             f10=length(f10)
), decreasing = TRUE)))


(df<-as.data.frame(pval_list[names(len)]))




(len<-sort(c(f1=length(f1),
             f2=length(f2),
             f3=length(f3),
             f4=length(f4),
             f5=length(f5),
             f6=length(f6),
             f7=length(f7),
             f8=length(f8),
             f9=length(f9),
             f10=length(f10)
             ), decreasing = TRUE))
# 
# 
# length(f1)<-n
# length(f2)<-n
# length(f3)<-n
# length(f4)<-n
# length(f5)<-n
# length(f6)<-n
# length(f7)<-n
# length(f8)<-n
# length(f9)<-n
# length(f10)<-n
# 
# (df<-cbind(f1,f2,f3,f5,f6,f7,f8,f9,f4,f10))




