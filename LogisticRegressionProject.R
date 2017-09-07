proj_na<-read.delim("Stat 229 Logisitic Problem Data Set 5.txt",na.strings = "") 
proj<-na.omit(proj_na)
#Step 1-- Univariate Tests-> if p>0.25 reject
chisq.test(proj$DIED,proj$FEMALE) #Keep
chisq.test(proj$DIED,proj$INS_TYP) #Keep
chisq.test(proj$DIED,proj$YEAR) #Keep
chisq.test(proj$DIED,proj$RACE) #Keep
chisq.test(proj$DIED,proj$ZIPINC_QRTL) #Keep
chisq.test(proj$DIED,proj$GSW_INT) #Keep
chisq.test(proj$DIED,proj$GUN_TYP) #Keep
chisq.test(proj$DIED,proj$SHOCK) #Keep
chisq.test(proj$DIED,proj$AWEEKEND) #Keep
chisq.test(proj$DIED,proj$ELECTIVE) #Keep
chisq.test(proj$DIED,proj$HOSPST) #Keep
mod_a<-glm(proj$DIED~proj$AGE,family="binomial")
summary(mod_a) #Keep
mod_b<-glm(proj$DIED~proj$TMPM)
summary(mod_b) #Keep
mod_1<-glm(proj$DIED~proj$AGE+proj$FEMALE+as.factor(proj$INS_TYP)+as.factor(proj$YEAR)+as.factor(proj$RACE)+as.factor(proj$ZIPINC_QRTL)+as.factor(proj$GSW_INT)+as.factor(proj$GUN_TYP)+proj$SHOCK+proj$AWEEKEND+proj$ELECTIVE+proj$TMPM+as.factor(proj$HOSPST),family="binomial")
summary(mod_1)
#######################################
#Age Female Ins_Type Year Race_6 GSW_INT2 GSW_INT5 Gun_Typ2,3,5 SHOCK ELECTIVE TMPM
mod_2<-glm(proj$DIED~proj$AGE+proj$FEMALE+as.factor(proj$INS_TYP)+as.factor(proj$YEAR)+as.factor(proj$RACE)+as.factor(proj$GSW_INT)+as.factor(proj$GUN_TYP)+proj$SHOCK+proj$ELECTIVE+proj$TMPM,family="binomial")
summary(mod_2)
anova(mod_2,mod_1,test="LRT")
proj$RACE_R<-ifelse(proj$RACE==6,1,0)
proj$INS_TYP_R<-ifelse(proj$INS_TYP==4,0,1)
proj$YEAR_R<-ifelse(proj$YEAR==2011,0,1)
proj$GUN_TYP_R<-ifelse(proj$GUN_TYP==2,0,1)
######################################################
mod_3<-glm(proj$DIED~proj$AGE+proj$FEMALE+proj$INS_TYP_R+proj$YEAR_R+proj$RACE_R+as.factor(proj$GSW_INT)+proj$GUN_TYP_R+proj$SHOCK+proj$ELECTIVE+proj$TMPM,family="binomial")
summary(mod_3)
anova(mod_3,mod_2,test="LRT")
#######################################
library(mfp)
f <- mfp(Surv(proj$DIED) ~ fp(proj$AGE,df=2)+fp(proj$TMPM,df=2),
         family = cox, select=0.05, verbose=TRUE)
summary(f)
f
602468-598421
598421-597603

#########################
proj$AgeFp1<-(proj$AGE/10)^3
proj$AgeFp2<-proj$AgeFp1*log(proj$AGE/10)
proj$TMPMFp1<-((proj$TMPM+12.7)/10)^3
proj$TMPMFp2<-proj$TMPMFp1*log((proj$TMPM+12.7)/10)
proj<-na.omit(proj)
#proj<-na.omit(proj)
mod_4<-glm(proj$DIED~proj$AgeFp1+proj$AgeFp2+proj$FEMALE+as.factor(proj$INS_TYP)+as.factor(proj$YEAR)+as.factor(proj$RACE_R)+as.factor(proj$GSW_INT)+as.factor(proj$GUN_TYP)+proj$SHOCK+proj$ELECTIVE+proj$TMPMFp1+proj$TMPMFp2,family="binomial")
summary(mod_4)
mod_5<-glm(proj$DIED~proj$AgeFp1+proj$AgeFp2+proj$FEMALE+proj$INS_TYP_R+proj$YEAR_R+proj$RACE_R+as.factor(proj$GSW_INT)+proj$GUN_TYP_R+proj$SHOCK+proj$ELECTIVE+proj$TMPMFp1+proj$TMPMFp2,family="binomial")
summary(mod_5)
mod_6<-glm(proj$DIED~proj$AgeFp1+proj$AgeFp2+proj$INS_TYP_R+proj$YEAR_R+proj$RACE_R+as.factor(proj$GSW_INT)+proj$GUN_TYP_R++proj$SHOCK+proj$TMPMFp1+proj$TMPMFp2,family="binomial")
summary(mod_6)

#####################################
#Testing Interactions
mod_7<-glm(proj$DIED~proj$AgeFp1+proj$AgeFp2+proj$INS_TYP_R+proj$YEAR_R+proj$RACE_R+as.factor(proj$GSW_INT)+proj$GUN_TYP_R+proj$SHOCK+proj$TMPMFp1+proj$TMPMFp2+proj$GSW_INT*proj$TMPMFp1,family="binomial")
summary(mod_7) 
logLik(mod_7)
anova(mod_6,mod_7,test="LRT")
mod_8<-glm(proj$DIED~proj$AgeFp1+proj$AgeFp2+proj$INS_TYP_R+proj$YEAR_R+proj$RACE_R+as.factor(proj$GSW_INT)+proj$GUN_TYP_R+proj$SHOCK+proj$TMPMFp1+proj$TMPMFp2+proj$AgeFp1*as.factor(proj$GSW_INT)+proj$AgeFp1*proj$TMPMFp1+proj$INS_TYP_R*proj$TMPMFp1+proj$YEAR_R*as.factor(proj$GSW_INT)+proj$SHOCK*proj$TMPMFp1+as.factor(proj$GSW_INT)*proj$TMPMFp1,family="binomial")
summary(mod_8) 
##########################
#Prelim Final Model
mod_9<-glm(proj$DIED~proj$AgeFp1+proj$AgeFp2+proj$INS_TYP_R+proj$YEAR_R+proj$RACE_R+as.factor(proj$GSW_INT)+proj$GUN_TYP_R+proj$SHOCK+proj$TMPMFp1+proj$TMPMFp2+proj$AgeFp1*proj$TMPMFp1+proj$SHOCK*proj$TMPMFp1,family="binomial")

summary(mod_9) 
###################################
#Assessing Model Fit
library(LogisticDx)
library(pROC)
#Not a great fit, need to look at outliers 
x<-gof(mod_9,g=11)
x$ctHL
x$gof
x$auc
diag<-dx(mod_9,byCov=FALSE)
plot(mod_9)
summary(mod_9)
proj$resid<-sqrt(mod_9$residuals^2)
proj$pred<-mod_9$fitted.values
proj$hat<-hatvalues(mod_9)
#proj$delX<-proj$resid/sqrt(1-proj$hat)
proj$Bhat<-(proj$resid*proj$hat)/(1-proj$hat)
library(dplyr)
diag_1<-data.frame(diag$`proj$AgeFp1`,diag$dChisq,diag$dDev,diag$dBhat,diag$h)
#colnames(diag_1)[1]<-"AgeFp1"
#proj_2<-merge(proj,diag_1, by.x="AgeFp1",by.y="AgeFp1")

proj_red<-select(proj,id,AgeFp1)
outlier_1<-filter(diag_1,diag.dChisq>=200)
outlier_dat<-filter(proj,resid>=200)
outlier_1a<-filter(diag_1,diag.h>=0.030)
outlier_2<-filter(proj,hat<=8.004614e-06&hat>=7.95e-06)
outlier_dat$id
outliers<-filter(proj,id==11307|id==23234|id==26085|id==5782|id==48451|id==25092|id==43050|id==22894|id==42869)
outliers$Bhat<-c(0.033744511,0.03491554,0.01364496,0.07039333,0.01979121,0.02134873,0.05641788,0.01746734,0.005541525)
outliers$dDev<-c(1.2191832,16.76190,13.12654,16.40904,13.54678,14.25517,12.86409,13.14250,0.2862531)


proj_5782<-proj[-5782,]
mod_10<-glm(DIED~AgeFp1+AgeFp2+INS_TYP_R+YEAR_R+RACE_R+as.factor(GSW_INT)+GUN_TYP_R+SHOCK+TMPMFp1+TMPMFp2+AgeFp1*TMPMFp1+SHOCK*TMPMFp1,family="binomial",data=proj_5782)
summary(mod_10)  
proj_11307<-proj[-11307,]
mod_11<-glm(DIED~AgeFp1+AgeFp2+INS_TYP_R+YEAR_R+RACE_R+as.factor(GSW_INT)+GUN_TYP_R+SHOCK+TMPMFp1+TMPMFp2+AgeFp1*TMPMFp1+SHOCK*TMPMFp1,family="binomial",data=proj_11307)
summary(mod_11) 
proj_23234<-proj[-23234,]
mod_12<-glm(DIED~AgeFp1+AgeFp2+INS_TYP_R+YEAR_R+RACE_R+as.factor(GSW_INT)+GUN_TYP_R+SHOCK+TMPMFp1+TMPMFp2+AgeFp1*TMPMFp1+SHOCK*TMPMFp1,family="binomial",data=proj_23234)
summary(mod_12) 
proj_26085<-proj[-26085,]
mod_13<-glm(DIED~AgeFp1+AgeFp2+INS_TYP_R+YEAR_R+RACE_R+as.factor(GSW_INT)+GUN_TYP_R+SHOCK+TMPMFp1+TMPMFp2+AgeFp1*TMPMFp1+SHOCK*TMPMFp1,family="binomial",data=proj_26085)
summary(mod_13) 
proj_48451<-proj[-48451,]
mod_14<-glm(DIED~AgeFp1+AgeFp2+INS_TYP_R+YEAR_R+RACE_R+as.factor(GSW_INT)+GUN_TYP_R+SHOCK+TMPMFp1+TMPMFp2+AgeFp1*TMPMFp1+SHOCK*TMPMFp1,family="binomial",data=proj_48451)
summary(mod_14) 
summary(mod_9)

#Can delete obs 5782
#obs 11307->19.5% change in a beta
#can delete obs 23234 and 26085
(8.084e-04-7.926e-04)/8.084e-04

proj_rem<-proj_na[-c(5782,11307,23234,26085,48451,25092,43050,22894,42869),]
#proj_rem<-proj_na[-c(11307,22894,23234,25092,26085,42869,43050),]
proj_rem$RACE_R<-ifelse(proj_rem$RACE==6,1,0)
proj_rem$INS_TYP_R<-ifelse(proj_rem$INS_TYP==4,0,1)
proj_rem$YEAR_R<-ifelse(proj_rem$YEAR==2011,0,1)
proj_rem$GUN_TYP_R<-ifelse(proj_rem$GUN_TYP==2,0,1)
proj_rem$AgeFp1<-(proj_rem$AGE/10)^3
proj_rem$AgeFp2<-proj_rem$AgeFp1*log(proj_rem$AGE/10)
proj_rem$TMPMFp1<-((proj_rem$TMPM+12.7)/10)^3
proj_rem$TMPMFp2<-proj_rem$TMPMFp1*log((proj_rem$TMPM+12.7)/10)
proj_rem<-na.omit(proj_rem)


mod_10<-glm(DIED~AgeFp1+AgeFp2+INS_TYP_R+YEAR_R+RACE_R+as.factor(GSW_INT)+GUN_TYP_R+SHOCK+TMPMFp1+TMPMFp2+AgeFp1*TMPMFp1+SHOCK*TMPMFp1,family="binomial",data=proj_rem)
summary(mod_10)
#5782->less than 3% change
#11307->12% change in agefp1 and Gsw_Int3
#23234->33% change in GSW_Int4
#26085-> Less than 5% change
#48451->Less than 2% change
#25092->13% change in agefp1
#43050->13% change in Gsw_int3
#22894->4.6% change in gsw_int3
#42869->33% change in gsw_int4
((coef(mod_10)-coef(mod_9))/(coef(mod_9)))*100
summary(mod_9)
logLik(mod_9)
mod_15<-glm(DIED~AgeFp1+AgeFp2+INS_TYP_R+YEAR_R+RACE_R+as.factor(GSW_INT)+GUN_TYP_R+SHOCK+TMPMFp1+TMPMFp2+AgeFp1*TMPMFp1+SHOCK*TMPMFp1,family="binomial",data=proj_rem)
summary(mod_15)
y<-gof(mod_10,g=10)
y$ctHL
y$gof
x$gof
plot(mod_15)
diag_rem<-dx(mod_15)

proj_rem$pred<-mod_15$fitted.values
proj_rem$sens<-ifelse(proj_rem$pred>=0.8,1,0)
table(proj_rem$DIED,proj_rem$sens)
122/(122+692) #0.75
483/(1244+483) #0.5
764/(764+1474) #0.4
1160/(1160+1709) #0.3

536/(536+85)
29510/(29510+212)
summary(mod_9)
exp(coef(mod_9))
exp(confint.lm(mod_9))

proj$sens<-ifelse(proj$pred>=0.85,1,0)
addmargins(table(proj$DIED,proj$sens))
29538/31637
419/476
vcov(mod_9)
(1.07e+03)^(1/3)
(3.03e+03)^(1/3)
SE<-sqrt(0.70+0.0166+(2*(-0.0028)))^(1/3)
SE
2.76+(1.96*SE)
2.76-(1.96*SE)
(1.007)^(1/3)
SE_1<-sqrt(100*0.00001+100*0.00000017+2*10*10*0.00003)^(1/3)
0.94+(1.96*SE_1)
0.94-(1.96*SE_1)
