#Problem 1
my_data = read.csv("Residential.csv",TRUE,",")
class(my_data)
head(my_data)

#Question a)
model_1ai = lm(PRC ~ DST + SQRFT + LND + RMS + BTHS + TRNS, data = my_data)
summary(model_1ai)
model_1aii = lm(log(PRC) ~ log(DST) + log(SQRFT) + log(LND) + log(RMS) + log(BTHS) + log(TRNS), data = my_data)
summary(model_1aii)

#Question b)
model_1b = lm(log(PRC) ~ log(DST), data = my_data)
summary(model_1b)

#Question c)
model_1c = lm(log(PRC) ~ log(DST) + log(SQRFT) + log(LND) + log(RMS) + log(BTHS) + log(TRNS) + I((log(TRNS))^2), data = my_data)
summary(model_1c)
#turning point
(2.32330/(2*(-0.13079)))*(-1)

#Question d)
model_1d = lm(log(PRC) ~ log(DST) + I((log(DST))^2)+ log(SQRFT) + log(LND) + log(RMS) + log(BTHS) + log(TRNS) + I((log(TRNS))^2), data = my_data)
summary(model_1d)


#PROBLEM 2
my_data = read.csv("Credit.csv",TRUE,",")
class(my_data)
head(my_data)
typeof(my_data$MRT)
new0_data = subset(my_data, MRT==0)
new1_data = subset(my_data, MRT==1)
new_data = rbind(new0_data, new1_data)
new_data
rownames(new_data) = seq(length=nrow(new_data))

#Question a)
model_2a = lm(APP ~ COL + EXP + UPL + MRT + EDUC, data = new_data)
summary(model_2a)

#Question b)
model_2bi = lm(EXP ~ COL, data = new_data)
summary(model_2bi)
model_2bii = lm(APP ~ COL + EXP + COL*EXP + UPL + MRT + EDUC, data = new_data)
summary(model_2bii)

#Question c)
-0.102961 + 0.004981*55 
# effect of COL on APP is 0.170994
model_exp = lm(APP ~ COL + EXP + I(COL*(EXP-55)) + UPL + MRT + EDUC, data = new_data)
summary(model_exp)


#Question d)
library(lmtest)
library(sandwich)
model_2d = coeftest(model_2a, vcov = vcovHC(model_2a, type = "HC0"))
model_2d

t = qt(.975, 1980)
t
a = 0.1921945
b = 0.19219454
SE_a = 0.0198499 
SE_b = 0.02657715 
left_u = a - t*SE_a
right_u = a + t*SE_a
left_u
right_u
left_r = b - t*SE_b
right_r = b + t*SE_b
left_r
right_r

#Question e)
modelfitted = fitted(model_2a)
greater_than_1 = subset(modelfitted,modelfitted>=1)
greater_than_1
lower_than_0 = subset(modelfitted,modelfitted<=0)
lower_than_0

#Problem 3
Koop2004=read.csv('Koop2004.csv',sep=',')
head(Koop2004)
model3a=lm(logwage~educ+abil+exper+motheduc+fatheduc+broken+sib,data=Koop2004)
summary(model3a)

#b
require(car)
linearHypothesis(model3a, c('educ=0','abil=0','exper=0', 'motheduc=0', 'fatheduc=0', 'broken=0', 'sib=0'))

#p/value =  2.2e-16 it means that it is very close to zero and therefore we can surely reject it, F stats = 546.55

#c
linearHypothesis(model3a, c('motheduc=0', 'fatheduc=0', 'broken=0', 'sib=0'))
#p-value=1.99e-11 , F stats=14.025, we can reject

cor(Koop2004$educ,Koop2004$fatheduc)

#d
meanabil<-mean(Koop2004$abil)
meanexper<-mean(Koop2004$exper)
meanmotheduc<-mean(Koop2004$motheduc)
meanfatheduc<-mean(Koop2004$fatheduc)
meansib<-mean(Koop2004$sib)

meanabil
meanexper
meanmotheduc
meanfatheduc
meansib


means12<-data.frame(1,12,meanabil,meanexper,meanmotheduc,meanfatheduc,0,meansib)
means12a<-data.frame(t(means12))
means12b<-data.matrix(means12a)
means12b

means13<-data.frame(1,13,meanabil,meanexper,meanmotheduc,meanfatheduc,0,meansib)
means13a<-data.frame(t(means13))
means13b<-data.matrix(means13a)
means13b

educ12=exp(model3a$coefficients %*% means12b[,1])
educ13=exp(model3a$coefficients %*% means13b[,1])
magwage=educ13-educ12          
magwage

#the estimateofthe marg. val. od additional year is 0.7048378

#e
Koop2004$highschool <-1*(Koop2004$educ<=12)
Koop2004$college<-1*(Koop2004$educ >12 & Koop2004$educ<=16)
Koop2004$graduate <-1*(Koop2004$educ > 16)
head(Koop2004)
model3e=lm(logwage~highschool+graduate+abil+exper+motheduc+fatheduc+broken+sib,data=Koop2004)
summary(model3e)
cor(Koop2004$highschool,Koop2004$college)

m<-lm(graduate~college+highschool,data=Koop2004)
summary(m)
cor(Koop2004$exper,Koop2004$abil)

