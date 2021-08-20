rm(list = ls())  ###remove all old data from the memory
library(stargazer)
library(caret)
library(e1071)
library(InformationValue)
library(pscl)

mydata = read.csv(file="loan.csv", header=TRUE, sep=";")
head(mydata)
str(mydata)
summary(mydata) #we see we have NAs in our data set - we need to get rid of them. Missing values for Gender, Married
                # Dependents, Self_Employed, LoanAmount, Loan_Amount_Term, Credit_History

xtabs(~Gender, data = mydata) #13 NAs
xtabs(~Married, data = mydata) #3 NAs
xtabs(~Self_Employed, data = mydata) #32 NAs
# there are also NAs on other variables, but it is not important for us to count them or identify them

#we have two types of missing values in our data set, blank and NA, we decided to rename all of them to NA, so we can use the following function
mydata[mydata == ""] <- NA 
newdata <- na.omit(mydata)

#part 3 
model_1 <- summary(lm(Loan_Status_Int ~ Gender, data = newdata))
model_1 #Gender not significant (at 10% level, significant)

model_2 <- summary(lm(Loan_Status_Int ~ Married, data = newdata))
model_2 # Married is significant

model_3 <- summary(lm(Loan_Status_Int ~ Dependents, data = newdata))
model_3 #Dependents not significant

model_4 <- summary(lm(Loan_Status_Int ~ Education, data = newdata))
model_4 #Education not significant (at 10% level, significant)

model_5 <- summary(lm(Loan_Status_Int ~ Self_Employed, data = newdata))
model_5 #Self Employed not significant

model_6 <- summary(lm(Loan_Status_Int ~ ApplicantIncome, data = newdata))
model_6 #ApplicantIncome not significant

model_7 <- summary(lm(Loan_Status_Int ~ CoapplicantIncome, data = newdata))
model_7 #Coapplicant income not significant

model_8 <- summary(lm(Loan_Status_Int ~ LoanAmount, data = newdata))
model_8 #Loan amount not significant

model_9 <- summary(lm(Loan_Status_Int ~ Loan_Amount_Term, data = newdata))
model_9 # Loan Term not significant

model_10 <- summary(lm(Loan_Status_Int ~ Credit_History, data = newdata))
model_10 #Credit_History is significant

model_11 <- summary(lm(Loan_Status_Int ~ Property_Area, data = newdata))
model_11 #only Semiurban is significant

#part 4
#LPM - 
model_LPM_1 <- summary(lm(Loan_Status_Int ~ Gender + Married + Dependents + Education + Self_Employed + 
                          ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term +
                          Credit_History + Property_Area, data = newdata)) 
model_LPM_1

#based on the model_LPM Credit History and Propert_AreaSemiurban are statistically significant for our analysis.
#We tried also other combinations but they turned out to be worse, as R^2 decreased and even became negative

#Let's only include those that were somehow significant in part 3
model_LPM <- lm(Loan_Status_Int ~ Gender + Married + Education + Credit_History + Property_Area, data = newdata) 
summary_LPM<-summary(model_LPM) #seems to be the best model to estimate by LPM but we see heteroskedasticity

#one of the drawbacks is that fitted values might lie out of the desired interval

beta_0_LPM <- summary_LPM$coefficients[1,1]
beta_1_LPM <- summary_LPM$coefficients[2,1]
beta_2_LPM <- summary_LPM$coefficients[3,1]
beta_3_LPM <- summary_LPM$coefficients[4,1]
beta_4_LPM <- summary_LPM$coefficients[5,1]
beta_5_LPM <- summary_LPM$coefficients[6,1]
beta_6_LPM <- summary_LPM$coefficients[7,1]

probability_LPM <- beta_0_LPM + beta_1_LPM*1 + beta_2_LPM*1 + beta_3_LPM*0 + beta_4_LPM*1 + beta_5_LPM*1 + beta_6_LPM*1
probability_LPM # this is ok, but some might still be out of the interval, however probability of 98% can be suspicious

#probit
model_probit <-glm(Loan_Status_Int ~ Gender + Married + Education + Credit_History + Property_Area, family = binomial
                             (link = "probit"), data = newdata)
summary(model_probit) #our probit model

model_probit2 <- summary(glm(Loan_Status_Int ~ Gender + Married + Dependents + Education + Self_Employed + 
                               ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term +
                               Credit_History + Property_Area, family = binomial
                             (link = "probit"), data = newdata))
model_probit2 #this is just a check, AIC is lower in the first one, so the first probit model is better
#but we can use this to check goodness of fit, that the restricted model is better!

#logit
model_logit <- glm(Loan_Status_Int ~ Gender + Married + Education + Credit_History + Property_Area, family = binomial
                            (link = "logit"), data = newdata)
summary(model_logit) #our logit model, we see that significance and the signs are same as for probit

#summary of all of our chosen models
stargazer(model_LPM, model_probit, model_logit, header = FALSE, title = "Results of all the models",
          single.row = TRUE, type = "latex", column.labels=c("LPM","Probit","Logit"))

#part 5
#another goodness of fit measure - pseudo R^2
probit <- (glm(Loan_Status_Int ~ Gender + Married + Education + Credit_History + Property_Area, family = binomial
               (link = "probit"), data = newdata))
logit <- glm(Loan_Status_Int ~ Gender + Married + Education + Credit_History + Property_Area, family = binomial
             (link = "logit"), data = newdata)
pR2(probit,McFadden)
pR2(logit,McFadden)

#McFadden (pseudoR2 measure) is higher for probit so it is better model to use. We also see that llhNull (residual deviance of
#unrestricted model) is lower than llh (same but for restricted model). Now unrestricted model is what
#we actually use. Restricted would be for instance with just one independent variable, lower is better -> we
#should use the probit model as it is now

#another goodness of fit measure is to use the percent correctly predicted (what amount of data was predicted correctly)
predicted <- predict(probit, newdata, type="response")
table(newdata$Loan_Status_Int, predicted > 0.5) #we decided to set 0.5 cutoff here

predicted <- predict(logit, newdata, type="response")
table(newdata$Loan_Status_Int, predicted > 0.5) #we decided to set 0.5 cutoff here
#these gave us the same result so not very useful for our analysis


#part 7 evaluate the effects
# we have dummy variables so need to use more advanced code
beta_0_probit <- summary(model_probit)$coefficients[1,1]
beta_1_probit <- summary(model_probit)$coefficients[2,1]
beta_2_probit <- summary(model_probit)$coefficients[3,1]
beta_3_probit <- summary(model_probit)$coefficients[4,1]
beta_4_probit <- summary(model_probit)$coefficients[5,1]
beta_5_probit <- summary(model_probit)$coefficients[6,1]
beta_6_probit <- summary(model_probit)$coefficients[7,1]

#we need to transform our data a little bit here
newdata$Gender<-ifelse(newdata$Gender=="Male", 1, 0)
newdata$Married<-ifelse(newdata$Married=="Yes", 1, 0)
newdata$Education<-ifelse(newdata$Education=="Graduate", 1, 0)
newdata$Property_AreaUrban<-ifelse(newdata$Property_Area=="Urban", 1, 0)
newdata$Property_AreaSemiurban<-ifelse(newdata$Property_Area=="Semiurban", 1, 0)

# APE: average partial effect for credit history
ape_credit_history <- mean(pnorm(beta_0_probit + beta_1_probit*newdata$Gender + beta_2_probit*newdata$Married + 
                                   beta_3_probit*newdata$Education + beta_4_probit + beta_5_probit*newdata$Property_AreaSemiurban + 
                                   beta_6_probit*newdata$Property_AreaUrban) - pnorm(beta_0_probit + beta_1_probit*newdata$Gender+ 
                                   beta_2_probit*newdata$Married + beta_3_probit*newdata$Education + beta_5_probit*newdata$Property_AreaSemiurban + 
                                   beta_6_probit*newdata$Property_AreaUrban ))
ape_credit_history


# PEA: partial effect at the average for credit history
pea_credit_history <- (pnorm(beta_0_probit + beta_1_probit*mean(newdata$Gender) + beta_2_probit*mean(newdata$Married) + 
                               beta_3_probit*mean(newdata$Education) + beta_4_probit + beta_5_probit*mean(newdata$Property_AreaSemiurban) + 
                               beta_6_probit*mean(newdata$Property_AreaUrban)) - pnorm(beta_0_probit + beta_1_probit*mean(newdata$Gender)+ 
                               beta_2_probit*mean(newdata$Married) + beta_3_probit*mean(newdata$Education) + beta_5_probit*mean(newdata$Property_AreaSemiurban) + 
                               beta_6_probit*mean(newdata$Property_AreaUrban)))
pea_credit_history

#part 8
predicted <- predict(probit, newdata, type="response")
table(newdata$Loan_Status_Int, predicted > 0.5) #we set cutoff at 0.5 here as well as in part 5

optcutoff <- optimalCutoff(newdata$Loan_Status_Int, predictedScores = predicted) #we find the optimal cutoff value, same for logit model as the results above are the same
optcutoff
confusionMatrix(newdata$Loan_Status_Int, predicted, threshold = optcutoff) #confusion matrix with optimal cutoff



