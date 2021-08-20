require(readxl)
require(dplyr)
require(stargazer)
require(sem)
library(sem)
library(systemfit)
library(AER)

data = read_excel("data.xlsx")
head(data)

#delete bad data
data = data[data$R0536300 == 1,]
mydata = data[data$U2857200 > 0,]
mydata2 = mydata[mydata$Z9065401 >= 0,]
mydata2 = mydata2[mydata2$R1302500 >= 0,]
mydata2 = mydata2[mydata2$Z9065401 >= 0,]
mydata2 = mydata2[mydata2$R9829600 >= 0,]
mydata2 = mydata2[mydata2$Z9083900 >= 0,]

#dummies
mydata2$black <- ifelse(mydata2$R1482600 == '1', 1, 0)
mydata2$hispanic <- ifelse(mydata2$R1482600 == '2', 1, 0)
mydata2$mixed <- ifelse(mydata2$R1482600 == '3', 1, 0)
mydata2$non_hisp_black <- ifelse(mydata2$R1482600 == '4', 1, 0)

#create exprsqr
mydata2$expsqr <- (mydata2$Z9065401)^2

#final dataset
dataset= subset(mydata2, select = -c(E7023102,R0536401,R0536402,R1235800,R1302400,R1302600,R1302700,U1852600,Z9033700,Z9034100 ) )
names(dataset)[names(dataset) == "Z9065401"] <- "Experience"
names(dataset)[names(dataset) == "U2857200"] <- "Wage"
names(dataset)[names(dataset) == "R1302500"] <- "educ_mom_bio"
names(dataset)[names(dataset) == "R9829600"] <- "math"
names(dataset)[names(dataset) == "R0536300"] <- "Gender"
names(dataset)[names(dataset) == "R1482600"] <- "Race"
names(dataset)[names(dataset) == "Z9083900"] <- "Degree"
names(dataset)[names(dataset) == "R0000100"] <- "ID"

#main model
summary(dataset)
model = lm(log(Wage) ~  Degree + Experience + expsqr  + black + hispanic + mixed , data = dataset)
summary(model)
stargazer(model, header = FALSE, title = "Effect of Degree on Wages",
          single.row = TRUE, type = "latex")

#proxy model
model_proxy = lm(log(Wage) ~  Degree + Experience + expsqr  + black + hispanic + mixed + math , data = dataset)
summary(model_proxy)
stargazer(model_proxy, header = FALSE, title = "Model with Proxy",
          single.row = TRUE, type = "latex")

#IV model - relevance
model_check_relevance = lm(Degree~ educ_mom_bio +Experience + expsqr + black + hispanic + mixed , data=dataset)
summary(model_check_relevance)

#TSLS
model_IV = tsls(log(Wage) ~ Degree + Experience + expsqr +  black + hispanic + mixed,
              ~  educ_mom_bio + Experience + expsqr + black + hispanic + mixed , data = dataset)
summary(model_IV)
stargazer(model_IV, header = FALSE, title = "Model with IV",
          single.row = TRUE, type = "latex")

#TSLS using ivreg(). Even though we usually run 2SLS estimation by function tsls(), it is not compatible with stargazer()
#We decided to use ivreg() function which yields same results. This is just for visualization.
model_IV2 = ivreg(log(Wage) ~ Degree + Experience + expsqr +  black + hispanic + mixed 
                  | educ_mom_bio + Experience + expsqr + black + hispanic + mixed , data = dataset)
summary(model_IV2)

stargazer(model, model_proxy, model_IV2, header = FALSE, title = "Model, Model Proxy, Model IV",
          single.row = TRUE, type = "latex", column.labels=c("Model","Model Proxy","Model IV - 2SLS"))

#regression based test
model1 = lm(log(Wage) ~  Experience + expsqr + Degree + black + hispanic + mixed,data=dataset)
model2= lm(Degree ~  Experience + expsqr + educ_mom_bio + black + hispanic + mixed,data=dataset)
r=cor(model1$residuals,model2$residuals)
r
t_test=r*sqrt((length(model1$residuals-2)/(1-r^2)))
t_test
#hausmann test
model1 = lm(log(Wage) ~  Experience + expsqr + Degree + black + hispanic + mixed,data=dataset)
model2 = tsls(log(Wage) ~ Degree + Experience + expsqr +  black + hispanic + mixed,
              ~  educ_mom_bio + Experience + expsqr + black + hispanic + mixed , data = dataset)
hausman.systemfit(model1,model2)

#additional endogeneity check. if residuals significant -> endogeneity
model2 = lm(Degree ~  Experience + expsqr + educ_mom_bio + black + hispanic + mixed,data=dataset)
model1 = lm(log(Wage) ~ Experience + expsqr +  black + hispanic + mixed + model2$residuals, data=dataset)
summary(model1)
