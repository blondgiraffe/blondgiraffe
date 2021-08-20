require(ggplot2)
require(readxl)
require(stargazer)
library(tidyverse)
require(eventstudies)


df=read_excel("C:/Users/stank/Stanuska/IES/Bachelor Thesis/DATA/summary_data/CRS_taxhavensplit.xlsx")

#taxhaven grafy
taxhaven <- subset(df, Taxhaven==1 & t_crs>= -6 & Year<= 6)
other <- subset(df,  t_crs>= -6 & t_crs<= 6 & Currency!='CHF'& Currency!='GBP' & Currency!='HKD' & Currency!='SGD' & Currency!='USD' & Currency!='EUR'& Currency!='JPY' & Currency!='AUD' & Currency!='CAD' & Currency!='CNY' & Currency!='NZD') 

#tax haven graph zoom in to 10 months before and after
ggplot(other, aes(t_crs, Value_res2, colour=Currency)) +
  geom_point()+
  geom_line()+
  scale_y_continuous(limit = c(-10, 500)) +
  scale_x_continuous(limit = c(-6, 6)) +
  geom_vline(xintercept=-0.5, col='black', size=1, linetype="dotted")+
  geom_hline(yintercept=100, col='black', size=1, linetype="dotted")+
  xlab('t = CRS local Effective date')+
  ylab('Volume exchanged to/from Bitcoin relative to t-1')#+
  #scale_color_manual(values = c("#ffc60a", "#82645E", "#E83151","#3BC62F","#3454D1"))

#tax haven, peropd 6, rescaled 6 months
ggplot(taxhaven, aes(t_crs, Value_res2, colour = Currency), size=2) +
  geom_point()+
  geom_line()+
  scale_y_continuous(limit = c(-0, 300)) +
  scale_x_continuous(limit = c(-12, 12)) +
  geom_vline(xintercept=-0.5, col='black', size=1, linetype="dotted")+
  geom_hline(yintercept=100, col='black', size=1, linetype="dotted")+
  xlab('t = CRS local Effective date')+
  ylab('Volume exchanged to/from Bitcoin relative to t-6')+
  scale_color_manual(values = c("#ffc60a", "#82645E", "#E83151","#3BC62F","#3454D1"))


#tax haven graph zoom in to 5 months before and after
ggplot(taxhaven, aes(t_crs, Value_res, colour = Currency)) +
  geom_point()+
  geom_line()+
  scale_y_continuous(limit = c(-10, 300)) +
  scale_x_continuous(limit = c(-12, 12)) +
  geom_vline(xintercept=-0.5, col='black', size=1, linetype="dotted")+
  geom_hline(yintercept=100, col='black', size=1, linetype="dotted")+
  xlab('t = CRS local Effective date')+
  ylab('Volume exchanged to/from Bitcoin relative to t-1')+
  scale_color_manual(values = c("#ffc60a", "#82645E", "#E83151","#3BC62F","#3454D1"))
  

#tax haven graph zoom in to 5 months before and after - log form
ggplot(taxhaven, aes(t_crs, log(Value_res), colour = Currency)) +
  geom_point()+
  geom_line()+
  scale_y_continuous(limit = c(3, 6)) +
  scale_x_continuous(limit = c(-3, 3)) +
  geom_vline(xintercept=-0.5, col='red', size=1)


#safehaven currencies
safehaven <- subset(df,Safehaven==1 & Year>= 2014 & Year<= 2018)
ggplot(safehaven, aes(t_crs, Value_res2, colour = Currency)) +
  geom_point()+
  geom_line()+
  scale_y_continuous(limit = c(20, 330)) +
  scale_x_continuous(limit = c(-6, 6)) +
  geom_vline(xintercept=-0.5, col='black', size=1, linetype="dotted")+
  geom_hline(yintercept=100, col='black', size=1, linetype="dotted")+
  xlab('t = CRS local Effective date')+
  ylab('Volume exchanged to/from Bitcoin relative to t-1')+
  scale_color_manual(values = c("#ffc60a","#82645E","#3BC62F","#3454D1"))


#major currencies(top 10 in use in world) treba upravit
majorcurrencies <- subset(df,(Currency=='AUD'|Currency=='CAD'|Currency=='CNY'|Currency=='NZD') & Year>= 2014 & Year<= 2018)
ggplot(majorcurrencies, aes(t_crs, Value_res2, colour = Currency)) +
  geom_point()+
  geom_line()+
  scale_y_continuous(limit = c(-10, 330)) +
  scale_x_continuous(limit = c(-6, 6)) +
  geom_vline(xintercept=-0.5, col='black', size=1, linetype="dotted")+
  geom_hline(yintercept=100, col='black', size=1, linetype="dotted")+
  xlab('t = CRS local Effective date')+
  ylab('Volume exchanged to/from Bitcoin relative to t-6')+
  scale_color_manual(values = c( "#82645E", "#E83151","#ffc60a","#3454D1"))


#EVENT STUDIES 
es<-eventstudy(firm.returns=taxhaven[Value_res])


#diff-in-diff
mean(df$Value_res[df$Taxhaven==1 & df$t_crs<0 & df$t_crs>= -6]) - mean(df$Value_res[df$Taxhaven==1 & df$t_crs>=0 & df$t_crs<6]) - 
  (mean(df$Value_res[df$Taxhaven==0  & df$t_crs<0 & df$t_crs>=-6 ]) - mean(df$Value_res[df$Taxhaven==0  & df$t_crs>=0 & df$t_crs<6]))  

smaller <- subset(df, Year>= 2014 & Year<= 2018)
summary(lm(Value_res ~ CRS_eff + CRS_eff*I(Taxhaven==1), data=smaller))
#CRS devlopment of volume traded is downward sloping, second they are pretty much diferent, third volume traded decrease over time
summary(lm(log(Value_res) ~ CRS_eff + CRS_eff*I(Taxhaven==1), data=smaller))


smaller2 <- subset(df, t_crs>= -6 & t_crs<= 6)
m1<-lm(log(Value_res2) ~ CRS_eff + CRS_eff*I(Taxhaven==1) + Intro_date, data=smaller2)
summary(lm(log(Value_res2) ~ CRS_eff + Intro_date + Intro_date*I(Taxhaven==1)+ CRS_eff*I(Taxhaven==1) , data=smaller2))


summary(m2<-lm(log(Value_res2) ~ CRS_eff + CRS_eff*I(Taxhaven==1)+ Currency, data=smaller2))

smaller3 <- subset(df, t_crs>= -2 & t_crs<= 2)
m3<-lm(log(Value_res) ~ CRS_eff + CRS_eff*I(Taxhaven==1), data=smaller3)
m4<-lm(log(Value_res) ~ CRS_eff + CRS_eff*I(Taxhaven==1)+ Currency, data=smaller3)


stargazer(m1, m3,
          dep.var.labels=c("Volume of Bitcoin traded rescaled to 6 months before CRS effective date",summary=F, no.space=T, rownames=F, single.row=T)
)

stargazer(m2, m4,
          dep.var.labels=c("Volume of Bitcoin traded rescaled to 6 months before CRS effective date",summary=F, no.space=T, rownames=F, single.row=T,font.size = "tiny")
)

taxhavenre <- subset(df, t_crs>= -6 & t_crs<= 6 )
summary(lm(log(Value_res) ~ CRS_eff + CRS_eff*I(Taxhaven==1), data=taxhavenre))


summary(lm(Value_res ~ CRS_eff + CRS_eff*I(Taxhaven==1 & Currency=='GBP'), data=smaller2))
summary(lm(log(Value_res) ~ CRS_eff + CRS_eff*I(Taxhaven==1 & Currency=='GBP'), data=smaller2))

#tax haven graph zoom in to 10 months before and after
nottaxhaven <- subset(df, Taxhaven==0 & Year>= 2014 & Year<= 2018 & Currency=='EUR' )
ggplot(nottaxhaven, aes(t_crs, Value_res2, color=Currency)) +
  geom_point()+
  geom_line()+
  scale_y_continuous(limit = c(-10, 300)) +
  scale_x_continuous(limit = c(-10, 10)) +
  geom_vline(xintercept=-0.5, col='black', size=1, linetype="dotted")+
  geom_hline(yintercept=100, col='black', size=1, linetype="dotted")+
  xlab('t = CRS local Effective date')+
  ylab('Volume exchanged to/from Bitcoin relative to t-1')#+
#scale_color_manual(values = c("#ffc60a", "#82645E", "#E83151","#3BC62F","#3454D1"))
 
#google  trends
map=read_excel("C:/Users/stank/Stanuska/IES/Bachelor Thesis/DATA/map.xlsx")
all=read_excel("C:/Users/stank/Stanuska/IES/Bachelor Thesis/DATA/time.xlsx")

ggplot(map, aes(Krajina, CRS)) +
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 45, size=15, hjust = 1))+
  #geom_hline(yintercept=100, col='black', size=1, linetype="dotted")+
  ylab('Interest in topic of Common Reportin Standard through Google') +
  xlab('Countries') 

ggplot(all, aes(time, CRS)) +
  geom_line()+
    #geom_hline(yintercept=100, col='black', size=1, linetype="dotted")+
  ylab('Interest in topic of Common Reportin Standard through Google over time') +
  xlab('Time')        

