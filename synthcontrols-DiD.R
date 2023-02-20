library(AER)
library(MASS)
library(reshape2)
library(ggplot2)
setwd("/Users/leo/Desktop/Year 3/Microeconometrics/ass 1")
#Section C - simulation 
rm(list=ls())
set.seed(2) #Seed - deterministically set random numbers generator
M=1000 #number of experiments
n=1000 #sample size

#Storage
b1 <- rep(0,M)
b2 <- rep(0,M)
b3 <- rep(0,M)
b4 <- rep(0,M)
b5 <- rep(0,M)
#coefficients
g0<-1.2
g1<-0.015
g2<- -0.02
g3<- -0.01



#DGP
for (m in 1:M){
#error
 error_m <-rnorm(n,0,0.55^2)
#age
age_m <- floor(runif(n,20,66))
#functions of age
 fage1_m=0.5-0.25 * (log(age_m-19,exp(1))/log(46,exp(1)))
 #edited from 0.05 to 0.5
fage2_m=0.25+0.5 *(log(age_m-19,exp(1))/log(46,exp(1)))
 #corrected to 0.2
 mage_m<- ifelse(age_m>43,0.02+0.06,0.02)
female_m<-rbinom(n,1,fage1_m)
 #mu_ATE is approx 0.05
 tau_m<- rnorm(n,mage_m,0.01)
 w_m<-rbinom(n,1,fage2_m)
 y0_m<-g0+g1*age_m+g2*female_m+g3*age_m*female_m+error_m
 y1_m<-y0_m+w_m*tau_m
 data<- data.frame(y1_m,age_m,error_m,tau_m,female_m,w_m)
 
 #MODEL 1
 ols1_m <- lm(y1_m ~ w_m )
 #MODEL 2
 ols2_m <- lm(y1_m ~ w_m+female_m+age_m+female_m*age_m)
#MODEL 3
 ols3_m <- lm(y1_m~ w_m+female_m+factor(age_m))
#MODEL 4 
 logit_m<-glm(w_m ~ age_m,family = binomial(link = "logit"))

 ptreat_m<-predict(logit_m,type='response')

 weight1_m<- 1/((ptreat_m^w_m)*((1-ptreat_m)^(1-w_m)))
 data<-cbind(data,weight1_m)
 ols4_m <- lm(y1_m ~ w_m ,weights=weight1_m)
 #MODEL 5
 logit2_m<-glm(w_m ~ factor(age_m),family = binomial(link = "logit"))
 ptreat2_m<-predict(logit2_m,type='response')
 weight2_m<- 1/((ptreat2_m^w_m)*((1-ptreat2_m)^(1-w_m)))
 ols5_m<- lm(y1_m ~ w_m,weights=weight2_m)
 #save coefficients

 b1[m]<-ols1_m[["coefficients"]][["w_m"]]
 b2[m]<-ols2_m[["coefficients"]][["w_m"]]
 b3[m]<-ols3_m[["coefficients"]][["w_m"]]
 b4[m]<-ols4_m[["coefficients"]][["w_m"]]
 b5[m]<-ols5_m[["coefficients"]][["w_m"]]
  }
betas<-data.frame(b1,b2,b3,b4,b5)
#transforming data 
beta<-melt(betas)
summary(betas)

#plot of the models
ggplot(beta, aes(x=value,color=variable)) +geom_density() +xlab("Treatment effect estimates")+ 
  labs(title="Distributions of betas in each model") 

#Section D - regression
#formatting data
rm(list=ls())
library(haven)
library(plm)
library(did)
library(plyr)
library(dplyr)
library(magrittr)
library(estimatr)
library(modelsummary)
setwd("/Users/leo/Desktop/Year 3/Microeconometrics/ass 1")
df <- read_dta(file="lfs_2010_2019_ages1564_20per.dta")
df$agegrp<-as.integer(df$agegrp)
df$province<-as.integer(df$province)

# we want age groups 1,2, regions 5(quebec) 6(ontario) 7(manitoba)
df1<-df[(df$agegrp==1|df$agegrp==2)&(df$province==5|df$province==6|df$province==7),]
df1$day<-1
df1$date<-as.Date(with(df1,paste(year,month,day,sep="-")),"%Y-%m-%d")
df1<-df1[(complete.cases(df1$empstat)),]
emp<-as.integer(df1$empstat)
emp<-ifelse((emp==1|emp==2),1,0)
df1$emp<-emp

means <- df1 %>% 
  group_by(year, province) %>%
  summarize(empmean = mean(emp,na.rm=TRUE))
#yearly
ggplot(means, aes(y=empmean,x=year,group=province,color=factor(province)))+geom_line()+
  ggtitle("Yearly Youth Employment Rate in Canada") +xlab("Year")+ylab("Employment rate") +
  scale_color_hue(labels=c("5" = "Quebec", "6"="Ontario", "7"="Manitoba"))+
  scale_x_continuous(n.breaks=10)

mmeans<-df1%>%
  group_by(date,province) %>%
  summarize(empmean=mean(emp,na.rm=TRUE))

#monthly
ggplot(mmeans, aes(y=empmean,x=date,group=province,color=factor(province)))+geom_line(size=0.3)+
  ggtitle("Monthly Youth Employment Rate in Canada") +xlab("Year")+ylab("Employment rate") +
  scale_color_hue(labels=c("5" = "Quebec", "6"="Ontario", "7"="Manitoba"))+
  scale_x_date(date_labels = "%m-%Y",date_breaks="2 year")
#limits = as.Date(c("2011-01-01", "2014-01-01"))

#Fixed effects - Task 3
#treated data using Quebec
df1$time =ifelse(df1$year==2017,0,NA)
df1$time = ifelse(df1$year==2018,1,df1$time)
df1$treat=ifelse(df1$province==6,1,NA)
df1$treat=ifelse(df1$province==5,0,df1$treat)
df2 <- df1[((df1$treat==1|df1$treat==0)&(df1$time==0|df1$time==1)),]
df2<-df2[complete.cases(df2$time),]
df2<-df2[complete.cases(df2$treat),]
#treated data using Manitoba
df1$treat2=ifelse(df1$province==6,1,NA)
df1$treat2=ifelse(df1$province==7,0,df1$treat)
df3 <- df1[((df1$treat==1|df1$treat==0)&(df1$time==0|df1$time==1)),]
df3<-df3[complete.cases(df3$time),]
df3<-df3[complete.cases(df3$treat),]

#fe1 - DiD not significant
fe1<-lm(emp~treat+time+treat*time,df2)
summary(fe1)
#fe2 - DiD still not significant
fe2<-lm_robust(emp~treat*time,fixed_effects=~month, data=df2,se_type="stata")
summary(fe2)
#fe3 - covariates -work hours (ATOTHRS), gender (sex),
#part time (FTPTMAIN) , industry (NAICS_18), union (UNION)
#age group(agegrp), earnings (HRLYEARN), education (EDUGRP)
#We would want wages, but there are no good measures in this dataset. - 12717 missing observations
#no significance when included, and may give multicollinearity
#union also decreases degrees of freedom significantly, and no effect were found when included
#hence education, sex, agegrp, month, edugrp
fe3<-lm_robust(emp~factor(agegrp)+factor(sex)+factor(edugrp)+treat*time,
               fixed_effects=~month, data=df2,se_type="stata")
summary(fe3)

#stargazer(fe1,fe2,fe3, type = "html", out="regression.html",title = "FE models")
modelsummary(list(fe1, fe2,fe3),stars=TRUE,title="FE models",output="regression.html")

#Task 4 - we will keep using Quebec
#generate data for dynamic specification
df4 <- df1[((df1$treat==1|df1$treat==0)),]
df4<-select(df4,c(year,month,emp,treat,province))
df4<-df4[(complete.cases(df4)),]
df4<-df4[(df4$year>=2014),]

#att using lm
ols1 <-lm_robust(emp ~ relevel(factor(year),ref="2017")*treat, 
                fixed_effects=~month, data=df4,se_type="stata")
summary(ols1)
modelsummary(ols1,stars=TRUE,title="Dynamic DiD model",output="regression2.html")

#graph
coef<-c(-0.01,-0.035,-0.022,0,-0.046,-0.061)
se<-c(0.012,0.012,0.012,0,0.012,0.012)
event<-c(-4,-3,-2,-1,0,1)
plot1<-data.frame(coef,se,event)
ggplot(data=plot1, mapping=aes(y=coef, x=event)) +
  geom_line(linetype="dashed") +
  geom_point() + 
  geom_errorbar(aes(ymin=(coef-1.96*se), ymax=(coef+1.96*se)), width=0.2) +
  ylim(c(-0.09,0.05)) +
  theme_bw()+
  ggtitle("DiD for Youth employment in Ontario and Quebec")+xlab("Event time")+ylab("Coefficient")
#method 2 - gives same results

df4$g<-ifelse(df4$treat==1,2018,0)
att1 <- att_gt(yname = "emp",
                        tname = "year",
                        gname = "g",
                        xformla = ~month,
                        data = df4,
                        panel=FALSE,
                    base_period="2017"
)
summary(att1)
#pretrend test - P= 0.02 do not reject
#graph
ggdid(att1,title="DiD for employment in Ontario and Quebec")

#task  5
df5 <- df1[((df1$treat==1|df1$treat==0)),]
df5<-select(df5,c(year,month,emp,treat,date))
df5<-df5[(complete.cases(df5$treat)),]
df5<-df5[(df5$year>=2014),]
ols2 <-lm(emp ~ relevel(factor(date),ref="2017-05-01")*treat, 
          data=df5)
summary(ols2)

#task 6
#test using did package
att2 <- att_gt(yname = "emp",
               tname = "year",
               gname = "g",
               xformla = ~month,
               data = df4,
               panel=FALSE,
              anticipation=1
)
summary(att2)

#another test
ols3 <-lm_robust(emp ~ relevel(factor(year),ref="2016")*treat, 
                 fixed_effects=~month, data=df4,se_type="stata")
summary(ols3)

modelsummary(ols3,stars=TRUE,title="Dynamic DiD model base 2016",output="regression3.html")

#task 7 - checking data for rest of sample
df6<-df[(df$agegrp!=1&df$agegrp!=2)&(df$province==5|df$province==6|df$province==7),]
emp<-as.integer(df6$empstat)
emp<-ifelse((emp==1|emp==2),1,0)
df6$emp<-emp
df6$treat=ifelse(df6$province==6,1,NA)
df6$treat=ifelse(df6$province==5,0,df6$treat)
df6 <- df6[((df6$treat==1|df6$treat==0)),]

df6<-select(df6,c(year,month,emp,treat,date))
df6<-df6[(complete.cases(df6)),]
df6<-df6[(df6$year>=2014),]

ols4 <-lm_robust(emp ~ relevel(factor(year),ref="2017")*treat, 
                 fixed_effects=~month, data=df6,se_type="stata")
summary(ols4)
modelsummary(ols4,stars=TRUE,title="Dynamic DiD model for rest of sample",output="regression4.html")

#Section E
library(Synth)
library(plyr)
#setwd("/Users/leo/Desktop/Year 3/Microeconometrics/ass 1")
#df <- read_dta(file="lfs_2010_2019_ages1564_20per.dta")
#df$agegrp<-as.integer(df$agegrp)
#df$province<-as.integer(df$province)

# we want age groups 1,2, regions 5(quebec) 6(ontario) 7(manitoba)
df1<-df[(df$agegrp==1|df$agegrp==2)&(df$province==5|df$province==6|df$province==7),]
#data preprocess
sdf<-select(df,c(agegrp,sex,empstat,edugrp,province,efamtype,year,wgt))
sdf<-sdf[(df$agegrp==1|df$agegrp==2),]
emp<-as.integer(sdf$empstat)
emp<-ifelse((emp==1|emp==2),1,0)
sdf$emp<-emp
sdf$male<-ifelse(sdf$sex==1,1,0)
sdf$fam<-case_when(
  sdf$efamtype==1~"1",
  sdf$efamtype>=2&sdf$efamtype<5 ~"2",
  sdf$efamtype>=14&sdf$efamtype<16 ~"3",
  sdf$efamtype>=5&sdf$efamtype<11 ~"4",
  sdf$efamtype>=11&sdf$efamtype<14|sdf$efamtype==15 ~"5",
  sdf$efamtype==18 ~"6"
)
sdf$fam1 = ifelse(sdf$fam == 1, 1, 0)
sdf$fam2 = ifelse(sdf$fam == 2, 1, 0)
sdf$fam3 = ifelse(sdf$fam == 3, 1, 0)
sdf$fam4 = ifelse(sdf$fam == 4, 1, 0)
sdf$fam5 = ifelse(sdf$fam == 5, 1, 0)
sdf$fam6 = ifelse(sdf$fam == 6, 1, 0)
sdf$edu1 = ifelse(sdf$edugrp == 1, 1, 0)
sdf$edu2 = ifelse(sdf$edugrp == 2, 1, 0)
sdf$edu3 = ifelse(sdf$edugrp == 3, 1, 0)
sdf<-sdf[complete.cases(sdf),]
sdf <- as.data.frame(sdf)
#generate year province means
allmeans<-ddply(sdf, .(year,province), summarize, empmean = weighted.mean(emp, wgt),
      e1mean = weighted.mean(edu1, wgt), e2mean = weighted.mean(edu2, wgt),
      e3mean = weighted.mean(edu3, wgt), sexmean = weighted.mean(male, wgt),
      f1mean = weighted.mean(fam1, wgt), f2mean = weighted.mean(fam2, wgt),
      f3mean = weighted.mean(fam3, wgt), f4mean = weighted.mean(fam4, wgt),
      f5mean = weighted.mean(fam5, wgt), f6mean = weighted.mean(fam6, wgt))

#model 1
dfprep <- dataprep(allmeans, 
                    predictors = "empmean", 
                    dependent = "empmean", 
                    unit.variable = "province",
                    time.variable = "year",
                    treatment.identifier = 6,
                    controls.identifier = c(1:5,7:10),
                    time.predictors.prior = 2010:2017,
                    time.optimize.ssr = 2010:2017,
                    time.plot = 2010:2019)
rsynth1 <- synth(dfprep)
path.plot(rsynth1, dfprep, Ylim = c(0.46, 0.65), Ylab = "Employment", tr.intake = 2018,
                Main="Model 1")

#model 2
dfprep2 <-dataprep(foo=allmeans, 
                  predictors = "empmean", 
                  dependent = "empmean", 
                  unit.variable = "province",
                  time.variable = "year",
                  treatment.identifier = 6,
                  controls.identifier = c(1:5,7:10),
                  time.predictors.prior = c(2014:2016),
                  time.optimize.ssr = c(2010:2017),
                  time.plot = c(2010:2019))

rsynth2 <-synth(dfprep2)
path.plot(synth.res=rsynth2, dataprep.res=dfprep2, Ylim = c(0.5, 0.65), Ylab = "Employment", tr.intake = 2018,
                  Main="Model 2")
#model 3 - emp excluded from predictor as it gives same results
dfprep3 <-dataprep(allmeans, 
                  predictors = c("sexmean","e2mean","e3mean","f2mean",
                                 "f3mean","f4mean","f5mean","f6mean"),
                  dependent = "empmean", 
                  unit.variable = "province",
                  time.variable = "year",
                  treatment.identifier = 6,
                  controls.identifier = c(1:5,7:10),
                  time.predictors.prior = c(2010:2017),
                  time.optimize.ssr = c(2010:2017),
                  time.plot = c(2010:2019))
rsynth3 <-synth(dfprep3)

path.plot(synth.res=rsynth3, dataprep.res =dfprep3, Ylim = c(0.5, 0.65), Ylab = "Employment", tr.intake = 2018,
          Main="Model 3")

#model 4
dfprep4 <- dataprep(foo=allmeans, 
                   predictors =  c("sexmean","e2mean","e3mean","f2mean",
                                  "f3mean","f4mean","f5mean","f6mean"),
                   dependent = "empmean", 
                   unit.variable = "province",
                   time.variable = "year",
                   treatment.identifier = 6,
                   controls.identifier = c(1:5,7:10),
                   time.predictors.prior = c(2014:2016),
                   time.optimize.ssr = c(2010:2017),
                   time.plot = c(2010:2019))
rsynth4 <- synth(dfprep4)
path.plot(synth.res=rsynth4, dataprep.res =dfprep4, Ylim = c(0.5, 0.65), Ylab = "Employment", tr.intake = 2018,
          Main="Model 4")
