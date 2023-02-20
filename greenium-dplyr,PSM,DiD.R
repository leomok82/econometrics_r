#Bond Data tidying
rm(list=ls())
library(dplyr) 
library(ggplot2)
library(readxl) #read excel files
library(stringr) #strings
library(stargazer) #nice tables
library(MatchIt) #NN matching
library(cobalt) #balance tables
library(estimatr) #robust lm
library(gt) #tables
library(tidyr) #reshape data
library(Synth)
library(plm)


setwd("/Users/leo/Desktop/Year 3/RAE/bonddata")
d1 <- read_excel("2007-2013.xlsx",col_names=TRUE)
d2 <- read_excel("2014-2015.xlsx",col_names=TRUE)
d3 <- read_excel("2016.xlsx",col_names=TRUE)
d4 <- read_excel("2017.xlsx",col_names=TRUE)
d5 <- read_excel("2018.xlsx",col_names=TRUE)
d6 <- read_excel("2019q1q2.xlsx",col_names=TRUE)
d7 <- read_excel("2019q3q4.xlsx",col_names=TRUE)
d8 <- read_excel("2020q1q2.xlsx",col_names=TRUE)
d9 <- read_excel("2020q3.xlsx",col_names=TRUE)
d10 <- read_excel("2020q4.xlsx",col_names=TRUE)
d11 <- read_excel("2021q1.xlsx",col_names=TRUE)
d12 <- read_excel("2021q2.xlsx",col_names=TRUE)
d13 <- read_excel("2021q3.xlsx",col_names=TRUE)
d14 <- read_excel("2021q4.xlsx",col_names=TRUE)
d15 <- read_excel("2022janfeb.xlsx",col_names=TRUE)
d16 <- read_excel("2022marapr.xlsx",col_names=TRUE)
d17 <- read_excel("2022mayjun.xlsx",col_names=TRUE)
d18 <- read_excel("2022jul.xlsx",col_names=TRUE)
d19 <- read_excel("2022aug.xlsx",col_names=TRUE)
d20 <- read_excel("2022sep.xlsx",col_names=TRUE)
d21 <- read_excel("2022oct.xlsx",col_names=TRUE)
d22 <- read_excel("2022nov.xlsx",col_names=TRUE)
d23 <- read_excel("2022dec.xlsx",col_names=TRUE)


#Some data types are incorrect, this changes it
d1[,12]<-lapply(d1[,12], as.character)
d4[,12]<-lapply(d4[,12], as.character)
d8[,12]<-lapply(d8[,12], as.character)
d9[,12]<-lapply(d9[,12], as.character)
d16[,12]<-lapply(d16[,12], as.character)

d3[,15]<-lapply(d3[,15], as.character)
d4[,15]<-lapply(d4[,15], as.character)
d6[,15]<-lapply(d6[,15], as.character)
d8[,15]<-lapply(d8[,15], as.character)
d10[,15]<-lapply(d10[,15], as.character)

d12[,3]<-lapply(d12[,3], as.numeric)

#d3,d4,d6,d8,d10
df<-bind_rows(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15
              ,d16,d17,d18,d19,d20,d21,d22,d23)
rm(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15
          ,d16,d17,d18,d19,d20,d21,d22,d23)
names(df)<-str_replace_all(names(df), c(" " = "." , "," = "" ))


colnames(df)[5]<-"Date"
colnames(df)[7]<-"Currency"
colnames(df)[8]<-"Country"
colnames(df)[12]<-"Amount.Issued"
colnames(df)[13]<-"YTM"
colnames(df)[14]<-"Spread"
colnames(df)[16]<-"Green"

#convert data types
df[,1]<-lapply(df[,1], as.factor) #issuer
df[,3]<-lapply(df[,3], as.numeric) #Coupon
df[,4]<-lapply(df[,4], as.Date) #Maturity
df[,5]<-lapply(df[,5], as.Date) #Issue Date
df[,7]<-lapply(df[,7], as.factor) #Currency
df[,8]<-lapply(df[,8], as.factor) #Country
df[,9]<-lapply(df[,9], as.factor) #Issuer type
df[,11]<-lapply(df[,11], as.factor) #Coupon Type
df[,12]<-lapply(df[,12], as.numeric) #amount Issued
df[,15]<-lapply(df[,15], as.numeric) #yield spread to maturity
df[,17]<-lapply(df[,17], as.numeric) #Issue price
df[,18]<-lapply(df[,18], as.factor) #sector
df[,19]<-lapply(df[,19], as.factor) #TRBC sector
df[,20]<-lapply(df[,20], as.factor) #seniority


#convert green to numeric
df$Green<-ifelse(df$Green=="Yes",1,df$Green)
df$Green<-ifelse(df$Green=="No",0,df$Green)

#df<-df %>% mutate_at(vars(Green), ~replace(., is.na(.), 0))
df[,16]<-lapply(df[,16], as.numeric) #green

#create category for year of issuance
df[,21]<-as.factor(format(df$Date,"%Y"))
colnames(df)[21]<-"Year"

#create category for time to maturity
df$TTM<-as.numeric(difftime(df$Maturity,df$Date,  units="days"))
df$logamount<-log(df$Amount.Issued,base=exp(1))

# count missing values
colname<-rep(0,21)
miss<-rep(0,21)
for (i in  1:21){
  print(colnames(df)[i])
  print(sum(is.na(df[,i])))
  colname[i]<-colnames(df)[i]
  miss[i]<-sum(is.na(df[,i]))
}
missing<-cbind(colname,miss)
missing

#missing is table of missing values

#filter out all entries with essential missing values
df <- df %>%
  filter(Amount.Issued!=0)
df<- df %>%
  filter_at(vars(Green, Coupon, Ticker, Maturity,Country,
                 TRBC.Sector), all_vars(!is.na(.)))

# 3 weirdly large TTM (> 1000 years)
df <- df %>%
  filter(df$TTM<50000)
#count greens
sum(df$Green==1)
# one occurance of 100000 issue price
df <- df%>%
  filter(df$Issue.Price<10000)
#150816 after removing missing obs

# 3359 green (~2% green)

#Drop levels that are unused after filtering
df <-droplevels(df)

#summary of categorical variables
vars <- c("Currency", "Country", "Issuer.Type", "Sector","Issuer","Year"
          , "Seniority")
categories<-rep(0,6)
categories[1]<-nlevels(df$Currency)
categories[2]<-nlevels(df$Country)
categories[3] <- nlevels(df$Issuer.Type)
categories[4] <- nlevels(df$Sector)
categories[5]<-nlevels(df$Issuer)
categories[6]<-nlevels(df$Year)
categories[7]<-nlevels(df$Seniority)


cats<-as.data.frame(cbind(vars,categories))
cats
gt(cats) %>% tab_header(title =md("Categorical Variables"))


#103 currencies, 123 countries, 52 sectors, 5 issuers

#summary
means <- df %>%
  select(Green, Coupon, YTM,Issue.Price,logamount, TTM,Spread) %>%
  summarize_each(funs(mean,min,max,sd))%>%
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var=var,mean=mean,sd=sd,min=min,max=max)
means<-means %>%mutate_at(vars(mean,sd,min,max),funs(round(.,3)))

gt(means)%>%tab_header(
  title = md("Summary Statistics")
)


summ <- df %>%
  group_by(Green) %>%
  select(Coupon, YTM,Issue.Price,logamount,TTM,Spread) %>%
  summarize_each(funs(mean,sd,min,max))
summ

green <-summ%>% filter(Green==1) %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var=var,mean=mean,sd=sd,min=min,max=max) 
green <- green[-2,]

nongreen <-summ%>% filter(Green==0) %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var=var,mean=mean,sd=sd,min=min,max=max) 
nongreen <-nongreen[-2,]

green<-green %>%mutate_at(vars(mean,sd,min,max),funs(round(.,3)))
nongreen<-nongreen%>%mutate_at(vars(mean,sd,min, max),funs(round(.,3)))

gt(green) %>%tab_header(
  title = md("Summary Statistics for Green bonds")
)
gt(nongreen) %>%tab_header(
  title = md("Summary Statistics for Non-Green bonds")
)
rm(summ)

#overlap check for psm
countryoverlap <- df %>%
  group_by(Country) %>%
  count(Green)
colnames(countryoverlap)[1]<-"Var"

senioroverlap<- df%>%
  group_by(Seniority) %>%
  count(Green)
colnames(senioroverlap)[1]<-"Var"

sectoroverlap<- df%>%
  group_by(Sector) %>%
  count(Green)
colnames(sectoroverlap)[1]<-"Var"
yearoverlap<-df%>%
  group_by(Year)%>%
  count(Green)
colnames(yearoverlap)[1]<-"Var"
overlap<-bind_rows(countryoverlap,senioroverlap,sectoroverlap,yearoverlap)
rm(countryoverlap,senioroverlap,sectoroverlap,yearoverlap)
#missing overlap - 2007-2011,Tobacco,Restaurants,Oilfield Machinery and Services
#, Independent Finance, Health Care Supply,Gas Utility - Pipelines, 
#Gaming, Cable/Media, Aerospace

#Probit
#log issue with amount in -infty if it is originally 0 - need to drop 0 observations

#probit // every covariate except liquidity
#Year, Issuer saved for later
proscore <- glm(Green ~  TTM+Currency+Spread
                +Sector+Seniority+logamount ,
                family = binomial(link = "probit"), data=df)
#log issue with yield in -infty if it is originally 0 - need to drop 0 observations
summary(proscore)

#currency and sector are not significant, logamount, TTM, are
#try omit issuertype because of multicollinearity with sector
df$probitfit<-as.numeric(fitted(proscore,type = "response"))
#Marginal effects - takes 10 mins to run
#ME<-margins(proscore,at=list(Coupon=means1$mean[1],Issue.Price=means1$mean[3],
#                             logamount=means1$mean[4],TTM=means1$mean[5],
#                             YTM=means1$mean[6]))

#----------

#check common support is the same (enough overlapping units) -
dftemp <- subset(df,Green==0)
  range(dftemp$probitfit)
dftemp2<-subset(df,Green==1)
range(dftemp2$probitfit)
rm(dftemp,dftemp2)
#-------Match (should include Issuer in exact)
match <-matchit(Green ~TTM+Currency+Spread
                      +Sector+Seniority+logamount, data=df,
                      exact=~Year+Country,method="nearest",
    distance = df$probitfit,replace=FALSE,ratio=1:1) 

matchdata<-match.data(match)

#nearest matching without exact gives -0.12 green bond premium
#ols1 <- lm_robust(YTM ~ Green,data=matchdata,weight="weights")
#Match3 is a CEM without exact

#match3 <-matchit(Green ~ TTM+Currency+Spread
#                 +Sector+Seniority+logamount, data=df,
#                method="cem",exact=~Issuer+Year+Country,distance=df$probitfit,
#                replace=FALSE,k2k=TRUE,ratio=1:1)
#matchdata3<-match.data(match3,weights="weights2",subclass="subclass2") 
#check balance of p scores
plot(match, type = "jitter", interactive = FALSE)
#1625 matches

#subclass 2 is the real subclass
#regress like loffler

#mean differences
treated <- subset(matchdata, Green == 1)
control <- subset(matchdata, Green == 0)

diff <-  control$YTM -treated$YTM 
mean(diff,na.rm=TRUE)


#diff in diff data
temp1 <- matchdata%>%
  filter(Green==1) %>% 
  arrange(subclass)
temp2 <- matchdata %>%
  filter(Green==0) %>% 
  arrange(subclass)
df3 <- as.data.frame(temp1$YTM) 
df3$premium <- as.numeric(temp2$YTM-temp1$YTM)
df3$Year<-as.numeric(as.character(temp1$Year))
#df3$Date<-as.Date(df3$Year,format="%Y")
df3$name<-temp1$Country
df3$Issuer<-temp1$Issuer
df3$ID<-temp1$subclass
rm(temp1,temp2)
df3<-df3[,-1]
df3 <-droplevels(df3)
df3 <- df3 %>% arrange(name)
#create treatment group
df3$treat<-0
df3$treat[df3$name %in% treatc]<-1

#diff in diff with europe 2018 policy
# UK brexit, but they likely remain treated so we can assume
#treatment countries 

treatc<-c("Austria","Belgium","Denmark","Finland","France","Germany"
,"Greece","Hungary","Ireland","Italy","Netherlands","Poland","Portugal"
,"Romania","Slovenia","Spain","Sweden","United Kingdom","Eurobond")
dates<-c(rep(2018,19))


#OLS - with country fixed effects - gdp etc controlled for
ols1 <-lm_robust(premium ~ relevel(factor(Year),ref="2016")*treat, 
                 fixed_effects=~name, data=df3,se_type="stata")
summary(ols1)
coefs <- c(-1.049,-0.167,0,-1.23,-1.376,-2.497,-1.762,-2.478,-0.798)
se <-  c(1.841,7.144,0,3.345,3.228,2.118,0.758,0.416,1.51)
event<-c(-4,-3,-2,-1,0,1,2,3,4)
#graph!
plot1<-cbind.data.frame(coefs,se,event)

ggplot(data=plot1, mapping=aes(y=coefs, x=event)) +
  geom_line(linetype="dashed") +
  geom_point() + 
  geom_errorbar(aes(ymin=(coefs-1.96*se), ymax=(coefs+1.96*se)), width=0.2) +
  theme_bw()+
  ggtitle("DiD for green bond premium")+xlab("Event time")+ylab("Coefficient")



#method 2 - changing to panel data

#US TW Swis Sweden New Zealand Japan Hong Kong (2017) France Eurobond China Australia
nomiss<-c("United States", "Taiwan","Sweden","Switzerland",
          "New Zealand","Japan","Hong Kong","France",
          "Eurobond","China","Australia")
df3$nomiss<-0
df3$nomiss[df3$name %in% nomiss] <-1
df5<-df3 %>%
  filter(nomiss==1)

#group by country to have a panel data
df5<-aggregate(premium ~ name + Year, data = df5,  mean) 
df5 <- df5 %>% arrange(name)
df5$g<-0
df5$g[(df5$name %in% treatc)&(df5$Year==2018)]<-1
#only these countries have enough obs
att1 <- att_gt(yname = "premium",
               tname = "Year",
               gname = "g",
               data = df5,
               panel=FALSE,
               base_period="2017"
)




#event study
dates<-c(2008,2003,2016,2009,2004,2015,2008,2016,2016,2001,2016,2006,2015,2016
       ,2015,2012,2016,2016,2007,2016,2013,2009,2015,2011,2016,2010,2016,2016
         ,2017,2010,2012,2016,2019,2014,2013)
countries<-c("Argentina","Australia","Austria","Belgium","Canada","Chile",
             "China (Mainland)","Denmark","Finland","France","Germany","Greece",
             "Hong Kong","Hungary","India","Indonesia","Ireland","Italy",
             "Malaysia","Netherlands","Norway","Pakistan","Peru","Philippines",
             "Poland","Portugal","Romania","Singapore","Slovenia","South Africa",
             "Spain","Sweden","Taiwan","Turkey","United Kingdom")
list<- as.data.frame(cbind(countries,dates))


colnames(events)[1]<-"name"
events$dates<-as.Date(as.character(events$dates),format="%Y")
colnames(events)[2]<-"when"

df4<-xts(df3,order.by=df3$Date)


#treatment dates


  
  