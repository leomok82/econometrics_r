setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(haven)
library(Synth)
library(tidyverse)

data = read_dta("lfs_2010_2019_ages1564_20per.dta") %>% filter(agegrp <= 2)
data$employed = ifelse(data$empstat <= 2, 1, 0)
data$male = ifelse(data$sex == 1, 1, 0)
data$edu1 = ifelse(data$edugrp == 1, 1, 0)
data$edu2 = ifelse(data$edugrp == 2, 1, 0)
data$edu3 = ifelse(data$edugrp == 3, 1, 0)
efamtype2fam = function(efam) {
  if (efam == 1) return(1)
  if (efam %in% 2:4) return(2)
  if (efam %in% c(14, 16, 17)) return(3)
  if (efam %in% 5:10) return(4)
  if (efam %in% c(11, 12, 13, 15)) return(5)
  if (efam == 18) return(6)
}
data$fam = sapply(as.integer(data$efamtype), efamtype2fam)
table(data$efamtype, data$fam)
data$fam1 = ifelse(data$fam == 1, 1, 0)
data$fam2 = ifelse(data$fam == 2, 1, 0)
data$fam3 = ifelse(data$fam == 3, 1, 0)
data$fam4 = ifelse(data$fam == 4, 1, 0)
data$fam5 = ifelse(data$fam == 5, 1, 0)
data$fam6 = ifelse(data$fam == 6, 1, 0)
dataProvinceYear = data.frame(expand.grid(2010:2019, 1:10), matrix(0, nrow = 100, ncol = 11))
colnames(dataProvinceYear) = c("year", "province", "employed", "male", "edu1", "edu2", "edu3", "fam1", "fam2", "fam3", "fam4", "fam5", "fam6")
row = 1
for (prov in 1:10) {
  for (y in 2010:2019) {
    df = data %>% filter(year == y) %>% filter(province == prov)
    dataProvinceYear[row, "employed"] = weighted.mean(df$employed, df$wgt)
    dataProvinceYear[row, "male"] = weighted.mean(df$male, df$wgt)
    dataProvinceYear[row, "edu1"] = weighted.mean(df$edu1, df$wgt)
    dataProvinceYear[row, "edu2"] = weighted.mean(df$edu2, df$wgt)
    dataProvinceYear[row, "edu3"] = weighted.mean(df$edu3, df$wgt)
    dataProvinceYear[row, "fam1"] = weighted.mean(df$fam1, df$wgt)
    dataProvinceYear[row, "fam2"] = weighted.mean(df$fam2, df$wgt)
    dataProvinceYear[row, "fam3"] = weighted.mean(df$fam3, df$wgt)
    dataProvinceYear[row, "fam4"] = weighted.mean(df$fam4, df$wgt)
    dataProvinceYear[row, "fam5"] = weighted.mean(df$fam5, df$wgt)
    dataProvinceYear[row, "fam6"] = weighted.mean(df$fam6, df$wgt)
    row = row + 1
  }
}

dataPrep = dataprep(dataProvinceYear, 
                    predictors = "employed", 
                    dependent = "employed", 
                    unit.variable = "province",
                    time.variable = "year",
                    treatment.identifier = 6,
                    controls.identifier = c(1:5,7:10),
                    time.predictors.prior = 2010:2017,
                    time.optimize.ssr = 2010:2017,
                    time.plot = 2010:2019)
synthResult = synth(dataPrep)
path.plot(synthResult, dataPrep, Ylim = c(0.46, 0.57), Ylab = "employed", tr.intake = 2018)
# doesnt look like parallel trends... in theory we try to perfectly copy employment from ONT but this doenst seem to work

dataPrep2 = dataprep(dataProvinceYear, 
                    predictors = "employed", 
                    dependent = "employed", 
                    unit.variable = "province",
                    time.variable = "year",
                    treatment.identifier = 6,
                    controls.identifier = c(1:5,7:10),
                    time.predictors.prior = 2010:2017,
                    time.optimize.ssr = 2014:2016,
                    time.plot = 2010:2019)
synthResult2 = synth(dataPrep2)
path.plot(synthResult2, dataPrep2, Ylim = c(0.46, 0.57), Ylab = "employed", tr.intake = 2018)
# no change in plot. what has happend in 2010, 2011, 2012, 2013 and 2017?

dataPrep3 = dataprep(dataProvinceYear, 
                    predictors = c("male", "edu2", "edu3", "fam2", "fam3", "fam4", "fam5", "fam6"), 
                    dependent = "employed", 
                    unit.variable = "province",
                    time.variable = "year",
                    treatment.identifier = 6,
                    controls.identifier = c(1:5,7:10),
                    time.predictors.prior = 2010:2017,
                    time.optimize.ssr = 2010:2017,
                    time.plot = 2010:2019)
synthResult3 = synth(dataPrep3)
path.plot(synthResult3, dataPrep3, Ylim = c(0.49, 0.57), Ylab = "employed", tr.intake = 2018)

dataPrep4 = dataprep(dataProvinceYear, 
                     predictors = c("male", "edu2", "edu3", "fam2", "fam3", "fam4", "fam5", "fam6"), 
                     dependent = "employed", 
                     unit.variable = "province",
                     time.variable = "year",
                     treatment.identifier = 6,
                     controls.identifier = c(1:5,7:10),
                     time.predictors.prior = 2010:2017,
                     time.optimize.ssr = 2014:2016,
                     time.plot = 2010:2019)
synthResult4 = synth(dataPrep4)
path.plot(synthResult4, dataPrep4, Ylim = c(0.49, 0.57), Ylab = "employed", tr.intake = 2018)
# looks more like parallel trends

