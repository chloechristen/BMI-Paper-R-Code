# data 1990
setwd("~/Downloads")
library(tidyverse)
library(haven)

nhis90 <- read_dta("nhis_00006.dta") %>% filter(year==1990) %>% filter(age>=25 & age<=54) %>%
  mutate(pov = if_else(pooryn==2,1,0)) %>% 
  mutate(educyrs = case_when(educrec2==0 ~ NA, educrec2==10 ~ 0, educrec2==20 ~ 3, 
                             educrec2==31 ~ 6, educrec2==32 ~ 8, educrec2==41 ~10, 
                             educrec2==42 ~ 12, educrec2==51 ~ 14, educrec2==54 ~ 16, 
                             educrec2==60 ~ 18, educrec2==99 ~ NA)) %>%
  mutate(employ = if_else(empstat<=122,1,0)) %>%
  mutate(sex = if_else(sex==2,1,0)) %>% mutate(race = if_else(racea>100,1,0)) %>%
  mutate(metroarea = if_else(metro>31,1,0))%>% 
  mutate(marstat = if_else(marst==11,0,1))


cc=complete.cases(nhis90[ ,c("bmicalc","pov","educyrs","employ","age","famsize")])
full90 <- nhis90 %>% filter(cc & empstat!=220 & bmicalc!=996)

# data 1982
setwd("~/Downloads")
library(tidyverse)
library(haven)
nhis82 <- read_dta("nhis_00006.dta") %>% filter(year==1982) %>% filter(age>=25 & age<=54) %>%
  mutate(pov = if_else(pooryn==2,1,0)) %>% 
  mutate(educyrs = case_when(educrec2==0 ~ NA, educrec2==10 ~ 0, educrec2==20 ~ 3, 
                             educrec2==31 ~ 6, educrec2==32 ~ 8, educrec2==41 ~10, 
                             educrec2==42 ~ 12, educrec2==51 ~ 14, educrec2==54 ~ 16, 
                             educrec2==60 ~ 18, educrec2==99 ~ NA)) %>%
  mutate(employ = if_else(empstat<=122,1,0)) %>%
  mutate(sex = if_else(sex==2,1,0)) %>% mutate(race = if_else(racea>100,1,0)) %>%
  mutate(marstat = if_else(marst==11,0,1))


cc=complete.cases(nhis82[ ,c("bmicalc","pov","educyrs","employ","age","sex")])
full82 <- nhis82 %>% filter(cc & empstat!=220 & bmicalc!=996)

# data 1976
setwd("~/Downloads")
library(tidyverse)
library(haven)
nhis76 <- read_dta("nhis_00006.dta") %>% filter(year==1976)
household <- nhis76 %>% group_by(nhishid) %>% summarise(househead = first(sex[relate==10]), 
                                                        kids = sum(age<18), adults=sum(age>=18), 
                                                        finc = first(incfamr68on[relate==10]), 
                                                        housesize = n(), agehh = first(age[relate==10]),
                                                        kids = (adults==0)*(kids-1)+(adults>0)*kids,
                                                        adults = if_else(adults==0,1,adults))
household <- household %>% mutate(cpov = case_when(housesize==1 & agehh<65 ~ 3069, 
                                                   housesize==1 & agehh>=65 ~ 2758,
                                                   housesize==2 & agehh<65 & kids==0 ~ 3838,
                                                   housesize==2 & kids==1 ~ 4300,
                                                   housesize==2 & agehh>=65 & kids==0 ~ 3446,
                                                   housesize==3 & kids==0  ~ 4468,
                                                   housesize==3 & kids==1 ~ 4613,
                                                   housesize==3 & kids==2  ~ 4876,
                                                   housesize==4 & kids==0 ~ 5890,
                                                   housesize==4 & kids==1  ~ 5977,
                                                   housesize==4 & kids==2  ~ 5771,
                                                   housesize==4 & kids==3  ~ 6063,
                                                   housesize==5 & kids==0  ~ 7109,
                                                   housesize==5 & kids==1  ~ 7194,
                                                   housesize==5 & kids==2  ~ 6964,
                                                   housesize==5 & kids==3  ~ 6788,
                                                   housesize==5 & kids==4  ~ 6934,
                                                   housesize==6 & kids==0  ~ 8154,
                                                   housesize==6 & kids==1  ~ 8180,
                                                   housesize==6 & kids==2  ~ 8008,
                                                   housesize==6 & kids==3  ~ 7833,
                                                   housesize==6 & kids==4  ~ 7602,
                                                   housesize==6 & kids==5  ~ 7718,
                                                   housesize>=7 & kids==0  ~ 10268,
                                                   housesize>=7 & kids==1  ~ 10357,
                                                   housesize>=7 & kids==2  ~ 10153,
                                                   housesize>=7 & kids==3  ~ 9979,
                                                   housesize>=7 & kids==4  ~ 9749,
                                                   housesize>=7 & kids==5  ~ 9399,
                                                   housesize>=7 & kids>=6  ~ 9313),
                                  pov = case_when(finc<=4 ~ NA,
                                                  finc>=11 & finc<=13 ~ 1,
                                                  finc>=41 ~ 0,
                                                  finc=30 & housesize<7 ~ 0,
                                                  finc=30 & housesize>=7 & kids<3 ~ NA, 
                                                  finc=30 & housesize>=7 & kids>=3 ~ 0,
                                                  finc=14 & cpov<3000 ~ 0,
                                                  finc=14 & cpov>3999 ~ 1,
                                                  finc=14 & cpov>3000 & cpov<3999 ~ NA,
                                                  finc=15 & cpov<4000 ~ 0,
                                                  finc=15 & cpov>4999 ~ 1,
                                                  finc=15 & cpov>4000 & cpov<4999 ~ NA,
                                                  finc=21 & cpov<5000 ~ 0,
                                                  finc=21 & cpov>5999 ~ 1,
                                                  finc=21 & cpov>5000 & cpov<5999 ~ NA,
                                                  finc=22 & cpov<6000 ~ 0,
                                                  finc=22 & cpov>6999 ~ 1,
                                                  finc=22 & cpov>6000 & cpov<6999 ~ NA,
                                                  finc=27 & cpov<7000 ~ 0,
                                                  finc=27 & cpov>9999 ~ 1,
                                                  finc=27 & cpov>7000 & cpov<9999 ~ NA))
full76 <- left_join(nhis76, household, by = "nhishid") %>% filter(age>=25 & age<=54) %>%
  mutate(educyrs = case_when(educrec2==10 ~ 0, educrec2==20 ~ 3, educrec2==31 ~ 6,
                             educrec2==32 ~ 8, educrec2==41 ~10, educrec2==42 ~ 12,
                             educrec2==51 ~ 14, educrec2==54 ~ 16, educrec2==60 ~ 18)) %>%
  mutate(employ = if_else(empstat<=122,1,0)) %>%
  mutate(sex = if_else(sex==2,1,0)) %>% mutate(race = if_else(racea>100,1,0)) %>%
  mutate(marstat = if_else(marst==11,0,1))

cc=complete.cases(full76[ ,c("bmicalc","pov","educyrs","age","famsize","sex","employ")])
full76 <- full76 %>% filter(cc & empstat!=220 & bmicalc!=996)


#create new dataset with all data
library(dplyr)
full <- bind_rows(full76, full82, full90) %>% mutate(year82 = if_else(year==1982,1,0),
                                                     year90 = if_else(year==1990,1,0))

#estimated OLS LM
model1 <- lm(bmicalc ~ pov + sex + educyrs + age + employ + + marst 
             + year82 + year90 + year82:pov + year90:pov, data=full)
summary(model1)


#Estimated LM 1976
model76 <- lm(bmicalc ~ pov + sex + educyrs + age + employ, data=full76)

#Estimated LM 1982
model82 <- lm(bmicalc ~ pov + sex + educyrs + age + employ, data=full82)

#Estimated LM 1990
model90 <- lm(bmicalc ~ pov + sex + educyrs + age + employ, data=full90)

#output table 
library(stargazer)
stargazer(model1,model76,model82,model90, type="text",out="mymodels.doc")


#plot residuals 
plot(model1,which=3)

#robust standard errors 
library(sandwich)
library(lmtest)
coeftest(model1, vcov=sandwich )


##########
#GRF 

setwd("~/Downloads")
library(tidyverse)
library(haven)
nhis76 <- read_dta("nhis_00006.dta") %>% filter(year==1976)
household <- nhis76 %>% group_by(nhishid) %>% summarise(househead = first(sex[relate==10]), 
                                                        kids = sum(age<18), adults=sum(age>=18), 
                                                        finc = first(incfamr68on[relate==10]), 
                                                        housesize = n(), agehh = first(age[relate==10]),
                                                        kids = (adults==0)*(kids-1)+(adults>0)*kids,
                                                        adults = if_else(adults==0,1,adults))
household <- household %>% mutate(cpov = case_when(housesize==1 & agehh<65 ~ 3069, 
                                                   housesize==1 & agehh>=65 ~ 2758,
                                                   housesize==2 & agehh<65 & kids==0 ~ 3838,
                                                   housesize==2 & kids==1 ~ 4300,
                                                   housesize==2 & agehh>=65 & kids==0 ~ 3446,
                                                   housesize==3 & kids==0  ~ 4468,
                                                   housesize==3 & kids==1 ~ 4613,
                                                   housesize==3 & kids==2  ~ 4876,
                                                   housesize==4 & kids==0 ~ 5890,
                                                   housesize==4 & kids==1  ~ 5977,
                                                   housesize==4 & kids==2  ~ 5771,
                                                   housesize==4 & kids==3  ~ 6063,
                                                   housesize==5 & kids==0  ~ 7109,
                                                   housesize==5 & kids==1  ~ 7194,
                                                   housesize==5 & kids==2  ~ 6964,
                                                   housesize==5 & kids==3  ~ 6788,
                                                   housesize==5 & kids==4  ~ 6934,
                                                   housesize==6 & kids==0  ~ 8154,
                                                   housesize==6 & kids==1  ~ 8180,
                                                   housesize==6 & kids==2  ~ 8008,
                                                   housesize==6 & kids==3  ~ 7833,
                                                   housesize==6 & kids==4  ~ 7602,
                                                   housesize==6 & kids==5  ~ 7718,
                                                   housesize>=7 & kids==0  ~ 10268,
                                                   housesize>=7 & kids==1  ~ 10357,
                                                   housesize>=7 & kids==2  ~ 10153,
                                                   housesize>=7 & kids==3  ~ 9979,
                                                   housesize>=7 & kids==4  ~ 9749,
                                                   housesize>=7 & kids==5  ~ 9399,
                                                   housesize>=7 & kids>=6  ~ 9313),
                                  pov = case_when(finc<=4 ~ NA,
                                                  finc>=11 & finc<=13 ~ 1,
                                                  finc>=41 ~ 0,
                                                  finc=30 & housesize<7 ~ 0,
                                                  finc=30 & housesize>=7 & kids<3 ~ NA, 
                                                  finc=30 & housesize>=7 & kids>=3 ~ 0,
                                                  finc=14 & cpov<3000 ~ 0,
                                                  finc=14 & cpov>3999 ~ 1,
                                                  finc=14 & cpov>3000 & cpov<3999 ~ NA,
                                                  finc=15 & cpov<4000 ~ 0,
                                                  finc=15 & cpov>4999 ~ 1,
                                                  finc=15 & cpov>4000 & cpov<4999 ~ NA,
                                                  finc=21 & cpov<5000 ~ 0,
                                                  finc=21 & cpov>5999 ~ 1,
                                                  finc=21 & cpov>5000 & cpov<5999 ~ NA,
                                                  finc=22 & cpov<6000 ~ 0,
                                                  finc=22 & cpov>6999 ~ 1,
                                                  finc=22 & cpov>6000 & cpov<6999 ~ NA,
                                                  finc=27 & cpov<7000 ~ 0,
                                                  finc=27 & cpov>9999 ~ 1,
                                                  finc=27 & cpov>7000 & cpov<9999 ~ NA))
full76 <- left_join(nhis76, household, by = "nhishid") %>% filter(age>=25 & age<=54) %>%
  mutate(educyrs = case_when(educrec2==10 ~ 0, educrec2==20 ~ 3, educrec2==31 ~ 6,
                             educrec2==32 ~ 8, educrec2==41 ~10, educrec2==42 ~ 12,
                             educrec2==51 ~ 14, educrec2==54 ~ 16, educrec2==60 ~ 18)) %>%
  mutate(docdays = case_when(dvint==201 ~ 14, dvint==202 ~ 180, dvint==204 ~ 270,
                             dvint==301 ~ 365, dvint==307 ~ 1095, dvint==400 ~ 1825,
                             dvint==100 ~ age*365, dvint==999 ~ NA)) %>%
  mutate(employ = if_else(empstat<=122,1,0)) %>%
  mutate(race = if_else(racea>100,1,0))%>%
  mutate(healthins = case_when(hipubcov==1 & hiprivate==1 ~ 0,
                               hipubcov==1 & hiprivate==2 ~ 1,
                               hipubcov==2 & hiprivate==1 ~ 2,
                               hipubcov==2 & hiprivate==2 ~ NA,
                               hipubcov==9 | hiprivate==9 ~ NA))%>%
  mutate(metroarea = if_else(metro>31,1,0))%>%
  mutate(marstat = case_when(marst==50 ~ 0, marst>=12 & marst<=40 ~ 1, marst==11 ~ 2))


cc=complete.cases(full76[ ,c("bmicalc","pov","educyrs","age","famsize","docdays","healthins","race","health","metroarea","region","marstat")])
forestdata = full76 %>% filter(cc & empstat!=220 & bmicalc!=996)


#1976 education

y = as.matrix(forestdata$bmicalc) 
w = as.matrix(forestdata$pov)
x = as.matrix(forestdata[ ,c("educyrs","age","famsize","employ", "sex","adults","kids","race","health","healthins","region","metroarea","marstat")])
tau.forest <- causal_forest(x, y, w)
average_treatment_effect(tau.forest, target.sample = "all")


#family of 5 x=education (1)
x.test <- as.matrix(data.frame(educyrs = c(0,3,6,8,10,12,14,16,18), age=37, famsize=5,
                               employ=1, sex=2, adults=2, kids=3, race=0, health=3, heathins=1,
                               region=3, metroarea=0, marstat=2))
tau.hat <- predict(tau.forest, x.test, estimate.variance = TRUE)
sigma.hat <- sqrt(tau.hat$variance.estimates)
plot(x.test[, 1], tau.hat$predictions, ylim = range(tau.hat$predictions + 1.96 * sigma.hat, tau.hat$predictions - 1.96 * sigma.hat, 0, 2), xlab = "x", ylab = "tau", type = "l")
lines(x.test[, 1], tau.hat$predictions + 1.64 * sigma.hat, col = 1, lty = 2)
lines(x.test[, 1], tau.hat$predictions - 1.64 * sigma.hat, col = 1, lty = 2)
abline(h=0, col="red")

#1976 Time
#single parent with 2 kids x=age
#single male 
x.test <- as.matrix(data.frame(educyrs = 12, age=25:54, famsize=3,
                               employ=1, sex=1, adults=1, kids=2, race=0, health=1, heathins=1,
                               region=3, metroarea=0, marstat=1))
tau.hat <- predict(tau.forest, x.test, estimate.variance = TRUE)
sigma.hat <- sqrt(tau.hat$variance.estimates)
plot(x.test[, 2], tau.hat$predictions, ylim = range(tau.hat$predictions + 1.96 * sigma.hat, tau.hat$predictions - 1.96 * sigma.hat, 0, 2), xlab = "x", ylab = "tau", type = "l")
lines(x.test[, 2], tau.hat$predictions + 1.64 * sigma.hat, col = 1, lty = 2)
lines(x.test[, 2], tau.hat$predictions - 1.64 * sigma.hat, col = 1, lty = 2)
abline(h=0, col="red")

#single female 
x.test <- as.matrix(data.frame(educyrs = 12, age=25:54, famsize=3,
                               employ=1, sex=2, adults=1, kids=2, race=0, health=1, heathins=1,
                               region=3, metroarea=0, marstat=1))
tau.hat <- predict(tau.forest, x.test, estimate.variance = TRUE)
sigma.hat <- sqrt(tau.hat$variance.estimates)
plot(x.test[, 2], tau.hat$predictions, ylim = range(tau.hat$predictions + 1.96 * sigma.hat, tau.hat$predictions - 1.96 * sigma.hat, 0, 2), xlab = "x", ylab = "tau", type = "l")
lines(x.test[, 2], tau.hat$predictions + 1.64 * sigma.hat, col = 1, lty = 2)
lines(x.test[, 2], tau.hat$predictions - 1.64 * sigma.hat, col = 1, lty = 2)
abline(h=0, col="red")

#married female 
x.test <- as.matrix(data.frame(educyrs = 12, age=25:54, famsize=3,
                               employ=1, sex=2, adults=1, kids=2, race=0, health=1, heathins=1,
                               region=3, metroarea=0, marstat=2))
tau.hat <- predict(tau.forest, x.test, estimate.variance = TRUE)
sigma.hat <- sqrt(tau.hat$variance.estimates)
plot(x.test[, 2], tau.hat$predictions, ylim = range(tau.hat$predictions + 1.96 * sigma.hat, tau.hat$predictions - 1.96 * sigma.hat, 0, 2), xlab = "x", ylab = "tau", type = "l")
lines(x.test[, 2], tau.hat$predictions + 1.64 * sigma.hat, col = 1, lty = 2)
lines(x.test[, 2], tau.hat$predictions - 1.64 * sigma.hat, col = 1, lty = 2)
abline(h=0, col="red")



### 1982
nhis82 <- read_dta("nhis_00006.dta") %>% filter(year==1982) %>% filter(age>=25 & age<=54) %>%
  mutate(pov = if_else(pooryn==2,1,0)) %>% 
  mutate(educyrs = case_when(educrec2==0 ~ NA, educrec2==10 ~ 0, educrec2==20 ~ 3, 
                             educrec2==31 ~ 6, educrec2==32 ~ 8, educrec2==41 ~10, 
                             educrec2==42 ~ 12, educrec2==51 ~ 14, educrec2==54 ~ 16, 
                             educrec2==60 ~ 18, educrec2==99 ~ NA)) %>%
  mutate(employ = if_else(empstat<=122,1,0)) %>%
  mutate(race = if_else(racea>100,1,0)) %>%
  mutate(healthins = case_when(hipubcov==1 & hiprivate==1 ~ 0,
                               hipubcov==1 & hiprivate==2 ~ 1,
                               hipubcov==2 & hiprivate==1 ~ 2,
                               hipubcov==2 & hiprivate==2 ~ NA,
                               hipubcov==9 | hiprivate==9 ~ NA)) %>%
  mutate(metroarea = if_else(metro>31,1,0)) %>%
  mutate(marstat = case_when(marst==50 ~ 0, marst>=12 & marst<=40 ~ 1, marst==11 ~ 2,
                             marst==0 | marst==99 ~ NA)) %>%
  mutate(adults = age>=18)

cc=complete.cases(nhis82[ ,c("bmicalc","pov","educyrs","employ","age","famsize","healthins","race","health","metroarea","region","marstat")])
forestdata82 = nhis82 %>% filter(cc & empstat!=220 & bmicalc!=996)


#education 
library(grf)

y = as.matrix(forestdata82$bmicalc) 
w = as.matrix(forestdata82$pov)
x = as.matrix(forestdata82[ ,c("educyrs","age","famsize","employ", "sex","adults","race","health","healthins","region","metroarea","marstat")])
tau.forest <- causal_forest(x, y, w)
average_treatment_effect(tau.forest, target.sample = "all")


#family of 5 x=education 
x.test <- as.matrix(data.frame(educyrs = c(0,3,6,8,10,12,14,16,18), age=37, famsize=5,
                               employ=1, sex=2, race=0, health=3, heathins=1,
                               region=3, metroarea=0, marstat=2))
tau.hat <- predict(tau.forest, x.test, estimate.variance = TRUE)
sigma.hat <- sqrt(tau.hat$variance.estimates)
plot(x.test[, 1], tau.hat$predictions, ylim = range(tau.hat$predictions + 1.96 * sigma.hat, tau.hat$predictions - 1.96 * sigma.hat, 0, 2), xlab = "x", ylab = "tau", type = "l")
lines(x.test[, 1], tau.hat$predictions + 1.64 * sigma.hat, col = 1, lty = 2)
lines(x.test[, 1], tau.hat$predictions - 1.64 * sigma.hat, col = 1, lty = 2)
abline(h=0, col="red")

#time 
#Single parent (2 kids) X=age (6)
#single male 
x.test <- as.matrix(data.frame(educyrs = 12, age=25:54, famsize=3,
                               employ=1, sex=1, adults=1, race=0, health=1, heathins=1,
                               region=3, metroarea=0, marstat=1))
tau.hat <- predict(tau.forest, x.test, estimate.variance = TRUE)
sigma.hat <- sqrt(tau.hat$variance.estimates)
plot(x.test[, 2], tau.hat$predictions, ylim = range(tau.hat$predictions + 1.96 * sigma.hat, tau.hat$predictions - 1.96 * sigma.hat, 0, 2), xlab = "x", ylab = "tau", type = "l")
lines(x.test[, 2], tau.hat$predictions + 1.64 * sigma.hat, col = 1, lty = 2)
lines(x.test[, 2], tau.hat$predictions - 1.64 * sigma.hat, col = 1, lty = 2)
abline(h=0, col="red")

#single female 
x.test <- as.matrix(data.frame(educyrs = 12, age=25:54, famsize=3,
                               employ=1, sex=2, adults=2, race=0, health=1, heathins=1,
                               region=3, metroarea=0, marstat=1))
tau.hat <- predict(tau.forest, x.test, estimate.variance = TRUE)
sigma.hat <- sqrt(tau.hat$variance.estimates)
plot(x.test[, 2], tau.hat$predictions, ylim = range(tau.hat$predictions + 1.96 * sigma.hat, tau.hat$predictions - 1.96 * sigma.hat, 0, 2), xlab = "x", ylab = "tau", type = "l")
lines(x.test[, 2], tau.hat$predictions + 1.64 * sigma.hat, col = 1, lty = 2)
lines(x.test[, 2], tau.hat$predictions - 1.64 * sigma.hat, col = 1, lty = 2)
abline(h=0, col="red")

#married female 
x.test <- as.matrix(data.frame(educyrs = 12, age=25:54, famsize=3,
                               employ=1, sex=2, adults=2, race=0, health=1, heathins=1,
                               region=3, metroarea=0, marstat=2))
tau.hat <- predict(tau.forest, x.test, estimate.variance = TRUE)
sigma.hat <- sqrt(tau.hat$variance.estimates)
plot(x.test[, 2], tau.hat$predictions, ylim = range(tau.hat$predictions + 1.96 * sigma.hat, tau.hat$predictions - 1.96 * sigma.hat, 0, 2), xlab = "x", ylab = "tau", type = "l")
lines(x.test[, 2], tau.hat$predictions + 1.64 * sigma.hat, col = 1, lty = 2)
lines(x.test[, 2], tau.hat$predictions - 1.64 * sigma.hat, col = 1, lty = 2)
abline(h=0, col="red")



#### 1990
nhis90 <- read_dta("nhis_00006.dta") %>% filter(year==1990) %>% filter(age>=25 & age<=54) %>%
  mutate(pov = if_else(pooryn==2,1,0)) %>% 
  mutate(educyrs = case_when(educrec2==0 ~ NA, educrec2==10 ~ 0, educrec2==20 ~ 3, 
                             educrec2==31 ~ 6, educrec2==32 ~ 8, educrec2==41 ~10, 
                             educrec2==42 ~ 12, educrec2==51 ~ 14, educrec2==54 ~ 16, 
                             educrec2==60 ~ 18, educrec2==99 ~ NA)) %>%
  mutate(employ = if_else(empstat<=122,1,0)) %>%
  mutate(race = if_else(racea>100,1,0)) %>%
  mutate(healthins = case_when(hipubcov==1 & hiprivate==1 ~ 0,
                               hipubcov==1 & hiprivate==2 ~ 1,
                               hipubcov==2 & hiprivate==1 ~ 2,
                               hipubcov==2 & hiprivate==2 ~ NA)) %>%
  mutate(metroarea = if_else(metro>31,1,0)) %>%
  mutate(marstat = case_when(marst==50 ~ 0, marst>=12 & marst<=40 ~ 1, marst==11 ~ 2,
                             marst==0 | marst==99 ~ NA)) %>%
  mutate(adults = age>=18) %>%
  mutate(diabetes = if_else(diabeticnow==2,1,0))

cc=complete.cases(nhis90[ ,c("bmicalc","pov","educyrs","employ","age","famsize","healthins","race","health","metroarea","region","marstat","diabetes")])
forestdata90 = nhis90 %>% filter(cc & empstat!=220 & bmicalc!=996)

y = as.matrix(forestdata90$bmicalc) 
w = as.matrix(forestdata90$pov)
x = as.matrix(forestdata90[ ,c("educyrs","age","famsize","employ", "sex","adults","race","health","healthins","region","metroarea","marstat")])
tau.forest2 <- causal_forest(x, y, w)
average_treatment_effect(tau.forest, target.sample = "all")


#education
#family of 5 x=education 
x.test <- as.matrix(data.frame(educyrs = c(0,3,6,8,10,12,14,16,18), age=37, famsize=5,
                               employ=1, sex=2, adults=2, race=0, health=3, heathins=1,
                               region=3, metroarea=0, marstat=2))
tau.hat <- predict(tau.forest, x.test, estimate.variance = TRUE)
sigma.hat <- sqrt(tau.hat$variance.estimates)
plot(x.test[, 1], tau.hat$predictions, ylim = range(tau.hat$predictions + 1.96 * sigma.hat, tau.hat$predictions - 1.96 * sigma.hat, 0, 2), xlab = "x", ylab = "tau", type = "l")
lines(x.test[, 1], tau.hat$predictions + 1.64 * sigma.hat, col = 1, lty = 2)
lines(x.test[, 1], tau.hat$predictions - 1.64 * sigma.hat, col = 1, lty = 2)
abline(h=0, col="red")

#time 
#Single parent (2 kids) X=age 
#single male
x.test <- as.matrix(data.frame(educyrs = 12, age=25:54, famsize=3,
                               employ=1, sex=1, adults=1, race=0, health=1, heathins=1,
                               region=3, metroarea=0, marstat=1))
tau.hat <- predict(tau.forest, x.test, estimate.variance = TRUE)
sigma.hat <- sqrt(tau.hat$variance.estimates)
plot(x.test[, 2], tau.hat$predictions, ylim = range(tau.hat$predictions + 1.96 * sigma.hat, tau.hat$predictions - 1.96 * sigma.hat, 0, 2), xlab = "x", ylab = "tau", type = "l")
lines(x.test[, 2], tau.hat$predictions + 1.64 * sigma.hat, col = 1, lty = 2)
lines(x.test[, 2], tau.hat$predictions - 1.64 * sigma.hat, col = 1, lty = 2)
abline(h=0, col="red")

#single female 
x.test <- as.matrix(data.frame(educyrs = 12, age=25:54, famsize=3,
                               employ=1, sex=2, adults=1, race=0, health=1, heathins=1,
                               region=3, metroarea=0, marstat=1))
tau.hat <- predict(tau.forest, x.test, estimate.variance = TRUE)
sigma.hat <- sqrt(tau.hat$variance.estimates)
plot(x.test[, 2], tau.hat$predictions, ylim = range(tau.hat$predictions + 1.96 * sigma.hat, tau.hat$predictions - 1.96 * sigma.hat, 0, 2), xlab = "x", ylab = "tau", type = "l")
lines(x.test[, 2], tau.hat$predictions + 1.64 * sigma.hat, col = 1, lty = 2)
lines(x.test[, 2], tau.hat$predictions - 1.64 * sigma.hat, col = 1, lty = 2)
abline(h=0, col="red")

#married female 
x.test <- as.matrix(data.frame(educyrs = 12, age=25:54, famsize=3,
                               employ=1, sex=2, adults=1, race=0, health=1, heathins=1,
                               region=3, metroarea=0, marstat=2))
tau.hat <- predict(tau.forest, x.test, estimate.variance = TRUE)
sigma.hat <- sqrt(tau.hat$variance.estimates)
plot(x.test[, 2], tau.hat$predictions, ylim = range(tau.hat$predictions + 1.96 * sigma.hat, tau.hat$predictions - 1.96 * sigma.hat, 0, 2), xlab = "x", ylab = "tau", type = "l")
lines(x.test[, 2], tau.hat$predictions + 1.64 * sigma.hat, col = 1, lty = 2)
lines(x.test[, 2], tau.hat$predictions - 1.64 * sigma.hat, col = 1, lty = 2)
abline(h=0, col="red")

