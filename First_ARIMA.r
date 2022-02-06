# This is a "dummy" code created only for Germany cases data, modeled with ARIMA
library(tidyverse)
library(forecast)
library(robustarima)
library(lmtest)

# Read and polish the data
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Data")
Cases_EU <- read.csv("Cases_EU_dc16.csv")
Cases_EU <- as_tibble(Cases_EU)
Cases_EU <- Cases_EU[,c(1,5,7)]
Cases_EU <- Cases_EU[Cases_EU$countriesAndTerritories %in% c("Denmark", "France", "Germany"), ]
Cases_EU <- Cases_EU[!grepl("2020", Cases_EU$dateRep),]
Cases_EU <- Cases_EU[!grepl("01/03/2021", Cases_EU$dateRep),]
Cases_EU$dateRep <- as.Date(Cases_EU$dateRep,format="%d/%m/%y")
colnames(Cases_EU)[1] <- "date"
Cases_EU_region <- split(Cases_EU, Cases_EU$countriesAndTerritories)
De_cases <- Cases_EU_region[[3]]
n <- nrow(De_cases)
De_cases$cases <- rev(De_cases$cases)
De_cases$date <- rev(De_cases$date)

# Select the first Covid Passport intervention time range (same as in SR)
De_cases_NP1 <- De_cases[c(122:189),]

# Look at first ACF/PACF plots
ggplot(data = De_cases_NP1, aes(x = date, y = cases)) +  geom_line() +  labs(title = "Cases in Germany", x = "Date", y = "Cases")
par(mfrow=c(2,1))
acf(De_cases_NP1$cases)
pacf(De_cases_NP1$cases)
# I was going to create a sasonal 7-day ARIMA model, but I don't think the plots show that. Like, the 
# 7-lag, for instance, is not significative at all. I guess I won't introduce it
# The ACF plot has positive correlation until lag 15. I will differentiate once

cases_dif <- c(NA,diff(De_cases_NP1$cases,lag=1))
De_cases_NP1 <- cbind(De_cases_NP1,cases_dif)
ggplot(data = De_cases_NP1, aes(x = date, y = cases_dif)) +  geom_line() +  labs(title = "Cases in Germany", x = "Date", y = "Cases")
par(mfrow=c(2,1))
acf(De_cases_NP1$cases_dif[-1])
pacf(De_cases_NP1$cases_dif[-1])
# No further differentiation seems necessary. 
# As for AR or MA terms, the plots seem to indicate that p=3 or q=3 might be good. Let's check

#Define the three vectors (NPI and two tendencies)
NPI1 <- c(rep(0,174),rep(NA,5),rep(1,(n-179)))
t1 <- c(c(0:173),rep(NA,5),c(174:(n-6)))
t2 <- c(rep(0,174),rep(NA,5),c(0:(n-180)))
matriu1 <- cbind(NPI1,t1,t2) # first NPI change alone
# Restrict the matrix to the same time interval
matriuNP1 <- matriu1[c(122:189),]

# Let's run first auto.arima with and without diff
autoarima_0 <- auto.arima(y=De_cases_NP1$cases,seasonal=T,xreg=matriuNP1)
autoarima_1 <- auto.arima(y=De_cases_NP1$cases_dif,seasonal=T,xreg=matriuNP1)

# Let's run as well our Arima models with diff
arima_013 <- Arima(y=De_cases_NP1$cases,order=c(0,1,3),xreg=matriuNP1)
arima_310 <- Arima(y=De_cases_NP1$cases,order=c(3,1,0),xreg=matriuNP1)
arima_313 <- Arima(y=De_cases_NP1$cases,order=c(3,1,3),xreg=matriuNP1)

# To check, the last one should be the same as
arima_303 <- Arima(y=De_cases_NP1$cases_dif,order=c(3,0,3),xreg=matriuNP1)

# Let's compare all models
aautoarima_0
checkresiduals(autoarima_0)

aautoarima_1

checkresiduals(autoarima_1)

arima_013
checkresiduals(arima_013)

arima_310
checkresiduals(arima_310)

arima_313
checkresiduals(arima_313)

arima_303
checkresiduals(arima_303)

# ARIMA function and actual models

do_arima <- function(dataset,number,lag,output,country,int) {
  rows <- nrow(dataset)
  NPI1 <- c(rep(0,number-1),rep(NA,lag),rep(1,rows-lag-number+1))
  t1 <- c(c(0:(number-2)),rep(NA,lag),c((number-1+lag):(rows-1)))
  t2 <- c(rep(0,number-1),rep(NA,lag),c(1:(rows-lag-number+1)))
  matriu <- cbind(NPI1,t1,t2)
  
  autoarima_0 <- auto.arima(y=dataset[,output],seasonal=T,xreg=matriu)
  capture.output(cbind(dataset[,output],matriu), file=paste0("ARIMA_",country,"_",output,"CP",int,".txt"), append=TRUE)
  capture.output(summary(autoarima_0), file=paste0("ARIMA_",country,"_",output,"CP",int,".txt"), append=TRUE)
  capture.output(coeftest(autoarima_0), file=paste0("ARIMA_",country,"_",output,"CP",int,".txt"), append=TRUE)
  capture.output(checkresiduals(autoarima_0), file=paste0("ARIMA_",country,"_",output,"CP",int,".txt"), append=TRUE)
  capture.output(confint(autoarima_0), file=paste0("ARIMA_",country,"_",output,"CP",int,".txt"), append=TRUE)
  jpeg(paste0("ARIMA_",country,"_",output,"CP",int,".jpg"))
  checkresiduals(autoarima_0)
  dev.off()
  }

# Denmark
Dk_cases_CP4 <- Dk_cases[c(20:60),]
Dk_cases_CP4$cases <- rev(Dk_cases_CP4$cases)
Dk_cases_CP4$date <- rev(Dk_cases_CP4$date)
ggplot(data = Dk_cases_CP4, aes(x = date, y = cases)) +  geom_line() +  labs(title = "Cases in Denmark", x = "Date", y = "Cases")
par(mfrow=c(2,1))
acf(Dk_cases_CP4$cases)
pacf(Dk_cases_CP4$cases)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/ARIMA/CP4")
do_arima(Dk_cases_CP4,25,5,"cases","Dk",4)
# Doing p=2 makes AIC/BIC slightly better but the residuals are worse, and I don't think the added complexity is worth it

Dk_cases_CP5 <- Dk_cases[c(1:35),]
Dk_cases_CP5$cases <- rev(Dk_cases_CP5$cases)
Dk_cases_CP5$date <- rev(Dk_cases_CP5$date)
ggplot(data = Dk_cases_CP5, aes(x = date, y = cases)) +  geom_line() +  labs(title = "Cases in Denmark", x = "Date", y = "Cases")
par(mfrow=c(2,1))
acf(Dk_cases_CP5$cases)
pacf(Dk_cases_CP5$cases)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/ARIMA/CP5")
do_arima(Dk_cases_CP5,14,5,"cases","Dk",5)

Dk_hosp_CP4 <- Dk_hosp[c(300:333),]
ggplot(data = Dk_hosp_CP4, aes(x = date, y = admissions)) +  geom_line() +  labs(title = "Admissions in Denmark", x = "Date", y = "Cases")
par(mfrow=c(2,1))
acf(Dk_hosp_CP4$admissions)
pacf(Dk_hosp_CP4$admissions)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/ARIMA/CP4")
do_arima(Dk_hosp_CP4,17,7,"admissions","Dk",4)

Dk_hosp_smooth_CP5 <- Dk_hosp_smooth[c(320:350),]
ggplot(data = Dk_hosp_CP5, aes(x = date, y = admissions)) +  geom_line() +  labs(title = "Admissions in Denmark", x = "Date", y = "Cases")
par(mfrow=c(2,1))
acf(Dk_hosp_CP5$admissions)
pacf(Dk_hosp_CP5$admissions)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/ARIMA/CP5")
do_arima(Dk_hosp_CP5,11,7,"admissions","Dk",5)


# Wales 
Wa_cases_CP1 <- Wa_cases[c(74:111),]
Wa_cases_CP1$cases <- rev(Wa_cases_CP1$cases)
Wa_cases_CP1$date <- rev(Wa_cases_CP1$date)
Wa_cases_CP1$cases <- Wa_cases_CP1$cases/Wa_pop*100000
ggplot(data = Wa_cases_CP1, aes(x = date, y = cases)) +  geom_line() +  labs(title = "Cases in Wales", x = "Date", y = "Cases")
par(mfrow=c(2,1))
acf(Wa_cases_CP1$cases)
pacf(Wa_cases_CP1$cases)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Wales/ARIMA")
do_arima(Wa_cases_CP1,12,5,"cases","Wa",1)

Wa_cases_CP2 <- Wa_cases[c(51:75),]
Wa_cases_CP2$cases <- rev(Wa_cases_CP2$cases)
Wa_cases_CP2$date <- rev(Wa_cases_CP2$date)
Wa_cases_CP2$cases <- Wa_cases_CP2$cases/Wa_pop*100000
ggplot(data = Wa_cases_CP2, aes(x = date, y = cases)) +  geom_line() +  labs(title = "Cases in Wales", x = "Date", y = "Cases")
par(mfrow=c(2,1))
acf(Wa_cases_CP2$cases)
pacf(Wa_cases_CP2$cases)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Wales/ARIMA")
do_arima(Wa_cases_CP2,11,5,"cases","Wa",2)

rows <- nrow(Wa_cases_CP2)
number <- 11
lag <- 5
NPI1 <- c(rep(0,number-1),rep(NA,lag),rep(1,rows-lag-number+1))
t1 <- c(c(0:(number-2)),rep(NA,lag),c((number-1+lag):(rows-1)))
t2 <- c(rep(0,number-1),rep(NA,lag),c(1:(rows-lag-number+1)))
matriu <- cbind(NPI1,t1,t2)
autoarima_0 <- Arima(y=Wa_cases_CP2$cases,xreg=matriu,order=c(0,0,2))
country <- "Wa"
output <- "cases"
int <- 1
capture.output(cbind(Wa_cases_CP2$cases,matriu), file=paste0("ARIMA_",country,"_",output,"CP",int,".txt"), append=TRUE)
capture.output(summary(autoarima_0), file=paste0("ARIMA_",country,"_",output,"CP",int,".txt"), append=TRUE)
capture.output(coeftest(autoarima_0), file=paste0("ARIMA_",country,"_",output,"CP",int,".txt"), append=TRUE)
capture.output(checkresiduals(autoarima_0), file=paste0("ARIMA_",country,"_",output,"CP",int,".txt"), append=TRUE)
capture.output(confint(autoarima_0), file=paste0("ARIMA_",country,"_",output,"CP",int,".txt"), append=TRUE)
jpeg(paste0("ARIMA_",country,"_",output,"CP",int,".jpg"))
checkresiduals(autoarima_0)
dev.off()


Wa_hosp_CP1 <- Wa_hosp[c(83:110),]
Wa_hosp_CP1$admissions <- rev(Wa_hosp_CP1$admissions)
Wa_hosp_CP1$date <- rev(Wa_hosp_CP1$date)
Wa_hosp_CP1$admissions <- Wa_hosp_CP1$admissions/Wa_pop*100000
ggplot(data = Wa_hosp_CP1, aes(x = date, y = admissions)) +  geom_line() +  labs(title = "Admissions in Wales", x = "Date", y = "Cases")
par(mfrow=c(2,1))
acf(Wa_hosp_CP1$admissions)
pacf(Wa_hosp_CP1$admissions)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Wales/ARIMA")
do_arima(Wa_hosp_CP1,11,7,"admissions","Wa",1)

Wa_hosp_CP2 <- Wa_hosp[c(40:85),]
Wa_hosp_CP2$admissions <- rev(Wa_hosp_CP2$admissions)
Wa_hosp_CP2$date <- rev(Wa_hosp_CP2$date)
Wa_hosp_CP2$admissions <- Wa_hosp_CP2$admissions/Wa_pop*100000
ggplot(data = Wa_hosp_CP2, aes(x = date, y = admissions)) +  geom_line() +  labs(title = "Admissions in Wales", x = "Date", y = "Cases")
par(mfrow=c(2,1))
acf(Wa_hosp_CP2$admissions)
pacf(Wa_hosp_CP2$admissions)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Wales/ARIMA")
do_arima(Wa_hosp_CP2,21,7,"admissions","Wa",2)
rows <- nrow(Wa_hosp_CP2)
number <- 21
lag <- 7
NPI1 <- c(rep(0,number-1),rep(NA,lag),rep(1,rows-lag-number+1))
t1 <- c(c(0:(number-2)),rep(NA,lag),c((number-1+lag):(rows-1)))
t2 <- c(rep(0,number-1),rep(NA,lag),c(1:(rows-lag-number+1)))
matriu <- cbind(NPI1,t1,t2)
autoarima_0 <- Arima(y=Wa_hosp_CP2$admissions,xreg=matriu,order=c(0,0,0))
country <- "Wa"
output <- "cases"
int <- 1

# Scotland
Sc_cases_CP1 <- Sc_cases[c(79:103),]
Sc_cases_CP1$cases <- rev(Sc_cases_CP1$cases)
Sc_cases_CP1$date <- rev(Sc_cases_CP1$date)
Sc_cases_CP1$cases <- Sc_cases_CP1$cases/Sc_pop*100000
ggplot(data = Sc_cases_CP1, aes(x = date, y = cases)) +  geom_line() +  labs(title = "Cases in Scotland", x = "Date", y = "Cases")
par(mfrow=c(2,1))
acf(Sc_cases_CP1$cases)
pacf(Sc_cases_CP1$cases)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Scotland/ARIMA/")
do_arima(Wa_cases_CP1,11,5,"cases","Sc",1)

Sc_hosp_CP2 <- Sc_hosp[c(74:100),]
Sc_hosp_CP2$admissions <- rev(Sc_hosp_CP2$admissions)
Sc_hosp_CP2$date <- rev(Sc_hosp_CP2$date)
Sc_hosp_CP1$admissions <- Sc_hosp_CP1$admissions/Sc_pop*100000
ggplot(data = Sc_hosp_CP2, aes(x = date, y = admissions)) +  geom_line() +  labs(title = "Admissions in Scotland", x = "Date", y = "Cases")
par(mfrow=c(2,1))
acf(Sc_hosp_CP2$admissions)
pacf(Sc_hosp_CP2$admissions)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Scotland/ARIMA/")
do_arima(Sc_hosp_CP2,11,7,"admissions","Sc",1)


# NI
NI_cases_CP1 <- NI_cases[c(37:77),]
NI_cases_CP1$cases <- rev(NI_cases_CP1$cases)
NI_cases_CP1$date <- rev(NI_cases_CP1$date)
NI_cases_CP1$cases <- NI_cases_CP1$cases/NI_pop*100000
ggplot(data = NI_cases_CP1, aes(x = date, y = cases)) +  geom_line() +  labs(title = "Cases in NI", x = "Date", y = "Cases")
par(mfrow=c(2,1))
acf(NI_cases_CP1$cases)
pacf(NI_cases_CP1$cases)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/NI/ARIMA")
do_arima(NI_cases_CP1,27,5,"cases","NI",1)
rows <- nrow(NI_cases_CP1)
number <- 27
lag <- 5
NPI1 <- c(rep(0,number-1),rep(NA,lag),rep(1,rows-lag-number+1))
t1 <- c(c(0:(number-2)),rep(NA,lag),c((number-1+lag):(rows-1)))
t2 <- c(rep(0,number-1),rep(NA,lag),c(1:(rows-lag-number+1)))
matriu <- cbind(NPI1,t1,t2)
autoarima_0 <- Arima(y=NI_cases_CP1$cases,xreg=matriu,order=c(2,0,1))
country <- "NI"
output <- "cases"
int <- 1
capture.output(cbind(NI_cases_CP1$cases,matriu), file=paste0("ARIMA_",country,"_",output,"CP",int,".txt"), append=TRUE)
capture.output(summary(autoarima_0), file=paste0("ARIMA_",country,"_",output,"CP",int,".txt"), append=TRUE)
capture.output(coeftest(autoarima_0), file=paste0("ARIMA_",country,"_",output,"CP",int,".txt"), append=TRUE)
capture.output(checkresiduals(autoarima_0), file=paste0("ARIMA_",country,"_",output,"CP",int,".txt"), append=TRUE)
capture.output(confint(autoarima_0), file=paste0("ARIMA_",country,"_",output,"CP",int,".txt"), append=TRUE)
jpeg(paste0("ARIMA_",country,"_",output,"CP",int,".jpg"))
checkresiduals(autoarima_0)
dev.off()

NI_hosp_CP1 <- NI_hosp[c(30:77),]
NI_hosp_CP1$admissions <- rev(NI_hosp_CP1$admissions)
NI_hosp_CP1$date <- rev(NI_hosp_CP1$date)
NI_hosp_CP1$admissions <- NI_hosp_CP1$admissions/NI_pop*100000
ggplot(data = NI_hosp_CP1, aes(x = date, y = admissions)) +  geom_line() +  labs(title = "Admissions in NI", x = "Date", y = "Cases")
par(mfrow=c(2,1))
acf(NI_hosp_CP1$admissions)
pacf(NI_hosp_CP1$admissions)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/NI/ARIMA/")
do_arima(NI_hosp_CP1,27,7,"admissions","NI",1)

# England
En_cases_CP1 <- En_cases[c(12:70),]
En_cases_CP1$cases <- rev(En_cases_CP1$cases)
En_cases_CP1$date <- rev(En_cases_CP1$date)
En_cases_CP1$cases <- En_cases_CP1$cases/En_pop*100000
ggplot(data = En_cases_CP1, aes(x = date, y = cases)) +  geom_line() +  labs(title = "Cases in England", x = "Date", y = "Cases")
par(mfrow=c(2,1))
acf(En_cases_CP1$cases)
pacf(En_cases_CP1$cases)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/England/ARIMA/")
do_arima(En_cases_CP1,36,5,"cases","En",1)

En_hosp_CP1 <- En_hosp[c(5:65),]
En_hosp_CP1$admissions <- rev(En_hosp_CP1$admissions)
En_hosp_CP1$date <- rev(En_hosp_CP1$date)
En_hosp_CP1$admissions <- En_hosp_CP1$admissions/En_pop*100000
ggplot(data = En_hosp_CP1, aes(x = date, y = admissions)) +  geom_line() +  labs(title = "Admissions in England", x = "Date", y = "Cases")
par(mfrow=c(2,1))
acf(En_hosp_CP1$admissions)
pacf(En_hosp_CP1$admissions)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/England/ARIMA/")
do_arima(En_hosp_CP1,32,7,"admissions","En",1)



# Other robust stuff

dataset <- cbind(Dk_cases_CP4,matriu1)
arimarob_0 <- arima.rob(cases ~ NPI1+t1+t2, data=dataset[-c(25:29),], p=0, d=0, q=1, )
summary(arimarob_0)

dataset <- NI_hosp_CP1
number <- 27
lag <- 7
rows <- nrow(dataset)
NPI1 <- c(rep(0,number-1),rep(NA,lag),rep(1,rows-lag-number+1))
t1 <- c(c(0:(number-2)),rep(NA,lag),c((number-1+lag):(rows-1)))
t2 <- c(rep(0,number-1),rep(NA,lag),c(1:(rows-lag-number+1)))
matriu <- cbind(NPI1,t1,t2)
dataset <- cbind(dataset,matriu)
arimarob_0 <- arima.rob(admissions ~ NPI1+t1+t2, data=dataset[-c(27:33),], p=0, d=0, q=1, )
summary(arimarob_0)

dataset <- Wa_cases_CP2
number <- 11
lag <- 5
rows <- nrow(dataset)
NPI1 <- c(rep(0,number-1),rep(NA,lag),rep(1,rows-lag-number+1))
t1 <- c(c(0:(number-2)),rep(NA,lag),c((number-1+lag):(rows-1)))
t2 <- c(rep(0,number-1),rep(NA,lag),c(1:(rows-lag-number+1)))
matriu <- cbind(NPI1,t1,t2)
dataset <- cbind(dataset,matriu)
Arima(y=Wa_cases_CP2$cases,xreg=matriu,order=c(0,0,2))

arimarob_0 <- arima.rob(cases ~ NPI1+t1+t2, data=dataset[-c(11:15),], p=0, d=0, q=2, )
summary(arimarob_0)
do_arima(Wa_cases_CP2,11,5,"cases","Wa",2)


arima.rob(formula, data, contrasts=NULL, start=NULL, end=NULL,
          p=0, q=0, d=0, sd=0, freq=1, sfreq=NULL, sma=FALSE,
          max.p=NULL, auto.ar=FALSE, n.predict=20, tol=10^(-6),
          max.fcal=2000, iter=FALSE, innov.outlier=FALSE, critv=NULL, ...)



