# Covid passport project
library(forecast)
library(segmented)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(MASS)
library(car)
library(bo)


# PART 1: Cases
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Data")
Cases_UK <- read.csv("Cases_UK_jan19.csv")
Cases_UK <- as_tibble(Cases_UK)
Cases_UK <- Cases_UK[,c(2,4,5)]
Cases_UK <- Cases_UK[!grepl("2020", Cases_UK$date),]
Cases_UK$date <- as.Date(Cases_UK$date)
colnames(Cases_UK)[3] <- "cases"

ggplot(data = Cases_UK, aes(x = date, y = cases, color=factor(areaName))) +  geom_bar(stat="identity") +  labs(title = "Cases in the UK", x = "Date", y = "Cases")

Cases_UK_region <- split(Cases_UK, Cases_UK$areaName)
En_cases <- Cases_UK_region[[1]]
NI_cases <- Cases_UK_region[[2]]
Sc_cases <- Cases_UK_region[[3]]
Wa_cases <- Cases_UK_region[[4]]

Cases_EU <- read.csv("Cases_EU_dc16.csv")
Cases_EU <- as_tibble(Cases_EU)
Cases_EU <- Cases_EU[,c(1,5,7)]
Cases_EU <- Cases_EU[Cases_EU$countriesAndTerritories %in% c("Denmark", "France", "Germany"), ]
Cases_EU <- Cases_EU[!grepl("2020", Cases_EU$dateRep),]
Cases_EU <- Cases_EU[!grepl("01/03/2021", Cases_EU$dateRep),]
Cases_EU$dateRep <- as.Date(Cases_EU$dateRep,format="%d/%m/%y")
colnames(Cases_EU)[1] <- "date"

ggplot(data = Cases_EU, aes(x = date, y = cases)) +  geom_bar(stat="identity") +  labs(title = "Cases in the EU", x = "Date", y = "Cases") + facet_wrap(~countriesAndTerritories, scale="free")

Cases_EU_region <- split(Cases_EU, Cases_EU$countriesAndTerritories)
Dk_cases <- Cases_EU_region[[1]]
Fr_cases <- Cases_EU_region[[2]]
De_cases <- Cases_EU_region[[3]]

# change the nom][[1]] thing all the same
smooth_7 <- function(dataset,nom) {
  smoothed <- dataset
  smoothed[,nom][[1]][1] <- (dataset[,nom][[1]][1]+dataset[,nom][[1]][2]+dataset[,nom][[1]][3]+dataset[,nom][[1]][4])/4
  smoothed[,nom][[1]][2] <- (dataset[,nom][[1]][1]+dataset[,nom][[1]][2]+dataset[,nom][[1]][3]+dataset[,nom][[1]][4]+dataset[,nom][[1]][5])/5
  smoothed[,nom][[1]][3] <- (dataset[,nom][[1]][1]+dataset[,nom][[1]][2]+dataset[,nom][[1]][3]+dataset[,nom][[1]][4]+dataset[,nom][[1]][5]+dataset[,nom][[1]][6])/6
  for(i in 4:(nrow(dataset)-3)) {
    smoothed[,nom][[1]][i] <- (dataset[,nom][[1]][i-3]+dataset[,nom][[1]][i-2]+dataset[,nom][[1]][i-1]+dataset[,nom][[1]][i]+dataset[,nom][[1]][i+1]+dataset[,nom][[1]][i+2]+dataset[,nom][[1]][i+3])/7
  }
  smoothed[,nom][[1]][nrow(dataset)] <- (dataset[,nom][[1]][nrow(dataset)-3]+dataset[,nom][[1]][nrow(dataset)-2]+dataset[,nom][[1]][nrow(dataset)-1]+dataset[,nom][[1]][nrow(dataset)])/4
  smoothed[,nom][[1]][nrow(dataset)-1] <- (dataset[,nom][[1]][nrow(dataset)-4]+dataset[,nom][[1]][nrow(dataset)-3]+dataset[,nom][[1]][nrow(dataset)-2]+dataset[,nom][[1]][nrow(dataset)-1]+dataset[,nom][[1]][nrow(dataset)])/5
  smoothed[,nom][[1]][nrow(dataset)-2] <- (dataset[,nom][[1]][nrow(dataset)-5]+dataset[,nom][[1]][nrow(dataset)-4]+dataset[,nom][[1]][nrow(dataset)-3]+dataset[,nom][[1]][nrow(dataset)-2]+dataset[,nom][[1]][nrow(dataset)-1]+dataset[,nom][[1]][nrow(dataset)])/6
  return(smoothed)
}

# Plots all countries cases + interventions

# Covid pass in Denmark: level 1 at 06/04 and level 2 at 08/07 and level 0 again at 10/09 and level 2 again at 12/11 and level 3 at 26/11
Dk_cases$cases[268] <- 0
Dk_cases_smooth <- smooth_7(Dk_cases,"cases")
colnames(Dk_cases_smooth)[2] <- "smooth_cases"
ggplot(data = Dk_cases, aes(x = date, y = cases)) +  geom_line() +  labs(title = "Cases in Denmark", x = "Date", y = "Cases")
Dk_smooth_plot <- ggplot(data = Dk_cases_smooth, aes(x = date, y = smooth_cases)) +  geom_line() +  labs(title = "Cases in Denmark", x = "Date", y = "Cases")+ geom_vline(xintercept = as.numeric(as.Date("2020-04-06")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2020-04-11")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2020-07-08")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2020-07-13")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2020-09-10")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2020-09-15")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2020-11-12")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2020-11-17")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2020-11-26")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2020-12-01")),color="red")

# Covid pass in Germany: level 1 at 03/08 and level 2 at23/08
# Unvaccinated restrictions in Germany: 02/12
De_cases_smooth <- smooth_7(De_cases,"cases")
colnames(De_cases_smooth)[2] <- "smooth_cases"
ggplot(data = De_cases, aes(x = date, y = cases)) +  geom_line() +  labs(title = "Cases in Germany", x = "Date", y = "Cases")
De_smooth_plot <- ggplot(data = De_cases_smooth, aes(x = date, y = smooth_cases)) +  geom_line() +  labs(title = "Cases in Germany", x = "Date", y = "Cases")+ geom_vline(xintercept = as.numeric(as.Date("2020-08-23")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2020-08-28")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2020-11-18")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2020-11-23")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2020-12-02")),color="blue")+ geom_vline(xintercept = as.numeric(as.Date("2020-12-07")),color="blue")

# Covid pass in France: level 1 at 21/07 and level 2 at 09/08 and level 3 at 30/08
Fr_cases_smooth <- smooth_7(Fr_cases,"cases")
colnames(Fr_cases_smooth)[2] <- "smooth_cases"
ggplot(data = Fr_cases, aes(x = date, y = cases)) +  geom_line() +  labs(title = "Cases in France", x = "Date", y = "Cases")
Fr_smooth_plot <- ggplot(data = Fr_cases_smooth, aes(x = date, y = smooth_cases)) +  geom_line() +  labs(title = "Cases in France", x = "Date", y = "Cases")+ geom_vline(xintercept = as.numeric(as.Date("2020-07-21")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2020-07-26")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2020-08-09")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2020-08-14")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2020-08-30")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2020-09-04")),color="red")

# Covid pass in Wales: level 1 at 11/10 and level 2 at 15/11
# Circuit breaker in Wales: from 23/10 to 09/11
Wa_cases_smooth <- smooth_7(Wa_cases,"cases")
colnames(Wa_cases_smooth)[3] <- "smooth_cases"
ggplot(data = Wa_cases, aes(x = date, y = cases)) +  geom_line() +  labs(title = "Cases in Wales", x = "Date", y = "Cases")
Wa_smooth_plot <- ggplot(data = Wa_cases_smooth, aes(x = date, y = smooth_cases)) +  geom_line() +  labs(title = "Cases in Wales", x = "Date", y = "Cases")+ geom_vline(xintercept = as.numeric(as.Date("2021-10-11")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-10-15")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-11-15")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-11-20")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-10-23")),color="blue")+ geom_vline(xintercept = as.numeric(as.Date("2021-11-09")),color="blue")

# Covid pass in NI: level 1 at 29/11 and level 2 at 13/12
NI_cases_smooth <- smooth_7(NI_cases,"cases")
colnames(NI_cases_smooth)[3] <- "smooth_cases"
ggplot(data = NI_cases, aes(x = date, y = cases)) +  geom_line() +  labs(title = "Cases in Northern Ireland", x = "Date", y = "Cases")
NI_smooth_plot <- ggplot(data = NI_cases_smooth, aes(x = date, y = smooth_cases)) +  geom_line() +  labs(title = "Cases in Northern Ireland", x = "Date", y = "Cases")+ geom_vline(xintercept = as.numeric(as.Date("2021-11-01")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-11-06")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-11-29")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-12-04")),color="red")

# Covid pass in Scotland: level 1 at 18/10
Sc_cases_smooth <- smooth_7(Sc_cases,"cases")
colnames(Sc_cases_smooth)[3] <- "smooth_cases"
ggplot(data = Sc_cases, aes(x = date, y = cases)) +  geom_line() +  labs(title = "Cases in Scotland", x = "Date", y = "Cases")
Sc_smooth_plot <- ggplot(data = Sc_cases_smooth, aes(x = date, y = smooth_cases)) +  geom_line() +  labs(title = "Cases in Scotland", x = "Date", y = "Cases")+ geom_vline(xintercept = as.numeric(as.Date("2021-10-18")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-10-23")),color="red")

# Covid pass in England: level 1 at 15/12
En_cases_smooth <- smooth_7(En_cases,"cases")
colnames(En_cases_smooth)[3] <- "smooth_cases"
ggplot(data = En_cases, aes(x = date, y = cases)) +  geom_line() +  labs(title = "Cases in England", x = "Date", y = "Cases")
En_smooth_plot <- ggplot(data = En_cases_smooth, aes(x = date, y = smooth_cases)) +  geom_line() +  labs(title = "Cases in England", x = "Date", y = "Cases")+ geom_vline(xintercept = as.numeric(as.Date("2021-10-18")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-10-23")),color="red")

grid.arrange(Dk_smooth_plot, De_smooth_plot, Fr_smooth_plot , Wa_smooth_plot, NI_smooth_plot, Sc_smooth_plot,En_smooth_plot, ncol=3)

NBreg <- function(dataset,intervention_row,intervention_date,lag_date,country,number,output,lag,dummy) {
  setwd(paste0("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/",country,"/",output))
  dir.create(paste0("CP",number))
  setwd(paste0("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/",country,"/",output,"/CP",number))
  n <- nrow(dataset)
  # dummy accounts for the order in reverse of dates in some datasets. if dummy=0, the order is inversed
  if(dummy ==0) {
    CP_int <- c(rep(1,(intervention_row-lag)),rep(NA,lag),rep(0,(n-intervention_row)))
    CP_t2 <- c(c((intervention_row-lag):1),rep(NA,lag),rep(0,(n-intervention_row)))
    CP_t1 <- c((n-1):(n-intervention_row+lag),rep(NA,lag),c((n-intervention_row-1):0))
  }
  else {
    CP_int <- c(rep(0,(intervention_row-1)),rep(NA,lag),rep(1,(n-intervention_row-lag+1)))
    CP_t2 <- c(rep(0,intervention_row-1),rep(NA,lag),c(1:(n-intervention_row-lag+1)))
    CP_t1 <- c(c(0:(intervention_row-2)),rep(NA,lag),c((intervention_row+lag-1):(n-1)))
  }
  g <- ggplot(data = dataset, aes(x = date, y = get(output))) +  geom_line() +  labs(title = paste0(output," in ",country), x = "Date", y = output)+ geom_vline(xintercept = as.numeric(as.Date(intervention_date)),color="red")+ geom_vline(xintercept = as.numeric(as.Date(lag_date)),color="red")

  dataset <- cbind(dataset,CP_int,CP_t1,CP_t2)
  m <- glm.nb(dataset[,output] ~ CP_int + CP_t1 + CP_t2, data = dataset)
  capture.output(dataset,file="glm_output.txt",append=TRUE)
  capture.output(summary(m), file="glm_output.txt", append=TRUE)
  est <- cbind(Estimate=coef(m),confint(m))
  capture.output(est, file="glm_output.txt", append=TRUE)
  capture.output(exp(est), file="glm_output.txt", append=TRUE)
  capture.output(durbinWatsonTest(m),file="glm_output.txt",append=TRUE)
  
  pred <- exp(est[1,1]+est[2,1]*CP_int+est[3,1]*CP_t1+est[4,1]*CP_t2)
  g2 <- g+geom_line(aes(y=pred),color="blue")
  
  pred2_wout <- exp(est[1,1]+est[3,1]*CP_t1)
  g3 <- g+geom_line(aes(y=pred2_wout),color="green")
  
  jpeg(paste0(country," images_CP",number,".jpg"))
  grid.arrange(grobs=list(g,g2,g3))
  dev.off()
  #columm <- ncol(dataset)
  #write(t(dataset),file=paste0(country,"_",output,"_CP_",number),ncolumns=columm)
  
  return(m)
}

# GERMANY SR analysis cases
# Cut the time series at monotone intervals
De_cases_smooth_CP1 <- De_cases_smooth[c(103:170),]
De_cases_CP1 <- De_cases[c(103:170),]
model_De1 <- NBreg(De_cases_smooth_CP1,15,"2020-08-23","2020-08-28","Germany",1,"smooth_cases",5,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Germany/smooth_cases/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_De1)
dev.off()
model_De1 <- NBreg(De_cases_CP1,15,"2020-08-23","2020-08-28","Germany",1,"cases",5,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Germany/cases/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_De1)
dev.off()

De_cases_smooth_CP2 <- De_cases_smooth[c(3:85),]
De_cases_CP2 <- De_cases[c(3:85),]
model_De2 <- NBreg(De_cases_smooth_CP2,28,"2020-11-18","2020-11-23","Germany",2,"smooth_cases",5,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Germany/smooth_cases/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_De2)
dev.off()
model_De2 <- NBreg(De_cases_CP2,28,"2020-11-18","2020-11-23","Germany",2,"cases",5,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Germany/cases/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_De2)
dev.off()
# To be sure to use negative binomial: variance exceeds the mean
mean(De_cases_smooth_CP2$smooth_cases)
var(De_cases_smooth_CP2$smooth_cases)


# FRANCE SR analysis cases
Fr_cases_smooth_CP1 <- Fr_cases_smooth[c(131:170),]
Fr_cases_CP1 <- Fr_cases[c(131:170),]
model_Fr1 <- NBreg(Fr_cases_smooth_CP1,20,"2020-07-21","2020-07-26","France",1,"smooth_cases",5,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/France/smooth_cases/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_Fr1)
dev.off()
model_Fr1 <- NBreg(Fr_cases_CP1,20,"2020-07-21","2020-07-26","France",1,"cases",5,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/France/cases/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_Fr1)
dev.off()

Fr_cases_smooth_CP2 <- Fr_cases_smooth[c(110:150),]
Fr_cases_CP2 <- Fr_cases[c(110:150),]
model_Fr2 <- NBreg(Fr_cases_smooth_CP2,22,"2020-08-09","2020-08-14","France",2,"smooth_cases",5,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/France/smooth_cases/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_Fr2)
dev.off()
model_Fr2 <- NBreg(Fr_cases_CP2,22,"2020-08-09","2020-08-14","France",2,"cases",5,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/France/cases/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_Fr2)
dev.off()

Fr_cases_smooth_CP3 <- Fr_cases_smooth[c(78:131),]
Fr_cases_CP3 <- Fr_cases[c(78:131),]
model_Fr3 <- NBreg(Fr_cases_smooth_CP3,31,"2020-08-30","2020-09-04","France",3,"smooth_cases",5,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/France/smooth_cases/CP3")
jpeg("Model_NBreg_residuals_CP3")
par(mfrow=c(2,2))
plot(model_Fr3)
dev.off()
model_Fr3 <- NBreg(Fr_cases_CP3,31,"2020-08-30","2020-09-04","France",3,"cases",5,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/France/cases/CP3")
jpeg("Model_NBreg_residuals_CP3")
par(mfrow=c(2,2))
plot(model_Fr3)
dev.off()

# DENMARK SR analysis cases
# 06/04, 07/08, 10/09, 12/11, 26/11
Dk_cases_smooth_CP1 <- Dk_cases_smooth[c(241:267),]
Dk_cases_CP1 <- Dk_cases[c(241:267),]
model_Dk1 <- NBreg(Dk_cases_smooth_CP1,16,"2020-04-06","2020-04-11","Denmark",1,"smooth_cases",5,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/smooth_cases/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_Dk1)
dev.off()
model_Dk1 <- NBreg(Dk_cases_CP1,16,"2020-04-06","2020-04-11","Denmark",1,"cases",5,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/cases/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_Dk1)
dev.off()

Dk_cases_smooth_CP2 <- Dk_cases_smooth[c(147:179),]
Dk_cases_CP2 <- Dk_cases[c(147:179),]
model_Dk2 <- NBreg(Dk_cases_smooth_CP2,17,"2020-07-08","2020-07-13","Denmark",2,"smooth_cases",5,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/smooth_cases/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_Dk2)
dev.off()
model_Dk2 <- NBreg(Dk_cases_CP2,17,"2020-07-08","2020-07-13","Denmark",2,"cases",5,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/cases/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_Dk2)
dev.off()

Dk_cases_smooth_CP3 <- Dk_cases_smooth[c(47:130),]
Dk_cases_CP3 <- Dk_cases[c(47:130),]
model_Dk3 <- NBreg(Dk_cases_smooth_CP3,52,"2020-09-10","2020-09-15","Denmark",3,"smooth_cases",5,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/smooth_cases/CP3")
jpeg("Model_NBreg_residuals_CP3")
par(mfrow=c(2,2))
plot(model_Dk3)
dev.off()
model_Dk3 <- NBreg(Dk_cases_CP3,52,"2020-09-10","2020-09-15","Denmark",3,"cases",5,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/cases/CP3")
jpeg("Model_NBreg_residuals_CP3")
par(mfrow=c(2,2))
plot(model_Dk3)
dev.off()

Dk_cases_smooth_CP4 <- Dk_cases_smooth[c(20:60),]
Dk_cases_CP4 <- Dk_cases[c(20:60),]
model_Dk4 <- NBreg(Dk_cases_smooth_CP4,17,"2020-11-12","2020-11-17","Denmark",4,"smooth_cases",5,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/smooth_cases/CP4")
jpeg("Model_NBreg_residuals_CP4")
par(mfrow=c(2,2))
plot(model_Dk4)
dev.off()
model_Dk4 <- NBreg(Dk_cases_CP4,17,"2020-11-12","2020-11-17","Denmark",4,"cases",5,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/cases/CP4")
jpeg("Model_NBreg_residuals_CP4")
par(mfrow=c(2,2))
plot(model_Dk4)
dev.off()

Dk_cases_smooth_CP5 <- Dk_cases_smooth[c(1:35),]
Dk_cases_CP5 <- Dk_cases[c(1:35),]
model_Dk5 <- NBreg(Dk_cases_smooth_CP5,22,"2020-11-26","2020-12-01","Denmark",5,"smooth_cases",5,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/smooth_cases/CP5")
jpeg("Model_NBreg_residuals_CP5")
par(mfrow=c(2,2))
plot(model_Dk5)
dev.off()
model_Dk5 <- NBreg(Dk_cases_CP5,22,"2020-11-26","2020-12-01","Denmark",5,"cases",5,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/cases/CP5")
jpeg("Model_NBreg_residuals_CP5")
par(mfrow=c(2,2))
plot(model_Dk5)
dev.off()

# WALES SR analysis cases
# Covid pass in Wales: level 1 at 11/10 and level 2 at 15/11
Wa_cases_smooth_CP1 <- Wa_cases_smooth[c(74:111),]
Wa_cases_CP1 <- Wa_cases[c(74:111),]
model_Wa1 <- NBreg(Wa_cases_smooth_CP1,27,"2021-10-11","2021-10-16","Wales",1,"smooth_cases",5,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Wales/smooth_cases/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_Wa1)
dev.off()
model_Wa1 <- NBreg(Wa_cases_CP1,27,"2021-10-11","2021-10-16","Wales",1,"cases",5,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Wales/cases/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_Wa1)
dev.off()

Wa_cases_smooth_CP2 <- Wa_cases_smooth[c(51:75),]
Wa_cases_CP2 <- Wa_cases[c(51:75),]
model_Wa2 <- NBreg(Wa_cases_smooth_CP2,15,"2021-11-15","2021-11-20","Wales",2,"smooth_cases",5,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Wales/smooth_cases/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_Wa2)
dev.off()
model_Wa2 <- NBreg(Wa_cases_CP2,15,"2021-11-15","2021-11-20","Wales",2,"cases",5,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Wales/cases/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_Wa2)
dev.off()

# Covid pass in NI: level 1 at 29/11 and level 2 at 13/12
NI_cases_smooth_CP1 <- NI_cases_smooth[c(37:77),]
NI_cases_CP1 <- NI_cases[c(37:77),]
model_NI1 <- NBreg(NI_cases_smooth_CP1,15,"2021-11-29","2021-12-04","NI",1,"smooth_cases",5,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/NI/smooth_cases/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_NI1)
dev.off()
model_NI1 <- NBreg(NI_cases_CP1,15,"2021-11-29","2021-12-04","NI",1,"cases",5,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/NI/cases/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_NI1)
dev.off()

# Covid pass in Scotland: level 1 at 18/10
Sc_cases_smooth_CP1 <- Sc_cases_smooth[c(79:103),]
Sc_cases_CP1 <- Sc_cases[c(79:103),]
model_Sc1 <- NBreg(Sc_cases_smooth_CP1,15,"2021-10-18","2021-10-23","Scotland",1,"smooth_cases",5,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Scotland/smooth_cases/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_Sc1)
dev.off()
model_Sc1 <- NBreg(Sc_cases_CP1,15,"2021-10-18","2021-10-23","Scotland",1,"cases",5,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Scotland/cases/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_Sc1)
dev.off()

# Covid pass in England: level 1 at 15/12
En_cases_smooth_CP1 <- En_cases_smooth[c(12:70),]
En_cases_CP1 <- En_cases[c(12:70),]
model_En1 <- NBreg(En_cases_smooth_CP1,24,"2021-12-15","2021-12-20","England",1,"smooth_cases",5,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/England/smooth_cases/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_En1)
dev.off()
model_En1 <- NBreg(En_cases_CP1,24,"2021-12-15","2021-12-20","England",1,"cases",5,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/England/cases/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_En1)
dev.off()

# One that was pretty good was NICP1, and it's one of the farthest away from England 
# so I will try to do the same for England at the same time interval, when there was no Covid Passport
En_cases_smooth_CP0 <- En_cases_smooth[c(37:77),]
En_cases_CP0 <- En_cases[c(37:77),]
model_En0 <- NBreg(En_cases_smooth_CP0,15,"2021-11-29","2021-12-04","England",0,"smooth_cases",5,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/England/smooth_cases/CP0")
jpeg("Model_NBreg_residuals_CP0")
par(mfrow=c(2,2))
plot(model_En0)
dev.off()
model_En0 <- NBreg(En_cases_CP0,15,"2021-11-29","2021-12-04","England",0,"cases",5,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/England/cases/CP0")
jpeg("Model_NBreg_residuals_CP0")
par(mfrow=c(2,2))
plot(model_En0)
dev.off()

# PART 2: Hospital admissions
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Data")
Hosp_EU <- read.csv("Hosp_EU_jan4.csv")
Hosp_EU <- as_tibble(Hosp_EU)
Hosp_EU <- Hosp_EU[,c(1,2,3,5)]
Hosp_EU <- Hosp_EU[Hosp_EU$country %in% c("Denmark", "France", "Germany"), ]
Hosp_EU <- Hosp_EU[!grepl("2020", Hosp_EU$date),]
Hosp_EU <- Hosp_EU[!grepl("ICU", Hosp_EU$indicator),]
Hosp_EU$date <- as.Date(Hosp_EU$date)
colnames(Hosp_EU)[4] <- "admissions"

ggplot(data = Hosp_EU, aes(x = date, y = admissions)) +  geom_bar(stat="identity") +  labs(title = "Hospital admissions in the EU", x = "Date", y = "Admissions") + facet_wrap(~country, scale="free")

Hosp_EU_region <- split(Hosp_EU, Hosp_EU$country)
Dk_hosp <- Hosp_EU_region[[1]]
Dk_hosp <- Dk_hosp[!grepl("Weekly", Dk_hosp$indicator),]
Fr_hosp <- Hosp_EU_region[[2]]
Fr_hosp <- Fr_hosp[!grepl("Weekly", Fr_hosp$indicator),]
De_hosp <- Hosp_EU_region[[3]]

setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Data")
Hosp_UK <- read.csv("Hosp_UK_jan19.csv")
Hosp_UK <- as_tibble(Hosp_UK)
Hosp_UK <- Hosp_UK[,c(2,4,5)]
Hosp_UK <- Hosp_UK[!grepl("2020", Hosp_UK$date),]
Hosp_UK$date <- as.Date(Hosp_UK$date)
colnames(Hosp_UK)[3] <- "admissions"

Hosp_UK_region <- split(Hosp_UK, Hosp_UK$areaName)
En_hosp <- Hosp_UK_region[[1]]
NI_hosp <- Hosp_UK_region[[2]]
Sc_hosp <- Hosp_UK_region[[3]]
Wa_hosp <- Hosp_UK_region[[4]]

Dk_hosp_smooth <- smooth_7(Dk_hosp,"admissions")
colnames(Dk_hosp_smooth)[4] <- "smooth_admissions"
ggplot(data = Dk_hosp, aes(x = date, y = admissions)) +  geom_line() +  labs(title = "Hospital admissions in Denmark", x = "Date", y = "Admissions")
Dk_hosp_smooth_plot <- ggplot(data = Dk_hosp_smooth, aes(x = date, y = smooth_admissions)) +  geom_line() +  labs(title = "Hospital admissions in Denmark", x = "Date", y = "Admissions")+ geom_vline(xintercept = as.numeric(as.Date("2021-04-06")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-04-13")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-07-08")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-07-15")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-09-10")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-09-17")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-11-12")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-11-19")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-11-26")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-12-03")),color="red")

Fr_hosp_smooth <- smooth_7(Fr_hosp,"admissions")
colnames(Fr_hosp_smooth)[4] <- "smooth_admissions"
ggplot(data = Fr_hosp, aes(x = date, y = admissions)) +  geom_line() +  labs(title = "Hospital admissions in France", x = "Date", y = "Admissions")
Fr_hosp_smooth_plot <- ggplot(data = Fr_hosp_smooth, aes(x = date, y = smooth_admissions)) +  geom_line() +  labs(title = "Hospital admissions in France", x = "Date", y = "Admissions")+ geom_vline(xintercept = as.numeric(as.Date("2021-07-21")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-07-28")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-08-09")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-08-16")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-08-30")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-09-06")),color="red")

De_hosp_smooth <- De_hosp
colnames(De_hosp_smooth)[4] <- "smooth_admissions"
ggplot(data = De_hosp, aes(x = date, y = admissions)) +  geom_line() +  labs(title = "Hospital admissions in Germany", x = "Date", y = "Admissions")
De_hosp_smooth_plot <- ggplot(data = De_hosp, aes(x = date, y = smooth_admissions)) +  geom_line() +  labs(title = "Hospital admissions in Germany", x = "Date", y = "Admissions")+ geom_vline(xintercept = as.numeric(as.Date("2021-08-23")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-08-30")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-11-18")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-11-25")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-12-02")),color="blue")+ geom_vline(xintercept = as.numeric(as.Date("2021-12-09")),color="blue")

Wa_hosp_smooth <- smooth_7(Wa_hosp,"admissions")
colnames(Wa_hosp_smooth)[3] <- "smooth_admissions"

NI_hosp_smooth <- smooth_7(NI_hosp,"admissions")
colnames(NI_hosp_smooth)[3] <- "smooth_admissions"

Sc_hosp_smooth <- smooth_7(Sc_hosp,"admissions")
colnames(Sc_hosp_smooth)[3] <- "smooth_admissions"

En_hosp_smooth <- smooth_7(En_hosp,"admissions")
colnames(En_hosp_smooth)[3] <- "smooth_admissions"

grid.arrange(Dk_hosp_smooth_plot, De_hosp_smooth_plot, Fr_hosp_smooth_plot, ncol=3)

# GERMANY SR analysis admissions
De_hosp_smooth_CP1 <- De_hosp_smooth[c(27:37),]
De_hosp_CP1 <- De_hosp[c(27:37),]
model_hosp_De1 <- NBreg(De_hosp_smooth_CP1,8,"2021-08-23","2021-08-30","Germany",1,"smooth_admissions",1,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Germany/smooth_admissions/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_hosp_De1)
dev.off()
model_hosp_De1 <- NBreg(De_hosp_CP1,8,"2021-08-23","2021-08-30","Germany",1,"admissions",1,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Germany/admissions/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_hosp_De1)
dev.off()

De_hosp_smooth_CP2 <- De_hosp_smooth[c(40:49),]
De_hosp_CP2 <- De_hosp[c(40:49),]
model_hosp_De2 <- NBreg(De_hosp_smooth_CP2,7,"2021-11-18","2021-11-25","Germany",2,"smooth_admissions",1,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Germany/smooth_admissions/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_hosp_De2)
dev.off()
model_hosp_De2 <- NBreg(De_hosp_CP2,7,"2021-11-18","2021-11-25","Germany",2,"admissions",1,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Germany/admissions/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_hosp_De2)
dev.off()
# To be sure to use negative binomial: variance exceeds the mean
mean(De_hosp_smooth_CP2$smooth_admissions)
var(De_hosp_smooth_CP2$smooth_admissions)

# FRANCE SR analysis admissions
Fr_hosp_smooth_CP1 <- Fr_hosp_smooth[c(184:221),]
Fr_hosp_CP1 <- Fr_hosp[c(184:221),]
model_hosp_Fr1 <- NBreg(Fr_hosp_smooth_CP1,19,"2021-07-21","2021-07-28","France",1,"smooth_admissions",7,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/France/smooth_admissions/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_hosp_Fr1)
dev.off()
model_hosp_Fr1 <- NBreg(Fr_hosp_CP1,19,"2021-07-21","2021-07-28","France",1,"admissions",7,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/France/admissions/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_hosp_Fr1)
dev.off()

Fr_hosp_smooth_CP2 <- Fr_hosp_smooth[c(207:242),]
Fr_hosp_CP2 <- Fr_hosp[c(207:242),]
model_hosp_Fr2 <- NBreg(Fr_hosp_smooth_CP2,15,"2021-08-09","2021-08-16","France",2,"smooth_admissions",7,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/France/smooth_admissions/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_hosp_Fr2)
dev.off()
model_hosp_Fr2 <- NBreg(Fr_hosp_CP2,15,"2021-08-09","2021-08-16","France",2,"admissions",7,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/France/admissions/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_hosp_Fr2)
dev.off()

Fr_hosp_smooth_CP3 <- Fr_hosp_smooth[c(226:276),]
Fr_hosp_CP3 <- Fr_hosp[c(226:276),]
model_hosp_Fr3 <- NBreg(Fr_hosp_smooth_CP3,17,"2021-08-30","2021-09-06","France",3,"smooth_admissions",7,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/France/smooth_admissions/CP3")
jpeg("Model_NBreg_residuals_CP3")
par(mfrow=c(2,2))
plot(model_hosp_Fr3)
dev.off()
model_hosp_Fr3 <- NBreg(Fr_hosp_CP3,17,"2021-08-30","2021-09-06","France",3,"admissions",7,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/France/admissions/CP3")
jpeg("Model_NBreg_residuals_CP3")
par(mfrow=c(2,2))
plot(model_hosp_Fr3)
dev.off()

# DENMARK SR analysis admissions
# 06/04, 07/08, 10/09, 12/11, 26/11
Dk_hosp_smooth_CP1 <- Dk_hosp_smooth[c(80:129),]
Dk_hosp_CP1 <- Dk_hosp[c(80:129),]
model_hosp_Dk1 <- NBreg(Dk_hosp_smooth_CP1,17,"2021-04-06","2021-04-13","Denmark",1,"smooth_admissions",7,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/smooth_admissions/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_hosp_Dk1)
dev.off()
model_hosp_Dk1 <- NBreg(Dk_hosp_CP1,17,"2021-04-06","2021-04-13","Denmark",1,"admissions",7,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/admissions/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_hosp_Dk1)
dev.off()

Dk_hosp_smooth_CP2 <- Dk_hosp_smooth[c(150:243),]
Dk_hosp_CP2 <- Dk_hosp[c(150:243),]
model_hosp_Dk2 <- NBreg(Dk_hosp_smooth_CP2,40,"2021-07-08","2021-07-15","Denmark",2,"smooth_admissions",7,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/smooth_admissions/CP2")
jpeg("Model_NBreg_residuals_CP3")
par(mfrow=c(2,2))
plot(model_hosp_Dk2)
dev.off()
model_hosp_Dk2 <- NBreg(Dk_hosp_CP2,40,"2021-07-08","2021-07-15","Denmark",2,"admissions",7,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/admissions/CP2")
jpeg("Model_NBreg_residuals_CP3")
par(mfrow=c(2,2))
plot(model_hosp_Dk2)
dev.off()

Dk_hosp_smooth_CP3 <- Dk_hosp_smooth[c(243:270),]
Dk_hosp_CP3 <- Dk_hosp[c(243:270),]
model_hosp_Dk3 <- NBreg(Dk_hosp_smooth_CP3,11,"2021-09-10","2021-09-17","Denmark",3,"smooth_admissions",7,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/smooth_admissions/CP3")
jpeg("Model_NBreg_residuals_CP3")
par(mfrow=c(2,2))
plot(model_hosp_Dk3)
dev.off()
model_hosp_Dk3 <- NBreg(Dk_hosp_CP3,11,"2021-09-10","2021-09-17","Denmark",3,"admissions",7,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/admissions/CP3")
jpeg("Model_NBreg_residuals_CP3")
par(mfrow=c(2,2))
plot(model_hosp_Dk3)
dev.off()

Dk_hosp_smooth_CP4 <- Dk_hosp_smooth[c(300:333),]
Dk_hosp_CP4 <- Dk_hosp[c(300:333),]
model_hosp_Dk4 <- NBreg(Dk_hosp_smooth_CP4,17,"2021-11-12","2021-11-19","Denmark",4,"smooth_admissions",7,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/smooth_admissions/CP4")
jpeg("Model_NBreg_residuals_CP4")
par(mfrow=c(2,2))
plot(model_hosp_Dk4)
dev.off()
model_hosp_Dk4 <- NBreg(Dk_hosp_CP4,17,"2021-11-12","2021-11-19","Denmark",4,"admissions",7,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/admissions/CP4")
jpeg("Model_NBreg_residuals_CP4")
par(mfrow=c(2,2))
plot(model_hosp_Dk4)
dev.off()

Dk_hosp_smooth_CP5 <- Dk_hosp_smooth[c(320:350),]
Dk_hosp_CP5 <- Dk_hosp[c(320:350),]
model_hosp_Dk5 <- NBreg(Dk_hosp_smooth_CP5,11,"2021-11-26","2021-12-03","Denmark",5,"smooth_admissions",7,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/smooth_admissions/CP5")
jpeg("Model_NBreg_residuals_CP5")
par(mfrow=c(2,2))
plot(model_hosp_Dk5)
dev.off()
model_hosp_Dk5 <- NBreg(Dk_hosp_CP5,11,"2021-11-26","2021-12-03","Denmark",5,"admissions",7,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/admissions/CP5")
jpeg("Model_NBreg_residuals_CP5")
par(mfrow=c(2,2))
plot(model_hosp_Dk5)
dev.off()

# WALES SR analysis admissions
# Covid pass in Wales: level 1 at 11/10 and level 2 at 15/11
Wa_hosp_smooth_CP1 <- Wa_hosp_smooth[c(83:110),]
Wa_hosp_CP1 <- Wa_hosp[c(83:110),]
model_Wa1 <- NBreg(Wa_hosp_smooth_CP1,18,"2021-10-11","2021-10-18","Wales",1,"smooth_admissions",7,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Wales/smooth_admissions/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_Wa1)
dev.off()
model_Wa1 <- NBreg(Wa_hosp_CP1,18,"2021-10-11","2021-10-18","Wales",1,"admissions",7,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Wales/admissions/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_Wa1)
dev.off()

Wa_hosp_smooth_CP2 <- Wa_hosp_smooth[c(40:85),]
Wa_hosp_CP2 <- Wa_hosp[c(40:85),]
model_Wa2 <- NBreg(Wa_hosp_smooth_CP2,26,"2021-11-15","2021-11-22","Wales",2,"smooth_admissions",7,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Wales/smooth_admissions/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_Wa2)
dev.off()
model_Wa2 <- NBreg(Wa_hosp_CP2,26,"2021-11-15","2021-11-22","Wales",2,"admissions",7,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Wales/admissions/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_Wa2)
dev.off()

# Covid pass in NI: level 1 at 29/11 
NI_hosp_smooth_CP1 <- NI_hosp_smooth[c(30:77),]
NI_hosp_CP1 <- NI_hosp[c(30:77),]
model_NI1 <- NBreg(NI_hosp_smooth_CP1,22,"2021-11-29","2021-12-06","NI",1,"smooth_admissions",7,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/NI/smooth_admissions/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_NI1)
dev.off()
model_NI1 <- NBreg(NI_hosp_CP1,22,"2021-11-29","2021-12-06","NI",1,"admissions",7,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/NI/admissions/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_NI1)
dev.off()

# Covid pass in Scotland: level 1 at 18/10
Sc_hosp_smooth_CP1 <- Sc_hosp_smooth[c(74:100),]
Sc_hosp_CP1 <- Sc_hosp[c(74:100),]
model_Sc1 <- NBreg(Sc_hosp_smooth_CP1,17,"2021-10-18","2021-10-25","Scotland",1,"smooth_admissions",7,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Scotland/smooth_admissions/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_Sc1)
dev.off()
model_Sc1 <- NBreg(Sc_hosp_CP1,17,"2021-10-18","2021-10-25","Scotland",1,"admissions",7,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Scotland/admissions/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_Sc1)
dev.off()

# Covid pass in England: level 1 at 15/12
En_hosp_smooth_CP1 <- En_hosp_smooth[c(5:65),]
En_hosp_CP1 <- En_hosp[c(5:65),]
model_En1 <- NBreg(En_hosp_smooth_CP1,30,"2021-12-15","2021-12-22","England",1,"smooth_admissions",7,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/England/smooth_admissions/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_En1)
dev.off()
model_En1 <- NBreg(En_hosp_CP1,30,"2021-12-15","2021-12-22","England",1,"admissions",7,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/England/admissions/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_En1)
dev.off()


# PART 3: Vaccines (doses)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Data")
Vacc_EU <- read.csv("Vaccines_EU_jan4.csv")
Vacc_EU <- as_tibble(Vacc_EU)
Vacc_EU <- Vacc_EU[,c(1,3,4)]
Vacc_EU <- Vacc_EU[Vacc_EU$Entity %in% c("Denmark", "France", "Germany"), ]
Vacc_EU <- Vacc_EU[!grepl("2020", Vacc_EU$Day),]
Vacc_EU$Day <- as.Date(Vacc_EU$Day)
colnames(Vacc_EU)[2] <- "date"
colnames(Vacc_EU)[3] <- "vaccines"

Vacc_EU_region <- split(Vacc_EU, Vacc_EU$Entity)
Dk_vacc <- Vacc_EU_region[[1]]
Fr_vacc <- Vacc_EU_region[[2]]
De_vacc <- Vacc_EU_region[[3]]

Dk_vacc_plot <- ggplot(data = Dk_vacc, aes(x = date, y = vaccines)) +  geom_line() +  labs(title = "Vaccines in Denmark", x = "Date", y = "Vaccines")+ geom_vline(xintercept = as.numeric(as.Date("2021-04-06")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-07-08")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-09-10")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-11-12")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-11-26")),color="red")
Fr_vacc_plot <- ggplot(data = Fr_vacc, aes(x = date, y = vaccines)) +  geom_line() +  labs(title = "Vaccines in France", x = "Date", y = "Vaccines")+  geom_vline(xintercept = as.numeric(as.Date("2021-07-21")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-08-09")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-08-30")),color="red")
De_vacc_plot <- ggplot(data = De_vacc, aes(x = date, y = vaccines)) +  geom_line() +  labs(title = "Vaccines in Germany", x = "Date", y = "Vaccines")+ geom_vline(xintercept = as.numeric(as.Date("2021-08-23")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-11-18")),color="red")+ geom_vline(xintercept = as.numeric(as.Date("2021-12-02")),color="blue")
grid.arrange(Dk_vacc_plot,Fr_vacc_plot,De_vacc_plot,ncol=3)


# GERMANY SR analysis vaccines
De_vacc_smooth_CP1 <- De_vacc[c(162:307),]
model_vacc_De1 <- NBreg(De_vacc_smooth_CP1,74,"2021-08-23","","Germany",1,"vaccines",0,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Germany/vaccines/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_vacc_De1)
dev.off()

De_vacc_smooth_CP2 <- De_vacc[c(280:335),]
model_vacc_De2 <- NBreg(De_vacc_smooth_CP2,43,"2021-11-18","","Germany",2,"vaccines",0,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Germany/vaccines/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_vacc_De2)
dev.off()

# To be sure to use negative binomial: variance exceeds the mean
mean(De_vacc_smooth_CP2$vaccines)
var(De_vacc_smooth_CP2$vaccines)

# FRANCE SR analysis vaccines
Fr_vacc_smooth_CP1 <- Fr_vacc[c(190:220),]
model_vacc_Fr1 <- NBreg(Fr_vacc_smooth_CP1,13,"2021-07-21","","France",1,"vaccines",0,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/France/vaccines/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_vacc_Fr1)
dev.off()

Fr_vacc_smooth_CP2 <- Fr_vacc[c(203:241),]
model_vacc_Fr2 <- NBreg(Fr_vacc_smooth_CP2,19,"2021-08-09","","France",2,"vaccines",0,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/France/vaccines/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_vacc_Fr2)
dev.off()

Fr_vacc_smooth_CP3 <- Fr_vacc[c(226:270),]
model_vacc_Fr3 <- NBreg(Fr_vacc_smooth_CP3,17,"2021-08-30","","France",3,"vaccines",0,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/France/vaccines/CP3")
jpeg("Model_NBreg_residuals_CP3")
par(mfrow=c(2,2))
plot(model_vacc_Fr3)
dev.off()

# DENMARK SR analysis vaccines
# 06/04, 07/08, 10/09, 12/11, 26/11
Dk_vacc_smooth_CP1 <- Dk_vacc[c(20:70),]
model_vacc_Dk1 <- NBreg(Dk_vacc_smooth_CP1,23,"2021-04-06","","Denmark",1,"vaccines",0,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/vaccines/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_vacc_Dk1)
dev.off()

Dk_vacc_smooth_CP2 <- Dk_vacc[c(110:150),]
model_vacc_Dk2 <- NBreg(Dk_vacc_smooth_CP2,26,"2021-07-08","","Denmark",2,"vaccines",0,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/vaccines/CP2")
jpeg("Model_NBreg_residuals_CP3")
par(mfrow=c(2,2))
plot(model_vacc_Dk2)
dev.off()

Dk_vacc_smooth_CP3 <- Dk_vacc[c(175:235),]
model_vacc_Dk3 <- NBreg(Dk_vacc_smooth_CP3,25,"2021-09-10","","Denmark",3,"vaccines",0,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/vaccines/CP3")
jpeg("Model_NBreg_residuals_CP3")
par(mfrow=c(2,2))
plot(model_vacc_Dk3)

Dk_vacc_smooth_CP4 <- Dk_vacc[c(245:275),]
model_vacc_Dk4 <- NBreg(Dk_vacc_smooth_CP4,18,"2021-11-12","","Denmark",4,"vaccines",0,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/vaccines/CP4")
jpeg("Model_NBreg_residuals_CP4")
par(mfrow=c(2,2))
plot(model_vacc_Dk4)
dev.off()

Dk_vacc_smooth_CP5 <- Dk_vacc[c(262:300),]
model_vacc_Dk5 <- NBreg(Dk_vacc_smooth_CP5,15,"2021-11-26","","Denmark",5,"vaccines",0,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/vaccines/CP5")
jpeg("Model_NBreg_residuals_CP5")
par(mfrow=c(2,2))
plot(model_vacc_Dk5)
dev.off()

# Vaccines UK first dose
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Data")
Vac1_UK <- read.csv("Vaccines1_UK_jan19.csv")
Vac1_UK <- as_tibble(Vac1_UK)
Vac1_UK <- Vac1_UK[,c(2,4,5)]
Vac1_UK <- Vac1_UK[!grepl("2020", Vac1_UK$date),]
Vac1_UK$date <- as.Date(Vac1_UK$date)
colnames(Vac1_UK)[3] <- "vaccines1"

Vac1_UK_region <- split(Vac1_UK, Vac1_UK$areaName)
En_vac1 <- Vac1_UK_region[[1]]
NI_vac1 <- Vac1_UK_region[[2]]
Sc_vac1 <- Vac1_UK_region[[3]]
Wa_vac1 <- Vac1_UK_region[[4]]

Wa_vac1_smooth <- smooth_7(Wa_vac1,"vaccines1")
colnames(Wa_vac1_smooth)[3] <- "smooth_vaccines1"
NI_vac1_smooth <- smooth_7(NI_vac1,"vaccines1")
colnames(NI_vac1_smooth)[3] <- "smooth_vaccines1"
Sc_vac1_smooth <- smooth_7(Sc_vac1,"vaccines1")
colnames(Sc_vac1_smooth)[3] <- "smooth_vaccines1"
En_vac1_smooth <- smooth_7(En_vac1,"vaccines1")
colnames(En_vac1_smooth)[3] <- "smooth_vaccines1"

# WALES SR analysis vaccines first dose
# Covid pass in Wales: level 1 at 11/10 and level 2 at 15/11
Wa_vac1_smooth_CP1 <- Wa_vac1_smooth[c(90:110),]
Wa_vac1_CP1 <- Wa_vac1[c(90:110),]
model_Wa1 <- NBreg(Wa_vac1_smooth_CP1,11,"2021-10-11","","Wales",1,"smooth_vaccines1",0,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Wales/smooth_vaccines1/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_Wa1)
dev.off()
model_Wa1 <- NBreg(Wa_vac1_CP1,11,"2021-10-11","","Wales",1,"vaccines1",0,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Wales/vaccines1/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_Wa1)
dev.off()

Wa_vac1_smooth_CP2 <- Wa_vac1_smooth[c(40:85),]
Wa_vac1_CP2 <- Wa_vac1[c(40:85),]
model_Wa2 <- NBreg(Wa_vac1_smooth_CP2,26,"2021-11-15","","Wales",2,"smooth_vaccines1",0,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Wales/smooth_vaccines1/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_Wa2)
dev.off()
model_Wa2 <- NBreg(Wa_vac1_CP2,26,"2021-11-15","","Wales",2,"vaccines1",0,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Wales/vaccines1/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_Wa2)
dev.off()


# Covid pass in NI: level 1 at 29/11 
NI_vac1_smooth_CP1 <- NI_vac1_smooth[c(30:77),]
NI_vac1_CP1 <- NI_vac1[c(30:77),]
model_NI1 <- NBreg(NI_vac1_smooth_CP1,22,"2021-11-29","","NI",1,"smooth_vaccines1",0,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/NI/smooth_vaccines1/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_Wa1)
dev.off()
model_NI1 <- NBreg(NI_vac1_CP1,22,"2021-11-29","","NI",1,"vaccines1",0,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/NI/vaccines1/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_NI1)
dev.off()

# Covid pass in Scotland: level 1 at 18/10
Sc_vac1_smooth_CP1 <- Sc_vac1_smooth[c(70:107),]
Sc_vac1_CP1 <- Sc_vac1[c(70:107),]
model_Sc1 <- NBreg(Sc_vac1_smooth_CP1,24,"2021-10-18","","Scotland",1,"smooth_vaccines1",0,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Scotland/smooth_vaccines1/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_Sc1)
dev.off()
model_Sc1 <- NBreg(Sc_vac1_CP1,24,"2021-10-18","","Scotland",1,"vaccines1",0,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Scotland/vaccines1/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_Sc1)
dev.off()

# Covid pass in England: level 1 at 15/12
En_vac1_smooth_CP1 <- En_vac1_smooth[c(20:50),]
En_vac1_CP1 <- En_vac1[c(20:50),]
model_En1 <- NBreg(En_vac1_smooth_CP1,16,"2021-12-15","","England",1,"smooth_vaccines1",0,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/England/smooth_vaccines1/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_En1)
dev.off()
model_En1 <- NBreg(En_vac1_CP1,16,"2021-12-15","","England",1,"vaccines1",0,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/England/vaccines1/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_En1)
dev.off()

# Vaccines UK second dose
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Data")
Vac2_UK <- read.csv("Vaccines2_UK_jan19.csv")
Vac2_UK <- as_tibble(Vac2_UK)
Vac2_UK <- Vac2_UK[,c(2,4,5)]
Vac2_UK <- Vac2_UK[!grepl("2020", Vac2_UK$date),]
Vac2_UK$date <- as.Date(Vac2_UK$date)
colnames(Vac2_UK)[3] <- "vaccines2"

Vac2_UK_region <- split(Vac2_UK, Vac2_UK$areaName)
En_vac2 <- Vac2_UK_region[[1]]
NI_vac2 <- Vac2_UK_region[[2]]
Sc_vac2 <- Vac2_UK_region[[3]]
Wa_vac2 <- Vac2_UK_region[[4]]

Wa_vac2_smooth <- smooth_7(Wa_vac2,"vaccines2")
colnames(Wa_vac2_smooth)[3] <- "smooth_vaccines2"
NI_vac2_smooth <- smooth_7(NI_vac2,"vaccines2")
colnames(NI_vac2_smooth)[3] <- "smooth_vaccines2"
Sc_vac2_smooth <- smooth_7(Sc_vac2,"vaccines2")
colnames(Sc_vac2_smooth)[3] <- "smooth_vaccines2"
En_vac2_smooth <- smooth_7(En_vac2,"vaccines2")
colnames(En_vac2_smooth)[3] <- "smooth_vaccines2"

# I AM HERE!!!!!!!!!!
# WALES SR analysis vaccines second dose
# Covid pass in Wales: level 1 at 11/10 and level 2 at 15/11
Wa_vac2_smooth_CP1 <- Wa_vac2_smooth[c(90:110),]
Wa_vac2_CP1 <- Wa_vac2[c(90:110),]
model_Wa1 <- NBreg(Wa_vac2_smooth_CP1,11,"2021-10-11","","Wales",1,"smooth_vaccines2",0,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Wales/smooth_vaccines2/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_Wa1)
dev.off()
model_Wa1 <- NBreg(Wa_vac2_CP1,11,"2021-10-11","","Wales",1,"vaccines2",0,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Wales/vaccines2/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_Wa1)
dev.off()

Wa_vac2_smooth_CP2 <- Wa_vac2_smooth[c(48:85),]
Wa_vac2_CP2 <- Wa_vac2[c(48:85),]
model_Wa2 <- NBreg(Wa_vac2_smooth_CP2,18,"2021-11-15","","Wales",2,"smooth_vaccines2",0,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Wales/smooth_vaccines2/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_Wa2)
dev.off()
model_Wa2 <- NBreg(Wa_vac2_CP2,18,"2021-11-15","","Wales",2,"vaccines2",0,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Wales/vaccines2/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_Wa2)
dev.off()

# Covid pass in NI: level 1 at 29/11 
NI_vac2_smooth_CP1 <- NI_vac2_smooth[c(35:62),]
NI_vac2_CP1 <- NI_vac2[c(35:62),]
model_NI1 <- NBreg(NI_vac2_smooth_CP1,17,"2021-11-29","","NI",1,"smooth_vaccines2",0,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/NI/smooth_vaccines2/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_Wa1)
dev.off()
model_NI1 <- NBreg(NI_vac2_CP1,17,"2021-11-29","","NI",1,"vaccines2",0,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/NI/vaccines2/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_NI1)
dev.off()

# Covid pass in Scotland: level 1 at 18/10
Sc_vac2_smooth_CP1 <- Sc_vac2_smooth[c(65:107),]
Sc_vac2_CP1 <- Sc_vac2[c(65:107),]
model_Sc1 <- NBreg(Sc_vac2_smooth_CP1,29,"2021-10-18","","Scotland",1,"smooth_vaccines2",0,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Scotland/smooth_vaccines2/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_Sc1)
dev.off()
model_Sc1 <- NBreg(Sc_vac2_CP1,29,"2021-10-18","","Scotland",1,"vaccines2",0,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Scotland/vaccines2/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_Sc1)
dev.off()

# Covid pass in England: level 1 at 15/12
En_vac2_smooth_CP1 <- En_vac2_smooth[c(25:49),]
En_vac2_CP1 <- En_vac2[c(25:49),]
model_En1 <- NBreg(En_vac2_smooth_CP1,11,"2021-12-15","","England",1,"smooth_vaccines2",0,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/England/smooth_vaccines2/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_En1)
dev.off()
model_En1 <- NBreg(En_vac2_CP1,11,"2021-12-15","","England",1,"vaccines2",0,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/England/vaccines2/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_En1)
dev.off()

# Vaccines UK third dose
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Data")
Vac3_UK <- read.csv("Vaccines3_UK_jan19.csv")
Vac3_UK <- as_tibble(Vac3_UK)
Vac3_UK <- Vac3_UK[,c(2,4,5)]
Vac3_UK <- Vac3_UK[!grepl("2020", Vac3_UK$date),]
Vac3_UK$date <- as.Date(Vac3_UK$date)
colnames(Vac3_UK)[3] <- "vaccines3"

Vac3_UK_region <- split(Vac3_UK, Vac3_UK$areaName)
En_vac3 <- Vac3_UK_region[[1]]
NI_vac3 <- Vac3_UK_region[[2]]
Sc_vac3 <- Vac3_UK_region[[3]]
Wa_vac3 <- Vac3_UK_region[[4]]

Wa_vac3_smooth <- smooth_7(Wa_vac3,"vaccines3")
colnames(Wa_vac3_smooth)[3] <- "smooth_vaccines3"
NI_vac3_smooth <- smooth_7(NI_vac3,"vaccines3")
colnames(NI_vac3_smooth)[3] <- "smooth_vaccines3"
Sc_vac3_smooth <- smooth_7(Sc_vac3,"vaccines3")
colnames(Sc_vac3_smooth)[3] <- "smooth_vaccines3"
En_vac3_smooth <- smooth_7(En_vac3,"vaccines3")
colnames(En_vac3_smooth)[3] <- "smooth_vaccines3"

# WALES SR analysis vaccines third dose
# Covid pass in Wales: level 1 at 11/10 and level 2 at 15/11
# Can't do level 1 here because we don't have data on early October
Wa_vac3_smooth_CP2 <- Wa_vac3_smooth[c(40:80),]
Wa_vac3_CP2 <- Wa_vac3[c(40:80),]
model_Wa2 <- NBreg(Wa_vac3_smooth_CP2,26,"2021-11-15","","Wales",2,"smooth_vaccines3",0,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Wales/smooth_vaccines3/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_Wa2)
dev.off()
model_Wa2 <- NBreg(Wa_vac3_CP2,26,"2021-11-15","","Wales",2,"vaccines3",0,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Wales/vaccines3/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_Wa2)
dev.off()

# Covid pass in NI: level 1 at 29/11 
NI_vac3_smooth_CP1 <- NI_vac3_smooth[c(35:62),]
NI_vac3_CP1 <- NI_vac3[c(35:62),]
model_NI1 <- NBreg(NI_vac3_smooth_CP1,17,"2021-11-29","","NI",1,"smooth_vaccines3",0,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/NI/smooth_vaccines3/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_Wa1)
dev.off()
model_NI1 <- NBreg(NI_vac3_CP1,17,"2021-11-29","","NI",1,"vaccines3",0,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/NI/vaccines3/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_NI1)
dev.off()

# Covid pass in Scotland: level 1 at 18/10
# Not enough data on early October

# Covid pass in England: level 1 at 15/12
En_vac3_smooth_CP1 <- En_vac3_smooth[c(25:49),]
En_vac3_CP1 <- En_vac3[c(25:49),]
model_En1 <- NBreg(En_vac3_smooth_CP1,11,"2021-12-15","","England",1,"smooth_vaccines3",0,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/England/smooth_vaccines3/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_En1)
dev.off()
model_En1 <- NBreg(En_vac3_CP1,11,"2021-12-15","","England",1,"vaccines3",0,0)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/England/vaccines3/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_En1)
dev.off()

# ALL CATALONIA
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Data")
Cat <- read.csv("Catalunya_all.csv",sep=";")
Cat <- as_tibble(Cat)
Cat <- Cat[,c(3,4,5,6,9,20,21)]
Cat <- Cat %>% group_by(DATA) %>% summarise(cases = sum(CASOS_CONFIRMAT), admissions = sum(INGRESSOS_TOTAL), vaccines1 = sum(VACUNATS_DOSI_1), vaccines2 = sum(VACUNATS_DOSI_2))
colnames(Cat)[1] <- "date"
Cat <- Cat[!grepl("2020", Cat$date),]
Cat$date <- as.Date(Cat$date)
smooth_cases = smooth_7(Cat,"cases")[,2]
smooth_admissions = smooth_7(Cat, "admissions")[,3]
smooth_vaccines1 = smooth_7(Cat,"vaccines1")[,4]
smooth_vaccines2 = smooth_7(Cat,"vaccines2")[,5]
Cat_smooth <- cbind(Cat$date,smooth_cases,smooth_admissions,smooth_vaccines1,smooth_vaccines2)
colnames(Cat_smooth) <- c("date","smooth_cases","smooth_admissions","smooth_vaccines1","smooth_vaccines2")

# level 1  03/12/2021
Cat_cases_smooth_CP1 <- Cat_smooth[c(315:355),]
Cat_cases_CP1 <- Cat[c(315:355),]
model_Cat1 <- NBreg(Cat_cases_smooth_CP1,23,"2021-12-03","2021-12-08","Catalunya",1,"smooth_cases",5,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Catalunya/smooth_cases/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_Cat1)
dev.off()
model_Cat1 <- NBreg(Cat_cases_CP1,23,"2021-12-03","2021-12-08","Catalunya",1,"cases",5,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Catalunya/cases/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_Cat1)
dev.off()

Cat_hosp_smooth_CP1 <- Cat_smooth[c(315:355),]
Cat_hosp_CP1 <- Cat[c(315:355),]
model_Cat1 <- NBreg(Cat_hosp_smooth_CP1,23,"2021-12-03","2021-12-10","Catalunya",1,"smooth_admissions",7,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Catalunya/smooth_admissions/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_Cat1)
dev.off()
model_Cat1 <- NBreg(Cat_hosp_CP1,23,"2021-12-03","2021-12-10","Catalunya",1,"admissions",7,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Catalunya/admissions/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_Cat1)
dev.off()

Cat_vacc1_smooth_CP1 <- Cat_smooth[c(315:354),]
Cat_vacc1_CP1 <- Cat[c(315:354),]
model_Cat1 <- NBreg(Cat_vacc1_smooth_CP1,23,"2021-12-03","","Catalunya",1,"smooth_vaccines1",0,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Catalunya/smooth_vaccines1/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_Cat1)
dev.off()
model_Cat1 <- NBreg(Cat_cases_CP1,23,"2021-12-03","","Catalunya",1,"vaccines1",0,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Catalunya/vaccines1/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_Cat1)
dev.off()

Cat_vacc2_smooth_CP1 <- Cat_smooth[c(315:354),]
Cat_vacc2_CP1 <- Cat[c(315:354),]
model_Cat1 <- NBreg(Cat_vacc2_smooth_CP1,23,"2021-12-03","","Catalunya",1,"smooth_vaccines2",0,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Catalunya/smooth_vaccines2/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_Cat1)
dev.off()
model_Cat1 <- NBreg(Cat_cases_CP1,23,"2021-12-03","","Catalunya",1,"vaccines2",0,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Catalunya/vaccines2/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_Cat1)
dev.off()


# PART 4: Deaths by age and sex
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Data/AgeSex")
Deaths <- read.csv("Cum_deaths_by_age_sex.csv")
Deaths <- as_tibble(Deaths)
Deaths <- Deaths[,c(2,7,8,10,11)]
Deaths <- Deaths [Deaths$country %in% c("Denmark", "France", "Germany", "Scotland", "England & Wales"), ]
Deaths <- Deaths %>% group_by(age_group) %>% mutate(death_male = c(cum_death_male[1], (cum_death_male - lag(cum_death_male))[-1]))
Deaths <- Deaths %>% group_by(age_group) %>% mutate(death_female = c(cum_death_female[1], (cum_death_female - lag(cum_death_female))[-1]))
Deaths <- Deaths[!grepl("2020", Deaths$death_reference_date),]
Deaths <- Deaths[!grepl("Total", Deaths$age_group),]
Deaths$death_reference_date <- as.Date(Deaths$death_reference_date)
colnames(Deaths)[3] <- "date"

Deaths_country <- split(Deaths, Deaths$country)
Dk_deaths <- Deaths_country[[1]]
EnWa_deaths <- Deaths_country[[2]]
Fr_deaths <- Deaths_country[[3]]
De_deaths <- Deaths_country[[4]]
Sc_deaths <- Deaths_country[[5]]

#Let's do one country
plots_country_deaths <- function(dades,code,name,num) {
  g1 <- ggplot(data = dades, aes(x = date, y = cum_death_male, colour=factor(age_group))) +  geom_point(size=0.5) +  labs(title = paste0(name," cummulative males"), x = "Date", y = "Deaths")
  g2 <- ggplot(data = dades, aes(x = date, y = cum_death_female, colour=factor(age_group))) +  geom_point(size=0.5) +  labs(title = paste0(name," cummulative females"), x = "Date", y = "Deaths")
  
  g3 <- ggplot(data = dades, aes(x = date, y = death_male, colour=factor(age_group))) +  geom_point(size=0.5) +  labs(title = paste0(name," males"), x = "Date", y = "Deaths")
  g4 <- ggplot(data = dades, aes(x = date, y = death_female, colour=factor(age_group))) +  geom_point(size=0.5) +  labs(title = paste0(name," females"), x = "Date", y = "Deaths")
  
  assign(paste0(code,"_deaths_age"),split(dades, dades$age_group))
  for(i in 1:num) {
    assign(paste0(code,"_deaths_",i-1,"0"),get(paste0(code,"_deaths_age"))[[i]][,-1])
  }
  
  g5 <- ggplot(data = get(paste0(code,"_deaths_",num-2,"0")), aes(x = date, y = death_male)) +  geom_bar(stat="identity") +  labs(title = paste0(name," males ",num-2,"0-",num-1,"0yr"), x = "Date", y = "Deaths")
  g6 <- ggplot(data = get(paste0(code,"_deaths_",num-2,"0")), aes(x = date, y = death_female)) +  geom_bar(stat="identity") +  labs(title = paste0(name," females ",num-2,"0-",num-1,"0yr"), x = "Date", y = "Deaths")
  grid.arrange(g1, g2, g3, g4, g5, g6, ncol=2)
}

plots_country_deaths(Dk_deaths,"Dk","Denmark",10)
plots_country_deaths(De_deaths,"De","Germany",10)
Fr_deaths <- Fr_deaths[Fr_deaths$age_group %in% c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+"), ]
plots_country_deaths(Fr_deaths,"Fr","France",10)
EnWa_deaths <- EnWa_deaths[EnWa_deaths$age_group %in% c("1-4", "5-9", "10-14", "15-19", "20-24","25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), ]
for(i in 1:48) {
  for(j in 1:9) {
    EnWa_deaths[((i-1)*19+2*j-1),4] <- EnWa_deaths[((i-1)*19+2*j-1),4] + EnWa_deaths[((i-1)*19+2*j),4]
    EnWa_deaths[((i-1)*19+2*j-1),5] <- EnWa_deaths[((i-1)*19+2*j-1),5] + EnWa_deaths[((i-1)*19+2*j),5]
    EnWa_deaths[((i-1)*19+2*j-1),6] <- EnWa_deaths[((i-1)*19+2*j-1),6] + EnWa_deaths[((i-1)*19+2*j),6]
  }
}
dummy <- c(1,3,5,7,9,11,13,15,17,19)
odd <- dummy
for(i in 1:47) {
  odd <- c(odd,dummy+19*i)
}
EnWa_deaths <- EnWa_deaths[odd,]
EnWa_deaths[,2] <- rep(c("1-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+"),48)

plots_country_deaths(EnWa_deaths,"EnWa","England and Wales",10)
plots_country_deaths(Sc_deaths,"Sc","Scotland",7)

# Let's create datasets for each country and the five levels we want (F, M, <20, 20-50, >50)
Dk_deaths <- Dk_deaths[,-c(4,5)]
date <- unique(Dk_deaths$date)
n <- length(date)
deaths_male <- c(rep(NA,n))
deaths_female <- c(rep(NA,n))
deaths_young <- c(rep(NA,n))
deaths_old <- c(rep(NA,n))
for(i in 1:n) {
  deaths_male[i] <- sum(Dk_deaths[c((1+10*(i-1)):(10*i)),4])
  deaths_female[i] <- sum(Dk_deaths[c((1+10*(i-1)):(10*i)),5])
  deaths_young[i] <- sum(Dk_deaths[c((1+10*(i-1)):(6+10*(i-1))),c(4,5)])
  deaths_old[i] <- sum(Dk_deaths[c((7+10*(i-1)):(10*i)),c(4,5)])
}
Dk_deaths_strat <- data.frame(date,cbind(deaths_male,deaths_female,deaths_young,deaths_old))
cluster_Dk <- c(5,4,5,5,5,5,5,5,3,5,5,5,5,4,5,5,5,4,5,5,5,5,5,5,4,5,5,5,7,7,5,5,5,5,5,4,5,5,5,5,4,4,2,5,5,5,5,5,4)
dummy <- 1
for(i in 1:(length(cluster_Dk)-1)) {
  dummy <- c(dummy,1+sum(cluster_Dk[c(1:i)]))
}
for(i in 1:length(dummy)) {
for(j in 2:6) {
    Dk_deaths_strat[dummy[i],j] <- sum(Dk_deaths_strat[c((dummy[i]):(dummy[i+1]-1)),j])
}
}

Dk_deaths_strat <- Dk_deaths_strat[dummy,]
Dk_deaths_strat <- Dk_deaths_strat[-nrow(Dk_deaths_strat),]

# DENMARK SR analysis deaths
# 06/04, 07/08, 10/09, 12/11, 26/11
Dk_death_CP1 <- Dk_deaths_strat[c(4:27),]
model_death_Dk1 <- NBreg(Dk_death_CP1,11,"2021-04-06","2021-04-25","Denmark",1,"deaths_male",3,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/deaths_male/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_death_Dk1)
dev.off()
model_death_Dk1 <- NBreg(Dk_death_CP1,11,"2021-04-06","2021-04-25","Denmark",1,"deaths_female",3,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/deaths_female/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_death_Dk1)
dev.off()
model_death_Dk1 <- NBreg(Dk_death_CP1,11,"2021-04-06","2021-04-25","Denmark",1,"deaths_young",3,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/deaths_young/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_death_Dk1)
dev.off()
model_death_Dk1 <- NBreg(Dk_death_CP1,11,"2021-04-06","2021-04-25","Denmark",1,"deaths_old",3,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/deaths_old/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_death_Dk1)
dev.off()

Dk_death_CP2 <- Dk_deaths_strat[c(21:44),]
model_death_Dk2 <- NBreg(Dk_death_CP2,11,"2021-08-07","2021-08-26","Denmark",2,"deaths_male",3,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/deaths_male/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_death_Dk2)
dev.off()
model_death_Dk2 <- NBreg(Dk_death_CP2,11,"2021-08-07","2021-08-26","Denmark",2,"deaths_female",3,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/deaths_female/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_death_Dk2)
dev.off()
model_death_Dk2 <- NBreg(Dk_death_CP2,11,"2021-08-07","2021-08-26","Denmark",2,"deaths_young",3,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/deaths_young/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_death_Dk2)
dev.off()
model_death_Dk2 <- NBreg(Dk_death_CP2,11,"2021-08-07","2021-08-26","Denmark",2,"deaths_old",3,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/deaths_old/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_death_Dk2)
dev.off()

Dk_death_CP3 <- Dk_deaths_strat[c(26:49),]
model_death_Dk3 <- NBreg(Dk_death_CP3,11,"2021-09-10","2021-09-29","Denmark",3,"deaths_male",3,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/deaths_male/CP3")
jpeg("Model_NBreg_residuals_CP3")
par(mfrow=c(2,2))
plot(model_death_Dk3)
dev.off()
model_death_Dk3 <- NBreg(Dk_death_CP3,11,"2021-09-10","2021-09-29","Denmark",3,"deaths_female",3,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/deaths_female/CP3")
jpeg("Model_NBreg_residuals_CP3")
par(mfrow=c(2,2))
plot(model_death_Dk3)
dev.off()
model_death_Dk3 <- NBreg(Dk_death_CP3,11,"2021-09-10","2021-09-29","Denmark",3,"deaths_young",3,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/deaths_young/CP3")
jpeg("Model_NBreg_residuals_CP3")
par(mfrow=c(2,2))
plot(model_death_Dk3)
dev.off()
model_death_Dk3 <- NBreg(Dk_death_CP3,11,"2021-09-10","2021-09-29","Denmark",3,"deaths_old",3,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Denmark/deaths_old/CP3")
jpeg("Model_NBreg_residuals_CP3")
par(mfrow=c(2,2))
plot(model_death_Dk3)
dev.off()

# Not enough data to model Dk4 and Dk5 yet


dates <- unique(De_deaths$date)
n <- length(dates)
deaths_male <- c(rep(NA,n))
deaths_female <- c(rep(NA,n))
deaths_young <- c(rep(NA,n))
deaths_old <- c(rep(NA,n))
for(i in 1:n) {
  deaths_male[i] <- sum(De_deaths[c((1+10*(i-1)):(10*i)),6])
  deaths_female[i] <- sum(De_deaths[c((1+10*(i-1)):(10*i)),7])
  deaths_young[i] <- sum(De_deaths[c((1+10*(i-1)):(6+10*(i-1))),c(6,7)])
  deaths_old[i] <- sum(De_deaths[c((7+10*(i-1)):(10*i)),c(6,7)])
}
De_deaths_strat <- data.frame(dates,cbind(deaths_male,deaths_female,deaths_young,deaths_old))
colnames(De_deaths_strat)[1] <- "date"

# GERMANY SR analysis deaths
De_death_CP1 <- De_deaths_strat[c(26:44),]
model_death_De1 <- NBreg(De_death_CP1,8,"2021-08-23","2021-09-11","Germany",1,"deaths_male",3,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Germany/deaths_male/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_death_De1)
dev.off()
model_death_De1 <- NBreg(De_death_CP1,8,"2021-08-23","2021-09-11","Germany",1,"deaths_female",3,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Germany/deaths_female/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_death_De1)
dev.off()
model_death_De1 <- NBreg(De_death_CP1,8,"2021-08-23","2021-09-11","Germany",1,"deaths_young",3,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Germany/deaths_young/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_death_De1)
dev.off()
model_death_De1 <- NBreg(De_death_CP1,8,"2021-08-23","2021-09-11","Germany",1,"deaths_old",3,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Germany/deaths_old/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_death_De1)
dev.off()

# Not enough points yet to do 10 weeks after the second intervention
De_death_CP2 <- De_deaths_strat[c(0:0),]
model_death_De2 <- NBreg(De_death_CP2,8,"2021-11-18","2021-11-30","Germany",2,"deaths_male",3,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Germany/deaths_male/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_death_De2)
dev.off()
model_death_De2 <- NBreg(De_death_CP2,8,"2021-11-18","2021-11-30","Germany",2,"deaths_female",3,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Germany/deaths_female/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_death_De2)
dev.off()
model_death_De2 <- NBreg(De_death_CP2,8,"2021-11-18","2021-11-30","Germany",2,"deaths_young",3,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Germany/deaths_young/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_death_De2)
dev.off()
model_death_De2 <- NBreg(De_death_CP2,8,"2021-11-18","2021-11-30","Germany",2,"deaths_old",3,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Germany/deaths_old/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_death_De2)
dev.off()


Fr_deaths <- Fr_deaths[c(271:nrow(Fr_deaths)),]
dates <- unique(Fr_deaths$date)
n <- length(dates)
deaths_male <- c(rep(NA,n))
deaths_female <- c(rep(NA,n))
deaths_young <- c(rep(NA,n))
deaths_old <- c(rep(NA,n))
for(i in 1:n) {
  deaths_male[i] <- sum(Fr_deaths[c((1+10*(i-1)):(10*i)),6])
  deaths_female[i] <- sum(Fr_deaths[c((1+10*(i-1)):(10*i)),7])
  deaths_young[i] <- sum(Fr_deaths[c((1+10*(i-1)):(6+10*(i-1))),c(6,7)])
  deaths_old[i] <- sum(Fr_deaths[c((7+10*(i-1)):(10*i)),c(6,7)])
}
Fr_deaths_strat <- data.frame(dates,cbind(deaths_male,deaths_female,deaths_young,deaths_old))
colnames(Fr_deaths_strat)[1] <- "date"
cluster_Fr <- c(2,rep(7,48),4)
dummy <- 1
for(i in 1:(length(cluster_Fr)-1)) {
  dummy <- c(dummy,1+sum(cluster_Fr[c(1:i)]))
}
for(i in 1:length(dummy)) {
  for(j in 2:6) {
    Fr_deaths_strat[dummy[i],j] <- sum(Fr_deaths_strat[c((dummy[i]):(dummy[i+1]-1)),j])
  }
}
Fr_deaths_strat <- Fr_deaths_strat[dummy,]

# FRANCE SR analysis deaths
Fr_death_CP1 <- Fr_deaths_strat[c(20:43),]
model_death_Fr1 <- NBreg(Fr_death_CP1,11,"2021-07-21","2021-08-09","France",1,"deaths_male",3,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/France/deaths_male/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_death_Fr1)
dev.off()
model_death_Fr1 <- NBreg(Fr_death_CP1,11,"2021-07-21","2021-08-09","France",1,"deaths_female",3,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/France/deaths_female/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_death_Fr1)
dev.off()
model_death_Fr1 <- NBreg(Fr_death_CP1,11,"2021-07-21","2021-08-09","France",1,"deaths_young",3,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/France/deaths_young/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_death_Fr1)
dev.off()
model_death_Fr1 <- NBreg(Fr_death_CP1,11,"2021-07-21","2021-08-09","France",1,"deaths_old",3,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/France/deaths_old/CP1")
jpeg("Model_NBreg_residuals_CP1")
par(mfrow=c(2,2))
plot(model_death_Fr1)
dev.off()

Fr_death_CP2 <- Fr_deaths_strat[c(23:46),]
model_death_Fr2 <- NBreg(Fr_death_CP2,11,"2021-08-09","2021-08-28","France",2,"deaths_male",3,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/France/deaths_male/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_death_Fr2)
dev.off()
model_death_Fr2 <- NBreg(Fr_death_CP2,11,"2021-08-09","2021-08-28","France",2,"deaths_female",3,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/France/deaths_female/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_death_Fr2)
dev.off()
model_death_Fr2 <- NBreg(Fr_death_CP2,11,"2021-08-09","2021-08-28","France",2,"deaths_young",3,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/France/deaths_young/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_death_Fr2)
dev.off()
model_death_Fr2 <- NBreg(Fr_death_CP2,11,"2021-08-09","2021-08-28","France",2,"deaths_old",3,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/France/deaths_old/CP2")
jpeg("Model_NBreg_residuals_CP2")
par(mfrow=c(2,2))
plot(model_death_Fr2)
dev.off()

Fr_death_CP3 <- Fr_deaths_strat[c(26:49),]
model_death_Fr3 <- NBreg(Fr_death_CP3,11,"2021-08-30","2021-09-18","France",3,"deaths_male",3,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/France/deaths_male/CP3")
jpeg("Model_NBreg_residuals_CP3")
par(mfrow=c(2,2))
plot(model_death_Fr3)
dev.off()
model_death_Fr3 <- NBreg(Fr_death_CP3,11,"2021-08-30","2021-09-18","France",3,"deaths_female",3,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/France/deaths_female/CP3")
jpeg("Model_NBreg_residuals_CP3")
par(mfrow=c(2,2))
plot(model_death_Fr3)
dev.off()
model_death_Fr3 <- NBreg(Fr_death_CP3,11,"2021-08-30","2021-09-18","France",3,"deaths_young",3,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/France/deaths_young/CP3")
jpeg("Model_NBreg_residuals_CP3")
par(mfrow=c(2,2))
plot(model_death_Fr3)
dev.off()
model_death_Fr3 <- NBreg(Fr_death_CP3,11,"2021-08-30","2021-09-18","France",3,"deaths_old",3,1)
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/France/deaths_old/CP3")
jpeg("Model_NBreg_residuals_CP3")
par(mfrow=c(2,2))
plot(model_death_Fr3)
dev.off()


# PART 5 : ARIMA 
# First try: Germany cases smoothed. NPIs at position 262 (2021/11/18), 175 (2021/08/23). 
# Remember 2020 instead of 2021 and inversed order
n <- nrow(De_cases)
De_cases$cases <- rev(De_cases$cases)
De_cases$date <- rev(De_cases$date)
NPI1 <- c(rep(0,174),rep(NA,5),rep(1,(n-179)))
NPI2 <- c(rep(0,261),rep(NA,5),rep(1,(n-266)))
t1 <- c(c(0:173),rep(NA,5),c(174:255),rep(NA,5),c(256:(n-11)))
t2 <- c(rep(0,174),rep(NA,5),c(0:(n-180)))
t3 <- c(rep(0,261),rep(NA,5),c(0:(n-267)))
matriu <- cbind(NPI1,NPI2,t1,t2,t3) 
De_arima_complete <- auto.arima(y=De_cases_smooth$smooth_cases,xreg=matriu)
checkresiduals(De_arima_complete)
autoplot(fitted(De_arima_complete))

De_cases_smooth_CP1 <- De_cases_smooth[c(103:170),]
De_cases_CP1 <- De_cases[c(103:170),]

De_cases_smooth_CP2 <- De_cases_smooth[c(3:85),]
De_cases_CP2 <- De_cases[c(3:85),]

# Differenciating both NPIs and restricting them to local timelines again
t1b <- c(c(0:173),rep(NA,5),c(174:(n-6)))
matriu1 <- cbind(NPI1,t1b,t2) # first NPI change alone
De_cases_smooth_arima1 <- auto.arima(y=De_cases_smooth$smooth_cases,xreg=matriu1)
# first NPI change alone but limited in time, same as SR
De_cases_smooth_arima2 <- auto.arima(y=sqrt(De_cases_smooth$smooth_cases[c(108:192)]),xreg=matriu1[c(108:192),])

matriu1b <- matriu1[c(108:192),]
matriu1b[,2] <- matriu1b[,2] - matriu1b[1,2]
De_cases_smooth_arima3 <- auto.arima(y=sqrt(De_cases_smooth$smooth_cases[c(108:192)]),xreg=matriu1b)
# Relevant coefs the same if t1 starts at 0 or not except intercept ofc

checkresiduals(De_cases_smooth_arima1)
checkresiduals(De_cases_smooth_arima2)
autoplot(fitted(De_cases_smooth_arima1))
autoplot(fitted(De_cases_smooth_arima2))

ggplot(data = De_cases_smooth[c(108:192),], aes(x = date, y = smooth_cases)) +  geom_line() +  labs(title = "Deaths in Germany", x = "Date", y = "Deaths")


(fit <- Arima(y=De_cases_smooth$smooth_cases[c(122:189)],order=c(1,1,1),xreg=matriu1[c(122:189),]))

matriu2 <- cbind(NPI2,t3,t4) # second NPI change alone
auto.arima(y=De_cases_smooth$smooth_cases,xreg=matriu2)



# I don't like (0,0,0) so I try to make my own choice 
ggplot(data = De_cases_smooth, aes(x = date, y = smooth_cases)) +  geom_line() +  labs(title = "Deaths in Germany", x = "Date", y = "Deaths")
De_cases_smooth_dif <- data.frame(De_cases_smooth$date[-291],diff(De_cases_smooth$smooth_cases))
colnames(De_cases_smooth_dif) <- c("date","smooth_cases")
ggplot(data = De_cases_smooth_dif, aes(x = date, y = smooth_cases)) +  geom_line() +  labs(title = "Deaths in Germany", x = "Date", y = "Deaths")
acf(De_cases_smooth_dif$smooth_cases) # yep more differentiating
De_cases_smooth_diff <- data.frame(De_cases_smooth_dif$date[-290],diff(De_cases_smooth_dif$smooth_cases))
colnames(De_cases_smooth_diff) <- c("date","smooth_cases")
ggplot(data = De_cases_smooth_diff, aes(x = date, y = smooth_cases)) +  geom_line() +  labs(title = "Deaths in Germany", x = "Date", y = "Deaths")
acf(De_cases_smooth_diff$smooth_cases)
pacf(De_cases_smooth_diff$smooth_cases)
(fit <- Arima(y=De_cases_smooth_diff$smooth_cases,order=c(1,0,1),xreg=matriu[-c(290,291),]))
(fit2 <- Arima(y=De_cases_smooth_diff$smooth_cases,order=c(6,0,6),xreg=matriu[-c(290,291),]))
checkresiduals(fit)
checkresiduals(fit2)










# Old

ihs <- function(x) {
  y <- log(x + sqrt(x ^ 2 + 1))
  return(y)
}



