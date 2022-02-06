# This is a "dummy" code created only for Germany cases data, modelled with ARIMA
library(tidyverse)
library(forecast)

# Read the dataset
Cases_EU <- read.csv("Cases_EU_dc16.csv")
Cases_EU <- as_tibble(Cases_EU)
Cases_EU <- Cases_EU[,c(1,5,7)]
Cases_EU <- Cases_EU[Cases_EU$countriesAndTerritories %in% c("Denmark", "France", "Germany"), ]
Cases_EU <- Cases_EU[!grepl("2020", Cases_EU$dateRep),]
Cases_EU <- Cases_EU[!grepl("01/03/2021", Cases_EU$dateRep),]
Cases_EU$dateRep <- as.Date(Cases_EU$dateRep,format="%d/%m/%y")
colnames(Cases_EU)[1] <- "date"

#Create the Germany smoothed data (using the smooth_7 function)
Cases_EU_region <- split(Cases_EU, Cases_EU$countriesAndTerritories)
De_cases <- Cases_EU_region[[3]]

smooth_7 <- function(dataset,nom) {
  smoothed <- dataset
  smoothed[,nom][[1]][[1]][1] <- (dataset[,nom][[1]][1]+dataset[,nom][[1]][2]+dataset[,nom][[1]][3]+dataset[,nom][[1]][4])/4
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

De_cases_smooth <- smooth_7(De_cases,"cases")
colnames(De_cases_smooth)[2] <- "smooth_cases"

#Define the three vectors (NPI and two tendencies)
n <- nrow(De_cases_smooth)
De_cases_smooth$smooth_cases <- rev(De_cases_smooth$smooth_cases)
De_cases_smooth$date <- rev(De_cases_smooth$date)
NPI1 <- c(rep(0,174),rep(NA,5),rep(1,(n-179)))
t1 <- c(c(0:173),rep(NA,5),c(174:(n-6)))
t2 <- c(rep(0,174),rep(NA,5),c(0:(n-180)))
matriu1 <- cbind(NPI1,t1,t2) # first NPI change alone

#This is a stupid ARIMA model of the first NPI (23/08) using the same interval than in the SR
#I get the error "No suitable ARIMA model found", as with other intervals
De_cases_smooth_arima <- auto.arima(y=sqrt(De_cases_smooth$smooth_cases[c(122:189)]),xreg=matriu1[c(122:189),])

#Some work, like 108:192
De_cases_smooth_arima <- auto.arima(y=sqrt(De_cases_smooth$smooth_cases[c(108:192)]),xreg=matriu1[c(108:192),])
checkresiduals(De_cases_smooth_arima)

