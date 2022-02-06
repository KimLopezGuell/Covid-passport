# This is a code to analyse incidence rate of cases and hospitalizations with COVID-19 in the UK and produce numerical results and plots

# Needed packages
library(epiR)
library(tidyverse)
library(forecast)
library(ggplot2)
library(MASS)
library(car)
library(bo)
library(lmtest)

# Labs of plots in English
Sys.setlocale("LC_ALL", "en_GB.UTF-8")

# Read data for cases
Cases_UK <- read.csv("Cases_UK_jan19.csv")
Cases_UK <- as_tibble(Cases_UK)
Cases_UK <- Cases_UK[,c(2,4,5)]
Cases_UK <- Cases_UK[!grepl("2020", Cases_UK$date),]
Cases_UK$date <- as.Date(Cases_UK$date)
colnames(Cases_UK)[3] <- "cases"
Cases_UK_region <- split(Cases_UK, Cases_UK$areaName)
En_cases <- Cases_UK_region[[1]]
NI_cases <- Cases_UK_region[[2]]
Sc_cases <- Cases_UK_region[[3]]
Wa_cases <- Cases_UK_region[[4]]

# 7-day average smooth function
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

# Covid pass in Wales: level 1 at 11/10 and level 2 at 15/11
Wa_cases_smooth <- smooth_7(Wa_cases,"cases")
colnames(Wa_cases_smooth)[3] <- "smooth_cases"
# Covid pass in NI: level 1 at 29/11 and level 2 at 13/12
NI_cases_smooth <- smooth_7(NI_cases,"cases")
colnames(NI_cases_smooth)[3] <- "smooth_cases"
# Covid pass in Scotland: level 1 at 18/10
Sc_cases_smooth <- smooth_7(Sc_cases,"cases")
colnames(Sc_cases_smooth)[3] <- "smooth_cases"
# Covid pass in England: level 1 at 15/12
En_cases_smooth <- smooth_7(En_cases,"cases")
colnames(En_cases_smooth)[3] <- "smooth_cases"

# Time intervals selected as stated in the methods section
Wa_cases_smooth_CP1 <- Wa_cases_smooth[c(74:111),]
Wa_cases_CP1 <- Wa_cases[c(74:111),]
Wa_cases_smooth_CP2 <- Wa_cases_smooth[c(51:75),]
Wa_cases_CP2 <- Wa_cases[c(51:75),]
NI_cases_smooth_CP1 <- NI_cases_smooth[c(37:77),]
NI_cases_CP1 <- NI_cases[c(37:77),]
Sc_cases_smooth_CP1 <- Sc_cases_smooth[c(79:103),]
Sc_cases_CP1 <- Sc_cases[c(79:103),]
En_cases_smooth_CP1 <- En_cases_smooth[c(12:70),]
En_cases_CP1 <- En_cases[c(12:70),]

# Read data for hospitalizations and do the same data curation as with cases
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

Wa_hosp_smooth <- smooth_7(Wa_hosp,"admissions")
colnames(Wa_hosp_smooth)[3] <- "smooth_admissions"
NI_hosp_smooth <- smooth_7(NI_hosp,"admissions")
colnames(NI_hosp_smooth)[3] <- "smooth_admissions"
Sc_hosp_smooth <- smooth_7(Sc_hosp,"admissions")
colnames(Sc_hosp_smooth)[3] <- "smooth_admissions"
En_hosp_smooth <- smooth_7(En_hosp,"admissions")
colnames(En_hosp_smooth)[3] <- "smooth_admissions"

Wa_hosp_smooth_CP1 <- Wa_hosp_smooth[c(83:110),]
Wa_hosp_CP1 <- Wa_hosp[c(83:110),]
Wa_hosp_smooth_CP2 <- Wa_hosp_smooth[c(40:85),]
Wa_hosp_CP2 <- Wa_hosp[c(40:85),]
NI_hosp_smooth_CP1 <- NI_hosp_smooth[c(30:77),]
NI_hosp_CP1 <- NI_hosp[c(30:77),]
Sc_hosp_smooth_CP1 <- Sc_hosp_smooth[c(74:100),]
Sc_hosp_CP1 <- Sc_hosp[c(74:100),]
En_hosp_smooth_CP1 <- En_hosp_smooth[c(5:65),]
En_hosp_CP1 <- En_hosp[c(5:65),]


# NBSR modelling function
NBSR <- function(Endummy,Enpop,Otherpop,datasetEn,dataset0,dataset,intervention_row,intervention_date,lag_date,dataset2,intervention_row2,intervention_date2,lag_date2,country,number,output,lag,dummy,many,trans) {
  n <- nrow(dataset)
  
  # Endummy if comparing with England
  # dummy if time is regular (if not, reversed)
  # many if there is more than one CP change to model and include
  # trans if we must transpose the pred vectors because of data type
  
  # work with incidence rate
  if(Endummy == 1) {
    datasetEn <- datasetEn/Enpop*100000
    dataset0[,output] <- dataset0[,output]/Otherpop*100000
    dataset[,output] <- dataset[,output]/Otherpop*100000
    dataset2[,output] <- dataset2[,output]/Otherpop*100000
  }
  
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
  
  # NBSR fitting and forecasting
  dataset <- cbind(dataset,CP_int,CP_t1,CP_t2)
  m <- glm.nb(dataset[,output] ~ CP_int + CP_t1 + CP_t2, data = dataset)
  est <- cbind(Estimate=coef(m),confint(m))
  pred <- exp(est[1,1]+est[2,1]*CP_int+est[3,1]*CP_t1+est[4,1]*CP_t2)
  pred2_wout <- est[1,1]+est[3,1]*CP_t1
  X <- cbind(1,CP_t1)
  cov_fore <- vcov(m)[c(1,3),c(1,3)]
  se_fore <- c()
  for(i in 1:nrow(X)) {
    se_fore <- cbind(se_fore,sqrt(t(X[i,]) %*% cov_fore %*% X[i,]))
  }
  
  # "glm_output.txt" file will contain the results
  capture.output(dataset,file="glm_output1.txt",append=TRUE)
  capture.output(summary(m), file="glm_output1.txt", append=TRUE)
  est <- cbind(Estimate=coef(m),confint(m))
  capture.output(est, file="glm_output1.txt", append=TRUE)
  capture.output(exp(est), file="glm_output1.txt", append=TRUE)
  capture.output(durbinWatsonTest(m),file="glm_output1.txt",append=TRUE)
  #plot the model residuals as well
  jpeg("Model_residuals1.jpg")
  par(mfrow=c(2,2))
  plot(m)
  dev.off()
  
  if (many == 1) {
    n2 <- nrow(dataset2)
    if(dummy ==0) {
      CP_int <- c(rep(1,(intervention_row2-lag)),rep(NA,lag),rep(0,(n2-intervention_row2)))
      CP_t2 <- c(c((intervention_row2-lag-1):0),rep(NA,lag),rep(0,(n2-intervention_row2)))
      CP_t1 <- c((n2-1):(n2-intervention_row2+lag),rep(NA,lag),c((n2-intervention_row2-1):0))
    }
    else {
      CP_int <- c(rep(0,(intervention_row2-1)),rep(NA,lag),rep(1,(n2-intervention_row2-lag+1)))
      CP_t2 <- c(rep(0,intervention_row2-1),rep(NA,lag),c(0:(n2-intervention_row2-lag)))
      CP_t1 <- c(c(0:(intervention_row2-2)),rep(NA,lag),c((intervention_row2+lag-1):(n2-1)))
    }
    
    dataset2 <- cbind(dataset2,CP_int,CP_t1,CP_t2)
    m2 <- glm.nb(dataset2[,output] ~ CP_int + CP_t1 + CP_t2, data = dataset2)
    
    est2 <- cbind(Estimate=coef(m2),confint(m2))
    
    pred3 <- exp(est2[1,1]+est2[2,1]*CP_int+est2[3,1]*CP_t1+est2[4,1]*CP_t2)
    pred4_wout <- est2[1,1]+est2[3,1]*CP_t1
    X2 <- cbind(1,CP_t1)
    cov_fore2 <- vcov(m2)[c(1,3),c(1,3)]
    se_fore2 <- c()
    for(i in 1:nrow(X2)) {
      se_fore2 <- cbind(se_fore2,sqrt(t(X2[i,]) %*% cov_fore2 %*% X2[i,]))
    }
    
    capture.output(dataset2,file="glm_output2.txt",append=TRUE)
    capture.output(summary(m2), file="glm_output2.txt", append=TRUE)
    capture.output(est2, file="glm_output2.txt", append=TRUE)
    capture.output(exp(est2), file="glm_output2.txt", append=TRUE)
    capture.output(durbinWatsonTest(m2),file="glm_output2.txt",append=TRUE)
    jpeg("Model_residuals2.jpg")
    par(mfrow=c(2,2))
    plot(m2)
    dev.off()
    
    if(dummy == 0) {
      total <- nrow(dataset0)
      pred3 <- c(pred3,rep(NA,total-n2))
      pred4_wout <- c(pred4_wout,rep(NA,total-n2))
      se_fore2 <- c(se_fore2,rep(NA,total-n2))
      pred <- c(rep(NA,total-n),pred)
      pred2_wout <- c(rep(NA,total-n),pred2_wout)  
      se_fore <- c(rep(NA,total-n),se_fore)    
    } else {
      total <- nrow(dataset0)
      pred <- c(pred,rep(NA,total-n))
      pred2_wout <- c(pred2_wout,rep(NA,total-n))
      se_fore <- c(se_fore,rep(NA,total-n))
      pred3 <- c(rep(NA,total-n2),pred3)
      pred4_wout <- c(rep(NA,total-n2),pred4_wout)
      se_fore2 <- c(rep(NA,total-n2),se_fore2)    
    }
  }
  
  # create data objects with the predictions for plotting
  ilink <- family(m)$linkinv
  ndata <- bind_cols(dataset, setNames(as_tibble(predict(m, dataset, se.fit = TRUE)[1:2]),
                                       c('fit_link','se_link')))
  ndata <- mutate(ndata,
                  fit_resp  = ilink(fit_link),
                  right_upr = ilink(fit_link + (2 * se_link)),
                  right_lwr = ilink(fit_link - (2 * se_link)))
  
  if ( many == 1) {
    ndata2 <- bind_cols(dataset2, setNames(as_tibble(predict(m2, dataset2, se.fit = TRUE)[1:2]),
                                           c('fit_link','se_link')))
    ndata2 <- mutate(ndata2,
                     fit_resp  = ilink(fit_link),
                     right_upr = ilink(fit_link + (2 * se_link)),
                     right_lwr = ilink(fit_link - (2 * se_link)),)
  }
  
  
  fore <- cbind(pred2_wout,as.vector(se_fore))
  colnames(fore) <- c("pred2_wout","se_fore")
  if (trans==1) {
    fore <- cbind(fore, ilink(pred2_wout), t(ilink(pred2_wout + (2 * se_fore))), t(ilink(pred2_wout - (2 * se_fore))))
  } else {
    fore <- cbind(fore, ilink(pred2_wout), ilink(pred2_wout + (2 * se_fore)), ilink(pred2_wout - (2 * se_fore)))
  }
  colnames(fore) <- c("pred2_wout","se_fore","forecast","right_upr","right_lwr")
  
  if (many == 1) {
    fore2 <- cbind(pred4_wout,as.vector(se_fore2))
    colnames(fore2) <- c("pred4_wout","se_fore2")
    if (trans == 1) {
      fore2 <- cbind(fore2, ilink(pred4_wout), t(ilink(pred4_wout + (2 * se_fore2))), t(ilink(pred4_wout - (2 * se_fore2))))
    } else {
      fore2 <- cbind(fore2, ilink(pred4_wout), ilink(pred4_wout + (2 * se_fore2)), ilink(pred4_wout - (2 * se_fore2)))
    }
    colnames(fore2) <- c("pred4_wout","se_fore2","forecast2","right_upr2","right_lwr2") 
  }
  
  if (many == 1) {
    dataset0 <- bind_cols(dataset0,as_tibble(fore),as_tibble(fore2))
    if (dummy == 1) {
      dataset0$right_upr[c(1:intervention_row)] <- 0
      dataset0$right_upr2[c(1:intervention_row2)] <- 0
      dataset0$right_lwr[c(1:intervention_row)] <- 0
      dataset0$right_lwr2[c(1:intervention_row2)] <- 0
      ndata2$right_upr[c(1:(intervention_row2))] <- 0
      ndata2$right_lwr[c(1:(intervention_row2))] <- 0
    }  else {
      dataset0$right_upr[c((n2+intervention_row-2):total)] <- 0
      dataset0$right_upr2[c((intervention_row2):n2)] <- 0
      dataset0$right_lwr[c((n2+intervention_row-2):total)] <- 0
      dataset0$right_lwr2[c((intervention_row2):n2)] <- 0
      #ndata2$right_upr[c(intervention_row2):n2] <- 0
      #ndata2$right_lwr[c(intervention_row2):n2] <- 0
    }
    # NBSR plot
    g <- ggplot(data = dataset0, aes(x = date, y = get(output))) +  geom_line() +  labs(x = "Date", y = "Incidence rate")+ annotate("rect",xmin=as.Date(intervention_date), xmax=as.Date(lag_date), ymin=-Inf, ymax=Inf,fill="red",alpha=0.1)+ annotate("rect",xmin=as.Date(intervention_date2), xmax=as.Date(lag_date2), ymin=-Inf, ymax=Inf,fill="red",alpha=0.1)+geom_line(aes(y=pred),color="orange",size=1)+geom_line(aes(y=exp(pred2_wout)),color="orange",size=1,linetype="dashed")+geom_line(aes(y=pred3),color="darkgreen",size=1)+geom_line(aes(y=exp(pred4_wout)),color="darkgreen",size=1,linetype="dashed")+ geom_vline(xintercept = as.numeric(as.Date(intervention_date)),color="red")+ geom_vline(xintercept = as.numeric(as.Date(intervention_date2)),color="red")
    g2 <- g+ geom_ribbon(data = ndata, aes(ymin = right_lwr, ymax = right_upr), alpha = 0.15) #+coord_cartesian(ylim = c(0,5)) 
    g3 <- g2+ geom_ribbon(data = ndata2, aes(ymin = right_lwr, ymax = right_upr), alpha = 0.15)
    g4 <- g3+ geom_ribbon(data = as.data.frame(dataset0), aes(ymin = right_lwr, ymax = right_upr), alpha = 0.15)
    g5 <- g4+ geom_ribbon(data = as.data.frame(dataset0), aes(ymin = right_lwr2, ymax = right_upr2), alpha = 0.15)
    if (Endummy == 1) {
      g6 <- g5+geom_line(aes(y=unlist(datasetEn)), color="darkblue")
      ggsave(filename=paste0(country,"_",output,".jpg"))
    } else {
      ggsave(filename=paste0(country,"_",output,".jpg"))
    }
  } else {
    dataset0 <- bind_cols(dataset0,as_tibble(fore))
    if (dummy == 1) {
      dataset0$right_upr[c(1:intervention_row)] <- 0
      dataset0$right_lwr[c(1:intervention_row)] <- 0
    }  else {
      dataset0$right_upr[c((intervention_row):n)] <- 0
      dataset0$right_lwr[c((intervention_row):n)] <- 0
    }
    # NBSR plot
    g <- ggplot(data = dataset0, aes(x = date, y = get(output))) +  geom_line() +  labs(x = "Date", y = "Incidence rate")+ annotate("rect",xmin=as.Date(intervention_date), xmax=as.Date(lag_date), ymin=-Inf, ymax=Inf,fill="red",alpha=0.1)+geom_line(aes(y=pred),color="orange",size=1)+geom_line(aes(y=exp(pred2_wout)),color="orange",size=1,linetype="dashed")
    g2 <- g+ geom_ribbon(data = ndata, aes(ymin = right_lwr, ymax = right_upr), alpha = 0.15)  #+coord_cartesian(ylim = c(0,5)) 
    g3 <- g2+ geom_ribbon(data = as.data.frame(dataset0), aes(ymin = right_lwr, ymax = right_upr), alpha = 0.15)
    if (Endummy == 1) {
      g4 <- g3+geom_line(aes(y=unlist(datasetEn)), color="darkblue")
      ggsave(filename=paste0(country,"_",output,".jpg"))
    } else {
      ggsave(filename=paste0(country,"_",output,".jpg"))
    }
    ggsave(filename=paste0(country,"_",output,".jpg"))
  }
  

  return(m)
}

# Population of each region, to plot incidence and be able to compare numbers better
Wa_pop <- 3169586
En_pop <- 56550138
NI_pop <- 1895510
Sc_pop <- 5466000

# Wales
NBSR(1,En_pop,Wa_pop,En_cases_smooth[c(51:111),3],Wa_cases_smooth[c(51:111),],Wa_cases_smooth_CP1,27,"2021-10-11","2021-10-16",Wa_cases_smooth_CP2,15,"2021-11-15","2021-11-20","Wales",1,"smooth_cases",5,0,1,0)
NBSR(1,En_pop,Wa_pop,En_hosp_smooth[c(40:110),3],Wa_hosp_smooth[c(40:110),],Wa_hosp_smooth_CP1,18,"2021-10-11","2021-10-18",Wa_hosp_smooth_CP2,26,"2021-11-15","2021-11-22","Wales",1,"smooth_admissions",7,0,1,0)
# NI
NBSR(1,En_pop,NI_pop,En_cases_smooth[c(37:77),3],NI_cases_smooth_CP1,NI_cases_smooth_CP1,15,"2021-11-29","2021-12-04",NI_cases_smooth_CP1,15,"2021-11-29","2021-12-04","NI",1,"smooth_cases",5,0,0,1)
NBSR(1,En_pop,NI_pop,En_hosp_smooth[c(30:77),3],NI_hosp_smooth_CP1,NI_hosp_smooth_CP1,22,"2021-11-29","2021-12-06",NI_hosp_smooth_CP1,22,"2021-11-29","2021-12-06","NI",1,"smooth_admissions",7,0,0,1)
# Scotland
NBSR(1,En_pop,Sc_pop,En_cases_smooth[c(79:103),3],Sc_cases_smooth_CP1,Sc_cases_smooth_CP1,15,"2021-10-18","2021-10-23",Sc_cases_smooth_CP1,15,"2021-10-18","2021-10-23","Scotland",1,"smooth_cases",5,0,0,1)
NBSR(1,En_pop,Sc_pop,En_hosp_smooth[c(74:100),3],Sc_hosp_smooth_CP1,Sc_hosp_smooth_CP1,17,"2021-10-18","2021-10-25",Sc_hosp_smooth_CP1,17,"2021-10-18","2021-10-25","Scotland",1,"smooth_admissions",7,0,0,1)
# England
En_cases_smooth_CP1$smooth_cases <- En_cases_smooth_CP1$smooth_cases/En_pop*100000
En_hosp_smooth_CP1$smooth_admissions <- En_hosp_smooth_CP1$smooth_admissions/En_pop*100000
NBSR(0,"","","",En_cases_smooth_CP1,En_cases_smooth_CP1,24,"2021-12-15","2021-12-20",En_cases_smooth_CP1,24,"2021-12-15","2021-12-20","England",1,"smooth_cases",5,0,0,1)
NBSR(0,"","","",En_hosp_smooth_CP1,En_hosp_smooth_CP1,30,"2021-12-15","2021-12-22",En_hosp_smooth_CP1,30,"2021-12-15","2021-12-22","England",1,"smooth_admissions",7,0,0,1)


# ARIMA function
do_arima <- function(dataset,number,lag,output,country,int) {
  rows <- nrow(dataset)
  NPI1 <- c(rep(0,number-1),rep(NA,lag),rep(1,rows-lag-number+1))
  t1 <- c(c(0:(number-2)),rep(NA,lag),c((number-1+lag):(rows-1)))
  t2 <- c(rep(0,number-1),rep(NA,lag),c(1:(rows-lag-number+1)))
  matriu <- cbind(NPI1,t1,t2)
  
  # Creates file with model summary and information, and also residual plots
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

# Wales 
Wa_cases_CP1 <- Wa_cases[c(74:111),]
Wa_cases_CP1$cases <- rev(Wa_cases_CP1$cases)
Wa_cases_CP1$date <- rev(Wa_cases_CP1$date)
Wa_cases_CP1$cases <- Wa_cases_CP1$cases/Wa_pop*100000
ggplot(data = Wa_cases_CP1, aes(x = date, y = cases)) +  geom_line() +  labs(title = "Cases in Wales", x = "Date", y = "Cases")
par(mfrow=c(2,1))
acf(Wa_cases_CP1$cases)
pacf(Wa_cases_CP1$cases)
do_arima(Wa_cases_CP1,12,5,"cases","Wa",1)
Wa_cases_CP2 <- Wa_cases[c(51:75),]
Wa_cases_CP2$cases <- rev(Wa_cases_CP2$cases)
Wa_cases_CP2$date <- rev(Wa_cases_CP2$date)
Wa_cases_CP2$cases <- Wa_cases_CP2$cases/Wa_pop*100000
ggplot(data = Wa_cases_CP2, aes(x = date, y = cases)) +  geom_line() +  labs(title = "Cases in Wales", x = "Date", y = "Cases")
par(mfrow=c(2,1))
acf(Wa_cases_CP2$cases)
pacf(Wa_cases_CP2$cases)
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
Wa_hosp_CP2 <- Wa_hosp[c(40:85),]
Wa_hosp_CP2$admissions <- rev(Wa_hosp_CP2$admissions)
Wa_hosp_CP2$date <- rev(Wa_hosp_CP2$date)
Wa_hosp_CP2$admissions <- Wa_hosp_CP2$admissions/Wa_pop*100000
ggplot(data = Wa_hosp_CP2, aes(x = date, y = admissions)) +  geom_line() +  labs(title = "Admissions in Wales", x = "Date", y = "Cases")
par(mfrow=c(2,1))
acf(Wa_hosp_CP2$admissions)
pacf(Wa_hosp_CP2$admissions)
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
Sc_hosp_CP2 <- Sc_hosp[c(74:100),]
Sc_hosp_CP2$admissions <- rev(Sc_hosp_CP2$admissions)
Sc_hosp_CP2$date <- rev(Sc_hosp_CP2$date)
Sc_hosp_CP1$admissions <- Sc_hosp_CP1$admissions/Sc_pop*100000
ggplot(data = Sc_hosp_CP2, aes(x = date, y = admissions)) +  geom_line() +  labs(title = "Admissions in Scotland", x = "Date", y = "Cases")
par(mfrow=c(2,1))
acf(Sc_hosp_CP2$admissions)
pacf(Sc_hosp_CP2$admissions)
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
do_arima(En_cases_CP1,36,5,"cases","En",1)
En_hosp_CP1 <- En_hosp[c(5:65),]
En_hosp_CP1$admissions <- rev(En_hosp_CP1$admissions)
En_hosp_CP1$date <- rev(En_hosp_CP1$date)
En_hosp_CP1$admissions <- En_hosp_CP1$admissions/En_pop*100000
ggplot(data = En_hosp_CP1, aes(x = date, y = admissions)) +  geom_line() +  labs(title = "Admissions in England", x = "Date", y = "Cases")
par(mfrow=c(2,1))
acf(En_hosp_CP1$admissions)
pacf(En_hosp_CP1$admissions)
do_arima(En_hosp_CP1,32,7,"admissions","En",1)


# For comparing each UK region with England, DiD modelling plots
DiD <- function(Enpop,Otherpop,datasetEn,dataset0,dataset,intervention_row,intervention_date,lag_date,dataset2,intervention_row2,intervention_date2,lag_date2,country,number,output,lag,dummy) {
  n <- nrow(datasetEn)
  
  # confidence intervals
  tmp <- as.matrix(cbind(datasetEn[[1]], Enpop))
  England <- epi.conf(tmp, ctype = "inc.rate", method = "exact", design = 1, conf.level = 0.95) * 100000
  
  tmp2 <- as.matrix(cbind(dataset0[[3]], Otherpop))
  Other <- epi.conf(tmp2, ctype = "inc.rate", method = "exact", design = 1, conf.level = 0.95) * 100000
  
  # put both patterns in the same comparable y axis
  if (England[intervention_row,1] > Other[intervention_row,1]) {
    diff <- England[intervention_row,1]-Other[intervention_row,1]
    England <- England - (England[intervention_row,1]-Other[intervention_row,1])
  } else {
    diff <- -(Other[intervention_row,1]-England[intervention_row,1])
    England <- England + (Other[intervention_row,1]-England[intervention_row,1])
  }
  
  colnames(Other) <- c("est2","lower2","upper2")
  
  dataset0 <- cbind(dataset0,England,Other)
  g <- ggplot(data = dataset0, aes(x = date, y = est2)) +  geom_line() +scale_y_continuous("Incidence rate",sec.axis = sec_axis(trans=~(.+diff)/1, name = "Incidence rate England"))+labs(x = "Date", y = "Incidence rate")+ annotate("rect",xmin=as.Date(intervention_date), xmax=as.Date(lag_date), ymin=-Inf, ymax=Inf,fill="red",alpha=0.1)+ annotate("rect",xmin=as.Date(intervention_date2), xmax=as.Date(lag_date2), ymin=-Inf, ymax=Inf,fill="red",alpha=0.1)
  g2 <- g+geom_line(aes(y=est), color="blue")
  g3 <- g2+ geom_ribbon(data = dataset0, aes(ymin = lower, ymax = upper), alpha = 0.15)
  g4 <- g3+ geom_ribbon(data = dataset0, aes(ymin = lower2, ymax = upper2), alpha = 0.15)
  
  ggsave(filename=paste0(country,"_",output,"_vsEngland.jpg"))
  
}

# Wales
DiD(En_pop,Wa_pop,En_cases_smooth[c(51:111),3],Wa_cases_smooth[c(51:111),],Wa_cases_smooth_CP1,27,"2021-10-11","2021-10-16",Wa_cases_smooth_CP2,15,"2021-11-15","2021-11-20","Wales",1,"smooth_cases",5,0)
DiD(En_pop,Wa_pop,En_hosp_smooth[c(40:110),3],Wa_hosp_smooth[c(40:110),],Wa_hosp_smooth_CP1,18,"2021-10-11","2021-10-18",Wa_hosp_smooth_CP2,26,"2021-11-15","2021-11-22","Wales",1,"smooth_admissions",7,0)
# NI
DiD(En_pop,NI_pop,En_cases_smooth[c(37:77),3],NI_cases_smooth_CP1,NI_cases_smooth_CP1,15,"2021-11-29","2021-12-04",NI_cases_smooth_CP1,15,"2021-11-29","2021-12-04","NI",1,"smooth_cases",5,0)
DiD(En_pop,NI_pop,En_hosp_smooth[c(30:77),3],NI_hosp_smooth_CP1,NI_hosp_smooth_CP1,22,"2021-11-29","2021-12-06",NI_hosp_smooth_CP1,22,"2021-11-29","2021-12-06","NI",1,"smooth_admissions",7,0)
# Scotland
DiD(En_pop,Sc_pop,En_cases_smooth[c(79:103),3],Sc_cases_smooth_CP1,Sc_cases_smooth_CP1,15,"2021-10-18","2021-10-23",Sc_cases_smooth_CP1,15,"2021-10-18","2021-10-23","Scotland",1,"smooth_cases",5,0)
DiD(En_pop,Sc_pop,En_hosp_smooth[c(74:100),3],Sc_hosp_smooth_CP1,Sc_hosp_smooth_CP1,17,"2021-10-18","2021-10-25",Sc_hosp_smooth_CP1,17,"2021-10-18","2021-10-25","Scotland",1,"smooth_admissions",7,0)

# Differences in differences, En-Wa-NI-Sc

# Wales CP1 cases and admissions
Dif_Wales_cases_CP1 <- cbind(c(En_cases_smooth[c(74:111),3][[1]]/En_pop,Wa_cases_smooth[c(74:111),3][[1]]/Wa_pop),c(rep(0,38),rep(1,38)),c(rep(1,21),rep(NA,5),rep(0,12),rep(1,21),rep(NA,5),rep(0,12)))
colnames(Dif_Wales_cases_CP1) <- c("smooth_cases","group","time")
Dif_Wales_cases_CP1 <- as_tibble(Dif_Wales_cases_CP1)
did_Wa_cases_CP1 <- lm(smooth_cases ~ group*time, data=Dif_Wales_cases_CP1)
capture.output(summary(did_Wa_cases_CP1), file="did_Wa_cases_CP1.txt", append=TRUE)
capture.output(confint(did_Wa_cases_CP1), file="did_Wa_cases_CP1.txt", append=TRUE)
capture.output(durbinWatsonTest(did_Wa_cases_CP1), file="did_Wa_cases_CP1.txt", append=TRUE)
write(t(Dif_Wales_cases_CP1),file="Dif_Wales_cases_CP1.txt",ncol=3)
jpeg("Dif_Wales_cases_CP1.jpg")
par(mfrow=c(2,2))
plot(did_Wa_cases_CP1)
dev.off()

Dif_Wales_hosp_CP1 <- cbind(c(En_hosp_smooth[c(83:110),3][[1]]/En_pop,Wa_hosp_smooth[c(83:110),3][[1]]/Wa_pop),c(rep(0,28),rep(1,28)),c(rep(1,10),rep(NA,7),rep(0,11),rep(1,10),rep(NA,5),rep(0,11)))
colnames(Dif_Wales_hosp_CP1) <- c("smooth_admissions","group","time")
Dif_Wales_hosp_CP1 <- as_tibble(Dif_Wales_hosp_CP1)
did_Wa_admissions_CP1 <- lm(smooth_admissions ~ group*time, data=Dif_Wales_hosp_CP1)
capture.output(summary(did_Wa_admissions_CP1), file="did_Wa_admissions_CP1.txt", append=TRUE)
capture.output(confint(did_Wa_admissions_CP1), file="did_Wa_admissions_CP1.txt", append=TRUE)
capture.output(durbinWatsonTest(did_Wa_admissions_CP1), file="did_Wa_admissions_CP1.txt", append=TRUE)
write(t(Dif_Wales_hosp_CP1),file="Dif_Wales_hosp_CP1.txt",ncol=3)
jpeg("Dif_Wales_hosp_CP1.jpg")
par(mfrow=c(2,2))
plot(did_Wa_admissions_CP1)
dev.off()

# Wales CP2 cases and admissions
Dif_Wales_cases_CP2 <- cbind(c(En_cases_smooth[c(51:75),3][[1]]/En_pop,Wa_cases_smooth[c(51:75),3][[1]]/Wa_pop),c(rep(0,25),rep(1,25)),c(rep(1,11),rep(NA,5),rep(0,11),rep(1,11),rep(NA,5),rep(0,11)))
colnames(Dif_Wales_cases_CP2) <- c("smooth_cases","group","time")
Dif_Wales_cases_CP2 <- as_tibble(Dif_Wales_cases_CP2)
did_Wa_cases_CP2 <- lm(smooth_cases ~ group*time, data=Dif_Wales_cases_CP2)
capture.output(summary(did_Wa_cases_CP2), file="did_Wa_cases_CP2.txt", append=TRUE)
capture.output(confint(did_Wa_cases_CP2), file="did_Wa_cases_CP2.txt", append=TRUE)
capture.output(durbinWatsonTest(did_Wa_cases_CP2), file="did_Wa_cases_CP2.txt", append=TRUE)
write(t(Dif_Wales_cases_CP2),file="Dif_Wales_cases_CP2.txt",ncol=3)
jpeg("Dif_Wales_cases_CP2.jpg")
par(mfrow=c(2,2))
plot(did_Wa_cases_CP2)
dev.off()

Dif_Wales_hosp_CP2 <- cbind(c(En_hosp_smooth[c(40:85),3][[1]]/En_pop,Wa_hosp_smooth[c(40:85),3][[1]]/Wa_pop),c(rep(0,46),rep(1,46)),c(rep(1,18),rep(NA,5),rep(0,21),rep(1,18),rep(NA,5),rep(0,21)))
colnames(Dif_Wales_hosp_CP2) <- c("smooth_admissions","group","time")
Dif_Wales_hosp_CP2 <- as_tibble(Dif_Wales_hosp_CP2)
did_Wa_admissions_CP2 <- lm(smooth_admissions ~ group*time, data=Dif_Wales_hosp_CP2)
capture.output(summary(did_Wa_admissions_CP2), file="did_Wa_admissions_CP2.txt", append=TRUE)
capture.output(confint(did_Wa_admissions_CP2), file="did_Wa_admissions_CP2.txt", append=TRUE)
capture.output(durbinWatsonTest(did_Wa_admissions_CP2), file="did_Wa_admissions_CP2.txt", append=TRUE)
write(t(Dif_Wales_hosp_CP2),file="Dif_Wales_hosp_CP2.txt",ncol=3)
jpeg("Dif_Wales_hosp_CP2.jpg")
par(mfrow=c(2,2))
plot(did_Wa_admissions_CP2)
dev.off()

# Scotland CP1 cases and admissions
Dif_Scotland_cases_CP1 <- cbind(c(En_cases_smooth[c(79:103),3][[1]]/En_pop,Sc_cases_smooth[c(79:103),3][[1]]/Sc_pop),c(rep(0,25),rep(1,25)),c(rep(1,10),rep(NA,5),rep(0,10),rep(1,10),rep(NA,5),rep(0,10)))
colnames(Dif_Scotland_cases_CP1) <- c("smooth_cases","group","time")
Dif_Scotland_cases_CP1 <- as_tibble(Dif_Scotland_cases_CP1)
did_Sc_cases_CP1 <- lm(smooth_cases ~ group*time, data=Dif_Scotland_cases_CP1)
capture.output(summary(did_Sc_cases_CP1), file="did_Sc_cases_CP1.txt", append=TRUE)
capture.output(confint(did_Sc_cases_CP1), file="did_Sc_cases_CP1.txt", append=TRUE)
capture.output(durbinWatsonTest(did_Sc_cases_CP1), file="did_Sc_cases_CP1.txt", append=TRUE)
write(t(Dif_Scotland_cases_CP1),file="Dif_Scotland_cases_CP1.txt",ncol=3)
jpeg("Dif_Scotland_cases_CP1.jpg")
par(mfrow=c(2,2))
plot(did_Sc_cases_CP1)
dev.off()

Dif_Scotland_hosp_CP1 <- cbind(c(En_hosp_smooth[c(74:100),3][[1]]/En_pop,Sc_hosp_smooth[c(74:100),3][[1]]/Sc_pop),c(rep(0,27),rep(1,27)),c(rep(1,10),rep(NA,5),rep(0,10),rep(1,10),rep(NA,5),rep(0,10)))
colnames(Dif_Scotland_hosp_CP1) <- c("smooth_admissions","group","time")
Dif_Scotland_hosp_CP1 <- as_tibble(Dif_Scotland_hosp_CP1)
did_Sc_admissions_CP1 <- lm(smooth_admissions ~ group*time, data=Dif_Scotland_hosp_CP1)
capture.output(summary(did_Sc_admissions_CP1), file="did_Sc_admissions_CP1.txt", append=TRUE)
capture.output(confint(did_Sc_admissions_CP1), file="did_Sc_admissions_CP1.txt", append=TRUE)
capture.output(durbinWatsonTest(did_Sc_admissions_CP1), file="did_Sc_admissions_CP1.txt", append=TRUE)
write(t(Dif_Scotland_hosp_CP1),file="Dif_Scotland_hosp_CP1.txt",ncol=3)
jpeg("Dif_Scotland_hosp_CP1.jpg")
par(mfrow=c(2,2))
plot(did_Sc_admissions_CP1)
dev.off()

# NI CP1 cases and admissions
Dif_NI_cases_CP1 <- cbind(c(En_cases_smooth[c(37:77),3][[1]]/En_pop,NI_cases_smooth[c(37:77),3][[1]]/NI_pop),c(rep(0,41),rep(1,41)),c(rep(1,14),rep(0,27),rep(1,14),rep(0,27)))
colnames(Dif_NI_cases_CP1) <- c("smooth_cases","group","time")
Dif_NI_cases_CP1 <- as_tibble(Dif_NI_cases_CP1)
did_NI_cases_CP1 <- lm(smooth_cases ~ group*time, data=Dif_NI_cases_CP1)
capture.output(summary(did_NI_cases_CP1), file="did_NI_cases_CP1.txt", append=TRUE)
capture.output(confint(did_NI_cases_CP1), file="did_NI_cases_CP1.txt", append=TRUE)
capture.output(durbinWatsonTest(did_NI_cases_CP1), file="did_NI_cases_CP1.txt", append=TRUE)
write(t(Dif_NI_cases_CP1),file="Dif_NI_cases_CP1.txt",ncol=3)
jpeg("Dif_NI_cases_CP1.jpg")
par(mfrow=c(2,2))
plot(did_NI_cases_CP1)
dev.off()

Dif_NI_hosp_CP1 <- cbind(c(En_hosp_smooth[c(30:77),3][[1]]/En_pop,NI_hosp_smooth[c(30:77),3][[1]]/NI_pop),c(rep(0,48),rep(1,48)),c(rep(1,21),rep(0,27),rep(1,21),rep(0,27)))
colnames(Dif_NI_hosp_CP1) <- c("smooth_admissions","group","time")
Dif_NI_hosp_CP1 <- as_tibble(Dif_NI_hosp_CP1)
did_NI_admissions_CP1 <- lm(smooth_admissions ~ group*time, data=Dif_NI_hosp_CP1)
capture.output(summary(did_NI_admissions_CP1), file="did_NI_admissions_CP1.txt", append=TRUE)
capture.output(confint(did_NI_admissions_CP1), file="did_NI_admissions_CP1.txt", append=TRUE)
capture.output(durbinWatsonTest(did_NI_admissions_CP1), file="did_NI_admissions_CP1.txt", append=TRUE)
write(t(Dif_NI_hosp_CP1),file="Dif_NI_hosp_CP1.txt",ncol=3)
jpeg("Dif_NI_hosp_CP1.jpg")
par(mfrow=c(2,2))
plot(did_NI_admissions_CP1)
dev.off()
