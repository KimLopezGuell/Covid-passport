library(epiR)
library(tidyverse)
library(forecast)
library(segmented)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(MASS)
library(car)
library(bo)
library(lmtest)

Sys.setlocale("LC_ALL", "en_GB.UTF-8")

NBregNEW <- function(Endummy,Enpop,Otherpop,datasetEn,dataset0,dataset,intervention_row,intervention_date,lag_date,dataset2,intervention_row2,intervention_date2,lag_date2,country,number,output,lag,dummy,many,trans) {
  setwd(paste0("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/",country,"/",output))
  n <- nrow(dataset)
   # Endummy if comparing with England
   # dummy if time is regular (if not, reversed)
   # many if there is more than one CP change to model and include
   # trans if we must transpose the pred vectors because of data type idk why
   
   if(Endummy == 1) {
     #datasetEn <- datasetEn/10
    datasetEn <- datasetEn/Enpop*100000
   dataset0[,output] <- dataset0[,output]/Otherpop*100000
   dataset[,output] <- dataset[,output]/Otherpop*100000
   dataset2[,output] <- dataset2[,output]/Otherpop*100000
   }
   
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
  
  capture.output(dataset,file="glm_output1.txt",append=TRUE)
  capture.output(summary(m), file="glm_output1.txt", append=TRUE)
  est <- cbind(Estimate=coef(m),confint(m))
  capture.output(est, file="glm_output1.txt", append=TRUE)
  capture.output(exp(est), file="glm_output1.txt", append=TRUE)
  capture.output(durbinWatsonTest(m),file="glm_output1.txt",append=TRUE)
  jpeg("Model_residuals1.jpg")
  par(mfrow=c(2,2))
  plot(m)
  dev.off()
  
  if (many == 1) {
  n2 <- nrow(dataset2)
  # dummy accounts for the order in reverse of dates in some datasets. if dummy=0, the order is inversed
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
  
  #capture.output(dataset2,file="glm_output2.txt",append=TRUE)
  #capture.output(summary(m2), file="glm_output2.txt", append=TRUE)
  #capture.output(est2, file="glm_output2.txt", append=TRUE)
  #capture.output(exp(est2), file="glm_output2.txt", append=TRUE)
  #capture.output(durbinWatsonTest(m2),file="glm_output2.txt",append=TRUE)
  #jpeg("Model_residuals2.jpg")
  #par(mfrow=c(2,2))
  #plot(m2)
  #dev.off()

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

#  coef_function <- function(data, indices) {
#    d <- data[indices,] #allows boot to select sample
#    fit <- glm.nb(d[,output] ~ d[,"CP_int"] + d[,"CP_t1"] + d[,"CP_t2"], data=d) #fit regression model
#    return(coef(fit)) #return coefficient estimates of model
#  }

  #newdata <- na.omit(dataset)
  #perform bootstrapping with 1000 replications
  #reps <- boot(data=newdata, statistic=coef_function, R=1000)
  #capture.output(reps,file=paste0("bootstrap_",country,"_",output,"_CP1.txt"),append=TRUE)
  
  #newdata <- na.omit(dataset2)
  #perform bootstrapping with 1000 replications
  #reps <- boot(data=newdata, statistic=coef_function, R=1000)
  #capture.output(reps,file=paste0("bootstrap_",country,"_",output,"_CP2.txt"),append=TRUE)
  
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
      #ndata$right_upr[c(intervention_row: (intervention_row+lag))] <- 0
    #ndata$right_lwr[c((n-intervention_row): (n-intervention_row-lag))] <- 0
    }
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
  

  #columm <- ncol(dataset)
  #write(t(dataset),file=paste0(country,"_",output,"_CP_",number),ncolumns=columm)
  
  return(m)
}

setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Plots_nice + Bootstrap")

# Population of each region, to plot incidence and be able to compare numbers better
Wa_pop <- 3169586
En_pop <- 56550138
NI_pop <- 1895510
Sc_pop <- 5466000

# Denmark
NBregNEW(0,"","","",Dk_cases_smooth[c(1:60),],Dk_cases_smooth_CP4,17,"2020-11-12","2020-11-17",Dk_cases_smooth_CP5,22,"2020-11-26","2020-12-01","Denmark",4,"smooth_cases",5,0,1,0)
NBregNEW(0,"","","",Dk_hosp_smooth[c(300:350),],Dk_hosp_smooth_CP4,17,"2021-11-12","2021-11-19",Dk_hosp_smooth_CP5,11,"2021-11-26","2021-12-03","Denmark",4,"smooth_admissions",7,1,1,0)
NBregNEW(0,"","","",Dk_vacc[c(245:300),],Dk_vacc_smooth_CP4,18,"2021-11-12","",Dk_vacc_smooth_CP5,15,"2021-11-26","","Denmark",4,"vaccines",0,1,1,0)

# Wales
NBregNEW(1,En_pop,Wa_pop,En_cases_smooth[c(51:111),3],Wa_cases_smooth[c(51:111),],Wa_cases_smooth_CP1,27,"2021-10-11","2021-10-16",Wa_cases_smooth_CP2,15,"2021-11-15","2021-11-20","Wales",1,"smooth_cases",5,0,1,0)
NBregNEW(1,En_pop,Wa_pop,En_hosp_smooth[c(40:110),3],Wa_hosp_smooth[c(40:110),],Wa_hosp_smooth_CP1,18,"2021-10-11","2021-10-18",Wa_hosp_smooth_CP2,26,"2021-11-15","2021-11-22","Wales",1,"smooth_admissions",7,0,1,0)

NBregNEW(0,"","","",Wa_vac1_smooth[c(40:110),],Wa_vac1_smooth_CP1,11,"2021-10-11","",Wa_vac1_smooth_CP2,26,"2021-11-15","","Wales",1,"smooth_vaccines1",0,0,1,0)
NBregNEW(0,"","","",Wa_vac2_smooth[c(48:110),],Wa_vac2_smooth_CP1,11,"2021-10-11","",Wa_vac2_smooth_CP2,18,"2021-11-15","","Wales",1,"smooth_vaccines2",0,0,1,0)
NBregNEW(0,"","","",Wa_vac3_smooth_CP2,Wa_vac3_smooth_CP2,26,"2021-11-15","",Wa_vac3_smooth_CP2,26,"2021-11-15","","Wales",2,"smooth_vaccines3",0,0,1,0)

# NI
NBregNEW(1,En_pop,NI_pop,En_cases_smooth[c(37:77),3],NI_cases_smooth_CP1,NI_cases_smooth_CP1,15,"2021-11-29","2021-12-04",NI_cases_smooth_CP1,15,"2021-11-29","2021-12-04","NI",1,"smooth_cases",5,0,0,1)
NBregNEW(1,En_pop,NI_pop,En_hosp_smooth[c(30:77),3],NI_hosp_smooth_CP1,NI_hosp_smooth_CP1,22,"2021-11-29","2021-12-06",NI_hosp_smooth_CP1,22,"2021-11-29","2021-12-06","NI",1,"smooth_admissions",7,0,0,1)

NBregNEW(0,"","","",NI_vac1_smooth_CP1,NI_vac1_smooth_CP1,22,"2021-11-29","",NI_vac1_smooth_CP1,22,"2021-11-29","","NI",1,"smooth_vaccines1",0,0,0,0)
NBregNEW(0,"","","",NI_vac2_smooth_CP1,NI_vac2_smooth_CP1,17,"2021-11-29","",NI_vac2_smooth_CP1,17,"2021-11-29","","NI",1,"smooth_vaccines2",0,0,0,0)
NBregNEW(0,"","","",NI_vac3_smooth_CP1,NI_vac3_smooth_CP1,17,"2021-11-29","",NI_vac3_smooth_CP1,17,"2021-11-29","","NI",1,"smooth_vaccines3",0,0,0,0)

# Scotland
NBregNEW(1,En_pop,Sc_pop,En_cases_smooth[c(79:103),3],Sc_cases_smooth_CP1,Sc_cases_smooth_CP1,15,"2021-10-18","2021-10-23",Sc_cases_smooth_CP1,15,"2021-10-18","2021-10-23","Scotland",1,"smooth_cases",5,0,0,1)
NBregNEW(1,En_pop,Sc_pop,En_hosp_smooth[c(74:100),3],Sc_hosp_smooth_CP1,Sc_hosp_smooth_CP1,17,"2021-10-18","2021-10-25",Sc_hosp_smooth_CP1,17,"2021-10-18","2021-10-25","Scotland",1,"smooth_admissions",7,0,0,1)

NBregNEW(0,"","","",Sc_vac1_smooth_CP1,Sc_vac1_smooth_CP1,24,"2021-10-18","",Sc_vac1_smooth_CP1,24,"2021-10-18","","Scotland",1,"smooth_vaccines1",0,0,0,0)
NBregNEW(0,"","","",Sc_vac2_smooth_CP1,Sc_vac2_smooth_CP1,29,"2021-10-18","",Sc_vac2_smooth_CP1,29,"2021-10-18","","Scotland",1,"smooth_vaccines2",0,0,0,0)

# England
En_cases_smooth_CP1$smooth_cases <- En_cases_smooth_CP1$smooth_cases/En_pop*100000
En_hosp_smooth_CP1$smooth_admissions <- En_hosp_smooth_CP1$smooth_admissions/En_pop*100000
NBregNEW(0,"","","",En_cases_smooth_CP1,En_cases_smooth_CP1,24,"2021-12-15","2021-12-20",En_cases_smooth_CP1,24,"2021-12-15","2021-12-20","England",1,"smooth_cases",5,0,0,1)
NBregNEW(0,"","","",En_hosp_smooth_CP1,En_hosp_smooth_CP1,30,"2021-12-15","2021-12-22",En_hosp_smooth_CP1,30,"2021-12-15","2021-12-22","England",1,"smooth_admissions",7,0,0,1)

NBregNEW(0,"","","",En_vac1_smooth_CP1,En_vac1_smooth_CP1,16,"2021-12-15","",En_vac1_smooth_CP1,16,"2021-12-15","","England",1,"smooth_vaccines1",0,0,0,0)
NBregNEW(0,"","","",En_vac2_smooth_CP1,En_vac2_smooth_CP1,11,"2021-12-15","",En_vac2_smooth_CP1,11,"2021-12-15","","England",1,"smooth_vaccines2",0,0,0,0)
NBregNEW(0,"","","",En_vac3_smooth_CP1,En_vac3_smooth_CP1,11,"2021-12-15","",En_vac3_smooth_CP1,11,"2021-12-15","","England",1,"smooth_vaccines3",0,0,0,0)

# Catalunya
NBregNEW(0,"","","",Cat_cases_smooth_CP1,Cat_cases_smooth_CP1,23,"2021-12-03","2021-12-08",Cat_cases_smooth_CP1,23,"2021-12-03","2021-12-08","Catalunya",1,"smooth_cases",5,1,0,0)
NBregNEW(0,"","","",Cat_hosp_smooth_CP1,Cat_hosp_smooth_CP1,23,"2021-12-03","2021-12-10",Cat_hosp_smooth_CP1,23,"2021-12-03","2021-12-10","Catalunya",1,"smooth_admissions",7,1,0,0)
NBregNEW(0,"","","",Cat_vacc1_smooth_CP1,Cat_vacc1_smooth_CP1,23,"2021-12-03","",Cat_vacc1_smooth_CP1,23,"2021-12-03","","Catalunya",1,"smooth_vaccines1",0,1,0,0)
NBregNEW(0,"","","",Cat_vacc2_smooth_CP1,Cat_vacc2_smooth_CP1,23,"2021-12-03","",Cat_vacc2_smooth_CP1,23,"2021-12-03","","Catalunya",1,"smooth_vaccines2",0,1,0,0)

# Catalunya separating Omicron and Delta
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Data")
Cases_Cat <- read.csv("Casos_Delta_Omicron_Cat.csv")
Cases_Cat2 <- read.csv("Casos_Delta_Omicron_Cat_TAR.csv")
Cases_Cat <- as_tibble(Cases_Cat)
Cases_Cat <- Cases_Cat[,c(1,3,5,6)]
colnames(Cases_Cat) <- c("date","smooth_cases","smooth_cases_Delta","smooth_cases_Omicron")
Cases_Cat$date <- as.Date(Cases_Cat$date,"%d/%m/%Y")
Cases_Cat2 <- as_tibble(Cases_Cat2)
Cases_Cat2 <- Cases_Cat2[,c(1,3,5,6)]
colnames(Cases_Cat2) <- c("date","smooth_cases","smooth_cases_Delta","smooth_cases_Omicron")
Cases_Cat2$date <- as.Date(Cases_Cat2$date,"%d/%m/%Y")
# Again 2021 turned to 2020 for some absurd reason. Whatever.

Cat_pop <- 7739758
Cases_Cat$smooth_cases_Delta <- Cases_Cat$smooth_cases_Delta/Cat_pop*100000
Cases_Cat$smooth_cases_Omicron <- Cases_Cat$smooth_cases_Omicron/Cat_pop*100000 
Cases_Cat$smooth_cases <- Cases_Cat$smooth_cases/Cat_pop*100000 
Cases_Cat2$smooth_cases_Delta <- Cases_Cat2$smooth_cases_Delta/Cat_pop*100000 
Cases_Cat2$smooth_cases_Omicron <- Cases_Cat2$smooth_cases_Omicron/Cat_pop*100000 
Cases_Cat2$smooth_cases <- Cases_Cat2$smooth_cases/Cat_pop*100000 

setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Plots_nice + Bootstrap/PCR_Cat_cases")
colors <- c("Delta" = "blue", "Omicron" = "green", "Total" = "black")
g <- ggplot(data = Cases_Cat, aes(x = as.Date(date), y = smooth_cases,color="Total")) +geom_line() + annotate("rect",xmin=as.Date("2021-10-08"), xmax=as.Date("2021-10-13"), ymin=-Inf, ymax=Inf,fill="red",alpha=0.1)+ annotate("rect",xmin=as.Date("2021-12-03"), xmax=as.Date("2021-12-08"), ymin=-Inf, ymax=Inf,fill="red",alpha=0.1)
g2 <- g + geom_line(aes(y=smooth_cases_Delta,color="Delta"))
g3 <- g2 + geom_line(aes(y=smooth_cases_Omicron,color="Omicron"))+scale_color_manual(values = colors)
ggsave(filename="Cases_plot.jpg")

setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Plots_nice + Bootstrap/TAR_Cat_cases")
g <- ggplot(data = Cases_Cat2, aes(x = as.Date(date), y = smooth_cases,color="Total")) +geom_line() + annotate("rect",xmin=as.Date("2021-10-08"), xmax=as.Date("2021-10-13"), ymin=-Inf, ymax=Inf,fill="red",alpha=0.1)+ annotate("rect",xmin=as.Date("2021-12-03"), xmax=as.Date("2021-12-08"), ymin=-Inf, ymax=Inf,fill="red",alpha=0.1)
g2 <- g + geom_line(aes(y=smooth_cases_Delta,color="Delta"))
g3 <- g2 + geom_line(aes(y=smooth_cases_Omicron,color="Omicron"))+scale_color_manual(values = colors)
ggsave(filename="Cases_plot.jpg")

# Sum??
Cases_Cat_sum <- Cases_Cat
Cases_Cat_sum$smooth_cases <- Cases_Cat_sum$smooth_cases + Cases_Cat2$smooth_cases
Cases_Cat_sum$smooth_cases_Delta <- Cases_Cat_sum$smooth_cases_Delta + Cases_Cat2$smooth_cases_Delta
Cases_Cat_sum$smooth_cases_Omicron <- Cases_Cat_sum$smooth_cases_Omicron + Cases_Cat2$smooth_cases_Omicron

g <- ggplot(data = Cases_Cat_sum, aes(x = as.Date(date), y = smooth_cases)) +geom_line() + annotate("rect",xmin=as.Date("2021-10-08"), xmax=as.Date("2021-10-13"), ymin=-Inf, ymax=Inf,fill="red",alpha=0.1)+ annotate("rect",xmin=as.Date("2021-12-03"), xmax=as.Date("2021-12-08"), ymin=-Inf, ymax=Inf,fill="red",alpha=0.1)
g2 <- g + geom_line(aes(y=smooth_cases_Delta), color="blue")
g3 <- g2 + geom_line(aes(y=smooth_cases_Omicron),color="green")
                          
# NBSR
setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Plots_nice + Bootstrap/PCR_Cat_cases")
Cat_cases_smooth_CP1 <- Cases_Cat[c(61:115),]
model_Cat <- NBregNEW(0,"","","",Cat_cases_smooth_CP1,Cat_cases_smooth_CP1,34,"2021-12-03","2021-12-08",Cat_cases_smooth_CP1,34,"2021-12-03","2021-12-08","Catalunya",1,"smooth_cases_Delta",5,1,0,1)
jpeg("Model_residuals.jpg")
par(mfrow=c(2,2))
plot(model_Cat)
dev.off()


setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Plots_nice + Bootstrap/TAR_Cat_cases")
Cat_cases_smooth_CP1 <- Cases_Cat2[c(61:115),]
model_Cat <- NBregNEW(0,"","","",Cat_cases_smooth_CP1,Cat_cases_smooth_CP1,34,"2021-12-03","2021-12-08",Cat_cases_smooth_CP1,34,"2021-12-03","2021-12-08","Catalunya",1,"smooth_cases_Delta",5,1,0,1)
jpeg("Model_residuals.jpg")
par(mfrow=c(2,2))
plot(model_Cat)
dev.off()


# Function
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
  jpeg(paste0("ARIMA_",country,"_",output,"CP",int,".jpg"))
  checkresiduals(autoarima_0)
  dev.off()
}

setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Data")
Cases_Cat <- read.csv("Casos_Delta_Omicron_Cat.csv")
Cases_Cat2 <- read.csv("Casos_Delta_Omicron_Cat_TAR.csv")
Cases_Cat <- as_tibble(Cases_Cat)
Cases_Cat <- Cases_Cat[,c(1,2,7,8)]
colnames(Cases_Cat) <- c("date","cases","cases_Omicron","cases_Delta")
Cases_Cat$date <- as.Date(Cases_Cat$date,"%d/%m/%Y")
Cases_Cat2 <- as_tibble(Cases_Cat2)
Cases_Cat2 <- Cases_Cat2[,c(1,2,7,8)]
colnames(Cases_Cat2) <- c("date","cases","cases_Omicron","cases_Delta")
Cases_Cat2$date <- as.Date(Cases_Cat2$date,"%d/%m/%Y")

Cases_Cat$cases_Delta <- Cases_Cat$cases_Delta/Cat_pop*100000
Cases_Cat2$cases_Delta <- Cases_Cat2$cases_Delta/Cat_pop*100000 


setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Catalunya_separate/PCR_Cat_cases")
Cat_cases_CP1 <- Cases_Cat[c(61:115),]
do_arima(Cat_cases_CP1,34,5,"smooth_cases_Delta","Cat",1)
rows <- nrow(Cat_cases_CP1)
number <- 34
lag <- 5
NPI1 <- c(rep(0,number-1),rep(NA,lag),rep(1,rows-lag-number+1))
t1 <- c(c(0:(number-2)),rep(NA,lag),c((number-1+lag):(rows-1)))
t2 <- c(rep(0,number-1),rep(NA,lag),c(1:(rows-lag-number+1)))
matriu <- cbind(NPI1,t1,t2)
autoarima_0 <- Arima(y=Cat_cases_CP1$cases_Delta,xreg=matriu,order=c(1,0,0))
country <- "Wa"
output <- "cases"
int <- 1
capture.output(cbind(dataset[,output],matriu), file=paste0("ARIMA_",country,"_",output,"CP",int,".txt"), append=TRUE)
capture.output(summary(autoarima_0), file=paste0("ARIMA_",country,"_",output,"CP",int,".txt"), append=TRUE)
capture.output(coeftest(autoarima_0), file=paste0("ARIMA_",country,"_",output,"CP",int,".txt"), append=TRUE)
capture.output(checkresiduals(autoarima_0), file=paste0("ARIMA_",country,"_",output,"CP",int,".txt"), append=TRUE)
jpeg(paste0("ARIMA_",country,"_",output,"CP",int,".jpg"))
checkresiduals(autoarima_0)
dev.off()

setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/Catalunya_separate/TAR_Cat_cases")
Cat_cases_CP1 <- Cases_Cat2[c(61:115),]
do_arima(Cat_cases_CP1,34,5,"cases_Delta","Cat",1)


do_arima(Cat_cases_smooth_CP1,34,5,"smooth_cases_Delta","Cat",1)



# For comparing each UK region with England

NBregNEW_vsEn <- function(Enpop,Otherpop,datasetEn,dataset0,dataset,intervention_row,intervention_date,lag_date,dataset2,intervention_row2,intervention_date2,lag_date2,country,number,output,lag,dummy) {
  n <- nrow(datasetEn)
  
  tmp <- as.matrix(cbind(datasetEn[[1]], Enpop))
  England <- epi.conf(tmp, ctype = "inc.rate", method = "exact", design = 1, conf.level = 0.95) * 100000
  
  tmp2 <- as.matrix(cbind(dataset0[[3]], Otherpop))
  Other <- epi.conf(tmp2, ctype = "inc.rate", method = "exact", design = 1, conf.level = 0.95) * 100000
  
  
  
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
  
  
  #columm <- ncol(dataset)
  #write(t(dataset),file=paste0(country,"_",output,"_CP_",number),ncolumns=columm)
}

# Population of each region, to plot incidence and be able to compare numbers better
Wa_pop <- 3169586
En_pop <- 56550138
NI_pop <- 1895510
Sc_pop <- 5466000

setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/DID")

NBregNEW_vsEn(En_pop,Wa_pop,En_cases_smooth[c(51:111),3],Wa_cases_smooth[c(51:111),],Wa_cases_smooth_CP1,27,"2021-10-11","2021-10-16",Wa_cases_smooth_CP2,15,"2021-11-15","2021-11-20","Wales",1,"smooth_cases",5,0)
NBregNEW_vsEn(En_pop,Wa_pop,En_hosp_smooth[c(40:110),3],Wa_hosp_smooth[c(40:110),],Wa_hosp_smooth_CP1,18,"2021-10-11","2021-10-18",Wa_hosp_smooth_CP2,26,"2021-11-15","2021-11-22","Wales",1,"smooth_admissions",7,0)

NBregNEW_vsEn(En_pop,NI_pop,En_cases_smooth[c(37:77),3],NI_cases_smooth_CP1,NI_cases_smooth_CP1,15,"2021-11-29","2021-12-04",NI_cases_smooth_CP1,15,"2021-11-29","2021-12-04","NI",1,"smooth_cases",5,0)
NBregNEW_vsEn(En_pop,NI_pop,En_hosp_smooth[c(30:77),3],NI_hosp_smooth_CP1,NI_hosp_smooth_CP1,22,"2021-11-29","2021-12-06",NI_hosp_smooth_CP1,22,"2021-11-29","2021-12-06","NI",1,"smooth_admissions",7,0)

NBregNEW_vsEn(En_pop,Sc_pop,En_cases_smooth[c(79:103),3],Sc_cases_smooth_CP1,Sc_cases_smooth_CP1,15,"2021-10-18","2021-10-23",Sc_cases_smooth_CP1,15,"2021-10-18","2021-10-23","Scotland",1,"smooth_cases",5,0)
NBregNEW_vsEn(En_pop,Sc_pop,En_hosp_smooth[c(74:100),3],Sc_hosp_smooth_CP1,Sc_hosp_smooth_CP1,17,"2021-10-18","2021-10-25",Sc_hosp_smooth_CP1,17,"2021-10-18","2021-10-25","Scotland",1,"smooth_admissions",7,0)



# Differences in differences, En-Wa-NI-Sc

setwd("/home/user/Escritorio/Kim/Oxford/Dissertation/Analysis/DID")
#Wales CP1 cases and admissions
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

#Wales CP2 cases and admissions
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

#Scotland CP1 cases and admissions
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

#NI CP1 cases and admissions
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










