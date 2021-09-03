##########################################################
# Name of file: 05a_Predicting_Deaths_Among_Positive-Cases.R
# Data release (if applicable):
# Original author(s): Chris Robertson chrisobertson@nhs.net
# Original date: 06 August 2020
# Latest update author (if not using version control) - Chris Robertson chrisobertson@nhs.net
# Latest update date (if not using version control) - 
# Latest update description (if not using version control)
# Type of script: Descriptive stats
# Written/run onL: R Studio SERVER
# Version of R that the script was most recently run on: R 3.6.1
# Description of content: reads in the cohort and merges in the risk groups 
#                         selects out the positive cases only and fits a prediction model
#                         reads in new positives and forecasts 28 day deaths + Hospitalisations
# Approximate run time: Unknown
##########################################################

# 01 Setup ####
#Libraries
library(plyr)
library(tidyverse)
library(survival)
#Load data

Location <- "/conf/"  # Server
#Location <- "//isdsf00d03/"  # Desktop

EAVE_cohort <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_Demog_Endpoints_Times.rds"))
EAVE_cohort <- filter(EAVE_cohort, !duplicated(EAVE_LINKNO))
rg <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_RG_EAVE.rds"))
rg <- filter(rg, !duplicated(EAVE_LINKNO))
ECOSS_path <- paste0(Location,"EAVE/GPanalysis/data/ECOSSdeduped_linked_2021-02-02.rds")
#rg <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_RG_Charlson.rds"))
#rg <- filter(rg, !duplicated(EAVE_LINKNO))
#rg <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_RG_SMR01.rds"))
#rg <- filter(rg, !duplicated(EAVE_LINKNO))

df <- EAVE_cohort %>% select(EAVE_LINKNO:ur6_2016_name, SpecimenDate, result, death_covid, icu_death, hosp_covid, tested, Time.To.Test:age_gp) %>% 
  left_join(rg, by="EAVE_LINKNO")
df <- df %>%  mutate(ageYear = ifelse(ageYear >= 100, 100, ageYear)) %>% 
  filter(result==1)

df <- df %>% 
  mutate(month = months(SpecimenDate),
         month = if_else(month=="February", "March", month),
         month = if_else(month=="June", "May", month),
         month = factor(month,levels=c("March","April","May")),
         Time.Test.Death = Time.To.Death - Time.To.Test,
         Time.Test.Hosp = Time.To.Hosp - Time.To.Test,
         In.Hosp.Before.Test = ifelse(Time.Test.Hosp<= -3, 1,0))
df <- df %>% 
  mutate(Time.Test.Death = if_else(Time.Test.Death<0,0,Time.Test.Death),
         Time.Test.Hosp = if_else(Time.Test.Hosp<0,0,Time.Test.Hosp))
max_SpecimenDate_train <- max(df$SpecimenDate)


Prediction_cohort <- readRDS(ECOSS_path)
max_spec <- as.Date(file.info(ECOSS_path)$ctime) # date ECOSS file was saved
Prediction_cohort <- filter(Prediction_cohort, !duplicated(EAVE_LINKNO))
Prediction_cohort <- EAVE_cohort %>% select(EAVE_LINKNO:ur6_2016_name, result, death_covid, icu_death, hosp_covid, tested, Time.To.Test:age_gp) %>% 
  left_join(Prediction_cohort, by="EAVE_LINKNO") %>% 
  filter(NCOV_RESULT=="Positive") 
Prediction_cohort <- left_join(Prediction_cohort, rg, by="EAVE_LINKNO") %>% 
  filter(SpecimenDate <= max_spec)  #check this date - latest possible specimen date

  

Prediction_cohort <- Prediction_cohort %>%  mutate(ageYear = ifelse(ageYear >= 100, 100, ageYear),
                                                   month = months(SpecimenDate),
                                                  month = if_else(month=="February", "March", month),
                                                  month = if_else(month %in% c("June","July","August","September"), "May", month),
                                                  month = factor(month,levels=c("March","April","May")),
                                                 Time.Test.Death = Time.To.Death - Time.To.Test,
                                                 Time.Test.Hosp = Time.To.Hosp - Time.To.Test,
                                                 In.Hosp.Before.Test = ifelse(Time.Test.Hosp<= -3, 1,0),
                                                 Time.To.Test = as.numeric(SpecimenDate - min(SpecimenDate))
                                                 )
Prediction_cohort <- Prediction_cohort %>%
  mutate(Time.Test.Death = if_else(Time.Test.Death<0,0,Time.Test.Death),
  Time.Test.Hosp = if_else(Time.Test.Hosp<0,0,Time.Test.Death)
  )

#rm(EAVE_cohort, rg, ECOSS_cohort)
#gc()

#z <- filter(Prediction_cohort, !(EAVE_LINKNO %in% EAVE_cohort$EAVE_LINKNO))

#hospital
prediction_vars_h <- c("EAVE_CARE_HOME","EAVE_CHRONIC_HEART_DIS","EAVE_CHRONIC_KIDNEY_DIS","EAVE_CHRONIC_LIVER_DIS", 
                        "EAVE_CHRONIC_RESP_DIS",   "EAVE_DEMENTIA",           "EAVE_DEPRESSION",         "EAVE_DIABETES",          
                       "EAVE_HAEMAT_MALIGNANCY",  "EAVE_HYPERTENSION" ,      "EAVE_IMMUNOSUPPRESSION",  "EAVE_PREGNANCY",         
                       "EAVE_SPLEEN_ANAEMIA" ,    "EAVE_TRANSPLANT"  ) 
#death
prediction_vars_d <- c("EAVE_CARE_HOME", "EAVE_CHRONIC_HEART_DIS" , "EAVE_CHRONIC_KIDNEY_DIS",
                       "EAVE_CHRONIC_RESP_DIS" ,  "EAVE_DEMENTIA",           "EAVE_HAEMAT_MALIGNANCY" ,
                       "EAVE_HOME_OXYGEN" ,       "EAVE_MS_DEGEN_DIS",       "EAVE_MYONEURAL_DIS",     
                       "EAVE_SPLEEN_ANAEMIA")
#survival curve tails off at 40 days so censor - for both endpoints
df.fit <- df %>%  mutate(hosp_covid= if_else(Time.Test.Hosp > 40, 0, hosp_covid),
                         Time.Test.Hosp= if_else(Time.Test.Hosp > 40, 40, Time.Test.Hosp),
                         death_covid= if_else(Time.Test.Death > 40, 0, death_covid),
                         Time.Test.Death= if_else(Time.Test.Death > 40, 40, Time.Test.Death) )

forecasting_cases <- "Exponential"
#forecasting_cases <- "SG"
SG_type <- "Higher" # "Lower" #"Worse"  # "Better" 

Future_Growth <- 1 #usually Between 1 and 0, multiplies the fitted growth rate
#1 is exponential growth, 0 has the epidemic carry on at the same level
#negative values will give a decrease.
#used 1, 0.5, 0

source("05b_Future_Cases_1.R") # only needs to be done once


#Response var
#z.rv <- "death_covid" 
#z.rv.time <- "Time.Test.Death" 
z.rv <- "hosp_covid" 
z.rv.time <- "Time.Test.Hosp" 

if (z.rv=="hosp_covid") prediction_vars <- prediction_vars_h
if (z.rv=="death_covid") prediction_vars <- prediction_vars_d


z.df <- if (z.rv=="hosp_covid") filter(df.fit, In.Hosp.Before.Test==0) else df.fit
fmla.final <- as.formula(paste("Surv(",z.rv.time,",",z.rv,") ~  pspline(ageYear) + pspline(Time.To.Test) + Sex + simd2020_sc_quintile + ",
                               paste(prediction_vars, collapse= "+")))
z.fit <- coxph(fmla.final , data=z.df)
summary(z.fit)

#prediction cohort
if (z.rv=="hosp_covid") z.time.to.test.range <- 60:90
if (z.rv=="death_covid") z.time.to.test.range <- 60:90

n_sims <- 20
rm(z.out)
for (i in 1:n_sims) {

#select the future cases 
if (z.rv=="hosp_covid") z.future$case <- round(rnorm(nrow(z.future), z.future$fit, sd=sqrt(z.future$fit*z.fit.cases$deviance/z.fit.cases$df.residual)))
  
source("05b_Future_Cases_2.R")
  
PC <- Prediction_cohort %>% 
  filter(SpecimenDate > max_SpecimenDate_train) %>% 
  mutate(month = months(SpecimenDate),
         Time.To.Test = sample(z.time.to.test.range, length(month), replace=TRUE) ) 

z.names <- c("EAVE_LINKNO", "ageYear","Time.To.Test","Sex","simd2020_sc_quintile", "SpecimenDate", prediction_vars)
PC.final <- dplyr::select(PC, all_of(z.names))
PC.final <- rbind(PC.final, dplyr::select(Sampled.Cases, all_of(z.names) ))

PC.final <- mutate(PC.final, Time.To.Test = sample(z.time.to.test.range, nrow(PC.final), replace=TRUE) )  
if (z.rv=="hosp_covid") z.time <- data.frame(hosp_covid=0,Time.Test.Hosp=0:14)
if (z.rv=="death_covid") z.time <- data.frame(death_covid=0,Time.Test.Death=0:28)


PC_long <- full_join(select(PC.final, EAVE_LINKNO), z.time, by=character()) %>% 
  left_join(select(PC.final, all_of(z.names) ), by="EAVE_LINKNO")

Fitted <- predict(z.fit, newdata=PC_long, type="expected", se.fit=TRUE)
PC_long <- PC_long %>% 
  mutate(fit = Fitted$fit, se_fit = Fitted$se.fit)
if (z.rv=="hosp_covid") PC_long <- mutate(PC_long, Date = SpecimenDate + Time.Test.Hosp)
if (z.rv=="death_covid") PC_long <- mutate(PC_long, Date = SpecimenDate + Time.Test.Death)

#the expected is cumulative hospitalisations/deaths - convert to daily
PC_long <- PC_long %>% mutate(fit.instant = fit-lag(fit))
if (z.rv=="hosp_covid") PC_long <- mutate(PC_long,fit.instant = if_else(Time.Test.Hosp==0,fit,fit.instant) ) 
if (z.rv=="death_covid") PC_long <- mutate(PC_long,fit.instant = if_else(Time.Test.Death==0,fit,fit.instant) )

  z_long <- PC_long %>% 
    mutate(event = rbinom(nrow(PC_long), size=1, prob=fit.instant) )
  
  
  z.event <- z_long %>% filter(event==1) %>% filter(!duplicated(EAVE_LINKNO)) %>% 
    select(EAVE_LINKNO, Date) %>% dplyr::rename("Date_event" = "Date")
  z <- z_long %>% left_join(z.event, by="EAVE_LINKNO") %>%  filter(is.na(Date_event) | Date_event >= Date) %>% 
    group_by(Date) %>% dplyr::summarise(sim_event = sum(event, na.rm=T),.groups = 'drop'  ) %>% 
    filter(Date <= max(PC.final$SpecimenDate)) %>% 
    mutate(Sim=i) 
  z.out <- if (exists("z.out")) rbind(z.out,z) else z
}

z <- z.out %>% group_by(Date) %>% dplyr::summarise(M=mean(sim_event), Med=median(sim_event), SD = sd(sim_event),
                                                   LCLq=quantile(sim_event,prob=0.025),
                                                   UCLq=quantile(sim_event, prob=0.975)) %>% 
  ungroup() %>% mutate(LCL=M - 1.96*SD, UCL = M + 1.96*SD) %>% 
  mutate(LCL = ifelse(LCL < 0, 0, LCL))

if (z.rv=="hosp_covid") { 
#https://www.opendata.nhs.scot/dataset/weekly-covid-19-statistical-data-in-scotland
#updated with data from Scotland_Tested_Positive_Hospitals_Died_ddmmyy.csv
z.hosp.adm <- read.csv("daily_covid_admissions.csv")
z.hosp.adm <- mutate(z.hosp.adm, Date = as.Date(as.character(Date), "%Y%m%d")) %>% 
  filter(Date >= as.Date("2020-07-01"))# %>% 
#  dplyr::select(-X)
#should not be need
#z.hosp.adm <- filter(z.hosp.adm, Date < as.Date("2020-10-01") ) %>% 
#  bind_rows(data.frame(NumberAdmitted=c(48,39,35,34,56,65,54,61,48,46,32), 
#                       Date = seq.Date(as.Date("2020-10-01"), as.Date("2020-10-11"), by="days")) )
#z.f <- filter(z, Date <=max(PC$SpecimenDate)+15)
gg.h<- ggplot(z,aes(x=Date)) + geom_line(aes(y=M)) + geom_line(aes(y=LCL), linetype=2)+
  geom_line(aes(y=UCL), linetype=2) +
  labs(y="Daily Admissions", title="Forecasted Hospital Admissions among Tested Positive") +
  geom_vline(xintercept=max(PC$SpecimenDate)+0.5, linetype=2)+
  geom_point(aes(x=Date, y=NumberAdmitted), data=z.hosp.adm) +
  theme(plot.title = element_text(size=11))
print(gg.h)
}


if (z.rv=="death_covid") {
  z.dth <- read.csv("Cumulative_Deaths_HB_DateDeath.csv") %>% 
  dplyr::select(NRS.Date.Death, Scotland) %>% 
  mutate(count = Scotland - lag(Scotland),
         Date= as.Date(as.character(NRS.Date.Death))) %>%
  filter(Date >= as.Date("2020-07-01")) %>% 
    dplyr::select(-Scotland, -NRS.Date.Death)
#should not be need
#z.dth <- filter(z.dth, Date < as.Date("2020-09-30") ) %>% 
#bind_rows(data.frame(count=c(4,4,1,2,3,0,3,7,5,0,3,1), 
#                      Date = seq.Date(as.Date("2020-09-30"), as.Date("2020-10-11"), by="days")) )
  
z.f <- filter(z, Date <=max(PC$SpecimenDate)+29)
  gg.d <- ggplot(z.f,aes(x=Date)) + geom_line(aes(y=M)) + geom_line(aes(y=LCL), linetype=2)+
  geom_line(aes(y=UCL), linetype=2) +
  labs(y="Daily Deaths", title="Forecasted Deaths among Tested Positive") +
  geom_vline(xintercept=max(PC$SpecimenDate)+0.5, linetype=2)+
  geom_point(aes(x=Date, y=count), data=z.dth) +
  theme(plot.title = element_text(size=11)) 
print(gg.d)
}

