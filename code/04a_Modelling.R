##########################################################
# Name of file: 04a_Modelling.R
# Data release (if applicable):
# Original author(s): Chris Robertson chrisobertson@nhs.net
# Original date: 30 August 2020
# Latest update author (if not using version control) - Chris Robertson chrisobertson@nhs.net
# Latest update date (if not using version control) - 
# Latest update description (if not using version control)
# Type of script: Descriptive stats
# Written/run onL: R Studio SERVER
# Version of R that the script was most recently run on: R 3.6.1
# Description of content: reads in the cohort and merges in the risk groups 
#                         multivriate modelling with EAVE and SMR risk groups
# Approximate run time: Unknown
##########################################################

# 01 Setup ####
#Libraries
library(plyr)
library(tidyverse)
library(survival)
library(survminer)
#Load data

Location <- "/conf/"  # Server
#Location <- "//isdsf00d03/"  # Desktop

EAVE_cohort <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_Demog_Endpoints_Times2020-11-10.rds"))
EAVE_cohort <- filter(EAVE_cohort, !duplicated(EAVE_LINKNO))
rg <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_RG_EAVE.rds"))
rg <- filter(rg, !duplicated(EAVE_LINKNO))
df <- EAVE_cohort %>% select(EAVE_LINKNO:ur6_2016_name, result, death_covid, icu_death, hosp_covid, tested, Time.To.Test:age_gp) %>% 
  left_join(rg, by="EAVE_LINKNO")
EAVE_weights <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_Weights.rds"))
df <- df %>%
  left_join(EAVE_weights, by="EAVE_LINKNO")


#rg <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_RG_Charlson.rds"))
#rg <- filter(rg, !duplicated(EAVE_LINKNO))
#df <- df %>% left_join(rg, by="EAVE_LINKNO")
#rg <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_RG_SMR01.rds"))
#rg <- filter(rg, !duplicated(EAVE_LINKNO))
#df <- df %>% left_join(rg, by="EAVE_LINKNO")

rg <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_RG_EAVE_BP_Smoke.rds"))
rg <- filter(rg, !duplicated(EAVE_LINKNO)) %>% 
  select(EAVE_LINKNO,EAVE_Smoking_Status_Worst, EAVE_BP)
df <- df %>% 
  left_join(rg, by="EAVE_LINKNO")



df <- df %>%  mutate(ageYear = ifelse(ageYear >= 100, 100, ageYear))
df <- mutate(df, EAVE_PREGNANCY = factor(case_when(Sex=="M" ~ "No",
                                                   ageYear <= 13 | ageYear >= 54 ~ "No",
                                                   TRUE ~ as.character(EAVE_PREGNANCY))))

rm(EAVE_cohort, rg)

z.var.list <- names(df)
z.sel.1 <- grepl("^EAVE", z.var.list)
z.sel.2 <- grepl("^CHRL", z.var.list)
z.sel.3 <- grepl("^SMR", z.var.list)
z.var.list <- z.var.list[z.sel.1|z.sel.2|z.sel.3]
z.var.list <- z.var.list[!(z.var.list=="EAVE_LINKNO")]
z.var.omit <- c("eave_weight", "EAVE_CHRONIC_PANCREATITIS","SMR_cystic_fibrosis","SMR_SCD")
z.var.list <- z.var.list[!(z.var.list %in% z.var.omit)]
#summary(df[,z.var.list])
z.response.vars <- c("tested","result","hosp_covid","icu_death","death_covid")

#if (exists("z.out")) rm(z.out)


z.rv <- "death_covid"
z.rv.time <- "Time.To.Death"
#using a random sample of controls for the modelling to speed things up
z.case <- filter(df, get(z.rv)==1)
z.control <- slice_sample(filter(df, get(z.rv)==0), n=nrow(z.case)*100)
z.df <- rbind(z.case,z.control)


z.fmla.min <- as.formula(paste(z.rv," ~ age_gp + Sex + simd2020_sc_quintile + ur6_2016_name"))
z.fit.min <- glm(formula = z.fmla.min  , data=z.df, family=binomial, weight=eave_weight)

z.fmla <- as.formula(paste(z.rv," ~ age_gp + Sex + simd2020_sc_quintile + ur6_2016_name + ",
                           paste(z.var.list, collapse= "+")))

z.fit <- glm(formula = z.fmla  , data=z.df, family=binomial, weight=eave_weight)

z <- MASS::stepAIC(z.fit, direction="backward",k=log(nrow(z.df)))

zt <- names(z$coefficients)
z.var.list <- zt[grepl("^EAVE", zt) | grepl("^CHR",zt) | grepl("^SMR", zt)]
z.var.list <- z.var.list[c(1:13,16)]
z.var.list <- gsub("Yes","",z.var.list)
z.var.list <- gsub("Ex Smoker","",z.var.list)
z.var.list <- gsub("Low","",z.var.list)


#zt[grepl("^EAVE", zt) | grepl("^CHR",zt) | grepl("^SMR", zt)]
#[1] "EAVE_CARE_HOMEYes"        "EAVE_CHRONIC_RESP_DISYes" "EAVE_DEMENTIAYes"        
#[4] "EAVE_DIABETESYes"         "EAVE_HOME_OXYGENYes"      "EAVE_MS_DEGEN_DISYes"    
#[7] "EAVE_MYONEURAL_DISYes"    "CHRL_CVDYes"              "CHRL_DementiaYes"        
#[10] "CHRL_MetastaticYes"       "CHRL_Liver_MSYes"         "SMR_blood_cancerYes"     
#[13] "SMR_circulatory_otherYes" "SMR_kidney_advancedYes"   "SMR_neuro_otherYes"      
#[16] "SMR_resp_infYes"          "SMR_resp_otherYes"  


##cox model
z.fmla <- as.formula(paste("Surv(",z.rv.time,",",z.rv,") ~  pspline(ageYear) + Sex + simd2020_sc_quintile + ",
                                         paste(z.var.list, collapse= "+")))
z.fit <- coxph(z.fmla , data=z.df, weights = eave_weight)
summary(z.fit)


ptemp <- termplot(z.fit, se=TRUE, plot=FALSE)
ageterm <- ptemp$ageYear  # this will be a data frame
center <- with(ageterm, y[x==30])
ytemp <- ageterm$y + outer(ageterm$se, c(0, -1.96, 1.96),'*')
matplot(ageterm$x, exp(ytemp - center), log='y',type='l', lty=c(1,2,2), col=1,
        xlab="Age at Positive Test", ylab="Death Hazard Ratio")
abline(h=1, lty=2)

ageterm <- ptemp$Time.To.Test  # this will be a data frame
center <- with(ageterm, y[x==0])
ytemp <- ageterm$y + outer(ageterm$se, c(0, -1.96, 1.96),'*')
matplot(ageterm$x, exp(ytemp - center), log='y',type='l', lty=c(1,2,2), col=1,
        xlab="Days from March 01", ylab="Hazard Ratio")
abline(h=1, lty=2)



