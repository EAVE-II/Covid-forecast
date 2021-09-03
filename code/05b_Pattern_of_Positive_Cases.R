##########################################################
# Name of file: 05b_Pattern_of_Positive_Cases.R
# Data release (if applicable):
# Original author(s): Chris Robertson chrisobertson@nhs.net
# Original date: 06 August 2020
# Latest update author (if not using version control) - Chris Robertson chrisobertson@nhs.net
# Latest update date (if not using version control) - 
# Latest update description (if not using version control)
# Type of script: Descriptive stats
# Written/run onL: R Studio SERVER
# Version of R that the script was most recently run on: R 3.6.1
# Description of content: reads in new positives and merges in the risk groups 
#                         plots the patterns of ages, gender and risk groups over time
# Approximate run time: Unknown
##########################################################

# 01 Setup ####
#Libraries
library(tidyverse)
#Load data

Location <- "/conf/"  # Server
#Location <- "//isdsf00d03/"  # Desktop

EAVE_cohort <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_Demog_Endpoints_Times.rds"))
EAVE_cohort <- filter(EAVE_cohort, !duplicated(EAVE_LINKNO))
rg <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_RG_EAVE.rds"))
rg <- filter(rg, !duplicated(EAVE_LINKNO))
#rg <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_RG_Charlson.rds"))
#rg <- filter(rg, !duplicated(EAVE_LINKNO))
#rg <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_RG_SMR01.rds"))
#rg <- filter(rg, !duplicated(EAVE_LINKNO))

ECOSS_cohort <- readRDS(paste0(Location,"EAVE/GPanalysis/data/ECOSSdeduped_linked_2021-04-20.rds"))
max_spec <- as.Date(file.info(paste0(Location,"EAVE/GPanalysis/data/ECOSSdeduped_linked_2021-04-20.rds"))$ctime) 
ECOSS_cohort <- filter(ECOSS_cohort, !duplicated(EAVE_LINKNO))
ECOSS_cohort <- EAVE_cohort %>% select(EAVE_LINKNO:ur6_2016_name, result, death_covid, icu_death, hosp_covid, tested, Time.To.Test:age_gp) %>% 
  left_join(ECOSS_cohort, by="EAVE_LINKNO") %>% 
  filter(NCOV_RESULT=="Positive") 
ECOSS_cohort <-left_join(ECOSS_cohort, rg, by="EAVE_LINKNO")

ECOSS_cohort <- ECOSS_cohort %>%  mutate(ageYear = ifelse(ageYear >= 100, 100, ageYear),
                                                   month = months(SpecimenDate),
                                                  month = if_else(month=="February", "March", month),
                                                  month = factor(month,levels=c("March","April","May","June","July","August","September"))
                                                     ) %>% 
  filter(SpecimenDate >= as.Date("2020-03-01") & SpecimenDate <= max_spec ) %>% 
  mutate(age_gp = cut(ageYear, breaks=c(-1,1, 4, 11, 17, 39, 64, 79, 101),
                      labels=c("0-1","2-4","5-11","12-17","18-39","40-64","65-79","80+")))
levels(ECOSS_cohort$n_risk_gps) <- paste0("RG-",levels(ECOSS_cohort$n_risk_gps))
ECOSS_cohort$ur6_2016_name <- factor(ECOSS_cohort$ur6_2016_name, exclude=NULL)
levels(ECOSS_cohort$ur6_2016_name)[7] <- "NA"



fun.plot <- function(z.var, z.title=z.var, legend_label=z.var, plot_data=TRUE) {
z.names <- if (is.factor(ECOSS_cohort[,z.var])) levels(ECOSS_cohort[,z.var]) else as.character(unique(ECOSS_cohort[,z.var]))
z.group.cols <- c("SpecimenDate",z.var)
df <- ECOSS_cohort %>% group_by(across(all_of(z.group.cols))) %>% dplyr::summarise(N=n()) %>% 
  pivot_wider(names_from=all_of(z.var),values_from=N, values_fill=0) %>% ungroup()
df.tmp <- df %>% dplyr::select(-SpecimenDate) %>% apply(1,sum)
df <- df %>% mutate(Total = df.tmp) 
df <-   mutate(df, across(all_of(z.names), ~./Total)) %>% 
#df <-   mutate_at(df, z.names, ~./Total) %>% 
  pivot_longer(cols=all_of(z.names))

#df %>% ggplot(aes(x=SpecimenDate, y=value, colour=name)) +geom_point() + geom_smooth() +
#  labs(x="Date Sample Taken", y= "Proportion", title=paste("Positive Test Case Mix -",z.var))

if(plot_data) 
{df %>% ggplot(aes(x=SpecimenDate, y=value)) +geom_point(aes(size=Total), alpha=0.2, shape=21, stroke=2) + 
  geom_smooth(span=0.5, size=2) +
  facet_wrap(~name, ncol=2, scales="free_y")+
  labs(x="Date Sample Taken", y= "Proportion", title=paste("Positive Test Case Mix -",z.title)) }
    
if(!plot_data) 
{df %>% ggplot(aes(x=SpecimenDate, y=value, colour=name)) + 
  geom_smooth(span=0.5, size=1, se=FALSE) + 
  labs(x="Date Sample Taken", y= "Proportion", 
       title=paste("Positive Test Case Mix -",z.title), colour=legend_label) } 


}

gg.sex <- fun.plot(z.var = "Sex", plot_data=FALSE) # don't use
gg.rg <- fun.plot(z.var = "n_risk_gps", z.title="Number of Co-Morbidity Risk Groups",
                  legend_label="Risk Group", plot_data=FALSE)
gg.age <- fun.plot(z.var="age_gp" , z.title="Age group",
                   legend_label="Age group", plot_data=FALSE)#don't use

gg.simd <- fun.plot(z.var="simd2020_sc_quintile", z.title="Deprivation Quintile",
                    legend_label="SIMD", plot_data=FALSE)
gg.ur <- fun.plot(z.var="ur6_2016_name", z.title="Urban Rural Classification",
                  legend_label="Status", plot_data=FALSE)

z.last.date <- max(ECOSS_cohort$SpecimenDate)
z <- ECOSS_cohort %>% filter(SpecimenDate >= z.last.date -27 & SpecimenDate <= z.last.date -21) %>% 
  group_by(age_gp, n_risk_gps) %>% dplyr::summarise(N=n()) %>% 
  pivot_wider(names_from = n_risk_gps, values_from=N)

#save plots for the powerpoint summary
png("/conf/EAVE/GPanalysis/outputs/temp/casemix_sex.png",  width = 14, height=8, units= "cm", res=200)#
gg.sex
dev.off()
png("/conf/EAVE/GPanalysis/outputs/temp/casemix_age.png", width = 14, height=8, units= "cm", res=200)#
gg.age
dev.off()
png("/conf/EAVE/GPanalysis/outputs/temp/casemix_simd.png",  width = 14, height=8, units= "cm", res=200)#
gg.simd
dev.off()
png("/conf/EAVE/GPanalysis/outputs/temp/casemix_ur.png",  width = 14, height=8, units= "cm", res=200)#
gg.ur
dev.off()
png("/conf/EAVE/GPanalysis/outputs/temp/casemix_rg.png", width = 14, height=8, units= "cm", res=200)#
gg.rg
dev.off()


#####################

#get Vaccinations from 01a_Vaccinations_input.R in Vaccine

z <- EAVE_cohort %>% left_join(Vaccinations , by="EAVE_LINKNO") %>% 
  mutate(Vaccinated_1 = if_else(is.na(date_vacc_1), 0,1),
         Vaccinated_2= if_else(is.na(date_vacc_2), 0,1))
ew <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_Weights.rds"))
ew <- filter(ew, !duplicated(EAVE_LINKNO))

z <- z %>%  left_join(ew, by="EAVE_LINKNO")
z <- z %>%  left_join(rg, by="EAVE_LINKNO")
#adding 1 to age to get age at march 2021
z <- z %>%  mutate(age_gp = cut(ageYear+1, breaks=c(-1,11,17,29,39,64,110), 
                                         labels=c("0-11","12-17","18-29","30-39","40-64","65+")) )
ad <- readRDS(paste0(Location,"EAVE/GPanalysis/data/all_deaths.rds"))
z <- z %>% dplyr::select(-NRS.Date.Death) %>% 
  left_join(dplyr::select(ad, EAVE_LINKNO, NRS.Date.Death), by="EAVE_LINKNO") 
z1 <- z %>%   filter(is.na(NRS.Date.Death) )
z_agg <- z1 %>% 
  filter(!is.na(n_risk_gps)) %>% 
  group_by(age_gp, n_risk_gps) %>% 
  dplyr::summarise(N=round(sum(eave_weight)), v1=sum(Vaccinated_1), v2=sum(Vaccinated_2)) %>%
  ungroup() %>% 
  mutate(x = v1/sum(v1)) %>% 
  mutate(v1x = round(v1 + 111583*x)) %>%  #change this to make table(Vaccinations$EAVE_LINKNO %in% z1$EAVE_LINKNO)
  mutate(v1 = v1x/N, v2=v2/N) %>% 
  filter(!(age_gp %in% c("0-11","12-17")))

z_agg %>% ggplot(aes(x=age_gp, y= v1, fill= n_risk_gps)) + geom_col(position="dodge") +
  labs(x="Age Group", y="Proporion Vaccinated", fill="Number of\nRisk Groups", 
       title= "Vaccination Dose 1 Uptake", subtitle = format(max(Vaccinations$date_vacc_1), "%d %B %Y"))

z_agg %>% ggplot(aes(x=age_gp, y= v2, fill= n_risk_gps)) + geom_col(position="dodge") +
  labs(x="Age Group", y="Proporion Vaccinated", fill="Number of\nRisk Groups", 
       title= "Vaccination Dose 2 Uptake", subtitle = format(max(Vaccinations$date_vacc_1), "%d %B %Y"))

