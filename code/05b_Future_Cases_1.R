#05b_Future_Cases_1
#run once at the beginning of each scenario

#J:\Statistics\Chris\Respiratory\Coronavirus\Eave_Covid_19\Data\Codes
#z <- c(0.03665624, 0.05364327, 0.39807778, 0.12047385, 0.30576665, 0.08538221)
#z <- c(0.03704227, 0.04283897, 0.31005231, 0.144,63453, 0.34426693, 0.12116499)
#z <- c(0.03920872, 0.04157783, 0.25302061, 0.16240227, 0.37941246, 0.12437811)
#z <- c(0.04473019, 0.04681833, 0.21859545, 0.17243653, 0.37652489, 0.14089460)
#z <- c(0.04499423, 0.04794257, 0.21663889, 0.16600436, 0.37854121, 0.14587873)
#z <- c(0.05924769, 0.06201178, 0.21535873, 0.16993150, 0.36473981, 0.12871049)
#z <- c(0.05906366, 0.06837607, 0.21558872, 0.16315857, 0.37020028, 0.12361271)
#z <- c(0.05417073, 0.05765214, 0.21111266, 0.16752541, 0.36485169, 0.14468737)
#z <- c(0.05504587, 0.06014271, 0.20387360, 0.16904519, 0.37716616, 0.13472647)
#z <- c(0.06125545, 0.06068652, 0.20102408, 0.17162905, 0.35293002, 0.15247487)
#z <- c(0.05843496, 0.05860434, 0.19986450, 0.16073848, 0.36585366, 0.15650407)
#z <- c(0.06040699, 0.05543983, 0.18618811, 0.17000481, 0.37493991, 0.15302035)
#z <- c(0.05523542, 0.04286718, 0.21096275, 0.17512298, 0.37371750, 0.14209417)
# z <- c(0.04362480 ,0.04515103 ,0.24190779, 0.16591415, 0.35910970, 0.14429253) #11 Jan 2021
#z <- c(0.15373097, 0.05390567,0.22984777, 0.19266284 ,0.31794360, 0.05190916) #March 16
z <- c(0.14267756, 0.07919547, 0.21810182,0.22061596, 0.28724073, 0.05216845) #April 20

z <- readRDS("//conf/EAVE/GPanalysis/progs/CR/all_ecoss_prop_age29Aug.rds")

Last_Week_Ecoss_Age <-data.frame(age_gp=factor(1:6,labels=c("0-11","12-17","18-29","30-39","40-64","65+")),
                                 P_Age = z)
#combine the hospital and death 
#Eave_cohort to sample from
z.names <- c("EAVE_LINKNO", "ageYear","Time.To.Test","Sex","simd2020_sc_quintile", "SpecimenDate", "n_risk_gps", 
              unique(c(prediction_vars_h, prediction_vars_d)))
df.sample <- EAVE_cohort %>% left_join(rg, by="EAVE_LINKNO") %>% 
  select(all_of(z.names)) %>% 
  filter(!(EAVE_LINKNO %in% Prediction_cohort$EAVE_LINKNO)) %>% 
  mutate(age_gp = cut(ageYear, breaks=c(-1,11,17,29,39,64,110), 
                      labels=c("0-11","12-17","18-29","30-39","40-64","65+")) ) %>% 
  arrange(age_gp,n_risk_gps)


Last_Week_cases <- Prediction_cohort %>% 
  filter(SpecimenDate >= max(SpecimenDate) - 9) %>%
  filter(SpecimenDate < max(SpecimenDate) - 2) %>% 
  mutate(age_gp = cut(ageYear, breaks=c(-1,11,17,29,39,64,110), 
                      labels=c("0-11","12-17","18-29","30-39","40-64","65+")) )
#dont use this for forecasts as the linked data does not have all cases
Last_Week_agg <- Last_Week_cases %>% group_by(age_gp, n_risk_gps) %>% 
  dplyr::summarise(N=n()) %>% ungroup() %>% 
  group_by(age_gp) %>% 
  mutate(P_given_age=N/sum(N)) %>% ungroup() %>% 
  left_join(Last_Week_Ecoss_Age) %>% 
  mutate(P=P_given_age*P_Age)

#Last_Week_agg <- Last_Week_cases %>% group_by(age_gp, n_risk_gps) %>% 
#  dplyr::summarise(N=n()) %>% ungroup() %>% 
#  mutate(P=N/sum(N))


Last_Week_agg %>% dplyr::select(-N, -P_given_age, -P_Age) %>% pivot_wider(names_from=n_risk_gps, values_from=P )

z.title <- paste0("Distribution of positive cases, week beginning ",format(min(Last_Week_cases$SpecimenDate), "%d %b") )
ggplot(Last_Week_agg, aes(x=age_gp, fill=n_risk_gps)) + geom_col(aes(y=P))+
  labs(x="Age Group", y="Proportion",title=z.title, fill = "Number of \nRisk Groups") +
  theme(plot.title = element_text(size=11))
png("/conf/EAVE/GPanalysis/outputs/temp/distribution_pos.png", width = 14, height=8, units= "cm", res=200)#)
ggplot(Last_Week_agg, aes(x=age_gp, fill=n_risk_gps)) + geom_col(aes(y=P))+
  labs(x="Age Group", y="Proportion",title=z.title, fill = "Number of \nRisk Groups") +
  theme(plot.title = element_text(size=11))
dev.off()

Scotland.Cases <- read.csv(paste0(file_path, "Cumulative_Reports_HB_SpecimenDate.csv") )%>% 
  dplyr::select(SpecimenDate, Scotland) %>% 
  mutate(N = Scotland - lag(Scotland),
         Date= as.Date(as.character(SpecimenDate))) %>% 
  dplyr::select(-Scotland, -SpecimenDate)

#should not be need
#Scotland.Cases <- filter(Scotland.Cases, Date < as.Date("2020-10-02") ) %>% 
#  bind_rows(data.frame(N=c(831,719,748,1108,1192,1257,1146,904,860,904,249), 
#                       Date = seq.Date(as.Date("2020-10-02"), as.Date("2020-10-12"), by="days")) )

if (forecasting_cases == "Exponential") { 
  z <- filter(Scotland.Cases, Date >= max(Date) -71 & Date < max(Date) - 1)
  z$Time <- as.numeric(z$Date - max(z$Date))
  library(mgcv)
  z.fit.cases <- gam(N ~ s(Time, k=4), data=z, family=poisson)
  #  z.dt <- log(2)/c(z.fit.cases$coefficients[2] ,confint.default(z.fit.cases)[2,c(2,1)])
  #  names(z.dt) <- c("DT.days","LCL","UCL")
  #  print(round(z.dt,2))
  z$fit <- predict(z.fit.cases, type="response")
  z.future <- data.frame(Date=seq.Date(from=max(z$Date)+1, by="days", length.out=28))
  z.future$Time <- as.numeric(z.future$Date - max(z$Date))
  #Future_Growth <- 0.5
  #  z.fit.cases$coefficients[1] <-  z.fit.cases$coefficients[1]*Future_Growth
  z.future$fit0 <-rep(z$fit[length(z$fit)], nrow(z.future) ) # no growth or decay
  z.future$fit1 <- predict(z.fit.cases,newdata=z.future, type="response") # model
  z.future$fit <- (1-Future_Growth)*z.future$fit0 + Future_Growth*z.future$fit1 #weighted average
  z_sd <- sqrt(z_variance_reduction*z.future$fit*z.fit.cases$deviance/z.fit.cases$df.residual)
  z.future$case <- round(rnorm(nrow(z.future), z.future$fit, sd=z_sd))
#  z.future$case <- round(rnorm(nrow(z.future), z.future$fit, sd=sqrt(z.future$fit)))
  z.future$case <- abs(z.future$case) # to ensure never zero
  print(z$N)
  print(z.future$case)
  z.title <- forecasting_cases 
}

if (forecasting_cases == "SG") {
  SG_Forecast <- read.csv(paste0(file_path, "SG_new_cases_forecast.csv")) %>% 
    mutate(Date=as.Date(as.character(Date), format="%d/%m/%Y") + 5) %>% 
    arrange(Scenario_name, Date, Quantile, desc(Infections)) %>% 
    group_by(Scenario_name, Date, Quantile) %>%  
    dplyr::summarise(Infections=first(Infections)) %>% ungroup() 
  
  z.c <- filter(SG_Forecast, Scenario_name==SG_type) %>% 
    dplyr::select(-Scenario_name) %>% 
    mutate(Quantile = paste0("Q",Quantile)) %>% 
    pivot_wider(names_from="Quantile", values_from="Infections") %>% 
#    mutate(Date=as.Date(as.character(Date), format="%d/%m/%Y") + 5) %>%  #add 5 days for infection to symptoms
    left_join(dplyr::select(Scotland.Cases, Date, N)) %>% 
    mutate(ratio = N/Q0.5)
  
  z.mult <- z.c$ratio[!is.na(z.c$ratio)]
  z.mult <- z.mult[1:(length(z.mult)-3)]
  z.mult <- median(z.mult)
  z.c <- z.c %>% mutate(Q0.95=Q0.95*z.mult,
                        Q0.5=Q0.5*z.mult,
                        Q0.05=Q0.05*z.mult) %>% 
    mutate(sd = (Q0.95-Q0.05)/4) %>% 
    mutate(case = round(rnorm(nrow(z.c), Q0.5, sd=sd)))
  
  z <- filter(z.c, !is.na(N))
  z <- filter(z, row_number() <= (nrow(z) -2)) %>% 
    dplyr::rename(fit=Q0.5)
  z.future <- filter(z.c, Date > max(z$Date)) %>% 
    dplyr::rename(fit=Q0.5)
  z.title <- paste0(forecasting_cases," ",SG_type," Forecasts") 
}

z.colour=c("Data"="black", "Forecasts" = "blue")
gg.pred <- ggplot(z, aes(x=Date, y=N)) + geom_point(aes(colour="Data")) + geom_line(aes(y=fit, colour="Data")) +
  labs(x="Date", y="Number of Cases", title=z.title) +
  geom_point(aes(y=case, colour="Forecasts"), data=z.future) + 
  geom_line(aes(y=fit, colour="Forecasts"), data = z.future) +
  scale_color_manual(values=z.colour) + theme(legend.title=element_blank())

 print(gg.pred) 
 
 png(paste0("/conf/EAVE/GPanalysis/outputs/temp/", z.title, Future_Growth,".png"))
 gg.pred
 dev.off()

