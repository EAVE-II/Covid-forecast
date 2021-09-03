#05b_Future_Cases_2
#run every simulation


z.join <- full_join(select(z.future, Date, fit), select(Last_Week_agg, -N), by=character())
z.join <- z.join %>% mutate(case = rpois(nrow(z.join), fit)) %>% 
  mutate(cases = rpois(length(P), case*P)) %>% 
  filter(cases>0) %>% arrange(age_gp, n_risk_gps, Date)

#z.join %>% group_by(Date) %>% dplyr::summarise(N=sum(cases)) 

z.comb <- z.join %>% group_by(age_gp, n_risk_gps) %>% dplyr::summarise(N=sum(cases)) %>%
  dplyr::rename(target_sample=N) %>% 
  ungroup() %>% as.data.frame()

#rm(Sampled.Cases)

z.df.sample <- df.sample %>% left_join(z.comb, by=c("age_gp","n_risk_gps")) %>% 
  mutate(target_sample= ifelse(is.na(target_sample),0,target_sample)) 

Sampled.Cases <- z.df.sample %>% group_by(age_gp, n_risk_gps) %>% sample_n(size=target_sample) %>% 
  arrange(age_gp, n_risk_gps) %>%  ungroup()
Sampled.Cases$SpecimenDate <- rep(z.join$Date, z.join$cases)

#z.agg <- z.Sampled.Cases %>% group_by(age_gp, n_risk_gps) %>% dplyr::summarise(tot=n())

#for (i.row in 1:nrow(z.comb)){
  #i.row <- 1
#  z <- slice_sample(filter(df.sample, age_gp==z.comb$age_gp[i.row] & n_risk_gps == z.comb$n_risk_gps[i.row]) , n=z.comb$N[i.row])
#  print(i.row)
#  z.specdate<- filter(z.join, age_gp==z.comb$age_gp[i.row] & n_risk_gps == z.comb$n_risk_gps[i.row])
#  z$SpecimenDate <- rep(z.specdate$Date, z.specdate$cases)
#  Sampled.Cases <- if (exists("Sampled.Cases")) rbind(Sampled.Cases, z) else z
#}


Sampled.Cases <- filter(Sampled.Cases, !duplicated(EAVE_LINKNO))
table(Sampled.Cases$SpecimenDate)
