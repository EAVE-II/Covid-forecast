#05b_Model_Selection

z.var.list <- names(df)
z.var.list <- z.var.list[grepl("^EAVE", z.var.list)]
#z.var.list <- z.var.list[grepl("^DIAG", z.var.list)]
#z.var.list <- z.var.list[grepl("^CHRL", z.var.list)]
#z.var.list <- z.var.list[grepl("^SMR", z.var.list)]
z.var.list <- z.var.list[!(z.var.list=="EAVE_LINKNO")]
#summary(df[,z.var.list])

#Response var
z.rv <- "hosp_covid" 
z.rv.time <- "Time.Test.Hosp" 

#get data into shape for fitting
z.df <- df
fmla.coxph.curve <- as.formula(paste("Surv(",z.rv.time,",",z.rv,") ~ Sex"))
plot(survfit(fmla.coxph.curve, data=z.df))

#survival curve tails off at 40 days so censor - for both endpoints
df.fit <- df %>%  mutate(hosp_covid= if_else(Time.Test.Hosp > 40, 0, hosp_covid),
                         Time.Test.Hosp= if_else(Time.Test.Hosp > 40, 40, Time.Test.Hosp),
                         death_covid= if_else(Time.Test.Death > 40, 0, death_covid),
                         Time.Test.Death= if_else(Time.Test.Death > 40, 40, Time.Test.Death) )

z.df <- df.fit %>% dplyr::select_at(c(z.var.list, z.rv, z.rv.time,
                                  "ageYear","Time.To.Test", "Sex","simd2020_sc_quintile")) %>% 
  pivot_longer(cols=all_of(z.var.list))


#df.orig <- df
#df <- filter(z.df, name=="EAVE_ASTHMA")

fn.fit.coxph <- function(df, time_var="Time.Test.Death", status_var="death_covid"){
  #fits a coxph model and keeps the coefficients (HR+CI) for the explanatory variable of interest (Values)
  #status_var is the response variable, time_var its associated time to var
  z.fit <- coxph(Surv(get(time_var), get(status_var)) ~  pspline(ageYear) + pspline(Time.To.Test) + Sex + simd2020_sc_quintile + value, data=df)
  z.t <- as.data.frame(summary(z.fit)$conf.int)
  z.sel <- grepl("value", rownames(z.t)) 
  z.t <- z.t[z.sel,c(1,3,4)]
  colnames(z.t) <- c("HR","LCL","UCL")
  z.t1 <- as.data.frame(summary(z.fit)$coefficients)
  z.sel <- grepl("value", rownames(z.t1)) 
  z.t1 <- z.t1[z.sel,"p"]
  z.t <- cbind(Var=rep(unique(df$name), nrow(z.t)), Name=rownames(z.t), Response=rep(status_var,nrow(z.t)), z.t, P= z.t1)
  z.t
}

z.out <- plyr::ddply(z.df, .(name), fn.fit.coxph,time_var=z.rv.time, status_var=z.rv )

#variables to include in final model
z <- z.out %>% filter(P < 0.05)
prediction_vars <- z$name
prediction_vars <- prediction_vars[prediction_vars != "EAVE_PER_VASCULAR_DIS"] # hosp_covid
prediction_vars <- prediction_vars[prediction_vars != "EAVE_STROKE_TIA"] # hosp_covid
prediction_vars <- prediction_vars[prediction_vars != "EAVE_MS_DEGEN_DIS"] # hosp_covid
prediction_vars <- prediction_vars[prediction_vars != "EAVE_HOME_OXYGEN"] # hosp_covid
prediction_vars <- prediction_vars[prediction_vars != "EAVE_ULCER_DIS"] # hosp_covid
prediction_vars <- prediction_vars[prediction_vars != "EAVE_MYONEURAL_DIS"] # hosp_covid
prediction_vars <- prediction_vars[prediction_vars != "EAVE_ASTHMA"] # hosp_covid

prediction_vars <- prediction_vars[prediction_vars != "EAVE_HYPERTENSION"] # hosp_covid
prediction_vars <- prediction_vars[prediction_vars != "EAVE_IMMUNOSUPPRESSION"] # hosp_covid

#prediction_vars <- prediction_vars[prediction_vars != "EAVE_DIABETES"] # death_covid
z.df <- df.fit

fmla.final <- as.formula(paste("Surv(",z.rv.time,",",z.rv,") ~  pspline(ageYear) + pspline(Time.To.Test) + Sex + simd2020_sc_quintile + ",
                                 paste(prediction_vars, collapse= "+")))
z.fit <- coxph(fmla.final , data=z.df)
summary(z.fit)
