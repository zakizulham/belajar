df <- read.csv("heart_failure_clinical_records_dataset.csv")
head(df)

# All
sfit <- survfit(Surv(time, DEATH_EVENT)~1, data=df)
sfit
summary(sfit)
plot(sfit)
ggsurvplot(sfit)
ggsurvplot(sfit, conf.int=TRUE, pval=TRUE,risk.table=TRUE,risk.table.height=.5)

# Sex
sfit <- survfit(Surv(time, DEATH_EVENT)~sex, data=df)
sfit
summary(sfit)
plot(sfit)
ggsurvplot(sfit)

ggsurvplot(sfit, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           legend.labs=c("Female", "Male"), legend.title="Sex",  
           palette=c("dodgerblue2", "orchid2"), 
           main="Kaplan-Meier Curve for Heart Failure Survival", 
           risk.table.height=.25)
# Anaemia
sfit <- survfit(Surv(time, DEATH_EVENT)~anaemia, data=df)
sfit
summary(sfit)
plot(sfit)
ggsurvplot(sfit)

ggsurvplot(sfit, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           legend.labs=c("Tidak Penderita", "Penderita"), legend.title="Anemia",  
           palette=c("dodgerblue2", "orchid2"), 
           main="Kaplan-Meier Curve for Heart Failure Survival", 
           risk.table.height=.25)
# Tekanan Darah tinggi
sfit <- survfit(Surv(time, DEATH_EVENT)~high_blood_pressure, data=df)
sfit
summary(sfit)
plot(sfit)
ggsurvplot(sfit)

ggsurvplot(sfit, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           legend.labs=c("Tidak Penderita", "Penderita"), legend.title="Tekanan Darah Tinggi",  
           palette=c("dodgerblue2", "orchid2"), 
           main="Kaplan-Meier Curve for Heart Failure Survival", 
           risk.table.height=.25)

# Merokok
sfit <- survfit(Surv(time, DEATH_EVENT)~smoking, data=df)
sfit
summary(sfit)
plot(sfit)
ggsurvplot(sfit)

ggsurvplot(sfit, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           legend.labs=c("bukan", "merokok"), legend.title="Perokok",  
           palette=c("dodgerblue2", "orchid2"), 
           main="Kaplan-Meier Curve for Heart Failure Survival", 
           risk.table.height=.25)
# Diabetes
sfit <- survfit(Surv(time, DEATH_EVENT)~diabetes, data=df)
sfit
summary(sfit)
plot(sfit)
ggsurvplot(sfit)

ggsurvplot(sfit, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           legend.labs=c("bukan", "penderita"), legend.title="Diabetes",  
           palette=c("dodgerblue2", "orchid2"), 
           main="Kaplan-Meier Curve for Heart Failure Survival", 
           risk.table.height=.25)
# CoxPH

fit <- coxph(Surv(time, DEATH_EVENT)~high_blood_pressure, data=df, method= 'breslow')
fit

summary(fit)
survdiff(Surv(time, DEATH_EVENT)~high_blood_pressure, df)

# mengubah numerik ke dalam kategorik

library(survival)
library(KMsurv)
library(ggplot2)

fit <- coxph(Surv(time, DEATH_EVENT)~high_blood_pressure+anaemia, data=HeartFailureData, method= 'breslow')
fit

summary(fit)
survdiff(Surv(time, DEATH_EVENT)~high_blood_pressure+anaemia, data = HeartFailureData)


#untuk variabel kategorik baru yang dimanipulasi dari numerik
survdiff(Surv(time, DEATH_EVENT)~age_new, data = HeartFailureDataModified)
survdiff(Surv(time, DEATH_EVENT)~creapho_new, data = HeartFailureDataModified)
survdiff(Surv(time, DEATH_EVENT)~EF_new, data = HeartFailureDataModified)
survdiff(Surv(time, DEATH_EVENT)~platelets_new, data = HeartFailureDataModified)
survdiff(Surv(time, DEATH_EVENT)~ss_new, data = HeartFailureDataModified)
survdiff(Surv(time, DEATH_EVENT)~sc_new, data = HeartFailureDataModified)
#model 3
fit <- coxph(Surv(time, DEATH_EVENT)~high_blood_pressure+ss_new+sc_new+age_new, data=HeartFailureDataSuperUltimate, method= 'breslow')
fit

summary(fit)

survdiff(Surv(time, DEATH_EVENT)~high_blood_pressure+ss_new+sc_new+age_new, data = HeartFailureDataSuperUltimate)
#Model 4
fit2 <- coxph(Surv(time, DEATH_EVENT)~high_blood_pressure+anaemia+ss_new+sc_new+age_new, data=HeartFailureDataSuperUltimate, method= 'breslow')
fit2

summary(fit2)

survdiff(Surv(time, DEATH_EVENT)~high_blood_pressure+ss_new+sc_new+age_new+anaemia, data = HeartFailureDataSuperUltimate)

