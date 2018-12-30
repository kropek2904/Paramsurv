#Applied Survival Analysis with R --> PARAMETRIC MODEL
#checking if the data follows the Weibull distrbtion
library(survival)
#checking if time follows Weibull distribution
result.surv <- survfit(Surv() ~ 1)#formula
survEst <- result.surv$surv
survTime <- result.surv$time
logLogSurvEst <- log(-log(survEst))
logSurvTime <- log(survTime)
result.lm <- lm(logLogSurvEst ~ logSurvTime)
result.lm
plot(logLogSurvEst ~ logSurvTime)
abline(result.lm)


#model weibull distribution + STEP
model.pharm.weib <- survreg(Surv(ttr, relapse) ~ grp + age +
                              + employment, dist="weibull")
modelAll.pharm.weib <- survreg(Surv(ttr, relapse) ~ grp + gender
                               + race + employment + yearsSmoking + levelSmoking + age + priorAttempts + longestNoSmoke,
                               dist="weibull")
model.step.pharm.weib <- step(modelAll.pharm.weib)

###Time effects Weibull

