#Applied Survival Analysis with R --> PARAMETRIC MODEL
#checking if the data follows the Weibull distrbtion
library(asaur)
library(survival)
install.packages()
#checking if time follows Weibull distribution
result.surv <- survfit(Surv(ttr, relapse) ~ 1)#formula
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

#residuals 
resid.deviance <- residuals(model.pharm.weib, type="deviance")
#par(mfrow=c(2,2))
plot(resid.deviance ~ age)
smoothSEcurve(resid.deviance, age)
title("Deviance residuals\nversus age")
plot(resid.deviance ~ grp)
title("Deviance residuals\nversus treatment group")
plot(resid.deviance ~ employment)
title("Deviance residuals\nversus employment")


###Time effects Weibull
hweibullPH <- function(x, shape, scale = 1, log = FALSE)
  hweibull(x, shape = shape, scale = scale ^ {-1 / shape}, log = log)
HweibullPH <- function(x, shape, scale = 1, log = FALSE)
   Hweibull(x, shape = shape, scale = scale ^ {-1 / shape}, log = log)
custom.weibullPH <- list(name = "weibullPH", pars = c("shape", "scale"),
                             location = "scale", transforms = c(log, log),
                             inv.transforms = c(exp, exp), inits = function(t)
                               c(1, median(t[t > 0]) / log(2)))
fs6 <- flexsurvreg(Surv(recyrs, censrec) ~ group, data = bc,
                      + dist = custom.weibullPH)


