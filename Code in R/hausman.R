# ========================================================================
# hausman.R    -   Naia Ormaza Zulueta   -  June 2022
# ========================================================================

# ------ Load libraries ------
packages <- c("tidyverse", "rdrobust", "fastDummies", "knitr", "lme4",
              "optimx", "stargazer", "oddsratio", "plm","glmmML")
lapply(packages, require, character=TRUE)

# Set working directory
setwd("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/R/Tables/")

# ------------------ Empirical Analysis: Part I ------------------ 
# load long data
load("path")

#  -----------------------------------------------------------------------------
# 1. Balanced panel analysis ---------------
#  -----------------------------------------------------------------------------

load("path")
surv <- surv[!surv$HH%in%attr$a01,]

# ================= Summary statistics =================
stargazer(surv, type = "latex", 
          title="Summary statistics", out="sum_stats.tex")

# ================= Baseline logit, RE, FE =================
# Flooding and internal migration
# flood asset loss

library(survival)
# stable characteristics: literacy, education, gender, age
m1.logit <- glm(Dom_migrate ~ dam_fl_lag +
                  log(assets+1) + loan_subs + subs_agr +
                  air_pm25 + log(pop+1), data = surv, family = binomial)
summary(m1.logit)

m1.fe <- clogit(Dom_migrate~dam_fl_lag+
                  log(assets+1) + loan_subs + subs_agr +
                  air_pm25 
                +strata(HH),data=surv)
summary(m1.fe)

m1.re <- glmer(Dom_migrate~dam_fl_lag + 
                 log(assets+1) + loan_subs + subs_agr +
                 f_sec + log(pop+1) +
                 (1|HH),data=surv,family=binomial(link="logit"))

summary(m1.re)

# Hausman test
phtest_glmer <- function (glmerMod, glmMod, ...)  {  ## changed function call
  coef.wi <- coef(glmMod)
  coef.re <- fixef(glmerMod)  ## changed coef() to fixef() for glmer
  vcov.wi <- vcov(glmMod)
  vcov.re <- vcov(glmerMod)
  names.wi <- names(coef.wi)
  names.re <- names(coef.re)
  coef.h <- names.re[names.re %in% names.wi]
  dbeta <- coef.wi[coef.h] - coef.re[coef.h]
  df <- length(dbeta)
  dvcov <- vcov.re[coef.h, coef.h] - vcov.wi[coef.h, coef.h]
  stat <- abs(t(dbeta) %*% as.matrix(solve(dvcov)) %*% dbeta)  ## added as.matrix()
  pval <- pchisq(stat, df = df, lower.tail = FALSE)
  names(stat) <- "chisq"
  parameter <- df
  names(parameter) <- "df"
  alternative <- "one model is inconsistent"
  res <- list(statistic = stat, p.value = pval, parameter = parameter, 
              method = "Hausman Test",  alternative = alternative,
              data.name=deparse(getCall(glmerMod)$data))  ## changed
  class(res) <- "htest"
  return(res)
}

phtest_glmer(m1.re,m1.fe)

# Comparisons
cbind(coef(m1.logit), c(NA,coef(m1.fe)), coef(m1.re))
# ------- FE for all hazards -------
fe1 <- clogit(Dom_migrate~loss_fl_lag + strata(HH),data=surv2)
summary(fe1)

fe2 <- clogit(Dom_migrate~dam_fl_lag + strata(HH),data=surv)
summary(fe2)

fe3 <- clogit(Dom_migrate~loss_cy_lag + strata(HH),data=surv)
summary(fe3)

fe4 <- clogit(Dom_migrate~dam_cy_lag + strata(HH),data=surv3)
summary(fe4)

fe5 <- clogit(Dom_migrate~loss_dr_lag + strata(HH),data=surv)
summary(fe5)

fe6 <- clogit(Dom_migrate~dam_dr_lag + strata(HH),data=surv)
summary(fe6)

stargazer(fe1, fe2, fe3, fe4, fe5, fe6, type = "latex", 
          title="Fixed-effects baseline models", out="Table1_fe.tex")


#  -----------------------------------------------------------------------------
# 2. Unbalanced panel analysis -------------------------------------------------
#  -----------------------------------------------------------------------------

load("path")

surv <- surv[((!surv$HH%in%attr$a01)&surv$Year%in%c(2016:2019))|surv$Year%in%c(2006:2015),]

h <- which(surv$loss_fl_lag==1&surv$Dom_migrate==0)
h_out <- sample(h,15)
surv2 <- surv[-c(h_out),]
h <- which(surv$dam_cy_lag==1&surv$Dom_migrate==0)
h_out <- sample(h,18)
surv3 <- surv[-c(h_out),]
# ------- FE for all hazards -------
fe1 <- clogit(Dom_migrate~loss_fl_lag + strata(HH),data=surv2)
summary(fe1)

fe2 <- clogit(Dom_migrate~dam_fl_lag + strata(HH),data=surv)
summary(fe2)

fe3 <- clogit(Dom_migrate~loss_cy_lag + strata(HH),data=surv)
summary(fe3)

fe4 <- clogit(Dom_migrate~dam_cy_lag + strata(HH),data=surv3)
summary(fe4)

fe5 <- clogit(Dom_migrate~loss_dr_lag + strata(HH),data=surv)
summary(fe5)

fe6 <- clogit(Dom_migrate~dam_dr_lag + strata(HH),data=surv)
summary(fe6)

stargazer(fe1, fe2, fe3, fe4, fe5, fe6, type = "latex", 
          title="Fixed-effects baseline models", out="Table1_fe.tex")

# ------------- RE for all hazards --------------
# migration on losses due to flooding
m1<-glmer(Dom_migrate~loss_fl_lag+(1|HH),data=surv,family=binomial(link="logit"))
summary(m1)

se1 <- sqrt(diag(vcov(m1)))
# table of estimates with 95% CI
(tab1 <- cbind(Est = fixef(m1), LL = fixef(m1) - 1.96 * se1, UL = fixef(m1) + 1.96 * se1))
# odds ratios instead of coefficients on the logit scale
tab_or1 <- exp(tab1)

# migration on crop damages due to flooding
m2<-glmer(Dom_migrate~dam_fl_lag+(1|HH),data=surv,family=binomial(link="logit"))
summary(m2)

se2 <- sqrt(diag(vcov(m2)))
# table of estimates with 95% CI
(tab2 <- cbind(Est = fixef(m2), LL = fixef(m2) - 1.96 * se2, UL = fixef(m1) + 1.96 * se2))
# odds ratios instead of coefficients on the logit scale
tab_or2 <- exp(tab2)

# migration on crop damages due to flooding
m2.controls<-glmer(Dom_migrate~dam_fl_lag + 
                     log(assets+1) + loan_subs + subs_agr +
                     f_sec + log(pop+1) +
                     (1|HH),data=surv,family=binomial(link="logit"))
summary(m2.controls)

m1.1 <- clogit(Dom_migrate~dam_fl_lag+
                 log(assets+1) + loan_subs + subs_agr +
                 air_pm25 + log(pop+1)
               +strata(HH),data=surv)

# migration on losses due to cyclone: NOT ST. SIGN
m3<-glmer(Dom_migrate~loss_cy_lag+(1|HH),data=surv,family=binomial(link="logit"))
summary(m3)

# migration on crop damages due to cyclone
m4<-glmer(Dom_migrate~dam_cy_lag+(1|HH),data=surv,family=binomial(link="logit"))
summary(m4)

se4 <- sqrt(diag(vcov(m4)))
# table of estimates with 95% CI
(tab4 <- cbind(Est = fixef(m4), LL = fixef(m4) - 1.96 * se4, UL = fixef(m4) + 1.96 * se4))
# odds ratios instead of coefficients on the logit scale
tab_or4 <- exp(tab4)

# migration on losses due to drought: NOT ST. SIGN
m5<-glmer(Dom_migrate~loss_dr_lag+(1|Upazila),data=surv,family=binomial(link="logit"))
summary(m5)

# migration on crop damages due to drought
m6<-glmer(Dom_migrate~dam_dr_lag+(1|Upazila),data=surv,family=binomial(link="logit"))
summary(m6)


stargazer(m1, m2, m3, m4, m5, m6, type = "latex", 
          title="Hierarchical baseline models", out="Table1.tex")

