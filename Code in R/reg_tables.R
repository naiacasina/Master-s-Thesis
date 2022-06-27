# ========================================================================
# reg_tables.R    -   Naia Ormaza Zulueta   -  June 2022
# ========================================================================
# Buffer of 3000m: without FE, time and hh FE, time-district and hh FE

# ------ Load libraries ------
packages <- c("tidyverse", "rdrobust", "fastDummies", "knitr", "lme4",
              "optimx", "stargazer", "oddsratio", "plm","glmmML", "foreign",
              "haven", "labelled", "readxl")
lapply(packages, require, character=TRUE)

# Set working directory
setwd("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/R/Tables/")

# ------- Buffer at 3000m: no FE ---------

load("path")

did.1.mig <- lm(formula = migr ~ t3*f_frac + t3*sw_frac + age + gender + literacy, data=df_buffer3)
summary(did.1.mig)

# Assets
did.1.ass <- lm(formula = assets ~ t3*f_frac + t3*sw_frac + age + literacy + gender, data=df_buffer3)
summary(did.1.ass)

# ------- Buffer at 3000m: time and hh FE ----------
data_main <- pdata.frame(df_buffer3, index=c("HH3", "t3"))
did.2.mig <- plm(formula = migr ~ t3*f_frac + t3*sw_frac + age + gender + literacy, data=data_main, model = "within", effect = "twoways", index = c("HH3","t3"))
summary(did.2.mig)

# Assets
did.2.ass <- plm(formula = assets ~ t3*f_frac + t3*sw_frac + age + literacy + gender, data=data_main, model = "within", effect = "twoways", index = c("HH3","t3"))
summary(did.2.ass)

# ------- Buffer at 3000m: time-district and hh FE ----------

df_buffer3$div <- 0
for (i in 1:length(df_buffer3$upazila)) {
  hh<-df_buffer3$HH[i]
  df_buffer3$div[i] <- bihs11_c1[bihs11_c1$a01==hh,"div_name"]
}

# Create dummy variables:
dataf <- dummy_cols(df_buffer3, select_columns = c('div','t3'))
division  <- unique(df_buffer3$div)

dataf.2 <- dummy_cols(dataf, select_columns = c('div'),
                      remove_selected_columns = TRUE)

dataf.1 <- dataf.2
for (i in 1:length(division)) {
  for (j in 1:2) {
    st <- paste('d.',as.character(i),as.character(j),sep="")
    dataf.1[,st] <- dataf.2[,i+15]*dataf.2[,22+j]
  }
}

dummies <- colnames(dataf.1[,25:dim(dataf.1)[2]])
paste(dummies,collapse = "+")

# Migr
data_main1 <- pdata.frame(dataf.1[,c(1,2,5,6,7,12,13,15,25:38)], index=c("HH3", "t3"))
did.3.mig <- plm(formula = migr ~ t3*f_frac + t3*sw_frac + age + literacy + 
                      d.11+d.12+d.21+d.22+d.31+d.32+d.41+d.42+d.51+d.52+d.61+d.62+d.71
                    , na.action=na.omit,data=data_main1, model = "within",index = c("HH3","t3"))
summary(did.3.mig)
# Assets
did.3.ass <- plm(formula = assets ~ t3*f_frac + t3*sw_frac + age + literacy + 
                      d.11+d.12+d.21+d.22+d.31+d.32+d.41+d.42+d.51+d.52+d.61+d.62+d.71
                    , data=data_main1, model = "within", index = c("HH3","t3"))
summary(did.3.ass)


stargazer(did.1.mig, did.2.mig, did.3.mig,  type = "latex", 
          title="Difference-in-differences estimation: domestic migration on the treatment", out="did_buf_mig.tex")

stargazer(did.1.ass, did.2.ass, did.3.ass,  type = "latex", 
          title="Difference-in-differences Estimation: household assets (value) on the treatment", out="did_buf_ass.tex")


# ---------- Robustness check: 3000m buffer, flood duration -----------

# No FE -----------------
load("path")

did.1.mig_rc <- lm(formula = migr ~ t3*dur_frac + t3*sw_frac + age + gender + literacy, data=df_buffer3_rc)
summary(did.1.mig_rc)

# Assets
did.1.ass_rc <- lm(formula = assets ~ t3*dur_frac + t3*sw_frac + age + literacy + gender, data=df_buffer3_rc)
summary(did.1.ass_rc)


# Time and household FE -----------------
load("path")
data_main <- pdata.frame(df_buffer3_rc, index=c("HH3", "t3"))
did.2.mig_rc <- plm(formula = migr ~ t3*dur_frac + t3*sw_frac + age + gender + literacy, data=data_main, model = "within", effect = "twoways", index = c("HH3","t3"))
summary(did.2.mig_rc)

# Assets
did.2.ass_rc <- plm(formula = assets ~ t3*dur_frac + t3*sw_frac + age + literacy + gender, data=data_main, model = "within", effect = "twoways", index = c("HH3","t3"))
summary(did.2.ass_rc)


# District-specific time and household FE -----------------
load("path")

df_buffer3_rc$div <- 0
for (i in 1:length(df_buffer3_rc$upazila)) {
  hh<-df_buffer3_rc$HH[i]
  df_buffer3_rc$div[i] <- bihs11_c1[bihs11_c1$a01==hh,"div_name"]
}

# Create dummy variables:
dataf <- dummy_cols(df_buffer3_rc, select_columns = c('div','t3'))
division  <- unique(df_buffer3_rc$div)

dataf.2 <- dummy_cols(dataf, select_columns = c('div'),
                      remove_selected_columns = TRUE)

dataf.1 <- dataf.2
for (i in 1:length(division)) {
  for (j in 1:2) {
    st <- paste('d.',as.character(i),as.character(j),sep="")
    dataf.1[,st] <- dataf.2[,i+19]*dataf.2[,26+j]
  }
}

dummies <- colnames(dataf.1[,29:dim(dataf.1)[2]])
paste(dummies,collapse = "+")

# Migr
data_main1 <- pdata.frame(dataf.1[,c(1,2,5,6,7,12,13,15,19,29:dim(dataf.1)[2])], index=c("HH3", "t3"))
did.3.mig_rc <- plm(formula = migr ~ t3*dur_frac + t3*sw_frac + age + literacy + 
                   d.11+d.12+d.21+d.22+d.31+d.32+d.41+d.42+d.51+d.52+d.61+d.62+d.71
                 , na.action=na.omit,data=data_main1, model = "within",index = c("HH3","t3"))
summary(did.3.mig_rc)
# Assets
did.3.ass_rc <- plm(formula = assets ~ t3*dur_frac + t3*sw_frac + age + literacy + 
                   d.11+d.12+d.21+d.22+d.31+d.32+d.41+d.42+d.51+d.52+d.61+d.62+d.71
                 , data=data_main1, model = "within", index = c("HH3","t3"))
summary(did.3.ass_rc)


stargazer(did.1.mig_rc, did.2.mig_rc, did.3.mig_rc,  type = "latex", 
          title="Robustness check: domestic migration on the extent of flooding", out="did_buf_mig_rc.tex")

stargazer(did.1.ass_rc, did.2.ass_rc, did.3.ass_rc,  type = "latex", 
          title="Robustness check: household assets (value) on the extent of flooding", out="did_buf_ass_rc.tex")

# All in one

stargazer(did.1.mig_rc, did.2.mig_rc, did.3.mig_rc,did.1.ass_rc, did.2.ass_rc, did.3.ass_rc,  type = "latex", 
          title="Robustness check: household assets (value) on the extent of flooding", out="did_buf_ass_all.tex")

