# ========================================================================
# reg_tables_appendix.R    -   Naia Ormaza Zulueta   -  June 2022
# ========================================================================
# Buffer of 3000m: without FE, time and hh FE, time-district and hh FE

# ------ Load libraries ------
packages <- c("tidyverse", "rdrobust", "fastDummies", "knitr", "lme4",
              "optimx", "stargazer", "oddsratio", "plm","glmmML", "foreign",
              "haven", "labelled", "readxl")
lapply(packages, require, character=TRUE)

# Set working directory
setwd("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/R/Tables/")


# -------------- Time-District and hh FE ------------------
# Buffer at 5000m -----------------
load("path")

bihs11_c1 <- read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2011-2012/Control Vars/001_mod_a_male.dta")

df_buffer$div <- 0
for (i in 1:length(df_buffer$upazila)) {
  hh<-df_buffer$HH[i]
  df_buffer$div[i] <- bihs11_c1[bihs11_c1$a01==hh,"div_name"]
}

# Create dummy variables:
dataf <- dummy_cols(df_buffer, select_columns = c('div','t'))
division  <- unique(df_buffer$div)

dataf.2 <- dummy_cols(dataf, select_columns = c('div'),
                      remove_selected_columns = TRUE)

dataf.1 <- dataf.2
for (i in 1:length(division)) {
  for (j in 1:2) {
    st <- paste('d.',as.character(i),as.character(j),sep="")
    dataf.1[,st] <- dataf.2[,i+23]*dataf.2[,30+j]
  }
}

dummies <- colnames(dataf.1[,33:dim(dataf.1)[2]])
paste(dummies,collapse = "+")

# Migration
data_main <- pdata.frame(dataf.1[,c(1,2,5,6,7,12,14,15,16,33:dim(dataf.1)[2])], index=c("HH", "t"))
did.5.mig <- plm(formula = migr ~ t*f_frac + t*sw_frac + age + literacy + 
                  d.11+d.12+d.21+d.22+d.31+d.32+d.41+d.42+d.51+d.52+d.61+d.62+d.71
                , na.action=na.omit,data=data_main, model = "within", index = c("HH","t"))
summary(did.5.mig)

# Assets
did.5.ass <- plm(formula = assets ~ t*f_frac + t*sw_frac + age + literacy + 
                    d.11+d.12+d.21+d.22+d.31+d.32+d.41+d.42+d.51+d.52+d.61+d.62+d.71
                  , data=data_main, model = "within", index = c("HH","t"))
summary(did.5.ass)

# Buffer at 4000m -----------------
load("path")
df_buffer2$div <- 0
for (i in 1:length(df_buffer2$upazila)) {
  hh<-df_buffer2$HH[i]
  df_buffer2$div[i] <- bihs11_c1[bihs11_c1$a01==hh,"div_name"]
}

# Create dummy variables:
dataf <- dummy_cols(df_buffer2, select_columns = c('div','t2'))
division  <- unique(df_buffer2$div)

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
data_main <- pdata.frame(dataf.1[,c(1,2,5,6,7,12,13,14,15,29:dim(dataf.1)[2])], index=c("HH2", "t2"))
did.4.mig <- plm(formula = migr ~ t2*f_frac + t2*sw_frac + age + literacy + 
                      d.11+d.12+d.21+d.22+d.31+d.32+d.41+d.42+d.51+d.52+d.61+d.62+d.71
                    , na.action=na.omit,data=data_main, model = "within", index = c("HH2","t2"))
summary(did.4.mig)

# Assets
did.4.ass <- plm(formula = assets ~ t2*f_frac + t2*sw_frac + age + literacy + 
                      d.11+d.12+d.21+d.22+d.31+d.32+d.41+d.42+d.51+d.52+d.61+d.62+d.71
                    , data=data_main, model = "within", index = c("HH2","t2"))
summary(did.4.ass)


# Buffer at 2000m -----------------
load("path")
df_buffer4$div <- 0
for (i in 1:length(df_buffer4$upazila)) {
  hh<-df_buffer4$HH[i]
  df_buffer4$div[i] <- bihs11_c1[bihs11_c1$a01==hh,"div_name"]
}

# Create dummy variables:
dataf <- dummy_cols(df_buffer4, select_columns = c('div','t4'))
division  <- unique(df_buffer4$div)

dataf.2 <- dummy_cols(dataf, select_columns = c('div'),
                      remove_selected_columns = TRUE)

dataf.1 <- dataf.2
for (i in 1:length(division)) {
  for (j in 1:2) {
    st <- paste('d.',as.character(i),as.character(j),sep="")
    dataf.1[,st] <- dataf.2[,i+18]*dataf.2[,25+j]
  }
}

dummies <- colnames(dataf.1[,28:dim(dataf.1)[2]])
paste(dummies,collapse = "+")

# Migr
data_main <- pdata.frame(dataf.1[,c(1,2,5,6,7,12,13,15,28:41)], index=c("HH4", "t4"))
did.2.mig <- plm(formula = migr ~ t4*f_frac + t4*sw_frac + age + literacy + 
                      d.11+d.12+d.21+d.22+d.31+d.32+d.41+d.42+d.51+d.52+d.61+d.62+d.71+d.72,
                    na.action=na.omit,data=data_main, model = "within", index = c("HH4","t4"))
summary(did.2.mig)

# Assets
did.2.ass <- plm(formula = assets ~ t4*f_frac + t4*sw_frac + age + literacy + 
                      d.11+d.12+d.21+d.22+d.31+d.32+d.41+d.42+d.51+d.52+d.61+d.62+d.71
                    , data=data_main, model = "within", index = c("HH2","t2"))
summary(did.2.ass)

# All in one

stargazer(did.5.mig, did.4.mig, did.2.mig,did.5.ass, did.4.ass, did.2.ass,  type = "latex", 
          title="Difference-in-differences: domestic migration and household assets (value) on exposure to flooding", out="did_buf_appendix.tex")




