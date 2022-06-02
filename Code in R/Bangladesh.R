# ========================================================================
# Bangladesh.R    -   Naia Ormaza Zulueta   -  May 2022
# In this file:
# - I gather all the files of interest for the analysis from the BIHS
# - The datasets are harmonized so that they match with admin3 shapefile
# - I create a long format dataframe for each hh between 2006-2019  with
#   the dependent variable of analysis (migration), independent variables
#   of interest (natural hazards) and a set of individual-level controls 
#   and Upazila-level controls
# ========================================================================

# Import libraries
library(terra)
library(readxl)
library(haven)
library(labelled)
library(foreign)

# ------------ BIHS datasets------------
# harmonized
bihs <- read_excel("Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/BIHS_hamonized/dataverse_files/BIHS_household_2011_15.xlsx")

# 1. 2011 SURVEY
# 1.1. Dependent + Independent vars
# to_factor() to encode vector as a factor (labelled in stata)
bihs11_di1 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2011-2012/Dependent + Independent Vars/019_mod_i1_male.dta"))
bihs11_di2 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2011-2012/Dependent + Independent Vars/038_mod_t1_male.dta"))
bihs11_di3 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2011-2012/Dependent + Independent Vars/041_mod_v1_male.dta"))

damage_11 <- read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2011-2012/Dependent + Independent Vars/019_mod_i1_male.dta")
losses_11 <- read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2011-2012/Dependent + Independent Vars/038_mod_t1_male.dta")
migrate_11 <- read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2011-2012/Dependent + Independent Vars/041_mod_v1_male.dta")

# 1.2 Controls
bihs11_c1 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2011-2012/Control Vars/001_mod_a_male.dta"))
bihs11_c2 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2011-2012/Control Vars/003_mod_b1_male.dta"))
bihs11_c3 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2011-2012/Control Vars/006_mod_d1_male.dta"))
bihs11_c4 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2011-2012/Control Vars/008_mod_e_male.dta"))
bihs11_c5 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2011-2012/Control Vars/009_mod_f_male.dta"))
bihs11_c6 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2011-2012/Control Vars/010_mod_g_male.dta"))
bihs11_c7 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2011-2012/Control Vars/019_mod_i1_male.dta"))
bihs11_c8 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2011-2012/Control Vars/022_mod_j2_male.dta"))

educ_11 <- read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2011-2012/Control Vars/003_mod_b1_male.dta")
assets_11 <- read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2011-2012/Control Vars/006_mod_d1_male.dta")
loans_11 <- read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2011-2012/Control Vars/009_mod_f_male.dta")
subs_11 <- read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2011-2012/Control Vars/022_mod_j2_male.dta")

# 2. 2015 SURVEY
# 2.1. Dependent + Independent vars
# to_factor() to encode vector as a factor (labelled in stata)
bihs15_di1 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2015/Dependent + Independent Vars/025_r2_mod_i1_male.dta"))
bihs15_di2 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2015/Dependent + Independent Vars/050_r2_mod_t1_male.dta"))
bihs15_di3 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2015/Dependent + Independent Vars/053_r2_mod_v1_male.dta"))

damage_15 <- read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2015/Dependent + Independent Vars/025_r2_mod_i1_male.dta")
losses_15 <- read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2015/Dependent + Independent Vars/050_r2_mod_t1_male.dta")
migrate_15 <- read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2015/Dependent + Independent Vars/053_r2_mod_v1_male.dta")

# 2.2 Controls
bihs15_c1 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2015/Control Vars/001_r2_mod_a_male.dta"))
bihs15_c2 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2015/Control Vars/003_r2_male_mod_b1.dta"))
bihs15_c3 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2015/Control Vars/010_r2_mod_d1_male.dta"))
bihs15_c4 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2015/Control Vars/012_r2_mod_e_male.dta"))
bihs15_c5 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2015/Control Vars/013_r2_mod_f_male.dta"))
bihs15_c6 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2015/Control Vars/014_r2_mod_g_male.dta"))
bihs15_c7 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2015/Control Vars/016_r2_mod_h2_male.dta"))
bihs15_c8 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2015/Control Vars/033_r2_mod_j2_male.dta"))

educ_15 <- read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2015/Control Vars/003_r2_male_mod_b1.dta")
assets_15 <- read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2015/Control Vars/010_r2_mod_d1_male.dta")
loans_15 <- read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2015/Control Vars/013_r2_mod_f_male.dta")
subs_15 <- read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2015/Control Vars/033_r2_mod_j2_male.dta")

# 3. 2018-2019 SURVEY
# 3.1. Dependent + Independent vars
# to_factor() to encode vector as a factor (labelled in stata)
bihs18_di1 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2018-2019/Dep indep vars/030_bihs_r3_male_mod_i1.dta"))
bihs18_di2 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2018-2019/Dep indep vars/067_bihs_r3_male_mod_t1b.dta"))
bihs18_di3 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2018-2019/Dep indep vars/072_bihs_r3_male_mod_v1.dta"))

damage_18 <- read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2018-2019/Dep indep vars/030_bihs_r3_male_mod_i1.dta")
losses_18 <- read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2018-2019/Dep indep vars/067_bihs_r3_male_mod_t1b.dta")
migrate_18 <- read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2018-2019/Dep indep vars/072_bihs_r3_male_mod_v1.dta")

# 3.2 Controls
bihs18_c1 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2018-2019/Control Vars/009_bihs_r3_male_mod_a.dta"))
bihs18_c2 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2018-2019/Control Vars/010_bihs_r3_male_mod_b1.dta"))
bihs18_c3 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2018-2019/Control Vars/015_bihs_r3_male_mod_d1.dta"))
bihs18_c4 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2018-2019/Control Vars/017_bihs_r3_male_mod_e.dta"))
bihs18_c5 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2018-2019/Control Vars/018_bihs_r3_male_mod_f.dta"))
bihs18_c6 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2018-2019/Control Vars/020_bihs_r3_male_mod_g.dta"))
bihs18_c7 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2018-2019/Control Vars/022_bihs_r3_male_mod_h2.dta"))
bihs18_c8 <- to_factor(read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2018-2019/Control Vars/040_bihs_r3_male_mod_j2a.dta"))

educ_18 <- read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2018-2019/Control Vars/010_bihs_r3_male_mod_b1.dta")
assets_18 <- read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2018-2019/Control Vars/015_bihs_r3_male_mod_d1.dta")
loans_18 <- read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2018-2019/Control Vars/018_bihs_r3_male_mod_f.dta")
subs_18 <- read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2018-2019/Control Vars/040_bihs_r3_male_mod_j2a.dta")

shp <- read.dbf(file = "/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/BIHS_hamonized/dataverse_files/BGD_adm3/BGD_adm3.dbf")

# harmonizing datasets

bihs11_c1$Upazila_Name <- gsub('Palash Paurashava', 'Palash', bihs11_c1$Upazila_Name)
bihs11_c1$Upazila_Name <- gsub('Barisal Sadar \\(Kotwa', 'Barisal Sadar (Kotwali)', bihs11_c1$Upazila_Name)
bihs11_c1$Upazila_Name <- gsub('Daulat Khan', 'Daulatkhan', bihs11_c1$Upazila_Name)
bihs11_c1$Upazila_Name <- gsub('Kaligang', 'Kaliganj', bihs11_c1$Upazila_Name)
bihs11_c1$Upazila_Name <- gsub('Harinakundu Upazila Tota', 'Harinakunda', bihs11_c1$Upazila_Name)
bihs11_c1$Upazila_Name <- gsub('Matlab', 'Matlab Uttar', bihs11_c1$Upazila_Name)
bihs11_c1$Upazila_Name <- gsub('Comilla Sadar \\(Kotwa', 'Comilla Sadar Dakshin', bihs11_c1$Upazila_Name)
bihs11_c1$Upazila_Name <- gsub('Kotalipara', 'Kotali Para', bihs11_c1$Upazila_Name)
bihs11_c1$Upazila_Name <- gsub('Kendua Thana', 'Kendua', bihs11_c1$Upazila_Name)
bihs11_c1$Upazila_Name <- gsub('Mohanganj Thana', 'Mohanganj', bihs11_c1$Upazila_Name)
bihs11_c1$Upazila_Name <- gsub('Noakhali Sadar \\(Sudh', 'Noakhali Sadar (Sudharam)', bihs11_c1$Upazila_Name)
bihs11_c1$Upazila_Name <- gsub('Palong', 'Shariatpur Sadar', bihs11_c1$Upazila_Name)
bihs11_c1$Upazila_Name <- gsub('Baliakandi', 'Balia Kandi', bihs11_c1$Upazila_Name)


bihs15_c1$Upazila_Name <- gsub('Palash Paurashava', 'Palash', bihs15_c1$Upazila_Name)
bihs15_c1$Upazila_Name <- gsub('Barisal Sadar', 'Barisal Sadar (Kotwali)', bihs15_c1$Upazila_Name)
bihs15_c1$Upazila_Name <- gsub('Daulat Khan', 'Daulatkhan', bihs15_c1$Upazila_Name)
bihs15_c1$Upazila_Name <- gsub('Kaligang', 'Kaliganj', bihs15_c1$Upazila_Name)
bihs15_c1$Upazila_Name <- gsub('Harinakundu Upazila Tota', 'Harinakunda', bihs15_c1$Upazila_Name)
bihs15_c1$Upazila_Name <- gsub('Matlab', 'Matlab Uttar', bihs15_c1$Upazila_Name)
bihs15_c1$Upazila_Name <- gsub('Comilla Sadar \\(Kotwa', 'Comilla Sadar Dakshin', bihs15_c1$Upazila_Name)
bihs15_c1$Upazila_Name <- gsub('Kotalipara', 'Kotali Para', bihs15_c1$Upazila_Name)
bihs15_c1$Upazila_Name <- gsub('Kendua Thana', 'Kendua', bihs15_c1$Upazila_Name)
bihs15_c1$Upazila_Name <- gsub('Mohanganj Thana', 'Mohanganj', bihs15_c1$Upazila_Name)
bihs15_c1$Upazila_Name <- gsub('Noakhali Sadar \\(Sudh', 'Noakhali Sadar (Sudharam)', bihs15_c1$Upazila_Name)
bihs15_c1$Upazila_Name <- gsub('Palong', 'Shariatpur Sadar', bihs15_c1$Upazila_Name)
bihs15_c1$Upazila_Name <- gsub('Baliakandi', 'Balia Kandi', bihs15_c1$Upazila_Name)



# Delete duplicated hh in 2015 survey
bihs15_c1$a01 <- round(bihs15_c1$a01)
bihs15_c1 <- bihs15_c1[!duplicated(bihs15_c1$a01),]
# Delete duplicated hh in 2018-2019 survey
bihs18_c1$a01 <- round(bihs18_c1$a01)
bihs18_c1 <- bihs18_c1[!duplicated(bihs18_c1$a01),]

# -------------- Creating the harmonized DSEt -------------- 
Year <- rep(c(2006:2019),times=dim(bihs11_c1)[1])
HH <- rep(c(1:dim(bihs11_c1)[1]), each=14)
surv <- data.frame(HH, Year)


# Save census datasets
save(bihs11_c1, file="/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/HRV/Cropped and Aggregated/census11.RData")
save(bihs15_c1, file="/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/HRV/Cropped and Aggregated/census15.RData")

# Enter Upazila names per hh
surv$Upazila <- 0

for (i in 0:(dim(surv)[1]-1)) {
  surv$Upazila[i*14+1] <- bihs11_c1$Upazila_Name[i+1]
  surv$Upazila[i*14+2] <- bihs11_c1$Upazila_Name[i+1]
  surv$Upazila[i*14+3] <- bihs11_c1$Upazila_Name[i+1]
  surv$Upazila[i*14+4] <- bihs11_c1$Upazila_Name[i+1]
  surv$Upazila[i*14+5] <- bihs11_c1$Upazila_Name[i+1]
  surv$Upazila[i*14+6] <- bihs11_c1$Upazila_Name[i+1]
  surv$Upazila[i*14+7] <- bihs11_c1$Upazila_Name[i+1]
  surv$Upazila[i*14+8] <- bihs11_c1$Upazila_Name[i+1]
  surv$Upazila[i*14+9] <- bihs11_c1$Upazila_Name[i+1]
  surv$Upazila[i*14+10] <- bihs11_c1$Upazila_Name[i+1]
  surv$Upazila[i*14+11] <- bihs11_c1$Upazila_Name[i+1]
  surv$Upazila[i*14+12] <- bihs11_c1$Upazila_Name[i+1]
  surv$Upazila[i*14+13] <- bihs11_c1$Upazila_Name[i+1]
  surv$Upazila[i*14+14] <- bihs11_c1$Upazila_Name[i+1]
}

# ----------- MIGRATION -----------
surv$Migrate <- 0
surv$Migrated_before <- 0

# Migration 2011
mig11 <- migrate_11[migrate_11$v1_01==1,]
for (i in 1:dim(mig11)[1]) {
  if (sum(surv[surv$HH==mig11$a01[i],4])>0){
    # check whether previous members within the hh migrated
    surv[(surv$HH==mig11$a01[i])&surv$Year==(2011-mig11$v1_03[i]),5] <- 1
  }
  surv[(surv$HH==mig11$a01[i])&surv$Year==(2011-mig11$v1_03[i]),4] <- 1
}
# Migration 2015
mig15 <- migrate_15[migrate_15$v1_01==1,]
for (i in 1:dim(mig15)[1]) {
  if (sum(surv[surv$HH==mig15$a01[i],4])>0 | mig15$v1_15[i]==1 | mig15$v1_15[i]==3){
    surv[(surv$HH==round(mig15$a01[i]))&(surv$Year==(2015-mig15$v1_03[i])),5] <- 1
  }
  surv[(surv$HH==round(mig15$a01[i]))&(surv$Year==(2015-mig15$v1_03[i])),4] <- 1
}
# Migration 2018-2019
mig18 <- migrate_18[migrate_18$v1_01==1,]
for (i in 1:dim(mig18)[1]) {
  if (sum(surv[surv$HH==mig18$a01[i],4] | mig18$v1_15[i]==1 | mig18$v1_15[i]==3)>0){
    surv[(surv$HH==round(mig18$a01[i]))&(surv$Year==(2019-mig18$v1_03[i])),5] <- 1
  }
  surv[(surv$HH==round(mig18$a01[i]))&(surv$Year==(2019-mig18$v1_03[i])),4] <- 1
}

# ----------- FLOODING LOSS -----------
surv$loss_fl <- 0
surv$dam_fl <- 0

# 2010-2011
fl_loss11 <- losses_11[losses_11$t1_02==9|losses_11$t1_02==11|losses_11$t1_02==14,]
fl_dam11 <- na.omit(damage_11[damage_11$i1_09b==1,])

for (i in 1:dim(fl_loss11)[1]) {
  surv[(surv$HH==fl_loss11$a01[i])&(surv$Year==fl_loss11$t1_05[i]), "loss_fl"] <- 1
}
for (i in 1:dim(fl_dam11)[1]) {
  surv[(surv$HH==fl_dam11$a01[i])&(surv$Year==2011), "dam_fl"] <- 1
}

# 2015
fl_loss15 <- losses_15[losses_15$t1_02==9|losses_15$t1_02==11|losses_15$t1_02==14,]
fl_dam15 <- na.omit(damage_15[damage_15$i1_09b==1,c(1,13)])
for (i in 1:dim(fl_loss15)[1]) {
  surv[(surv$HH==round(fl_loss15$a01[i]))&(surv$Year==fl_loss15$t1_05[i]), "loss_fl"] <- 1
}
for (i in 1:dim(fl_dam15)[1]) {
  surv[(surv$HH==round(fl_dam15$a01[i]))&(surv$Year==2015), "dam_fl"] <- 1
}

# 2018-2019
fl_loss18 <- losses_18[losses_18$t1b_01==1|losses_18$t1b_01==37|losses_18$t1b_01==38|losses_18$t1b_01==40,]
fl_dam18 <- na.omit(damage_18[damage_18$i1_09b==1,c(1,17)])
for (i in 1:dim(fl_loss18)[1]) {
  if(fl_loss18$t1b_03[i]==1){
    y <- bihs18_c1[bihs18_c1$a01==fl_loss18$a01[i], "a16_1_yy"]
  }else{y<-2016}
  surv[(surv$HH==round(fl_loss18$a01[i]))&(surv$Year==y[[1]]), "loss_fl"] <- 1
}
for (i in 1:dim(fl_dam18)[1]) {
  surv[(surv$HH==round(fl_dam18$a01[i]))&(surv$Year==2018), "dam_fl"] <- 1
}

# ----------- RELIGION -----------
surv$religion <- 0
for (i in 0:dim(bihs11_c1)[1]) {
  rel <- as.character.factor(bihs11_c1$a13[i+1])  # take hh's religion
  surv[(i*14+1):(i*14+14),"religion"] <- rel
}

# dummy for religion
surv$muslim <- ifelse(surv$religion == "Muslim", 1, 0)
surv$hindu <- ifelse(surv$religion == "Hindu", 1, 0)
surv$christian <- ifelse(surv$religion == "Christian", 1, 0)
surv$buddist <- ifelse(surv$religion == "Buddist", 1, 0)
surv$others <- ifelse(surv$religion == "Others", 1, 0)


# ----------- EDUCATION -----------
surv$literacy <- 0
surv$educ <- 0

# hh head's educ and literacy
educ_11 <- educ_11[educ_11$b1_03==1,c(1,9,10)]
educ_15 <- educ_15[educ_15$b1_03==1,c(1,12,13)]
educ_18 <- educ_18[educ_18$b1_03==1,c(1,23,24)]

for (i in 0:(dim(bihs11_c1)[1]-1)) {
  # dummy for "can read and write"
  surv[(i*14+1):(i*14+14),"literacy"] <- ifelse(educ_11$b1_07[i+1]==4,1,0)
  # dummy for above secondary
  surv[(i*14+1):(i*14+14),"educ"] <- ifelse(9<educ_11$b1_08[i+1]&educ_11$b1_08[i+1]<99,1,0)
}

# ----------- ASSETS -----------
surv$assets <- 0

assets_11 <- na.omit(assets_11[assets_11$d1_03==1,c(1,10,12)])
assets_15 <- na.omit(assets_15[assets_15$d1_03==1,c(1,11,13)])
assets_18 <- na.omit(assets_18[assets_18$d1_03==1,c(1,15,17)])
assets_tot <- rbind(assets_11,assets_15)
assets_tot <- rbind(assets_tot,assets_18)

for (i in 1:dim(surv)[1]) {
  surv$assets[i] <- sum(assets_tot[(assets_tot$a01==surv$HH[i])&(assets_tot$d1_08<=surv$Year[i]),3]) 
}

# ---------- SUBSIDIES & LOANS ----------

surv$loan_subs <- 0  # loan for subsistence
surv$loan_oth <- 0   # loan for other purposes than subsistence
surv$loan_abr <- 0   # loan to go abroad

surv$subs_agr <- 0   # agricultural subsidies reception

surv <- surv[1:91042,]

# loans -----------
loans_11 <- loans_11[loans_11$f02==1, c(1,8)]
loans_15 <- loans_15[loans_15$f02==1, c(1,12)]
loans_18 <- na.omit(loans_18[loans_18$f06_a==1, c(1,19)])

subsistence <- c(10,12,13,17,25)
others <- c(1:26)
others <- others[!others %in% subsistence]

# check the years!!!!! should I include them this way??
y_11 <- c(2006:2011)
y_15 <- c(2012:2015)
y_18 <- c(2016:2019)

for (i in 1:dim(loans_11)[1]) {
  hh <- loans_11$a01[i]
  surv[surv$HH==hh&(surv$Year%in%y_11), "loan_subs"] <- ifelse(loans_11$f06_a[i] %in% subsistence,1,0)
  surv[surv$HH==hh&(surv$Year%in%y_11), "loan_oth"] <- ifelse(loans_11$f06_a[i] %in% others,1,0)
  surv[surv$HH==hh&(surv$Year%in%y_11), "loan_abr"] <- ifelse(loans_11$f06_a[i]==24,1,0)
}

for (i in 1:dim(loans_15)[1]) {
  hh <- round(loans_15$a01[i])
  surv[surv$HH==hh&(surv$Year%in%y_15), "loan_subs"] <- ifelse(loans_15$f06_a[i] %in% subsistence,1,0)
  surv[surv$HH==hh&(surv$Year%in%y_15), "loan_oth"] <- ifelse(loans_15$f06_a[i] %in% others,1,0)
  surv[surv$HH==hh&(surv$Year%in%y_15), "loan_abr"] <- ifelse(loans_15$f06_a[i]==24,1,0)
}

for (i in 1:dim(loans_18)[1]) {
  hh <- round(loans_18$a01[i])
  surv[surv$HH==hh&(surv$Year%in%y_18), "loan_subs"] <- ifelse(loans_18$f06_a[i] %in% subsistence,1,0)
  surv[surv$HH==hh&(surv$Year%in%y_18), "loan_oth"] <- ifelse(loans_18$f06_a[i] %in% others,1,0)
  surv[surv$HH==hh&(surv$Year%in%y_18), "loan_abr"] <- ifelse(loans_18$f06_a[i]==24,1,0)
}

# subsidies -----------

subs_11 <- subs_11[subs_11$j2_01==1,c(1)]
subs_15 <- subs_15[subs_15$j2_01==1,c(1)]
subs_18 <- na.omit(subs_18[subs_18$j2a_02==1,c(1,4)])

for (i in 1:dim(subs_11)[1]) {
  surv[surv$HH==subs_11$a01[i]&surv$Year==2011,"subs_agr"] <- 1
}
for (i in 1:dim(subs_15)[1]) {
  surv[surv$HH==subs_15$a01[i]&surv$Year==2015,"subs_agr"] <- 1
}

for (i in 1:dim(subs_18)[1]) {
  y <- c(subs_18$j2a_03y[i]:2019) # check year in which the agricultural subsidy card was obtained
  surv[surv$HH==subs_18$a01[i]&(surv$Year%in%y),"subs_agr"] <- 1
}


# --------------- ECONOMIC DEVELOPMENT PROXIED BY CHILD STUNTING -------------

# Load cropped and aggregated raster data
load(file="/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/HRV/Cropped and Aggregated/food_sec.RData")

Year <- c(2006:2019)
food_security <- data.frame(Year)
# Dates of the events
dates <- c(2006:2017)

shp <- read.dbf(file="/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/gadm40_BGD_shp/gadm40_BGD_3.dbf")

for (i in 1:dim(shp)[1]) {food_security[as.character(i)] <- 0}

# Fill in values for child stunting per upazila level
for (i in 1:length(dates)) {
  y <- dates[i]
  df_fsec <- data.frame(cropped_fsec[i])
  colnames(df_fsec) <- c('Year','stunting')
  for (k in 1:dim(shp)[1]) {
    food_security[food_security$Year==y, 1+k] <- food_security[food_security$Year==y, 1+k]+ df_fsec$stunting[k]
  }
}
# repeat 2017 values in 2018-2019
for (year in c(2018,2019)) {
  for (k in 1:dim(shp)[1]) {
    food_security[food_security$Year==year, 1+k] <- food_security[food_security$Year==year, 1+k]+ df_fsec$stunting[k]
  }
}

# DF of numbers with Upazila Names
df_upazila <- data.frame(1:dim(shp)[1],shp$NAME_3)

# Add values to survey
surv$f_sec <- 0
for (i in 1:dim(surv)[1]) {
  upaz <- surv$Upazila[i]
  col_n <- df_upazila[df_upazila$shp.NAME_3==upaz, 1]    # equiv. number
  year <- surv$Year[i]
  f_sec_value <- food_security[food_security$Year==year, col_n+1]
  surv$f_sec[i] <- f_sec_value
}

# --------------  POPULATION -----------------

Year <- c(2006:2019)
population <- data.frame(Year)
# Dates of the events
dates <- c(2005,2010,2015)

shp <- read.dbf(file="/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/gadm40_BGD_shp/gadm40_BGD_3.dbf")

for (i in 1:dim(shp)[1]) {population[as.character(i)] <- 0}

# Load cropped and aggregated raster data
load(file="/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/HRV/Cropped and Aggregated/population.RData")

y1 <- c(2006,2007,2008,2009)
y2 <- c(2010,2011,2012,2013,2014)
y3 <- c(2015,2016,2017,2018,2019)
years <- list(y1,y2,y3)

for (i in 1:length(dates)) {
  y <- years[i]
  df_pop <- data.frame(cropped_pop[i])
  colnames(df_pop) <- c('Year','population')
  for (k in 1:dim(shp)[1]) {
    population[population$Year%in%y[[1]], 1+k] <- population[population$Year%in%y[[1]], 1+k]+ df_pop$population[k]
  }
}

# Add values to survey
surv$pop <- 0
for (i in 1:dim(surv)[1]) {
  upaz <- surv$Upazila[i]
  col_n <- df_upazila[df_upazila$shp.NAME_3==upaz, 1]    # equiv. number
  year <- surv$Year[i]
  pop_val <- population[population$Year==year, col_n+1]
  surv$pop[i] <- pop_val
}

# --------------- SAVE -----------------
save(surv,file="/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/R/Saved Data/long_data.Rdata")
