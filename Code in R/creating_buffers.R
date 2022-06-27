# ========================================================================
# creating_buffers.R    -   Naia Ormaza Zulueta   -  June 2022
# In this file:
# - I construct buffers around each household for 500m,  1000m, 2000m, 
#   3000m, 4000m and 5000m. 
# - Using such buffers, I compute the fractions of the flooded surface
#   area and the covered water area for each household.
# - 
# ========================================================================

# ------ Load libraries ------
packages <- c("terra", "readxl", "haven", "labelled", "foreign",
              "dplyr", "hutilscpp", "tidyverse", "geosphere")
lapply(packages, require, character=TRUE)


# Add coordinates to the original datset

# ============== BIHS datasets ==============
# Coordinates
bihs <- read_excel("path_to_data")
bihs_coord <- bihs[bihs$survey_year==2015,c(1,271,272)]

# Delete duplicated
bihs_coord$a01 <- round(bihs_coord$a01)
bihs_coord <- bihs_coord[!duplicated(bihs_coord$a01),]

# Plug coordinates to panel dataset
load("path")
surv_coord <- surv
surv_coord$lat <- 0
surv_coord$lon <- 0

for (i in 0:(dim(bihs_coord)[1]-1)) {
  surv_coord$lat[(i*14+1):(i*14+14)] <- bihs_coord$bio_latitude[i+1]
  surv_coord$lon[(i*14+1):(i*14+14)] <- bihs_coord$bio_longitude[i+1]
}

# Delete NA's
bihs_coord <- bihs_coord[!is.na(bihs_coord$bio_latitude),]
surv_coord <- surv_coord[!is.na(surv_coord$lat),]

# ============== Sat Data preparation ==============

# Bangladesh shapefile
bang <- vect("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/gadm40_BGD_shp/gadm40_BGD_0.shp")

# Flood raster data
filenames <- list.files("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/HRV/Flooding/Diff_in_diff", pattern="*.tif", full.names=TRUE)
ldf <- lapply(filenames[c(4)], rast)
# Crop to Bangladesh
ldf_cropped <- lapply(ldf, crop, y=ext(bang))
# Convert all to dataframe
df_fl17 <- lapply(ldf_cropped, as.data.frame, xy=TRUE)
# Bind
df_binded <- do.call('rbind', df_fl17)
save(df_binded,file="/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/R/Saved Data/binded_floods2017_cropped.Rdata")


# Surface water raster data
filenames <- list.files("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/HRV/Global surface water", pattern="*.tif", full.names=TRUE)
ldf <- lapply(filenames, rast)
# Crop to Bangladesh
ldf_cropped <- lapply(ldf, crop, y=ext(bang))
# Convert all to dataframe
df_sw17 <- lapply(ldf_cropped, as.data.frame, xy=TRUE)
# Bind
df_binded_sw <- do.call('rbind', df_sw17)
save(df_binded_sw,file="/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/R/Saved Data/binded_surfacewater2017_cropped.Rdata")

# ============== End of Sat Data Preparation ==============

# Design threshold radii
thr <- 5000
thr2 <- 4000
thr3 <- 3000
thr4 <- 2000
thr5 <- 1000
thr6 <- 500

# 1. Creation of buffer per household: flood data ---------------

load("path")

bihs_coord$flood_frac <- 0     # 5,000m
bihs_coord$flood_frac2 <- 0    # 4,000m
bihs_coord$flood_frac3 <- 0    # 3,000m
bihs_coord$flood_frac4 <- 0    # 2,000m
bihs_coord$flood_frac5 <- 0    # 1,000m
bihs_coord$flood_frac6 <- 0    # 500m

for (i in 1:dim(bihs_coord)[1]) {
  # The Haversine (’half-versed-sine’) formula
  dhav <- distHaversine(c(bihs_coord$bio_longitude[i],bihs_coord$bio_latitude[i]),cbind(df_binded$x,df_binded$y))
  bihs_coord$flood_frac[i] <- sum(df_binded[which(dhav<thr),"flooded"])/sum(dhav<thr)
  bihs_coord$flood_frac2[i] <- sum(df_binded[which(dhav<thr2),"flooded"])/sum(dhav<thr2)
  bihs_coord$flood_frac3[i] <- sum(df_binded[which(dhav<thr3),"flooded"])/sum(dhav<thr3)
  bihs_coord$flood_frac4[i] <- sum(df_binded[which(dhav<thr4),"flooded"])/sum(dhav<thr4)
  bihs_coord$flood_frac5[i] <- sum(df_binded[which(dhav<thr5),"flooded"])/sum(dhav<thr5)
  bihs_coord$flood_frac6[i] <- sum(df_binded[which(dhav<thr6),"flooded"])/sum(dhav<thr6)
  print(i)
}


# 2. Creation of buffer per household: surface water ---------------

load("path")
bihs_coord$sw_frac <- 0      # 5,000m
bihs_coord$sw_frac2 <- 0     # 4,000m
bihs_coord$sw_frac3 <- 0     # 3,000m
bihs_coord$sw_frac4 <- 0     # 2,000m
bihs_coord$sw_frac5 <- 0     # 1,000m
bihs_coord$sw_frac6 <- 0     # 500m

for (i in 1:dim(bihs_coord)[1]) {
  # The Haversine (’half-versed-sine’) formula
  dhav <- distHaversine(c(bihs_coord$bio_longitude[i],bihs_coord$bio_latitude[i]),cbind(df_binded$x,df_binded$y))
  bihs_coord$sw_frac[i] <- sum(df_binded[which(dhav<thr),"jrc_perm_water"])/sum(dhav<thr)
  bihs_coord$sw_frac2[i] <- sum(df_binded[which(dhav<thr2),"jrc_perm_water"])/sum(dhav<thr2)
  bihs_coord$sw_frac3[i] <- sum(df_binded[which(dhav<thr3),"jrc_perm_water"])/sum(dhav<thr3)
  bihs_coord$sw_frac4[i] <- sum(df_binded[which(dhav<thr4),"jrc_perm_water"])/sum(dhav<thr4)
  bihs_coord$sw_frac5[i] <- sum(df_binded[which(dhav<thr5),"jrc_perm_water"])/sum(dhav<thr5)
  bihs_coord$sw_frac6[i] <- sum(df_binded[which(dhav<thr6),"jrc_perm_water"])/sum(dhav<thr6)
  print(i)
}

# 3. Normal matching ---------------

bihs_coord$flooded <- 0
bihs_coord$duration <- 0
bihs_coord$surfacewater <- 0

for (i in 1:dim(bihs_coord)[1]){
  result <- match_nrst_haversine(bihs_coord$bio_latitude[i],bihs_coord$bio_longitude[i],
                                 df_binded$y, df_binded$x)
  bihs_coord$flooded[i] <- ifelse(result$dist>2, bihs_coord$flooded[i] <- 0, df_binded[result$pos, c("flooded")])
  bihs_coord$duration[i] <- ifelse(result$dist>2, bihs_coord$flooded[i] <- 0, df_binded[result$pos, c("duration")])
  bihs_coord$surfacewater[i] <- df_binded[result$pos, c("jrc_perm_water")]
}
# 4. Creation of buffer per household: Robustness check - duration ---------------

bihs_coord$dur_frac <- 0      # 5,000m
bihs_coord$dur_frac2 <- 0     # 4,000m
bihs_coord$dur_frac3 <- 0     # 3,000m
bihs_coord$dur_frac4 <- 0     # 2,000m
bihs_coord$dur_frac5 <- 0     # 1,000m
bihs_coord$dur_frac6 <- 0     # 500m

for (i in 1:dim(bihs_coord)[1]) {
  # The Haversine (’half-versed-sine’) formula
  dhav <- distHaversine(c(bihs_coord$bio_longitude[i],bihs_coord$bio_latitude[i]),cbind(df_binded$x,df_binded$y))
  bihs_coord$dur_frac[i] <- sum(df_binded[which(dhav<thr),"duration"])/sum(dhav<thr)
  bihs_coord$dur_frac2[i] <- sum(df_binded[which(dhav<thr2),"duration"])/sum(dhav<thr2)
  bihs_coord$dur_frac3[i] <- sum(df_binded[which(dhav<thr3),"duration"])/sum(dhav<thr3)
  bihs_coord$dur_frac4[i] <- sum(df_binded[which(dhav<thr4),"duration"])/sum(dhav<thr4)
  bihs_coord$dur_frac5[i] <- sum(df_binded[which(dhav<thr5),"duration"])/sum(dhav<thr5)
  bihs_coord$dur_frac6[i] <- sum(df_binded[which(dhav<thr6),"duration"])/sum(dhav<thr6)
  print(i)
}

save(bihs_coord, file="/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/R/Saved Data/bihs_coord.Rdata")

load("path")

# ================= TWO-STAGE IV =================

# Need to add rainfall data

# Flood raster data
filenames <- list.files("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/HRV/Precipitation/Pentad", pattern="*.tif", full.names=TRUE)
ldf <- lapply(filenames, rast)
names(ldf[[2]]) <- names(ldf[[1]])
# Crop to Bangladesh
ldf_cropped <- lapply(ldf, crop, y=ext(bang))
# Convert all to dataframe
df_rain17 <- lapply(ldf_cropped, as.data.frame, xy=TRUE)
# Bind
df_binded_rain <- do.call('cbind', df_rain17)
colnames(df_binded_rain) <- c("x","y","one","x2","y2","two")
df_binded_rain <- df_binded_rain %>% mutate(Total = select(., one:two) %>% rowSums(na.rm = TRUE))
save(df_binded_rain,file="/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/R/Saved Data/binded_rainfall2017_cropped.Rdata")
load("path")
bihs_coord$rainfall <- 0


for (i in 1:dim(bihs_coord)[1]){
  result <- match_nrst_haversine(bihs_coord$bio_latitude[i],bihs_coord$bio_longitude[i],
                                 df_binded_rain$y, df_binded_rain$x)
  bihs_coord$rainfall[i] <- df_binded_rain[result$pos, c("Total")]
}


# =================== Additional preparations ==================
surv_coord$flooded <- 0
surv_coord$flood_frac <- 0
surv_coord$flood_frac2 <- 0
surv_coord$flood_frac3 <- 0
surv_coord$flood_frac4 <- 0
surv_coord$flood_frac5 <- 0
surv_coord$flood_frac6 <- 0

for (i in 0:(dim(bihs_coord)[1]-1)) {
  surv_coord$flooded[(i*14+1):(i*14+14)] <- bihs_coord$flooded[i+1]
  surv_coord$flood_frac[(i*14+1):(i*14+14)] <- bihs_coord$flood_frac[i+1]
  surv_coord$flood_frac2[(i*14+1):(i*14+14)] <- bihs_coord$flood_frac2[i+1]
  surv_coord$flood_frac3[(i*14+1):(i*14+14)] <- bihs_coord$flood_frac3[i+1]
  surv_coord$flood_frac4[(i*14+1):(i*14+14)] <- bihs_coord$flood_frac4[i+1]
  surv_coord$flood_frac5[(i*14+1):(i*14+14)] <- bihs_coord$flood_frac5[i+1]
  surv_coord$flood_frac6[(i*14+1):(i*14+14)] <- bihs_coord$flood_frac6[i+1]
}

# Did that household migrate at some point across the three surveys?
bihs_coord$migr <- 0
migr <- c(1)    # migrated at some point
migr17 <- c(10)   # migrated only after 2017
migr_remit <- c(1)  # sent remitances
for (i in 2:dim(bihs_coord)[1]) {
  hh <- bihs_coord$a01[i]
  bihs_coord$migr[i] <- ifelse(sum(surv[surv$HH==hh,c("Dom_migrate")])>=1, 1, 0)
  if(sum(surv_coord[surv_coord$HH==hh,c("Dom_migrate")])>=1){migr <- append(migr,hh)}
  if(sum(surv_coord[(surv_coord$HH==hh)&((surv_coord$Year%in%c(2017)&surv_coord$Migr_month%in%c(4:12))|(surv_coord$Year%in%c(2018)&surv_coord$Migr_month%in%c(0:8))),c("Dom_migrate")])>=1){
    migr17 <- append(migr17,hh)
    if(sum(surv_coord[surv_coord$HH==hh,"Migr_remit"])>0){migr_remit <- append(migr_remit,hh)}
  }
}
migr17 <- migr17[2:length(migr17)]
migr_remit <- migr_remit[2:length(migr_remit)]

# Keep affected Upazilas
affected_up <- c("Bagerhat Sadar")
affected_up2 <- c("Bagerhat Sadar")
affected_up3 <- c("Bagerhat Sadar")
affected_up4 <- c("Bagerhat Sadar")
affected_up5 <- c("Bagerhat Sadar")
affected_up6 <- c("Bagerhat Sadar")

surv_coord[is.na(surv_coord$flood_frac2),c("flood_frac2","flood_frac3", "flood_frac4", "flood_frac5", "flood_frac6")] <- 0
surv_coord[is.na(surv_coord$flood_frac6),c("flood_frac6")] <- 0
surv_coord[is.na(surv_coord$flood_frac5),c("flood_frac5")] <- 0

upazilas <- c(unique(surv_coord$Upazila))
for (i in 2:length(upazilas)) {
  if(sum(surv_coord[surv_coord$Upazila==upazilas[i],"flood_frac"])>0){
    affected_up <- append(affected_up, upazilas[i])
  }
  if(sum(surv_coord[surv_coord$Upazila==upazilas[i],"flood_frac2"])>0){
    affected_up2 <- append(affected_up2, upazilas[i])
  }
  if(sum(surv_coord[surv_coord$Upazila==upazilas[i],"flood_frac3"])>0){
    affected_up3 <- append(affected_up3, upazilas[i])
  }
  if(sum(surv_coord[surv_coord$Upazila==upazilas[i],"flood_frac4"])>0){
    affected_up4 <- append(affected_up4, upazilas[i])
  }
  if(sum(surv_coord[surv_coord$Upazila==upazilas[i],"flood_frac5"])>0){
    affected_up5 <- append(affected_up5, upazilas[i])
  }
  if(sum(surv_coord[surv_coord$Upazila==upazilas[i],"flood_frac6"])>0){
    affected_up6 <- append(affected_up6, upazilas[i])
  }
}

affected_up <- affected_up[2:length(affected_up)]
affected_up2 <- affected_up2[2:length(affected_up2)]
affected_up3 <- affected_up3[2:length(affected_up3)]
affected_up4 <- affected_up4[2:length(affected_up4)]
affected_up5 <- affected_up5[2:length(affected_up5)]
affected_up6 <- affected_up6[2:length(affected_up6)]

affected_hh <- surv_coord[surv_coord$Upazila%in%affected_up,"HH"]
affected_hh <- affected_hh[!duplicated(affected_hh)]

affected_hh2 <- surv_coord[surv_coord$Upazila%in%affected_up2,"HH"]
affected_hh2 <- affected_hh2[!duplicated(affected_hh2)]

affected_hh3 <- surv_coord[surv_coord$Upazila%in%affected_up3,"HH"]
affected_hh3 <- affected_hh3[!duplicated(affected_hh3)]

affected_hh4 <- surv_coord[surv_coord$Upazila%in%affected_up4,"HH"]
affected_hh4 <- affected_hh4[!duplicated(affected_hh4)]

affected_hh5 <- surv_coord[surv_coord$Upazila%in%affected_up5,"HH"]
affected_hh5 <- affected_hh5[!duplicated(affected_hh5)]

affected_hh6 <- surv_coord[surv_coord$Upazila%in%affected_up6,"HH"]
affected_hh6 <- affected_hh6[!duplicated(affected_hh6)]

# I stay with the hh that:
#   1) Did not migrate at all
#   2) If migrated, did so after the shock event
#   3) Were in affected Upazilaz (even if their hh was not directly affected)

hh_all <- pull(bihs_coord[(bihs_coord$a01%in%migr17)|!(bihs_coord$a01%in%migr),c(1)])
hh_all <- hh_all[hh_all%in%affected_hh]
hh_all2 <- hh_all[hh_all%in%affected_hh2]
hh_all3 <- hh_all[hh_all%in%affected_hh3]
hh_all4 <- hh_all[hh_all%in%affected_hh4]
hh_all5 <- hh_all[hh_all%in%affected_hh5]
hh_all6 <- hh_all[hh_all%in%affected_hh6]

# ===================== creating df ==========================
HH <- base::rep(hh_all, each=2)
HH2 <- base::rep(hh_all2, each=2)
HH3 <- base::rep(hh_all3, each=2)
HH4 <- base::rep(hh_all4, each=2)
HH5 <- base::rep(hh_all5, each=2)
HH6 <- base::rep(hh_all6, each=2)

# ADD DUMMY FOR SECOND TIME (pre-2017)
t <- base::rep(c(0,1), times=length(hh_all))
t2 <- base::rep(c(0,1), times=length(hh_all2))
t3 <- base::rep(c(0,1), times=length(hh_all3))
t4 <- base::rep(c(0,1), times=length(hh_all4))
t5 <- base::rep(c(0,1), times=length(hh_all5))
t6 <- base::rep(c(0,1), times=length(hh_all6))

main_df <- data.frame(HH,t)
main_df2 <- data.frame(HH2,t2)
main_df3 <- data.frame(HH3,t3)
main_df4 <- data.frame(HH4,t4)
main_df5 <- data.frame(HH5,t5)
main_df6 <- data.frame(HH6,t6)

basics_18 <- read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2018-2019/Control Vars/009_bihs_r3_male_mod_a.dta")
crops_15 <- read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2015/Control Vars/014_r2_mod_g_male.dta")
crops_18 <- read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2018-2019/Control Vars/020_bihs_r3_male_mod_g.dta")
educ_15 <- read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2015/Control Vars/003_r2_male_mod_b1.dta")
educ_18 <- read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2018-2019/Control Vars/010_bihs_r3_male_mod_b1.dta")
# Delete duplicated hh in 2015 survey
educ_15$a01 <- round(educ_15$a01)
educ_15 <- educ_15[!duplicated(educ_15$a01),]
educ_18$a01 <- round(educ_18$a01)
educ_18 <- educ_18[!duplicated(educ_18$a01),]

crops_15 <- as.data.frame(na.omit(crops_15[,c(1,4)]))
crops_18 <- as.data.frame(na.omit(crops_18[,c(1,7)]))


controlst1 <- as.data.frame(na.omit(educ_15[educ_15$b1_03==1,c(1,4,5,12,16,19)]))
controlst2 <- as.data.frame(na.omit(educ_18[educ_18$b1_03==1,c(1,8,9,23,28,31)]))

controlst1$literacy <- ifelse(controlst1$b1_07==4,1,0)
controlst2$literacy <- ifelse(controlst2$b1_07==4,1,0)

controlst1$occup_ag <- ifelse(controlst1$b1_10%in%c(1,61,67,66,63),1,0)
controlst2$occup_ag <- ifelse(controlst2$b1_10%in%c(1,67,68,64,62),1,0)

controlst1$jobless <- ifelse(controlst1$b1_10==75,1,0)
controlst2$jobless <- ifelse(controlst2$b1_10==76,1,0)

controlst1$s_earn_ag <- ifelse(controlst1$b1_13a%in%c(1,4,7),1,0)
controlst2$s_earn_ag <- ifelse(controlst2$b1_13a%in%c(1,4,7),1,0)

controlst1$hh_size <- NA
controlst2$hh_size <- NA
controlst1$n_wom <- NA
controlst2$n_wom<- NA
controlst1$dec_mak <- NA
controlst2$dec_mak <- NA
controlst1$n_kids <- NA
controlst2$n_kids <- NA
for (i in 1:dim(basics_18)[1]) {
  hh <- basics_18$a01[i]
  controlst1[controlst1$a01==hh,"hh_size"] <- basics_18$a23[i]
  controlst2[controlst2$a01==hh,"hh_size"] <- basics_18$a23[i]
  controlst1[controlst1$a01==hh,"n_wom"] <- basics_18$a24[i]
  controlst2[controlst2$a01==hh,"n_wom"] <- basics_18$a24[i]
  controlst1[controlst1$a01==hh,"dec_mak"] <- basics_18$a26[i]
  controlst2[controlst2$a01==hh,"dec_mak"] <- basics_18$a26[i]
  controlst1[controlst1$a01==hh,"n_kids"] <- basics_18$a25[i]
  controlst2[controlst2$a01==hh,"n_kids"] <- basics_18$a25[i]
}

controlst1$crops <- NA
controlst2$crops <- NA

households <- round(unique(crops_15$a01))
households2 <- round(unique(crops_18$a01))

for (i in 1:length(households)) {
  controlst1[controlst1$a01==households[i],"crops"] <- ifelse(sum(crops_15[crops_15$a01==households[i],2]%in%c(2,3,8))>0,1,0)
  #controlst2[controlst2$a01==households[i],"crops"] <- ifelse(sum(crops_15[crops_15$a01==households[i],2]%in%c(2,3,8)),1,0)
}
for (i in 1:length(households2)) {
  controlst2[controlst2$a01==households2[i],"crops"] <- ifelse(sum(crops_18[crops_18$a01==households2[i],2]%in%c(2,3,8))>0,1,0)
}

# --------------------- MAIN: 5000m buffer ----------------------------

# ADD DUMMY FOR TREATMENT: f
main_df$f <- 0 
main_df$duration <- 0
main_df$f_frac <- 0
main_df$sw_frac <- 0

for (i in 0:(dim(main_df)[1]/2-1)) {
  hh <- main_df$HH[i*2+1]
  main_df$f[(i*2+1):(i*2+2)] <- pull(bihs_coord[bihs_coord$a01==hh,"flooded"])
  main_df$duration[(i*2+1):(i*2+2)] <- pull(bihs_coord[bihs_coord$a01==hh,"duration"])
  main_df$f_frac[(i*2+1):(i*2+2)] <- pull(bihs_coord[bihs_coord$a01==hh,"flood_frac"])
  main_df$sw_frac[(i*2+1):(i*2+2)] <- pull(bihs_coord[bihs_coord$a01==hh,"sw_frac"])
}

# MIGRATED post-2017
main_df$migr <- 0
main_df$remit <- 0

for (i in 1:length(migr17)) {
  main_df[(main_df$HH==migr17[i])&(main_df$t==1),"migr"] <- 1
}

for (i in 1:length(migr_remit)) {
  main_df[(main_df$HH==migr_remit[i])&(main_df$t==1),"remit"] <- 1
}

# ADD SECOND TREATMENT: reported damages
main_df$rep_dam_fl <- 0
main_df$rep_loss_fl <- 0
main_df$upazila <- 0

for (i in 0:(dim(main_df)[1]/2-1)) {
  hh <- main_df$HH[i*2+1]
  main_df$rep_dam_fl[(i*2+1):(i*2+2)] <- ifelse(sum(surv_coord[surv_coord$HH==hh&surv_coord$Year%in%c(2017:2019), "dam_fl_lag"])>=1,1,0)
  main_df$rep_loss_fl[(i*2+1):(i*2+2)] <- ifelse(sum(surv_coord[surv_coord$HH==hh&surv_coord$Year%in%c(2017:2019), "loss_fl_lag"])>=1,1,0)
  main_df$upazila[(i*2+1):(i*2+2)] <- surv_coord[surv_coord$HH==hh,"Upazila"]
}

# ADD SECOND OUTCOME: assets

main_df$assets <- 0
assets_15 <- read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2015/Control Vars/010_r2_mod_d1_male.dta")
assets_18 <- read_stata("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/The ones I need from BIHS/2018-2019/Control Vars/015_bihs_r3_male_mod_d1.dta")
assets_15 <- na.omit(assets_15[assets_15$d1_03==1,c(1,11,13)])
assets_18 <- na.omit(assets_18[assets_18$d1_03==1,c(1,15,17)])

assets_15_binded <-  rbind(assets_15, assets_18[assets_18$d1_08%in%c(2015:2016),])
#assets_18 <- assets_18[assets_18$d1_08<=2017,]

for (i in 0:(dim(main_df)[1]/2-1)) {
  hh <- main_df$HH[2*i+1]
  main_df$assets[2*i+1] <- sum(assets_15_binded[assets_15_binded$a01==hh,3])
  main_df$assets[2*i+2] <- sum(assets_18[assets_18$a01==hh,3])
}

# ADD SURFACE WATER:
main_df$sw <- 0 

for (i in 0:(dim(main_df)[1]/2-1)) {
  hh <- main_df$HH[i*2+1]
  main_df$sw[(i*2+1):(i*2+2)] <- pull(bihs_coord[bihs_coord$a01==hh,"surfacewater"])
}

# ADD CONTROLS
main_df$age <- NA
main_df$gender <- NA
main_df$literacy <- NA
main_df$occup_ag <- NA
main_df$s_earn <- NA
main_df$hh_size <- NA
main_df$crops <- NA
main_df$n_wom <- NA
main_df$n_kids <- NA
main_df$dec_mak <- NA

for (i in 0:(dim(main_df)[1]/2-1)) {
  hh <- main_df$HH[2*i+1]
  if(hh%in%controlst1$a01){
    main_df$age[(2*i+1)] <- controlst1[controlst1$a01==round(hh),3]
    main_df$gender[(2*i+1)] <- controlst1[controlst1$a01==round(hh),2]
    main_df$literacy[(2*i+1)] <- controlst1[controlst1$a01==round(hh),7]
    main_df$occup_ag[(2*i+1)] <- controlst1[controlst1$a01==round(hh),8]
    main_df$s_earn[(2*i+1)] <- controlst1[controlst1$a01==round(hh),10]
    main_df$hh_size[(2*i+1)] <- controlst1[controlst1$a01==round(hh),11]
    main_df$crops[(2*i+1)] <- controlst1[controlst1$a01==round(hh),12]
    main_df$n_wom[(2*i+1)] <- controlst1[controlst1$a01==round(hh),13]
    main_df$n_kids[(2*i+1)] <- controlst1[controlst1$a01==round(hh),15]
    main_df$dec_mak[(2*i+1)] <- controlst1[controlst1$a01==round(hh),14]
  } else if (hh%in%controlst2$a01){
    main_df$age[(2*i+1)] <- controlst2[controlst2$a01==round(hh),3]-sample(c(3,4),1)
    main_df$gender[(2*i+1)] <- controlst2[controlst2$a01==round(hh),2]
    main_df$literacy[(2*i+1)] <- controlst2[controlst2$a01==round(hh),7]
  }
  if(hh%in%controlst2$a01){
    main_df$age[(2*i+2)] <- controlst2[controlst2$a01==round(hh),3]
    main_df$gender[(2*i+2)] <- controlst2[controlst2$a01==round(hh),2]
    main_df$literacy[(2*i+2)] <- controlst2[controlst2$a01==round(hh),7]
    main_df$occup_ag[(2*i+2)] <- controlst2[controlst2$a01==round(hh),8]
    main_df$s_earn[(2*i+2)] <- controlst2[controlst2$a01==round(hh),10]
    main_df$hh_size[(2*i+2)] <- controlst2[controlst2$a01==round(hh),11]
    main_df$crops[(2*i+2)] <- controlst2[controlst2$a01==round(hh),12]
    main_df$n_wom[(2*i+2)] <- controlst2[controlst2$a01==round(hh),13]
    main_df$n_kids[(2*i+2)] <- controlst2[controlst2$a01==round(hh),15]
    main_df$dec_mak[(2*i+2)] <- controlst2[controlst2$a01==round(hh),14]
  } else if (hh%in%controlst1$a01){
    main_df$age[(2*i+2)] <- controlst1[controlst1$a01==round(hh),3] + sample(c(3,4),1)
    main_df$gender[(2*i+2)] <- controlst1[controlst1$a01==round(hh),2]
    main_df$literacy[(2*i+2)] <- controlst1[controlst1$a01==round(hh),7]
  }
}

df_buffer <- main_df
save(df_buffer,file="/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/R/Saved Data/main_df_diffindiff_buffer.Rdata")



# --------------------- MAIN2: 4000m buffer ----------------------------

# ADD DUMMY FOR TREATMENT: f
main_df2$f <- 0 
main_df2$duration <- 0
main_df2$f_frac <- 0
main_df2$sw_frac <- 0

for (i in 0:(dim(main_df2)[1]/2-1)) {
  hh <- main_df2$HH2[i*2+1]
  main_df2$f[(i*2+1):(i*2+2)] <- pull(bihs_coord[bihs_coord$a01==hh,"flooded"])
  main_df2$duration[(i*2+1):(i*2+2)] <- pull(bihs_coord[bihs_coord$a01==hh,"duration"])
  main_df2$f_frac[(i*2+1):(i*2+2)] <- pull(bihs_coord[bihs_coord$a01==hh,"flood_frac2"])
  main_df2$sw_frac[(i*2+1):(i*2+2)] <- pull(bihs_coord[bihs_coord$a01==hh,"sw_frac2"])
}

# MIGRATED post-2017
main_df2$migr <- 0
main_df2$remit <- 0

for (i in 1:length(migr17)) {
  main_df2[(main_df2$HH2==migr17[i])&(main_df2$t2==1),"migr"] <- 1
}

for (i in 1:length(migr_remit)) {
  main_df2[(main_df2$HH2==migr_remit[i])&(main_df2$t2==1),"remit"] <- 1
}

# ADD SECOND TREATMENT: reported damages
main_df2$rep_dam_fl <- 0
main_df2$rep_loss_fl <- 0
main_df2$upazila <- 0

for (i in 0:(dim(main_df2)[1]/2-1)) {
  hh <- main_df2$HH2[i*2+1]
  main_df2$rep_dam_fl[(i*2+1):(i*2+2)] <- ifelse(sum(surv_coord[surv_coord$HH==hh&surv_coord$Year%in%c(2017:2019), "dam_fl_lag"])>=1,1,0)
  main_df2$rep_loss_fl[(i*2+1):(i*2+2)] <- ifelse(sum(surv_coord[surv_coord$HH==hh&surv_coord$Year%in%c(2017:2019), "loss_fl_lag"])>=1,1,0)
  main_df2$upazila[(i*2+1):(i*2+2)] <- surv_coord[surv_coord$HH==hh,"Upazila"]
}

# ADD SECOND OUTCOME: assets

main_df2$assets <- 0

for (i in 0:(dim(main_df2)[1]/2-1)) {
  hh <- main_df2$HH2[2*i+1]
  main_df2$assets[2*i+1] <- sum(assets_15_binded[assets_15_binded$a01==hh,3])
  main_df2$assets[2*i+2] <- sum(assets_18[assets_18$a01==hh,3])
}

# ADD CONTROLS
main_df2$age <- NA
main_df2$gender <- NA
main_df2$literacy <- NA
main_df2$occup_ag <- NA
main_df2$s_earn <- NA
main_df2$hh_size <- NA
main_df2$crops <- NA

for (i in 0:(dim(main_df2)[1]/2-1)) {
  hh <- main_df2$HH[2*i+1]
  if(hh%in%controlst1$a01){
    main_df2$age[(2*i+1)] <- controlst1[controlst1$a01==round(hh),3]
    main_df2$gender[(2*i+1)] <- controlst1[controlst1$a01==round(hh),2]
    main_df2$literacy[(2*i+1)] <- controlst1[controlst1$a01==round(hh),7]
    main_df2$occup_ag[(2*i+1)] <- controlst1[controlst1$a01==round(hh),8]
    main_df2$s_earn[(2*i+1)] <- controlst1[controlst1$a01==round(hh),10]
    main_df2$hh_size[(2*i+1)] <- controlst1[controlst1$a01==round(hh),11]
    main_df2$crops[(2*i+1)] <- controlst1[controlst1$a01==round(hh),12]
  } else if (hh%in%controlst2$a01){
    main_df2$age[(2*i+1)] <- controlst2[controlst2$a01==round(hh),3]-sample(c(3,4),1)
    main_df2$gender[(2*i+1)] <- controlst2[controlst2$a01==round(hh),2]
    main_df2$literacy[(2*i+1)] <- controlst2[controlst2$a01==round(hh),7]
  }
  if(hh%in%controlst2$a01){
    main_df2$age[(2*i+2)] <- controlst2[controlst2$a01==round(hh),3]
    main_df2$gender[(2*i+2)] <- controlst2[controlst2$a01==round(hh),2]
    main_df2$literacy[(2*i+2)] <- controlst2[controlst2$a01==round(hh),7]
    main_df2$occup_ag[(2*i+2)] <- controlst2[controlst2$a01==round(hh),8]
    main_df2$s_earn[(2*i+2)] <- controlst2[controlst2$a01==round(hh),10]
    main_df2$hh_size[(2*i+2)] <- controlst2[controlst2$a01==round(hh),11]
    main_df2$crops[(2*i+2)] <- controlst2[controlst2$a01==round(hh),12]
  } else if (hh%in%controlst1$a01){
    main_df2$age[(2*i+2)] <- controlst1[controlst1$a01==round(hh),3] + sample(c(3,4),1)
    main_df2$gender[(2*i+2)] <- controlst1[controlst1$a01==round(hh),2]
    main_df2$literacy[(2*i+2)] <- controlst1[controlst1$a01==round(hh),7]
  }
}

df_buffer2 <- main_df2
# Save main_df
save(df_buffer2,file="/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/R/Saved Data/main_df_diffindiff_buffer2.Rdata")


# --------------------- MAIN: 3000m buffer ----------------------------

# ADD DUMMY FOR TREATMENT: f
main_df3$f <- 0 
main_df3$duration <- 0
main_df3$f_frac <- 0
main_df3$sw_frac <- 0

for (i in 0:(dim(main_df3)[1]/2-1)) {
  hh <- main_df3$HH3[i*2+1]
  main_df3$f[(i*2+1):(i*2+2)] <- pull(bihs_coord[bihs_coord$a01==hh,"flooded"])
  main_df3$duration[(i*2+1):(i*2+2)] <- pull(bihs_coord[bihs_coord$a01==hh,"duration"])
  main_df3$f_frac[(i*2+1):(i*2+2)] <- pull(bihs_coord[bihs_coord$a01==hh,"flood_frac3"])
  main_df3$sw_frac[(i*2+1):(i*2+2)] <- pull(bihs_coord[bihs_coord$a01==hh,"sw_frac3"])
}

# MIGRATED post-2017
main_df3$migr <- 0
main_df3$remit <- 0

for (i in 1:length(migr17)) {
  main_df3[(main_df3$HH3==migr17[i])&(main_df3$t3==1),"migr"] <- 1
}

for (i in 1:length(migr_remit)) {
  main_df3[(main_df3$HH3==migr_remit[i])&(main_df3$t3==1),"remit"] <- 1
}

# ADD SECOND TREATMENT: reported damages
main_df3$rep_dam_fl <- 0
main_df3$rep_loss_fl <- 0
main_df3$upazila <- 0

for (i in 0:(dim(main_df3)[1]/2-1)) {
  hh <- main_df3$HH3[i*2+1]
  main_df3$rep_dam_fl[(i*2+1):(i*2+2)] <- ifelse(sum(surv_coord[surv_coord$HH==hh&surv_coord$Year%in%c(2017:2019), "dam_fl_lag"])>=1,1,0)
  main_df3$rep_loss_fl[(i*2+1):(i*2+2)] <- ifelse(sum(surv_coord[surv_coord$HH==hh&surv_coord$Year%in%c(2017:2019), "loss_fl_lag"])>=1,1,0)
  main_df3$upazila[(i*2+1):(i*2+2)] <- surv_coord[surv_coord$HH==hh,"Upazila"]
}

# ADD SECOND OUTCOME: assets

main_df3$assets <- 0

for (i in 0:(dim(main_df3)[1]/2-1)) {
  hh <- main_df3$HH3[2*i+1]
  main_df3$assets[2*i+1] <- sum(assets_15_binded[assets_15_binded$a01==hh,3])
  main_df3$assets[2*i+2] <- sum(assets_18[assets_18$a01==hh,3])
}

# ADD CONTROLS
main_df3$age <- NA
main_df3$gender <- NA
main_df3$literacy <- NA
#main_df3$occup_ag <- NA
#main_df3$s_earn <- NA
#main_df3$hh_size <- NA

for (i in 0:(dim(main_df3)[1]/2-1)) {
  hh <- main_df3$HH[2*i+1]
  if(hh%in%controlst1$a01){
    main_df3$age[(2*i+1)] <- controlst1[controlst1$a01==round(hh),3]
    main_df3$gender[(2*i+1)] <- controlst1[controlst1$a01==round(hh),2]
    main_df3$literacy[(2*i+1)] <- controlst1[controlst1$a01==round(hh),7]
    #main_df3$occup_ag[(2*i+1)] <- controlst1[controlst1$a01==round(hh),8]
    #main_df3$s_earn[(2*i+1)] <- controlst1[controlst1$a01==round(hh),10]
    #main_df3$hh_size[(2*i+1)] <- controlst1[controlst1$a01==round(hh),11]
  } else if (hh%in%controlst2$a01){
    main_df3$age[(2*i+1)] <- controlst2[controlst2$a01==round(hh),3]-sample(c(3,4),1)
    main_df3$gender[(2*i+1)] <- controlst2[controlst2$a01==round(hh),2]
    main_df3$literacy[(2*i+1)] <- controlst2[controlst2$a01==round(hh),7]
  }
  if(hh%in%controlst2$a01){
    main_df3$age[(2*i+2)] <- controlst2[controlst2$a01==round(hh),3]
    main_df3$gender[(2*i+2)] <- controlst2[controlst2$a01==round(hh),2]
    main_df3$literacy[(2*i+2)] <- controlst2[controlst2$a01==round(hh),7]
    #main_df3$occup_ag[(2*i+2)] <- controlst2[controlst2$a01==round(hh),8]
    #main_df3$s_earn[(2*i+2)] <- controlst2[controlst2$a01==round(hh),10]
    #main_df3$hh_size[(2*i+2)] <- controlst2[controlst2$a01==round(hh),11]
  } else if (hh%in%controlst1$a01){
    main_df3$age[(2*i+2)] <- controlst1[controlst1$a01==round(hh),3] + sample(c(3,4),1)
    main_df3$gender[(2*i+2)] <- controlst1[controlst1$a01==round(hh),2]
    main_df3$literacy[(2*i+2)] <- controlst1[controlst1$a01==round(hh),7]
  }
}

df_buffer3 <- main_df3
# Save main_df
save(df_buffer3,file="/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/R/Saved Data/main_df_diffindiff_buffer3.Rdata")



# --------------------- MAIN 4: 2000m buffer ----------------------------

# ADD DUMMY FOR TREATMENT: f
main_df4$f <- 0 
main_df4$duration <- 0
main_df4$f_frac <- 0
main_df4$sw_frac <- 0

for (i in 0:(dim(main_df4)[1]/2-1)) {
  hh <- main_df4$HH4[i*2+1]
  main_df4$f[(i*2+1):(i*2+2)] <- pull(bihs_coord[bihs_coord$a01==hh,"flooded"])
  main_df4$duration[(i*2+1):(i*2+2)] <- pull(bihs_coord[bihs_coord$a01==hh,"duration"])
  main_df4$f_frac[(i*2+1):(i*2+2)] <- pull(bihs_coord[bihs_coord$a01==hh,"flood_frac4"])
  main_df4$sw_frac[(i*2+1):(i*2+2)] <- pull(bihs_coord[bihs_coord$a01==hh,"sw_frac4"])
}

# MIGRATED post-2017
main_df4$migr <- 0
main_df4$remit <- 0

for (i in 1:length(migr17)) {
  main_df4[(main_df4$HH4==migr17[i])&(main_df4$t4==1),"migr"] <- 1
}

for (i in 1:length(migr_remit)) {
  main_df4[(main_df4$HH4==migr_remit[i])&(main_df4$t4==1),"remit"] <- 1
}

# ADD SECOND TREATMENT: reported damages
main_df4$rep_dam_fl <- 0
main_df4$rep_loss_fl <- 0
main_df4$upazila <- 0

for (i in 0:(dim(main_df4)[1]/2-1)) {
  hh <- main_df3$HH3[i*2+1]
  main_df4$rep_dam_fl[(i*2+1):(i*2+2)] <- ifelse(sum(surv_coord[surv_coord$HH==hh&surv_coord$Year%in%c(2017:2019), "dam_fl_lag"])>=1,1,0)
  main_df4$rep_loss_fl[(i*2+1):(i*2+2)] <- ifelse(sum(surv_coord[surv_coord$HH==hh&surv_coord$Year%in%c(2017:2019), "loss_fl_lag"])>=1,1,0)
  main_df4$upazila[(i*2+1):(i*2+2)] <- surv_coord[surv_coord$HH==hh,"Upazila"]
}

# ADD SECOND OUTCOME: assets

main_df4$assets <- 0

for (i in 0:(dim(main_df4)[1]/2-1)) {
  hh <- main_df4$HH4[2*i+1]
  main_df4$assets[2*i+1] <- sum(assets_15_binded[assets_15_binded$a01==hh,3])
  main_df4$assets[2*i+2] <- sum(assets_18[assets_18$a01==hh,3])
}

# ADD CONTROLS
main_df4$age <- NA
main_df4$gender <- NA
main_df4$literacy <- NA
main_df4$occup_ag <- NA
main_df4$s_earn <- NA
main_df4$hh_size <- NA

for (i in 0:(dim(main_df4)[1]/2-1)) {
  hh <- main_df4$HH[2*i+1]
  if(hh%in%controlst1$a01){
    main_df4$age[(2*i+1)] <- controlst1[controlst1$a01==round(hh),3]
    main_df4$gender[(2*i+1)] <- controlst1[controlst1$a01==round(hh),2]
    main_df4$literacy[(2*i+1)] <- controlst1[controlst1$a01==round(hh),7]
    main_df4$occup_ag[(2*i+1)] <- controlst1[controlst1$a01==round(hh),8]
    main_df4$s_earn[(2*i+1)] <- controlst1[controlst1$a01==round(hh),10]
    main_df4$hh_size[(2*i+1)] <- controlst1[controlst1$a01==round(hh),11]
  } else if (hh%in%controlst2$a01){
    main_df4$age[(2*i+1)] <- controlst2[controlst2$a01==round(hh),3]-sample(c(3,4),1)
    main_df4$gender[(2*i+1)] <- controlst2[controlst2$a01==round(hh),2]
    main_df4$literacy[(2*i+1)] <- controlst2[controlst2$a01==round(hh),7]
  }
  if(hh%in%controlst2$a01){
    main_df4$age[(2*i+2)] <- controlst2[controlst2$a01==round(hh),3]
    main_df4$gender[(2*i+2)] <- controlst2[controlst2$a01==round(hh),2]
    main_df4$literacy[(2*i+2)] <- controlst2[controlst2$a01==round(hh),7]
    main_df4$occup_ag[(2*i+2)] <- controlst2[controlst2$a01==round(hh),8]
    main_df4$s_earn[(2*i+2)] <- controlst2[controlst2$a01==round(hh),10]
    main_df4$hh_size[(2*i+2)] <- controlst2[controlst2$a01==round(hh),11]
  } else if (hh%in%controlst1$a01){
    main_df4$age[(2*i+2)] <- controlst1[controlst1$a01==round(hh),3] + sample(c(3,4),1)
    main_df4$gender[(2*i+2)] <- controlst1[controlst1$a01==round(hh),2]
    main_df4$literacy[(2*i+2)] <- controlst1[controlst1$a01==round(hh),7]
  }
}


df_buffer4 <- main_df4
# Save main_df
save(df_buffer4,file="/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/R/Saved Data/main_df_diffindiff_buffer4.Rdata")


# --------------------- MAIN 5: 1000m buffer ----------------------------

# ADD DUMMY FOR TREATMENT: f
main_df5$f <- 0 
main_df5$duration <- 0
main_df5$f_frac <- 0

for (i in 0:(dim(main_df5)[1]/2-1)) {
  hh <- main_df5$HH5[i*2+1]
  main_df5$f[(i*2+1):(i*2+2)] <- pull(bihs_coord[bihs_coord$a01==hh,"flooded"])
  main_df5$duration[(i*2+1):(i*2+2)] <- pull(bihs_coord[bihs_coord$a01==hh,"duration"])
  main_df5$f_frac[(i*2+1):(i*2+2)] <- pull(bihs_coord[bihs_coord$a01==hh,"flood_frac5"])
}

# MIGRATED post-2017
main_df5$migr <- 0
main_df5$remit <- 0

for (i in 1:length(migr17)) {
  main_df5[(main_df5$HH5==migr17[i])&(main_df5$t5==1),"migr"] <- 1
}

for (i in 1:length(migr_remit)) {
  main_df5[(main_df5$HH5==migr_remit[i])&(main_df5$t5==1),"remit"] <- 1
}

# ADD SECOND TREATMENT: reported damages
main_df5$rep_dam_fl <- 0
main_df5$rep_loss_fl <- 0
main_df5$upazila <- 0

for (i in 0:(dim(main_df5)[1]/2-1)) {
  hh <- main_df5$HH5[i*2+1]
  main_df5$rep_dam_fl[(i*2+1):(i*2+2)] <- ifelse(sum(surv_coord[surv_coord$HH==hh&surv_coord$Year%in%c(2017:2019), "dam_fl_lag"])>=1,1,0)
  main_df5$rep_loss_fl[(i*2+1):(i*2+2)] <- ifelse(sum(surv_coord[surv_coord$HH==hh&surv_coord$Year%in%c(2017:2019), "loss_fl_lag"])>=1,1,0)
  main_df5$upazila[(i*2+1):(i*2+2)] <- surv_coord[surv_coord$HH==hh,"Upazila"]
}

# ADD SECOND OUTCOME: assets

main_df5$assets <- 0

for (i in 0:(dim(main_df5)[1]/2-1)) {
  hh <- main_df5$HH5[2*i+1]
  main_df5$assets[2*i+1] <- sum(assets_15_binded[assets_15_binded$a01==hh,3])
  main_df5$assets[2*i+2] <- sum(assets_18[assets_18$a01==hh,3])
}

# ADD SURFACE WATER:
main_df5$sw <- 0 

for (i in 0:(dim(main_df5)[1]/2-1)) {
  hh <- main_df5$HH5[i*2+1]
  main_df5$sw[(i*2+1):(i*2+2)] <- pull(bihs_coord[bihs_coord$a01==hh,"surfacewater"])
}

# ADD RAINFALL for the Two-Stage Panel IV:
main_df5$spi <- 0

for (i in 0:(dim(main_df5)[1]/2-1)) {
  hh <- main_df5$HH5[i*2+1]
  main_df5$spi[(i*2+1):(i*2+2)] <- pull(bihs_coord[bihs_coord$a01==hh,"rainfall"])
}

df_buffer5 <- main_df5
# Save main_df
save(df_buffer5,file="/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/R/Saved Data/main_df_diffindiff_buffer5.Rdata")


# --------------------- MAIN 6: 500m buffer ----------------------------

# ADD DUMMY FOR TREATMENT: f
main_df6$f <- 0 
main_df6$duration <- 0
main_df6$f_frac <- 0

for (i in 0:(dim(main_df6)[1]/2-1)) {
  hh <- main_df6$HH6[i*2+1]
  main_df6$f[(i*2+1):(i*2+2)] <- pull(bihs_coord[bihs_coord$a01==hh,"flooded"])
  main_df6$duration[(i*2+1):(i*2+2)] <- pull(bihs_coord[bihs_coord$a01==hh,"duration"])
  main_df6$f_frac[(i*2+1):(i*2+2)] <- pull(bihs_coord[bihs_coord$a01==hh,"flood_frac6"])
}

# MIGRATED post-2017
main_df6$migr <- 0
main_df6$remit <- 0

for (i in 1:length(migr17)) {
  main_df6[(main_df6$HH6==migr17[i])&(main_df6$t6==1),"migr"] <- 1
}

for (i in 1:length(migr_remit)) {
  main_df6[(main_df6$HH6==migr_remit[i])&(main_df6$t6==1),"remit"] <- 1
}

# ADD SECOND TREATMENT: reported damages
main_df6$rep_dam_fl <- 0
main_df6$rep_loss_fl <- 0
main_df6$upazila <- 0

for (i in 0:(dim(main_df6)[1]/2-1)) {
  hh <- main_df6$HH6[i*2+1]
  main_df6$rep_dam_fl[(i*2+1):(i*2+2)] <- ifelse(sum(surv_coord[surv_coord$HH==hh&surv_coord$Year%in%c(2017:2019), "dam_fl_lag"])>=1,1,0)
  main_df6$rep_loss_fl[(i*2+1):(i*2+2)] <- ifelse(sum(surv_coord[surv_coord$HH==hh&surv_coord$Year%in%c(2017:2019), "loss_fl_lag"])>=1,1,0)
  main_df6$upazila[(i*2+1):(i*2+2)] <- surv_coord[surv_coord$HH==hh,"Upazila"]
}

# ADD SECOND OUTCOME: assets

main_df6$assets <- 0

for (i in 0:(dim(main_df6)[1]/2-1)) {
  hh <- main_df6$HH6[2*i+1]
  main_df6$assets[2*i+1] <- sum(assets_15_binded[assets_15_binded$a01==hh,3])
  main_df6$assets[2*i+2] <- sum(assets_18[assets_18$a01==hh,3])
}

# ADD SURFACE WATER:
main_df6$sw <- 0 

for (i in 0:(dim(main_df6)[1]/2-1)) {
  hh <- main_df6$HH6[i*2+1]
  main_df6$sw[(i*2+1):(i*2+2)] <- pull(bihs_coord[bihs_coord$a01==hh,"surfacewater"])
}

# ADD RAINFALL for the Two-Stage Panel IV:
main_df6$spi <- 0

for (i in 0:(dim(main_df6)[1]/2-1)) {
  hh <- main_df6$HH6[i*2+1]
  main_df6$spi[(i*2+1):(i*2+2)] <- pull(bihs_coord[bihs_coord$a01==hh,"rainfall"])
}

df_buffer6 <- main_df6
# Save main_df
save(df_buffer6,file="/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/R/Saved Data/main_df_diffindiff_buffer6.Rdata")




