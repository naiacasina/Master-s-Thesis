# ========================================================================
# correlations.R    -   Naia Ormaza Zulueta   -  May 2022
# In this file:
# - I gather all the flooding events between 2006-2019 cropped to the 
#   boundaries of Bangladesh and aggregated to Upazila-level 
# - Using it as an independent var, I run a regression of Reported Losses
#   due to flooding on actual flooding events
# ========================================================================
library(foreign)

Year <- rep(c(2006:2015), each=12)
Month <- rep(c(1:12), times=10)
events_fl <- data.frame(Year, Month)

# Flooding
fl_events <- list(c(2006,04,05),c(2006,05,07),c(2006,06,08),c(2006,07,09),
                  c(2006,08,09),c(2006,08,09),c(2006,08,09),c(2006,08,09),
                  c(2007,06),c(2007,06),c(2007,06),c(2007,06,07),
                  c(2007,07), c(2007,07,09), c(2007,07,08), c(2007,07,10),
                  c(2007,07,10), c(2007,07,10), c(2007,08), c(2007,08),
                  c(2007,08,10), c(2007,09,10), c(2007,10), c(2007,11,12),
                  c(2008,05), c(2008,07), c(2008,07,08), c(2008,08,09),
                  c(2008,08,09), c(2008,08,09), c(2008,09), c(2008,09), 
                  c(2009,04,05), c(2009,07), c(2009,07), c(2009,08),
                  c(2009,10), c(2010,03,04), c(2010,04), c(2010,05,06),
                  c(2010,06), c(2010,07), c(2010,07), c(2010,09),
                  c(2010,09), c(2010,09), c(2010,10), c(2011,7),c(2011,8,9,10),
                  c(2011,8,9),c(2011,9,10),c(2012,9), c(2013,10,11), c(2014,6),
                  c(2014,8,9),c(2014,8,9),c(2014,8,9),c(2014,8,9),c(2014,9,10),
                  c(2015,6),c(2015,7,8),c(2015,7,8),c(2015,8,9))

shp <- read.dbf(file="/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/gadm40_BGD_shp/gadm40_BGD_3.dbf")

# Load aggregated flood events (cropped_fl)
load(file="/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/HRV/Cropped and Aggregated/flood.RData")

# New columns for events_fl
for (i in 1:dim(shp)[1]) {events_fl[as.character(i)] <- 0}

for (i in 1:length(fl_events)) {
  l1 <- length(unlist(fl_events[i]))
  y <- unlist(fl_events[i])[1]
  df_fl <- data.frame(cropped_fl[i])
  df_fl$flooded[is.na(df_fl$flooded)] <- 0
  for (j in 2:l1) {
    for (k in 1:dim(shp)[1]) {
      events_fl[(events_fl$Year==y)&(events_fl$Month==unlist(fl_events[i])[j]), 2+k] <- events_fl[(events_fl$Year==y)&(events_fl$Month==unlist(fl_events[i])[j]), 2+k]+ df_fl$flooded[k]
    }
  }
}

# --------------- Survey Data ----------------
# Load census datasets
load("path")
load("path")

# 2011
floods_11 <- bihs11_di2[(bihs11_di2$t1_02=="major loss of crops due to flood")|(bihs11_di2$t1_02=="loss of productive assets due to floods")|(bihs11_di2$t1_02=="loss of livestock due to flood"),]

# 2015
floods_15 <- bihs15_di2[(bihs15_di2$t1_02=="Loss of productive assets due to floods")|(bihs15_di2$t1_02=="Major loss of crops due to flood")|(bihs15_di2$t1_02=="Loss of livestock due to flood"),]

# Creation of DFrame
Year <- rep(c(2006:2015), each=12)
Month <- rep(c(1:12), times=10)
reports_fl <- data.frame(Year, Month)

# DF of numbers with Upazila Names
df_upazila <- data.frame(1:dim(shp)[1],shp$NAME_3)
# New columns for reports_fl
for (i in 1:dim(shp)[1]) {reports_fl[as.character(i)] <- 0}


# Compute matrix of total loss carried out by the survey hh due to floods
for (i in 1:dim(floods_11)[1]) {
  upaz <- bihs11_c1$Upazila_Name[floods_11$a01[i]]   # take Upazila name of hh
  col_n <- df_upazila[df_upazila$shp.NAME_3==upaz, 1]    # equiv. number
  reports_fl[(reports_fl$Year==floods_11$t1_05[i])&(reports_fl$Month==floods_11$t1_04[i]),2+col_n] <- reports_fl[(reports_fl$Year==floods_11$t1_05[i])&(reports_fl$Month==floods_11$t1_04[i]),3] + floods_11$t1_07[i]
}

for (i in 1:dim(floods_15)[1]) {
  upaz <- bihs15_c1$Upazila_Name[floods_15$a01[i]]   # take Upazila name of hh
  col_n <- df_upazila[df_upazila$shp.NAME_3==upaz, 1]    # equiv. number
  reports_fl[(reports_fl$Year==floods_15$t1_05[i])&(reports_fl$Month==floods_15$t1_04[i]),2+col_n] <- reports_fl[(reports_fl$Year==floods_15$t1_05[i])&(reports_fl$Month==floods_15$t1_04[i]),3] + floods_15$t1_07[i]
}

# ------------------ Compute correlations ------------------
# Plain correlations --------
mat11 <- data.matrix(events_fl)
mat15 <- data.matrix(reports_fl)
mat11 <- mat11[13:120,3:464]
mat15 <- mat15[13:120,3:464]

datuak <- data.frame(c(mat11),c(mat15))
datuak <- lm(c.mat15. ~ c.mat11., data=datuak)
summary(datuak)
# ---------------------------

# With controls -------------
fl11 <- floods_11[,c(1,4,5,7)]
names(fl11)[names(fl11) == "t1_04"] <- "month"
names(fl11)[names(fl11) == "t1_05"] <- "year"
names(fl11)[names(fl11) == "t1_07"] <- "loss"
fl11$dist <- 0
fl11$upazila_1 <- 0
fl11$upazila_2 <- 0
fl11$flood_ev <- 0

for (i in 1:dim(fl11)[1]) {
  dist <- bihs11_c1$District_Name[fl11$a01[i]]  # take District name of hh
  upaz <- bihs11_c1$Upazila_Name[fl11$a01[i]]   # take Upazila name of hh
  up_n <- df_upazila[df_upazila$shp.NAME_3==upaz, 1]    # equiv. number
  fl11$dist <- dist
  fl11$upazila_1[i] <- up_n
  fl11$upazila_2[i] <- upaz
  fl11$flood_ev[i] <- events_fl[(events_fl$Year==fl11$year[i])&(events_fl$Month==fl11$month[i]),2+up_n]
}

fl15 <- floods_15[,c(1,6,7,9)]
names(fl15)[names(fl15) == "t1_04"] <- "month"
names(fl15)[names(fl15) == "t1_05"] <- "year"
names(fl15)[names(fl15) == "t1_07"] <- "loss"
fl15$dist <- 0
fl15$upazila_1 <- 0
fl15$upazila_2 <- 0
fl15$flood_ev <- 0

for (i in 1:dim(fl15)[1]) {
  dist <- bihs15_c1$District_Name[fl15$a01[i]]  # take District name of hh
  upaz <- bihs15_c1$Upazila_Name[fl15$a01[i]]   # take Upazila name of hh
  up_n <- df_upazila[df_upazila$shp.NAME_3==upaz, 1]    # equiv. number
  fl15$dist[i] <- dist
  fl15$upazila_1[i] <- up_n
  fl15$upazila_2[i] <- upaz
  fl15$flood_ev[i] <- events_fl[(events_fl$Year==fl15$year[i])&(events_fl$Month==fl15$month[i]),2+up_n]
}

fl15_2014 <- fl15[(fl15$year==2014)&((fl15$month==8)|(fl15$month==9)|(fl15$month==10)),]

# Save reported and actual events
save(fl15_2014,file="path")
save(events_fl,file="path")


# 57th uholde
Year <- rep(2014, each=12)
Month <- rep(c(1:12), times=1)
events_14 <- data.frame(Year, Month)

for (i in 1:dim(shp)[1]) {events_14[as.character(i)] <- 0}

i<- 57
l1 <- length(unlist(fl_events[i]))
y <- unlist(fl_events[i])[1]
df_fl <- data.frame(cropped_fl[i])
df_fl$flooded[is.na(df_fl$flooded)] <- 0
for (j in 2:l1) {
  for (k in 1:dim(shp)[1]) {
    events_14[(events_14$Year==y)&(events_14$Month==unlist(fl_events[i])[j]), 2+k] <- events_14[(events_14$Year==y)&(events_14$Month==unlist(fl_events[i])[j]), 2+k]+ df_fl$flooded[k]
  }
}

flevents14 <- df_upazila
flevents14$ev <- 0

for (i in 1:dim(df_upazila)[1]) {
  if(sum(events_14[,2+i])>0){flevents14$ev[i]<-1}
}
save(events_14,file="path")
# ---------------------------
library(foreign)

shp3 <- read.dbf(file="path")
                       