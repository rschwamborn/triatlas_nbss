# NBSS analysis across the Atlantic and TLs 
# v.24_new data with MLD and zooplankton_slopes_Biovol_only
# Ralf Schwamborn
# September 11, 2024

# Data sources: ------------------

# Google Drive: https://drive.google.com/drive/folders/1GhVDNwR4dby6lItU5Me5GroIRBvS0ISe

# Google Sheet:https://docs.google.com/spreadsheets/d/1aYoHXladrYPNUMSLqn_bjkhbVGkCHnmuglOl4RBISW4/edit?gid=1562218338#gid=1562218338 



# clean memory
gc()  
gc(reset=T)
rm(list = ls()) 


# load libraries ----------------

library(scales)
library(ggplot2)
library(maps)
library(MASS)

opar <- par() # save plot parameters
# old.par <- par(mar = c(0, 0, 0, 0))
# par(opar)



# set working directory

setwd("C:/Users/RALF/Documents/Papers/000 -Paper_Fish_Zoopl_Size_Spectra_Brazil_Africa")


# 1. Import Data  --------------------------------------------------------------

#X_vectormm3log <- read.table("~/Papers/000 -Paper_Fish_Zoopl_Size_Spectra_Brazil_Africa/Data/X_Vector_forNBSS_biovolumes.csv", quote="\"", comment.char="")
#X_vectormm3 <- read.table("~/Papers/000 -Paper_Fish_Zoopl_Size_Spectra_Brazil_Africa/biovolume_means_X_vector.csv", quote="\"", comment.char="")
#Data.All.Atl <- read.csv("~/Papers/000 -Paper_Fish_Zoopl_Size_Spectra_Brazil_Africa/Data/Atlantic_size_spectraV5b_forR.csv")

Data.All.Atl <- Atlantic_size_spectraV5c_forR_Biovol_only <- read.csv("~/Papers/000 -Paper_Fish_Zoopl_Size_Spectra_Brazil_Africa/Data/Atlantic_size_spectraV5c_forR_Biovol_only.csv")

X_vectormm3log  <- read.table("~/Papers/000 -Paper_Fish_Zoopl_Size_Spectra_Brazil_Africa/log_biovolume_means_X_vector_v5c.csv", quote="\"", comment.char="")


X_vectormm3log <-     X_vectormm3log$V1
  length(X_vectormm3log) # 56 bin midths (log scale)
  X_vectormm3 <-    10^X_vectormm3log 


   dim(Data.All.Atl) # 3371   rows, 107 columns

   
   
  # View(Data.All.Atl)
   
   # 2. A first quick look at the data -----------------------------------------

   summary(Data.All.Atl)
   
   attach(Data.All.Atl)
   
summary(slope)   
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -3.2900 -1.2575 -0.9400 -0.8909 -0.5900  2.1000    1307 


Data.All.Atl$target_organisms <- as.factor(Data.All.Atl$target_organisms)


Data.All.Atl$SST_C_insitu <- as.numeric(Data.All.Atl$SST_C_insitu)


summary(Data.All.Atl$target_organisms)

# detritus + zooplankton       mesopelagic fish            micronekton 
# 741                    356                    119 
# Phytoplankton            zooplankton 
# 575                   1580 


# 3. cleanup and data formatting -----------------------------------------------

# 3.1 convert zeros to empty bins ("NA") in NBSS  ------------------------------

# NBSS data (X5.68E.14 to X1.02E.03, columns no. 37 to 91, column 92 is NBSS slope)

summary(Data.All.Atl)

head (Data.All.Atl[,37:92])

# View (Data.All.Atl)


NBSS.raw.only <- Data.All.Atl[,37:92]

NBSS.raw.only[NBSS.raw.only == 0] <- NA

Data.All.Atl[,37:92] <- NBSS.raw.only

summary(Data.All.Atl)

# View (Data.All.Atl) 

# 3.2  Data formats: text to factors or numbers, colours  -------------------------------

# factors
Data.All.Atl$target_organisms <- as.factor(Data.All.Atl$target_organisms)
Data.All.Atl$target_target_org_code_code <- as.factor(Data.All.Atl$target_org_code)
Data.All.Atl$Gear            <- as.factor(Data.All.Atl$Gear)
Data.All.Atl$operation_mode            <- as.factor(Data.All.Atl$operation_mode)
Data.All.Atl$Biomass_determination            <- as.factor(Data.All.Atl$Biomass_determination)
Data.All.Atl$Contact_Person            <- as.factor(Data.All.Atl$Contact_Person)
Data.All.Atl$day_night            <- as.factor(Data.All.Atl$day_night)


# numbers
Data.All.Atl$SST            <- as.numeric(Data.All.Atl$SST_C_insitu)

Data.All.Atl$primprod            <- as.numeric(Data.All.Atl$primprod)
Data.All.Atl$salinity            <- as.numeric(Data.All.Atl$salinity)
Data.All.Atl$pH            <- as.numeric(Data.All.Atl$pH)
Data.All.Atl$chla            <- as.numeric(Data.All.Atl$chla)
Data.All.Atl$nitrate            <- as.numeric(Data.All.Atl$nitrate)
Data.All.Atl$silicate            <- as.numeric(Data.All.Atl$silicate)

# colour codes -----------------------------------------------------------------


summary(Data.All.Atl$target_organisms)
Data.All.Atl$target.org.colour <- as.character( Data.All.Atl$target_organisms)

Data.All.Atl$target_organisms <- as.character( Data.All.Atl$target_organisms)

Data.All.Atl$target_organisms[target_organisms == "Phytoplankton"]  <- "phytoplankton"

attach(Data.All.Atl)
Data.All.Atl$target.org.colour[target.org.colour == "phytoplankton"]  <- "forestgreen"
Data.All.Atl$target.org.colour[target.org.colour == "zooplankton"]  <- "dodgerblue"
Data.All.Atl$target.org.colour[target.org.colour == "micronekton"]  <- "purple"
Data.All.Atl$target.org.colour[target.org.colour == "mesopelagic fish"]  <- "darkorange"
Data.All.Atl$target.org.colour <- as.factor( Data.All.Atl$target.org.colour)
Data.All.Atl$target_organisms <- as.factor( Data.All.Atl$target_organisms)

summary(Data.All.Atl$target.org.colour)
# darkorange  dodgerblue forestgreen      purple 
# 376        1579         727         119 


summary(Data.All.Atl$target_organisms)
# detritus + zooplankton       mesopelagic fish            micronekton 
#                    741                    356                    119 
#          phytoplankton            zooplankton 
#                    575                   1580 

# Delete empty lines (no NBSS slopes) -----------------------------------------


length(Data.All.Atl$ConsecutiveNumber) # 2801 lines (datasets, ALL Data)

#df[!(is.na(df$start_pc) | df$start_pc==""), ]

Data.All.Atl <- Data.All.Atl[!(is.na(Data.All.Atl$slope) | Data.All.Atl$slope==""), ]


length(Data.All.Atl$ConsecutiveNumber) # 1494 useful lines (datasets with NBSS slopes, ALL Data)


summary(Data.All.Atl$target_organisms)
# new:
# detritus + zooplankton       mesopelagic fish            micronekton          phytoplankton 
# 741                                10                     11                      555 
# zooplankton 
# 831 



# 4. Quality checks, plots and further cleanups  -------------------------------

 
# eliminate "exotic" NBSS (outliers) -------------------------------------
# keep only NBSS slopes between -2 and -0.1 
Data.All.Atl$slope.clean <-    Data.All.Atl$slope

Data.All.Atl$slope.clean[Data.All.Atl$slope.clean < -2]  <- NA
Data.All.Atl$slope.clean[Data.All.Atl$slope.clean > -0.1]  <- NA

length(Data.All.Atl$slope.clean) # 2148 data points

# 135 exotic data deleted

summary(Data.All.Atl$slope)  # # 1494 NBSS data  

hist(Data.All.Atl$slope)

# "slope.clean" new data vector with "clean" slopes (no exotic ones), tails were cut off from the distribtion
summary(Data.All.Atl$slope.clean) # "clean" only: 135 exotic data deleted (approx 9% of the data)
hist(Data.All.Atl$slope.clean)



# New productivity indices -----------------------

# New productivity index <-  log10Chla+nitr+silic ------------------
# in situ data only -------------------------

# prod.index 5 ------------------

attach (Data.All.Atl)

#standardize (log10, then divide by mean)
chla2_stand.temp <- (2*log10(Data.All.Atl$chla_model) )
chla2_stand.ok <- chla2_stand.temp/ mean(chla2_stand.temp)

Nitrat_stand.temp <- log10(Data.All.Atl$Nitrates_Mathilde)
Nitrat_stand.ok <- Nitrat_stand.temp/ median( na.exclude( Nitrat_stand.temp))

MLD_stand.temp <- (log10(1/Data.All.Atl$MLD) )

MLD_stand.ok <- MLD_stand.temp/ median(na.exclude(MLD_stand.temp))

Data.All.Atl$prod.index5_2Chl_nitr_MLD_mutipl <- chla2_stand.ok*Nitrat_stand.ok*MLD_stand.ok

Data.All.Atl$prod.index5_2Chl_nitr_MLD_mutipl[Data.All.Atl$prod.index5_2Chl_nitr_MLD_mutipl== "-Inf"] <- NA

attach (Data.All.Atl)


hist(Data.All.Atl$prod.index5_2Chl_nitr_MLD_mutipl)


attach (Data.All.Atl)


Data.All.Atl$prod.index4_2Chl_nitr_MLD <- (2*log10(Data.All.Atl$chla_model) +
                                             log10(Data.All.Atl$Nitrates_Mathilde)+
                                             log10(1/Data.All.Atl$MLD) )
Data.All.Atl$prod.index4_2Chl_nitr_MLD[Data.All.Atl$prod.index4_2Chl_nitr_MLD== "-Inf"] <- NA

attach (Data.All.Atl)


#plot(Data.All.Atl$prod.index2 ~ Data.All.Atl$prod.index4_2Chl_nitr_MLD)



Data.All.Atl$prod.index1 <-  log10(1+Chlorophyll__mg_m3_insitu) * Nitrate__uM_L1_insitu * silicate
#prod.index.add <-  log10(Chlorophyll__mg_m3_insitu + Nitrate__uM_L1_insitu + silicate)
attach (Data.All.Atl)

Data.All.Atl$prod.index2 <-  (2*log10(1+Chlorophyll__mg_m3_insitu)) + log10(1+Nitrate__uM_L1_insitu) + log10(1+silicate)
# in situ data only -------------------------

summary(Data.All.Atl$prod.index2)
attach (Data.All.Atl)



# prod.index3 = 2* log10chla + log10Nitrate --------------------------------------
# in situ data only -------------------------
Data.All.Atl$prod.index3_2Chl_nitr <-  (2*log10(1+Chlorophyll__mg_m3_insitu)) + log10(1+Nitrate__uM_L1_insitu)


# prod.index4 =  log10chla + log10Nitrate --------------------------------------
# in situ data only -------------------------
Data.All.Atl$prod.index4_Chl_nitr <-  (log10(1+Chlorophyll__mg_m3_insitu)) + log10(1+Nitrate__uM_L1_insitu)



summary(Data.All.Atl$prod.index3_2Chl_nitr)
attach (Data.All.Atl)


summary(Data.All.Atl$prod.index2)
attach (Data.All.Atl)

hist(prod.index2)
# hist(prod.index.add)

plot(Salinity_insitu , prod.index1, xlim = c(31, 38))
plot(Temperature_C_insitu , prod.index1, xlim = c(1, 33))

plot(Salinity_insitu , prod.index1, xlim = c(31, 38))
plot(Temperature_C_insitu , prod.index1, xlim = c(1, 33))






# 5 Subsets by communities ------------------------------------------------------

summary(Data.All.Atl$target_organisms)

phytopl.All.Atl <- subset(Data.All.Atl, target_organisms == "phytoplankton" )# 565 data rows

zoopl.All.Atl <- subset(Data.All.Atl, target_organisms == "zooplankton" )# 846 data rows
dim(zoopl.All.Atl)
summary(zoopl.All.Atl$Gear)


mesop_fish.All.Atl <-  subset(Data.All.Atl, target_organisms == "mesopelagic fish" )
# 47 data rows

micronekton.All.Atl <-  subset(Data.All.Atl, target_organisms == "micronekton" )
# 47 data rows




# # II.1 Phytoplankton ------------
# II.1 check slopes for phytoplankton and recalculate with Robust regression ---------

phytopl.All.Atl$new_slopes <- phytopl.All.Atl$Comment
hist(phytopl.All.Atl$slope)

phytopl.All.Atl[1,35:92]

phytopl.All.Atl[1,37:92]

phytopl.All.Atl.NBSSmatrix <- as.matrix(phytopl.All.Atl[,37:92])
dim(phytopl.All.Atl.NBSSmatrix)
# 555 columns , 55 rows of Biovolume NBSS values 

#first NBSS plots, phytopl. --------------
lnumb <- 350
length(log10( phytopl.All.Atl.NBSSmatrix[lnumb,]))


lnumb <- 350
plot (log10( phytopl.All.Atl.NBSSmatrix[lnumb,]) ~ X_vectormm3log  ,
      ,
      main = paste( phytopl.All.Atl$target_organisms[lnumb]  , " , ",  phytopl.All.Atl$CruiseID[lnumb] )     )

summary(mod1<-  lm(log10( phytopl.All.Atl.NBSSmatrix[lnumb,]) ~ X_vectormm3log))
mod1$coefficients[2]
mod1_slope <- as.numeric(mod1$coefficients[2])
abline(mod1)
phytopl.All.Atl$slope[lnumb]
ratio_ralfvsChristina_slopes <- mod1_slope / phytopl.All.Atl$slope[lnumb]
ratio_ralfvsChristina_slopes
# ratio = 1, slopes calculated by Cristina = slopes calclate by Ralf (lm, OLSR)
# 

# robust regression  (MASS) ---------

library(MASS)

lnumb <- 300
modrr <- MASS::rlm(log10( phytopl.All.Atl.NBSSmatrix[lnumb,]) ~ X_vectormm3log)
summary(modrr)

slope_rr <- as.numeric(modrr$coefficients[2])
intercept_rr <- as.numeric(modrr$coefficients[1])




# as function 1 (lm, OLSR, gives slope) ---------

fun.1slope.lm.olsr <- function(yvalues , xvalues) {
  mod1<-  lm( yvalues ~ xvalues)
  mod1$coefficients[2]
  mod1_slope <- as.numeric(mod1$coefficients[2])
  ; mod1_slope}

lnumb <- 300
fun.1slope.lm.olsr(log10( phytopl.All.Atl.NBSSmatrix[lnumb,]) , X_vectormm3log)

# as function 2 (gives a table with OLSR slope, OLSR intercept, OLSR p value, ----------------
# R-squared, AIC,  robust regression slope and intercept) ---------

fun.2.lm.olsr.rob.reg.complete.table <- function(yvalues , xvalues) {
  mod1<-  lm( yvalues ~ xvalues)
  
  
  slope_olsr <- as.numeric(mod1$coefficients[2])
  intercept_olsr <- as.numeric(mod1$coefficients[1])
  
  p_value <-  summary(mod1)$coefficients[2,4] 
  R_squared <-  summary(mod1)$r.squared
  AIC_model = AIC(mod1)
  modrr <- MASS::rlm(yvalues ~ xvalues)
  
  
  slope_robust_regr <- as.numeric(modrr$coefficients[2])
  intercept_robust_regr <- as.numeric(modrr$coefficients[1])
  
  
  
  mod1_table <- data.frame(   slope_olsr=slope_olsr, intercept_olsr=intercept_olsr,  p_value=p_value, 
                              R_squared = R_squared, AIC_model = AIC_model,  slope_robust_regr =slope_robust_regr , intercept_robust_regr =intercept_robust_regr     )
  
  
  ; mod1_table}



lnumb <- 300
fun.2.lm.olsr.rob.reg.complete.table(log10( phytopl.All.Atl.NBSSmatrix[lnumb,]) , X_vectormm3log)


# apply function 1 to phytoplankton data (OLSR slopes recalculated, for cross-checking)------

out.slope.vec <- c(rep (NA, length(phytopl.All.Atl$Longitude)) )

for (i in 1: length(phytopl.All.Atl$Longitude)){
  
  lnumb <- i
  out.slope.vec[lnumb] <- fun.1slope.lm.olsr(log10( phytopl.All.Atl.NBSSmatrix[lnumb,]) , X_vectormm3log)
}

hist(out.slope.vec)

phytopl.All.Atl$new_slopes <-out.slope.vec

hist(phytopl.All.Atl$new_slopes / phytopl.All.Atl$slope)

phytopl.All.Atl$new_slopes / phytopl.All.Atl$slope # all OK!cross-checking OK!
# Conclusion: cross-checking phytopl slopes OK! n = 565 useful slope data 

which.max(phytopl.All.Atl$new_slopes / phytopl.All.Atl$slope) # line 415 is worst


lnumb <- 415
plot (log10( phytopl.All.Atl.NBSSmatrix[lnumb,]) ~ X_vectormm3log  ,
      ,
      main = paste( phytopl.All.Atl$target_organisms[lnumb]  , " , ",  phytopl.All.Atl$CruiseID[lnumb] )     )


#  Compare OLSR slopes with robust regression and check "p" values --------------
# apply function 2 ----------------

out.p.vec <- c(rep (NA, length(phytopl.All.Atl$Longitude)) )

out.slope.rr.vec <- c(rep (NA, length(phytopl.All.Atl$Longitude)) )


for (i in 1: length(phytopl.All.Atl$Longitude)){
  
  lnumb <- i
  
  out.table <- fun.2.lm.olsr.rob.reg.complete.table(log10( phytopl.All.Atl.NBSSmatrix[lnumb,]) , X_vectormm3log)
  
  out.p.vec[lnumb] <- out.table$p_value [1] 
  out.slope.rr.vec[lnumb] <-  out.table$slope_robust_regr[1]
  
}

hist(out.p.vec)
hist(out.slope.rr.vec)
hist (out.slope.vec )


phytopl.All.Atl$rob_reg__slopes <- out.slope.rr.vec

phytopl.All.Atl$p_values_olsr_slopes <- out.p.vec


ratio_slopes_rr_olsr <- phytopl.All.Atl$rob_reg__slopes / phytopl.All.Atl$slope # all OK!cross-checking OK!

hist(ratio_slopes_rr_olsr)

# USE ONLY P < 0.05 models

phytopl.All.Atl.p.ok <- subset  (phytopl.All.Atl, p_values_olsr_slopes  < 0.05)

dim(phytopl.All.Atl.p.ok) # 510 phytopl. data sets with  p < 0.05

ratio_slopes_rr_olsr_p_ok <- phytopl.All.Atl.p.ok$rob_reg__slopes / phytopl.All.Atl.p.ok$slope # all OK!cross-checking OK!

hist(ratio_slopes_rr_olsr_p_ok)

ratio_slopes_rr_olsr_p_ok

summary(ratio_slopes_rr_olsr_p_ok)
which.max(ratio_slopes_rr_olsr_p_ok) # line 79, with 1.4 times higher rr slope (outlier?)
which.min(ratio_slopes_rr_olsr_p_ok) # line 143, with 0.85 times higher rr slope (outlier?) , 1.2 times higher olsr slope, OK
# acceptable bias of OLSR vs Rob_reg = 30%   (ratio < 0.77 or ratio > 1.3) 

+1/1.3

# plot

phytopl.All.Atl.NBSSmatrix.p.ok <- as.matrix(phytopl.All.Atl.p.ok[,37:92])


lnumb <- 84

length(phytopl.All.Atl.NBSSmatrix.p.ok [lnumb,])


plot (log10( phytopl.All.Atl.NBSSmatrix.p.ok[lnumb,]) ~ X_vectormm3log ,
      main = paste( phytopl.All.Atl.p.ok$target_organisms[lnumb]  , 
                    " , st = ",  phytopl.All.Atl.p.ok$StationID[lnumb],
                    " , ",  phytopl.All.Atl.p.ok$CruiseID[lnumb] ,
                    " , line = "  , lnumb)     )

# action: delete one outlier (delete first NBSS data point of line 84, ACEx/SIMTECO st. 22)
# 
 phytopl.All.Atl.NBSSmatrix.p.ok2 <- phytopl.All.Atl.NBSSmatrix.p.ok
# 
 phytopl.All.Atl.NBSSmatrix.p.ok2[84,9] <- NA
# 
 phytopl.All.Atl.p.ok[84,9+36] <- NA
# 
 phytopl.All.Atl.p.ok[84, 37:100]
 phytopl.All.Atl.NBSSmatrix.p.ok2 [84,]



lnumb <- 84
plot (log10( phytopl.All.Atl.NBSSmatrix.p.ok2[lnumb,]) ~ X_vectormm3log ,
      main = paste( phytopl.All.Atl.p.ok$target_organisms[lnumb]  , 
                    " , st = ",  phytopl.All.Atl.p.ok$StationID[lnumb],
                    " , ",  phytopl.All.Atl.p.ok$CruiseID[lnumb] ,
                    " , line = "  , lnumb)     )







# apply function 1 again to phytoplankton data (OLSR slopes recalculated, for cross-checking)------

out.slope.vec <- c(rep (NA, length(phytopl.All.Atl.p.ok$Longitude)) )

for (i in 1: length(phytopl.All.Atl.p.ok$Longitude)){
  
  lnumb <- i
  out.slope.vec[lnumb] <- fun.1slope.lm.olsr(log10( phytopl.All.Atl.NBSSmatrix.p.ok2[lnumb,]) , X_vectormm3log)
}

hist(out.slope.vec)

phytopl.All.Atl.p.ok$new_slopes <-out.slope.vec

phytopl.All.Atl.p.ok$new_slopes / phytopl.All.Atl.p.ok$slope # all OK!cross-checking OK!
# Conclusion: cross-checking phytopl slopes OK! n = 565 useful slope data 

#  Compare OLSR slopes with robust regression and check "p" values --------------
# apply function 2 ----------------

out.p.vec <- c(rep (NA, length(phytopl.All.Atl.p.ok$Longitude)) )

out.slope.rr.vec <- c(rep (NA, length(phytopl.All.Atl.p.ok$Longitude)) )

out.slope.r.sq.vec <- c(rep (NA, length(phytopl.All.Atl.p.ok$Longitude)) )



for (i in 1: length(phytopl.All.Atl.p.ok$Longitude)){
  
  lnumb <- i
  
  out.table <- fun.2.lm.olsr.rob.reg.complete.table(log10( phytopl.All.Atl.NBSSmatrix.p.ok2[lnumb,]) , X_vectormm3log)
  
  out.p.vec[lnumb] <- out.table$p_value [1] 
  out.slope.rr.vec[lnumb] <-  out.table$slope_robust_regr[1]
  out.slope.r.sq.vec[lnumb] <-  out.table$R_squared [1]
  
}

hist(out.p.vec)
hist(out.slope.rr.vec)
hist (out.slope.vec )
hist (out.slope.r.sq.vec)

phytopl.All.Atl.p.ok$rob_reg__slopes <- out.slope.rr.vec

phytopl.All.Atl.p.ok$p_values_olsr_slopes <- out.p.vec

phytopl.All.Atl.p.ok$r.square.olsr <- out.slope.r.sq.vec



phytopl.All.Atl.p.ok$ratio_slopes_rr_olsr <- phytopl.All.Atl.p.ok$rob_reg__slopes / phytopl.All.Atl.p.ok$slope # all OK!cross-checking OK!
ratio_slopes_rr_olsr <- phytopl.All.Atl.p.ok$ratio_slopes_rr_olsr

hist(ratio_slopes_rr_olsr)

summary(ratio_slopes_rr_olsr)

summary(phytopl.All.Atl.p.ok$p_values_olsr_slopes)
summary(phytopl.All.Atl.p.ok$r.square.olsr)


# removal of all rows where r_squared is lower than 50% -----------
dim(phytopl.All.Atl.p.ok) # 494  rows

phytopl.All.Atl.p.rsqok <-  phytopl.All.Atl.p.ok 
phytopl.All.Atl.p.rsqok <-   subset (phytopl.All.Atl.p.ok, r.square.olsr > 0.5)
dim(phytopl.All.Atl.p.rsqok) # 459  rows

# removal of all rows where outlier bias is > 30%  ----------
# (slope ratio olsr/rob. reg.   has to be between  0.77  and 1.3)
phytopl.All.Atl.p.rsq.slope_ratio.ok <-  phytopl.All.Atl.p.rsqok 
phytopl.All.Atl.p.rsq.slope_ratio.ok <-   subset (phytopl.All.Atl.p.rsq.slope_ratio.ok,
                                                  ratio_slopes_rr_olsr < 1.3)
dim(phytopl.All.Atl.p.rsq.slope_ratio.ok) # 458  rows in C units

phytopl.All.Atl.p.rsq.slope_ratio.ok <-   subset (phytopl.All.Atl.p.rsq.slope_ratio.ok,
                                                  ratio_slopes_rr_olsr > 0.77)
dim(phytopl.All.Atl.p.rsq.slope_ratio.ok) # 462  rows in Biovl units

# Phytoplankton: 462 top-quality NBSS size spectra for biovolume (458 top-quality NBSS size spectra in  Carbon units)
# We omitted all linear models (delete rows), where p > 0.05, and
# r_squared is lower than 50% , and one dataset
# outlier bias (slope ratio olsr/rob. reg.   between  0.77  and 1.3)  was above 30%
# up to one outlier per station  (obvious outliers, see supp mat), at up to three station were removed.
# Phytoplankton dataset: one obvious outlier was removed, for  ACEx/Simteco cruise station station 9 (see suppl mat, fig xx)


# check top-quality phytoplankton NBSS (random plots) ---------


phytopl.NBSS.matrix.ok3 <- as.matrix(phytopl.All.Atl.p.rsq.slope_ratio.ok[,37:92])
dim(phytopl.NBSS.matrix.ok3)


lnumb <- round( runif( n = 1 ,min = 1, max = 458), 0)
plot (log10(phytopl.NBSS.matrix.ok3[lnumb,]) ~ X_vectormm3log,
      main = paste( phytopl.All.Atl.p.rsq.slope_ratio.ok$target_organisms[lnumb]  , ", ",  
                    phytopl.All.Atl.p.rsq.slope_ratio.ok$CruiseID[lnumb], ", ",
                    phytopl.All.Atl.p.rsq.slope_ratio.ok$Gear[lnumb]), ylim = c(-20, 15))
abline(lm(log10(phytopl.NBSS.matrix.ok3[lnumb,]) ~ X_vectormm3log), col = "red")
abline(MASS::rlm(log10(phytopl.NBSS.matrix.ok3[lnumb,]) ~ X_vectormm3log), col = "darkgreen")

# Checked, OK!


# save complete File (phytoplankton only) with slopes and indices ---------------

# write.csv(phytopl.All.Atl.p.rsq.slope_ratio.ok, file = "outp_Biovol_phytopl_All_Atl_p_rsq_slope_ratio_ok.csv" )
# outp_Biovol_phytopl_All_Atl_p_rsq_slope_ratio_ok.csv

phytopl.Biovol.All.Atl.ok3 <- read.csv("~/Papers/000 -Paper_Fish_Zoopl_Size_Spectra_Brazil_Africa/outp_Biovol_phytopl_All_Atl_p_rsq_slope_ratio_ok_V6c.csv")

phytopl.Carbon.All.Atl.ok3 <- read.csv("~/Papers/000 -Paper_Fish_Zoopl_Size_Spectra_Brazil_Africa/outp_Carbon_phytopl_All_Atl_p_rsq_slope_ratio_ok.csv")


summary(phytopl.Biovol.All.Atl.ok3)

summary(phytopl.Carbon.All.Atl.ok3)




# analyse top-quality dataset, 458 top-quality NBSS size spectra

summary(phytopl.All.Atl.p.rsq.slope_ratio.ok) 
summary( phytopl.Biovol.All.Atl.ok3)


# n = 462 top-quality NBSS size spectra in Biovolume units

# BIOVOLUME UNITS:
# slope.clean        new_slopes      rob_reg__slopes   p_values_olsr_slopes r.square.olsr   
# Min.   :-1.8344   Min.   :-1.8344   Min.   :-1.8343   Min.   :0.000e+00    Min.   :0.5018  
# 1st Qu.:-1.1861   1st Qu.:-1.1861   1st Qu.:-1.2083   1st Qu.:4.800e-08    1st Qu.:0.7242  
# Median :-1.0717   Median :-1.0717   Median :-1.0808   Median :1.578e-05    Median :0.8139  
# Mean   :-1.0546   Mean   :-1.0546   Mean   :-1.0698   Mean   :1.177e-03    Mean   :0.8018  
# 3rd Qu.:-0.9114   3rd Qu.:-0.9114   3rd Qu.:-0.9222   3rd Qu.:5.406e-04    3rd Qu.:0.8956  
# Max.   :-0.2747   Max.   :-0.2748   Max.   :-0.3081   Max.   :2.566e-02    Max.   :0.9857  
# 
# ratio_slopes_rr_olsr
# Min.   :0.8691      
# 1st Qu.:0.9936      
# Median :1.0037      
# Mean   :1.0183      
# 3rd Qu.:1.0352      
# Max.   :1.2844 


# n = 458 top-quality NBSS size spectra in  Carbon units
# CARBON UNITS:
#          new_slopes      rob_reg__slopes   p_values_olsr_slopes
#       Min.   :-2.1962   Min.   :-2.2017   Min.   :0.000e+00   
#       1st Qu.:-1.3995   1st Qu.:-1.4095   1st Qu.:2.000e-08   
#       Median :-1.2558   Median :-1.2562   Median :4.034e-05   
#       Mean   :-1.2317   Mean   :-1.2390   Mean   :1.873e-03   
#       3rd Qu.:-1.0281   3rd Qu.:-1.0658   3rd Qu.:7.134e-04   
#       Max.   :-0.3708   Max.   :-0.4416   Max.   :3.722e-02

#  r.square.olsr    ratio_slopes_rr_olsr
#  Min.   :0.5010   Min.   :0.8649      
#  1st Qu.:0.7200   1st Qu.:0.9880      
#  Median :0.8210   Median :1.0015      
#  Mean   :0.8038   Mean   :1.0082      
#  3rd Qu.:0.9009   3rd Qu.:1.0261      
#  Max.   :0.9895   Max.   :1.2574 



# Conclusion: 
#       cross-checking phytopl slopes OK! n = 565 useful slope data 
#        n = 458 top-quality NBSS size spectra in carbon units (p <0.05, rsquared > 50%, outlier bias < 30%, only one outlier removed)
#        n = 462 top-quality, reliable NBSS size spectra in Biovolume units





#####
####
# TASK list (1-4) -------------------------
# Next steps: 

#       Next step 1 (OK): use Rob. regression slopes and obtain bootstrap 95% confidence interval of global median and mean slopes ------------------
#       Next step 2 (OK). use Rob. regression slopes and compare C vs Biovol slopes (t-test, Mann-W U test, permutation test)-----------------

#       Next step 3 (OK): use Rob. regression slopes and check wich factors affect these phytopl. NBSS slopes (Biovol and C unit)-------------


#       Next step 3b: median slopes - linear models with forcing  factors, with ans without interaction
#          Most complete: Chlorophyll__mg_m3_insitu (3 data missing) and Temperature_C_insitu_OK (2 data points missing) 
#           #  from  457 COMPLETE (NBSS slopes, in situ temp., in situ chlorophyll) top-quality, reliable NBSS size spectra in Biovolume units
            #  from  462 top-quality, reliable NBSS size spectra in Biovolume units
#           #  with MLD, Phosphates, ect: 111 NAs ...  about 346 complete datasets! 

#       Next step 3c: median slopes - non-linear models (gam) with forcing factors
                   # slope ~ s(SST) + s(MLD) + s(chla) + s(Nitrate) 

# Next step 4 NBSS by key regions ------------------
#        4a slope and intercept by regions, and mean biomass in three  key size ranges by regions
#        Next step 4b: median slopes by regions and T ranges, 
#       Next step 4c: compare and test (perm.test)
#        Phytoplankton are typically classified into three size classes: pico- (<2 µm), nano- (2-20 µm) and micro-phytoplankton (>20 µm) (Sieburth et al., 1978).
#        three size classes: pico- (0.2-2 µm), nano- (2-20 µm), micro-phytoplankton (20-200 micron)
(0.2/1000)^3;(2/1000)^3 ; (20/1000)^3 ;   (200/1000)^3
8e-12
8e-09
8e-06
0.008
#        Biovolume:        three size classes: 
# pico-phytoplankton (8e-12 to 8e-09 mm3),
# nano-phytoplankton (8e-09 to 8e-09 mm3), 
# micro-phytoplankton (8e-09 mm3 to 0.008 mm3)

#        Carbon: (convert from biovolume) 




### TASKS 1-4 (phytoplankton) -----------
## TASKS 1-4 (for phytoplankton NBSS) ---------------------

####
### TASKS 1 & 2 ---------
# '#- TASKS 1 & 2 -    ---------

#       Next step 1: use Rob. regression slopes and obtain bootstrap 95% confidence interval of global median and mean slopes for C and Vol 
#       Task 1---------

#       Next step 2. use Rob. regression slopes and compare C vs Biovol slopes (t-test, Mann-W U test, permutation test)


# T1 a read data ---------------
phytopl.Biovol.All.Atl.ok3 <- read.csv("~/Papers/000 -Paper_Fish_Zoopl_Size_Spectra_Brazil_Africa/outp_Biovol_phytopl_All_Atl_p_rsq_slope_ratio_ok_V6c.csv")

phytopl.Carbon.All.Atl.ok3 <- read.csv("~/Papers/000 -Paper_Fish_Zoopl_Size_Spectra_Brazil_Africa/outp_Carbon_phytopl_All_Atl_p_rsq_slope_ratio_ok.csv")


summary(phytopl.Biovol.All.Atl.ok3)

summary(phytopl.Carbon.All.Atl.ok3)


# T1b slopes in C and biovol  -------
phytoBiovol__slopes_RR <-  phytopl.Biovol.All.Atl.ok3$rob_reg__slopes

phytoCarbon__slopes_RR <-  phytopl.Carbon.All.Atl.ok3$rob_reg__slopes


length(phytoBiovol__slopes_RR) # 462 data

phytoBiovol__slopes_RR_2 <-  sample(phytoBiovol__slopes_RR, 458) # 458 RR slope data,OK

length(phytoCarbon__slopes_RR)# 458 data (4 top-quality data less than biovolume)

phytoCarbon__slopes_RR_2 <-  phytoCarbon__slopes_RR # 458 RR slope data,OK


hist(phytoCarbon__slopes_RR_2)

hist(phytoBiovol__slopes_RR_2)

# shapiro-wilk test for normality

shapiro.test(phytoCarbon__slopes_RR_2)# OK
#p-value = 0.06577, normality OK, not sign. different form normal. dist.

 shapiro.test(phytoBiovol__slopes_RR_2)# OK
# p-value = 0.1364, normality OK, not sign. different form normal. dist.
 
# Bartlett test (homoscedasticity) 
 
df1 <-  data.frame( phytoBiovol__slopes_RR_2 = phytoBiovol__slopes_RR_2,
                    phytoCarbon__slopes_RR_2 = phytoCarbon__slopes_RR_2 )
 
df2<- stack(df1)

head(df2)



bartlett.test(df2$values ~ df2$ind)
#p-value = 0.0003628, variances are not homogeneous!
 

sd(phytoBiovol__slopes_RR_2)
sd(phytoCarbon__slopes_RR_2)


df3 <- df2

names(df3) <- c(  "NBSS RR slope" , " Unit") 

df3$NBSS_unit_long <- df2$ind

# 
# df3$Unit[1:458]  <- replace (df3$Unit, 1:458, "Biovol" ) 
# df3$Unit[459: length(df3$Unit) ]  <- replace (df3$Unit, (459: length(df3$Unit)), "Carbon" ) 

# View (df3)

df3$NBSS_unit_long <- as.factor (df3$NBSS_unit_long) 

boxplot ( df2$values ~ df2$ind, ylab = "Robust regression NBSS slopes")


# t-test -------------

t.test(phytoCarbon__slopes_RR_2,phytoBiovol__slopes_RR_2 )
#  p-value < 2.2e-16
# highly signifficant difference


# Mann-Whitney U test

 wilcox.test(phytoCarbon__slopes_RR_2,phytoBiovol__slopes_RR_2 )
 #  p-value < 2.2e-16
 # highly signifficant difference !!!
 
 
# PERMANOVA ---------------

 
 library(coin)
 
 # ?independence_test()
 

 independence_test( df2$values ~ df2$ind )
 
 # p-value < 2.2e-16 
 # highly signifficant difference !!!
 

 # T 1c BOOTSTRAP 95% CI ----------------------------
 
 
 ##  Bootstrap the mean of a LARGE sample. 
 #         Large sample:  n = 30
 
 # Biovolume
 test.sam <- phytoBiovol__slopes_RR_2
 length(test.sam) #458
 ## [1] 30
 media.teste <-median(test.sam) #mean
 media.teste
 ## [1] 3.0036
 median(test.sam) #median=3
 ## [1] 3.162
 
 # Calulate the Parametric 95% Conf.int., using the "t" distribution
 
 # begin by calculating the prametric standard error of the mean
 n = length(test.sam)
 St.Error  = sd(test.sam)/sqrt(n)
 St.Error    # Standard error of the mean = 0.342
 ## [1] 0.3418033
 # Calulate the Parametric 95% Conf.int. using the "t" distribution and the St. Error 
 # mean - t(0.95,df) *St.Error ; mean + t(0.95,df) *St.Error
 qt(0.975,29) # 2.05  , VALUE OF THE "t" distrib., approx. 2
 ## [1] 2.04523
 # mean -  2*St.Error , mean +  2 *St.Error
 mean(test.sam) -  2*St.Error ; mean(test.sam) +  2 *St.Error
 ## [1] 2.319993
 ## [1] 3.687207
 # Param 95% Conf.int. (distrib.: t): 2.32 < mean < 3.69
 
 # NOW BOOTSRAP
 # Bootstrap st.error and 95% Conf.int
 # Principle: sampling with replacement (replace = TRUE), 
 # permits repetition of the same values
 
 # sample(test.sam,  size =n, replace = TRUE)
 
 
 Boot.runs = 10000 # number of Bootstrap runs
 n = length(test.sam) # sample size (300)
 
 
 boot.samples = matrix(sample(test.sam, size = Boot.runs * n, 
                              replace = TRUE),
                       Boot.runs, n)
 # generates a matrix with 10000 lines and 3000 columns (samples)
 
 dim(boot.samples) # number of rows and columns
 ## [1] 10000    30
 #View(as.data.frame(boot.samples))
 
 boot.statistics = apply(boot.samples, 1, median)
 # calculates the mean for each line (i.e., for each sampe)
 # gives a vector with  10000 means
 
 median(boot.statistics) # overall median of medians: 3.0,
 ## [1] 3.0041
 hist(boot.statistics) # histogram
 
 

 # Non-parametr. ordinary Bootstrap (no assumptions, uses quantiles):
 quantile(boot.statistics, c(0.025, 0.975)) # correct 95% quantiles OK!
 ##     2.5%    97.5% 
#   -1.106374 -1.060654
 
 
# Carbon 
 
 test.sam <- phytoCarbon__slopes_RR_2
 length(test.sam) #458
 ## [1] 30
 media.teste <-median(test.sam) #mean
 media.teste
 ## [1] 3.0036
 median(test.sam) #median=3
 ## [1] 3.162
 
 # Calulate the Parametric 95% Conf.int., using the "t" distribution
 
 # begin by calculating the prametric standard error of the mean
 n = length(test.sam)
 St.Error  = sd(test.sam)/sqrt(n)
 St.Error    # Standard error of the mean = 0.342
 ## [1] 0.3418033
 # Calulate the Parametric 95% Conf.int. using the "t" distribution and the St. Error 
 # mean - t(0.95,df) *St.Error ; mean + t(0.95,df) *St.Error
 qt(0.975,29) # 2.05  , VALUE OF THE "t" distrib., approx. 2
 ## [1] 2.04523
 # mean -  2*St.Error , mean +  2 *St.Error
 mean(test.sam) -  2*St.Error ; mean(test.sam) +  2 *St.Error
 ## [1] 2.319993
 ## [1] 3.687207
 # Param 95% Conf.int. (distrib.: t): 2.32 < mean < 3.69
 
 # NOW BOOTSRAP
 # Bootstrap st.error and 95% Conf.int
 # Principle: sampling with replacement (replace = TRUE), 
 # permits repetition of the same values
 
 # sample(test.sam,  size =n, replace = TRUE)
 
 
 Boot.runs = 10000 # number of Bootstrap runs
 n = length(test.sam) # sample size (300)
 
 
 boot.samples = matrix(sample(test.sam, size = Boot.runs * n, 
                              replace = TRUE),
                       Boot.runs, n)
 # generates a matrix with 10000 lines and 3000 columns (samples)
 
 dim(boot.samples) # number of rows and columns
 ## [1] 10000    30
 #View(as.data.frame(boot.samples))
 
 boot.statistics = apply(boot.samples, 1, median)
 # calculates the mean for each line (i.e., for each sampe)
 # gives a vector with  10000 means
 
 median(boot.statistics) # overall median of medians: 3.0,
 ## [1] 3.0041
 hist(boot.statistics) # histogram
 
 
 
 # Non-parametr. ordinary Bootstrap (no assumptions, uses quantiles):
 quantile(boot.statistics, c(0.025, 0.975)) # correct 95% quantiles OK!
 ##     2.5%    97.5% 
 # -1.277629 -1.227709 
 
 
 
 # T2a Plots All data Carbon vs Biovolume ---------------
 
 # PLot ALL NBSS Data, Biovolume -------
 
 
 
 
 # plot all points and rlm moldes (blue lines), and red median line  ----------
 
 lnumb <- 34
 plot (log10( phytopl.NBSS.matrix.ok3[lnumb,]) ~ X_vectormm3log, main = "ATLANTIC, Biovolume units",
       xlab = "log10( Biovolume (mm3 ind. -1)) ",
       ylab = "log10(Normalized Biovolume (mm3 m-3 mm-3) ",
       xlim = c(-11, -1), ylim = c(0, 14) ,
       pch = 16, col = "white")
 for (lnumb in 1 : nrow(phytopl.NBSS.matrix.ok3) ) 
 {    points (log10( phytopl.NBSS.matrix.ok3[lnumb,]) ~ X_vectormm3log ,
              pch = 16, col = alpha("darkgreen", 0.3))
   abline( rlm(log10( phytopl.NBSS.matrix.ok3[lnumb,]) ~ X_vectormm3log), col =alpha ("navy", 0.3))
 }
 
 
 
 # Biovolume  (462 useful Data)
 (median(phytopl.All.Atl.p.rsq.slope_ratio.ok$rob_reg__slopes) )
 # median RR slope : -1.080821
 (min(phytopl.All.Atl.p.rsq.slope_ratio.ok$rob_reg__slopes)) 
 (max(phytopl.All.Atl.p.rsq.slope_ratio.ok$rob_reg__slopes) )
 # min - max  RR slope : -1.279371
 # -1.834 to -0.308
 
 
 
 # matrix to to vector (transpose), TSWA -----------------------  
 x1 <- rep( X_vectormm3log, nrow(phytopl.NBSS.matrix.ok3) )
 y1 <- c ( t(phytopl.NBSS.matrix.ok3))
 
 # regression line (Rob. regr.)
 #plot(log10(y1) ~ x1, xlim = c(-11, -1), ylim = c(0, 14) )
 #points(  log10(y1) ~ x1 )
 # abline(rlm (log10(y1) ~ x1, xlim = c(-11, -1), ylim = c(0, 14)))
 abline(rlm (log10(y1) ~ x1, xlim = c(-11, -1), ylim = c(0, 14)), 
        lwd = 2.5, lty = 2, col = "darkorange" )
 
 # means and medians without considering NAs  
 mean (c(4,NA, 18,34)) # NA
 median( c(4,NA, 18, 34)  ) #NA
 
 mean_   <- function(...) mean(..., na.rm=T)
 median_ <- function(...) median(..., na.rm=T)
 
 mean_ (c(4,NA, 8, 34)) #15.33, OK
 median_ (c(4,NA, 8,34)) #8, OK
 
 # apply median by columns
 
 median_vec_ALLBiovol <- apply( phytopl.NBSS.matrix.ok3, 2, median_) 
 mean_vec_ALLBiovol <- apply(phytopl.NBSS.matrix.ok3, 2, mean_) 
 
 lines(  log10(median_vec_ALLBiovol) ~ X_vectormm3log,pch = 16, col = "red" , lwd = 2.5)
 
 
 
 
 # PLot ALL NBSS Data, Carbon -----------
 
 
 
 
 # Boxplot Carbon vs Biovolume NBSS slopes
 
 
 
 
 
 
 
 ######
 #### 
 ###### '####- TASK 3 -   ---------
 
 #       Next step 3: use Rob. regression slopes and check wich factors affect these phytopl. NBSS slopes (Biovol and C unit)
 
 #       Next step 3a: median slpes by regions and T ranges, compare and test (perm.test)
 
 #       Next step 3b: median slopes - linear models with forcing  factors, with ans without interaction
 #          Most complete: Chlorophyll__mg_m3_insitu (3 data missing) and Temperature_C_insitu_OK (2 data points missing) 
 #           #  from  457 COMPLETE (NBSS slopes, in situ temp., in situ chlorophyll) top-quality, reliable NBSS size spectra in Biovolume units
 #  from  462 top-quality, reliable NBSS size spectra in Biovolume units
 #           #  with MLD, Phosphates, ect: 111 NAs ...  about 346 complete datasets! 
 
 #       Next step 3c: median slopes - non-linear models (gam) with forcing factors
 # slope ~ s(SST) + s(MLD) + s(chla) + s(Nitrate) 
 
 
 
 

# I.first analyses (not by temperature classes)-----------------------

## SST -------------------------------------------------------------------------

plot(Data.All.Atl$SST ~ Data.All.Atl$SST_C_insitu)
# delete zeros
Data.All.Atl$SST[Data.All.Atl$SST == 0] <- NA
Data.All.Atl$SST[Data.All.Atl$SST_C_insitu == 0] <- NA

plot(Data.All.Atl$SST ~ Data.All.Atl$SST_C_insitu)

Data.All.Atl$SST_C_insitu[Data.All.Atl$SST_C_insitu > 1000] <- NA

Data.All.Atl$SST[Data.All.Atl$SST < 5] <- NA

# Ordinary least squares regression (OLSR) -----------------------------------
plot(Data.All.Atl$SST ~ Data.All.Atl$SST_C_insitu)

plot(Data.All.Atl$SST ~ Data.All.Atl$SST_C_insitu, 
     col = alpha( "dodgerblue", 0.5) , pch = 16, cex = 0.8)  


lm.sst <- lm(Data.All.Atl$SST ~ Data.All.Atl$SST_C_insitu)
summary(lm.sst)
abline(lm.sst, col = "red", lwd = 1.8)
summary(Data.All.Atl$SST)
summary(Data.All.Atl$SST_C_insitu)

# Robust regression -------------------------------------------------
library(MASS)
lm.sst.rob <- rlm(Data.All.Atl$SST ~ Data.All.Atl$SST_C_insitu)
summary(lm.sst.rob)
abline(lm.sst.rob, col = "blue", lwd = 1.8)

# best: robust regression ("rlm")! less influenced by outliers!

# # Quantile regression ---------------------------------------------------
# # Quantile regression makes no assumptions about the distribution of the underlying data, and is robust to outliers in the dependent variable.
# # install.packages("quantreg")     # may be needed if not already installed
# library("quantreg")                # load the required package
# fm.rq <- rq(SST ~ SST_C_insitu, data=Data.All.Atl)
# abline(fm.rq, col= "green", lwd = 1.8)
# summary(fm.rq, se = "boot", bsmethod= "xy")
# summary(fm.rq, se = "boot", bsmethod= "wild")

# 
# # Kendall–Theil Sen Siegel nonparametric linear regression
# # source: https://rcompanion.org/handbook/F_12.html
# # Kendall–Theil regression is a completely nonparametric approach to linear regression where there is one independent and one dependent variable.  It is robust to outliers in the dependent variable.  It simply computes all the lines between each pair of points, and uses the median of the slopes of these lines.  This method is sometimes called Theil–Sen.  A modified, and preferred, method is named after Siegel.
# 
# # The method yields a slope and intercept for the fit line, and a p-value for the slope can be determined as well.  Efron’s pseudo r-squared can be determined from the residual and predicted values.
# #The mblm function in the mblm package uses the Siegel method by default.  The Theil–Sen procedure can be chosen with the repeated=FALSE option. See library(mblm); ?mblm for more details.
# 
# 
# library(mblm)
# 
# model.k = mblm(Data.All.Atl$SST ~ Data.All.Atl$SST_C_insitu)
# abline(model.k, col = "purple", lwd = 2)
# 
# summary(Data.All.Atl$SST,Data.All.Atl$SST_C_insitu )
# 
# # resulting line of mblm method is identical  to rlm ("robust  regression"), in this example


## Chl a -----------------------------------------------------------------------
 
# delete zeros
Data.All.Atl$chla[Data.All.Atl$chla == 0] <- NA
Data.All.Atl$Chlorophyll__mg_m3_insitu[Data.All.Atl$Chlorophyll__mg_m3_insitu == 0] <- NA


plot(Data.All.Atl$chla ~ Data.All.Atl$Chlorophyll__mg_m3_insitu)

Data.All.Atl$chla[Data.All.Atl$chla > 5] <- NA

plot(Data.All.Atl$chla ~ Data.All.Atl$Chlorophyll__mg_m3_insitu)

plot(log10(Data.All.Atl$chla) ~ log10(Data.All.Atl$Chlorophyll__mg_m3_insitu))


## Salinty -----------------------------------------------------------------------

# delete zeros
Data.All.Atl$salinity[Data.All.Atl$salinity == 0] <- NA # remote sensing
Data.All.Atl$Salinity_insitu[Data.All.Atl$Salinity_insitu == 0] <- NA # in situ

plot(Data.All.Atl$Salinity_insitu ~ Data.All.Atl$salinity, 
     main = "Salinty, in situ v remote sensing")


plot(Data.All.Atl$SST ~ Data.All.Atl$SST_C_insitu, 
     main = "SST, in situ v remote sensing")



# # Quantile regression ---------------------------------------------------
# # install.packages("quantreg")     # may be needed if not already installed
# library("quantreg")                # load the required package
# fm.rq <- rq(SST ~ SST_C_insitu, data=Data.All.Atl)
# summary(fm.rq)
# abline(fm.rq, col= "green")

library(scales)

# in situ chl a vs Salinty
plot(Data.All.Atl$Chlorophyll__mg_m3_insitu ~ Data.All.Atl$Salinity_insitu , 
     main = "Salinty, in situ v remote sensing",
     xlim = c(31, 38),  col = alpha("blue", 0.4))

symbols(Data.All.Atl$Chlorophyll__mg_m3_insitu ~ Data.All.Atl$Salinity_insitu, main = "circles = SST",
         circles = (Data.All.Atl$Temperature_C_insitu), fg= alpha("blue", 0.4), inches=0.2, xlim = c(31,38))


symbols(Data.All.Atl$Temperature_C_insitu ~ Data.All.Atl$Salinity_insitu, main = "circles = Chla",
        circles = (Data.All.Atl$Chlorophyll__mg_m3_insitu), fg= alpha("blue", 0.4), inches=0.2, xlim = c(31,38))

# nice plots ---------------------------- 

# T-S diagram with circle size = Chla  ------------------
symbols(Data.All.Atl$SST_C_insitu ~ Data.All.Atl$Salinity_insitu, main = "circles = Chla, in situ", 
        circles = (Data.All.Atl$Chlorophyll__mg_m3_insitu), fg= alpha("navyblue", 0.6), inches=0.2,
        xlim = c(31,38),
        ylim = c(0, 32))
 # rect(31.5, 10, 38, 27)
 # rect(36.9, 27, 38, 32)
 # rect(31.5, 3, 38, 10)


# limit at 20 degress Celsius
rect(36, 27.8, 38, 32) # TW, oligotrophic tropical water (high salinity high temp, low nutr, low chl a)
rect(34.5, 27.8, 36, 32) # Warm Coastal waters, warm, nutrient-rich waters (e.g., productive coastal waters in summer") 
rect(34.5, 20, 38, 27.8)  # mixed warm waters 
rect(31.5, 3, 34, 20) # upwelling, extremely productive, cold waters, temp < 20 deg. celsius, salinity < 34, upwelling
rect(34, 3, 36, 20) # productive, cold waters, temp < 20 deg. celsius, salinity 34 to 36, upwelling
rect(36, 3, 37, 20) # less productive cold waters, salinity > 36

# temperatures for classes: 3 to 20, 20 to 27.8, < 27.8 (27.8 to 31)  ----------

summary(SST_C_insitu)
hist(Data.All.Atl$SST_C_insitu )
abline (v = c(3,20,27.8, 31), col = "red")

hist(Data.All.Atl$Chlorophyll__mg_m3_insitu)

hist(log10(Data.All.Atl$Chlorophyll__mg_m3_insitu))

 
 # T-S diagram with circle size = nitrate  --------------------
symbols(Data.All.Atl$SST_C_insitu ~ Data.All.Atl$Salinity_insitu, main = "circles = nitrate, in situ", 
        circles = (Data.All.Atl$Nitrate__uM_L1_insitu), fg= alpha("navyblue", 0.6), inches=0.2,
        xlim = c(31,38),
        ylim = c(0, 32))
# rect(31.5, 10, 38, 27)
# rect(36.9, 27, 38, 32)
# rect(31.5, 3, 38, 10)

# Analysis by water masses
# OligoTrophic TW
# TW: Salinity_insitu = 36.44 - 37.55 (max.) ; Temperature_C_insitu : 27.8 - 36.16 (max.)


summary(Salinity_insitu)
summary(Temperature_C_insitu)


# limit at 20 degress Celsius
rect(36, 27.8, 38, 32) # TW, oligotrophic tropical water (high salinity high temp, low nutr, low chl a)
rect(34.5, 27.8, 36, 32) # Warm Coastal waters, warm, nutrient-rich waters (e.g., productive coastal waters in summer") 
rect(34.5, 20, 38, 27.8)  # mixed warm waters 
rect(31.5, 3, 34, 20) # upwelling, extremely productive, cold waters, temp < 20 deg. celsius, salinity < 34, upwelling
rect(34, 3, 36, 20) # productive, cold waters, temp < 20 deg. celsius, salinity 34 to 36, upwelling
rect(36, 3, 37, 20) # less productive cold waters, salinity > 36



plot(Data.All.Atl$Salinity_insitu , Data.All.Atl$Nitrate__uM_L1_insitu, xlim = c(30, 39), ylim = c(0,30),
     main = "Nitrate, in situ")
abline (v = 36.9)
text (38, 25, " sal = 36.9")

plot(Data.All.Atl$Temperature_C_insitu , Data.All.Atl$Nitrate__uM_L1_insitu, xlim = c(0, 35), ylim = c(0,30),
     main = "Nitrate, in situ")
abline (v = 27)
text (32, 25, " temp = 27")

# T-S diagram with circle size = silicate  -------------
symbols(Data.All.Atl$SST_C_insitu ~ Data.All.Atl$Salinity_insitu, main = "circles = silicate", 
        circles = (Data.All.Atl$silicate), fg= alpha("navyblue", 0.6), inches=0.2,
        xlim = c(31,38),
        ylim = c(0, 32))
# rect(31.5, 20, 38, 27) 
# rect(36.9, 27, 38, 32)
# rect(31.5, 3, 38, 20)



# limit at 20 degress Celsius
rect(36, 27.8, 38, 32) # TW, oligotrophic tropical water (high salinity high temp, low nutr, low chl a)
rect(34.5, 27.8, 36, 32) # Warm Coastal waters, warm, nutrient-rich waters (e.g., productive coastal waters in summer") 
rect(34.5, 20, 38, 27.8)  # mixed warm waters 
rect(31.5, 3, 34, 20) # upwelling, extremely productive, cold waters, temp < 20 deg. celsius, salinity < 34, upwelling
rect(34, 3, 36, 20) # productive, cold waters, temp < 20 deg. celsius, salinity 34 to 36, upwelling
rect(36, 3, 37, 20) # less productive cold waters, salinity > 36


# T-S diagram with circle size = productivity index (logChla*nitrate*siliocate)  -------------
symbols(Data.All.Atl$SST_C_insitu ~ Data.All.Atl$Salinity_insitu, main = "circles = Prod.-Index", 
        circles = ((Data.All.Atl$prod.index2)), fg= alpha("navyblue", 0.6), inches=0.3,
        xlim = c(31,38),
        ylim = c(0, 32))
# rect(31.5, 20, 38, 27) 
# rect(36.9, 27, 38, 32)
# rect(31.5, 3, 38, 20)
# limit at 20 degress Celsius
rect(36, 27.8, 38, 32) # TW, oligotrophic tropical water (high salinity high temp, low nutr, low chl a)
rect(34.5, 27.8, 36, 32) # Warm Coastal waters, warm, nutrient-rich waters (e.g., productive coastal waters in summer") 
rect(34.5, 20, 38, 27.8)  # mixed warm waters 
rect(31.5, 3, 34, 20) # upwelling, extremely productive, cold waters, temp < 20 deg. celsius, salinity < 34, upwelling
rect(34, 3, 36, 20) # productive, cold waters, temp < 20 deg. celsius, salinity 34 to 36, upwelling
rect(36, 3, 37, 20) # less productive cold waters, salinity > 36





# maps --------------

plot (Data.All.Atl$Longitude ~ Data.All.Atl$Latitude)

#USING MAPS package (simple)
library(maps)
map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-70, 60), xlim=c(-95, 30), mar=c(0,0,0,0))
points( Data.All.Atl$Longitude,Data.All.Atl$Latitude,   col="red", pch=16)

map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-70, 60), xlim=c(-95, 30), mar=c(0,0,0,0))
#points( Data.All.Atl$Longitude,Data.All.Atl$Latitude,   col="red", pch=16)
symbols(Data.All.Atl$Latitude~ Data.All.Atl$Longitude, 
        circles = (sqrt(Data.All.Atl$prod.index4_Chl_nitr)), fg= alpha("red", 0.6), inches=0.3,
        add = TRUE)


par(opar)


# 5 Subsets by communities ------------------------------------------------------

summary(Data.All.Atl$target_organisms)

phytopl.All.Atl <- subset(Data.All.Atl, target_organisms == "phytoplankton" )

zoopl.All.Atl <- subset(Data.All.Atl, target_organisms == "zooplankton" )
dim(zoopl.All.Atl)
summary(zoopl.All.Atl$Gear)


mesop_fish.All.Atl <-  subset(Data.All.Atl, target_organisms == "mesopelagic fish" )



# maps --------------

plot (Data.All.Atl$Longitude ~ Data.All.Atl$Latitude)

#USING MAPS package (simple)
library(maps)
map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-70, 60), xlim=c(-95, 30), mar=c(0,0,0,0))
points( Data.All.Atl$Longitude,Data.All.Atl$Latitude,   col="red", pch=16)

map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-70, 60), xlim=c(-95, 30), mar=c(0,0,0,0))
#points( Data.All.Atl$Longitude,Data.All.Atl$Latitude,   col="red", pch=16)
symbols(Data.All.Atl$Latitude~ Data.All.Atl$Longitude, 
        circles = (sqrt(Data.All.Atl$prod.index4_Chl_nitr)), fg= alpha("red", 0.6), inches=0.3,
        add = TRUE)


par(opar)

# maps by taxonomic groups/communities -------

# Phytopl 
#USING MAPS package (simple)
library(maps)
map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-70, 60), xlim=c(-95, 30), mar=c(0,0,0,0))
points( phytopl.All.Atl$Longitude,phytopl.All.Atl$Latitude,   col="darkgreen", pch=16)

# Zoopl 
#USING MAPS package (simple)
library(maps)
map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-70, 60), xlim=c(-95, 30), mar=c(0,0,0,0))
points( zoopl.All.Atl$Longitude,zoopl.All.Atl$Latitude,   col="navy", pch=16)

# mesopelagic fish 
#USING MAPS package (simple)
library(maps)
map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-70, 60), xlim=c(-95, 30), mar=c(0,0,0,0))
points( mesop_fish.All.Atl$Longitude,mesop_fish.All.Atl$Latitude,   col="darkorange", pch=16)

# All communities together

library(maps)
library (scales)
map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-70, 60), xlim=c(-95, 30), mar=c(0,0,0,0))
points( phytopl.All.Atl$Longitude,phytopl.All.Atl$Latitude,   col= (alpha ("darkgreen", 0.4)), pch=16, cex = 0.5)
points( zoopl.All.Atl$Longitude,zoopl.All.Atl$Latitude,   col= (alpha ("navy", 0.4)) , pch=16,  cex = 0.5)
points( mesop_fish.All.Atl$Longitude,mesop_fish.All.Atl$Latitude,   col= (alpha ("darkorange", 0.9)), pch=16,  cex = 0.7)




# T-S diagram with circle size = NBSS slope -------------------
  
symbols(Data.All.Atl$SST_C_insitu ~ Data.All.Atl$Salinity_insitu, main = "circles = NBSS slope", 
        circles = (Data.All.Atl$slope.clean * -1), fg= alpha("navyblue", 0.6), inches=0.09,
        xlim = c(31,38),
        ylim = c(0, 32))

summary(Data.All.Atl$slope)
summary(Data.All.Atl$slope.clean)



# limit at 20 degress Celsius
rect(36, 27.8, 38, 32) # TW, oligotrophic tropical water (high salinity high temp, low nutr, low chl a)
rect(34.5, 27.8, 36, 32) # Warm Coastal waters, warm, nutrient-rich waters (e.g., productive coastal waters in summer") 
rect(34.5, 20, 38, 27.8)  # mixed warm waters 
rect(31.5, 3, 34, 20) # upwelling, extremely productive, cold waters, temp < 20 deg. celsius, salinity < 34, upwelling
rect(34, 3, 36, 20) # productive, cold waters, temp < 20 deg. celsius, salinity 34 to 36, upwelling
rect(36, 3, 37, 20) # less productive cold waters, salinity > 36



# remote sensing vs in situ TS diagrams -------------------------

# in situ SST a vs Salinty
plot(Data.All.Atl$SST_C_insitu ~ Data.All.Atl$Salinity_insitu , 
     main = " SST vs Salinty, in situ data",
     xlim = c(31, 38))

# Remote sensing  SST vs Salinty
plot(Data.All.Atl$SST ~ Data.All.Atl$salinity , 
     main = " SST vs Salinty, Remote sensing ",
     xlim = c(31, 38))


# 5.1 phyoplankton NBSS slopes-------------------------------------------

# Zoopl. T-S diagram with circle size = NBSS slope -------------------

symbols(zoopl.All.Atl$SST_C_insitu ~ zoopl.All.Atl$Salinity_insitu, main = "circles = NBSS slope, zoopl.", 
        circles = (zoopl.All.Atl$slope.clean * -1), fg= alpha("blue", 0.6), inches=0.09,
        xlim = c(31,38),
        ylim = c(0, 32))



# limit at 20 degress Celsius
rect(36, 27.8, 38, 32) # TW, oligotrophic tropical water (high salinity high temp, low nutr, low chl a)
rect(34.5, 27.8, 36, 32) # Warm Coastal waters, warm, nutrient-rich waters (e.g., productive coastal waters in summer") 
rect(34.5, 20, 38, 27.8)  # mixed warm waters 
rect(31.5, 3, 34, 20) # upwelling, extremely productive, cold waters, temp < 20 deg. celsius, salinity < 34, upwelling
rect(34, 3, 36, 20) # productive, cold waters, temp < 20 deg. celsius, salinity 34 to 36, upwelling
rect(36, 3, 37, 20) # less productive cold waters, salinity > 36


# Phytopl. T-S diagram with circle size = NBSS slope -------------------

symbols(phytopl.All.Atl$SST_C_insitu ~ phytopl.All.Atl$Salinity_insitu, main = "circles = NBSS slope, phytopl.", 
        circles = (phytopl.All.Atl$slope.clean * -1), fg= alpha("darkgreen", 0.6), inches=0.09,
        xlim = c(31,38),
        ylim = c(0, 32))

# limit at 20 degress Celsius
rect(36, 27.8, 38, 32) # TW, oligotrophic tropical water (high salinity high temp, low nutr, low chl a)
rect(34.5, 27.8, 36, 32) # Warm Coastal waters, warm, nutrient-rich waters (e.g., productive coastal waters in summer") 
rect(34.5, 20, 38, 27.8)  # mixed warm waters 
rect(31.5, 3, 34, 20) # upwelling, extremely productive, cold waters, temp < 20 deg. celsius, salinity < 34, upwelling
rect(34, 3, 36, 20) # productive, cold waters, temp < 20 deg. celsius, salinity 34 to 36, upwelling
rect(36, 3, 37, 20) # less productive cold waters, salinity > 36

# NBSS slopes vs productivity index -------------------------- 

par(opar)

plot(phytopl.All.Atl$slope.clean ~ log10(phytopl.All.Atl$prod.index1), col = alpha("darkgreen", alpha = 0.6))
plot(phytopl.All.Atl$slope.clean ~ log10(phytopl.All.Atl$prod.index2), col = alpha("darkgreen", alpha = 0.6))

lm1 <- lm(phytopl.All.Atl$slope.clean ~ log10(phytopl.All.Atl$prod.index2))
summary(lm1)

plot(phytopl.All.Atl$slope.clean ~ (phytopl.All.Atl$Temperature_C_insitu))
lm2 <- lm(phytopl.All.Atl$slope.clean ~ (phytopl.All.Atl$Temperature_C_insitu))
summary(lm2)


lm4 <- lm(phytopl.All.Atl$slope.clean ~ log10(phytopl.All.Atl$prod.index3_2Chl_nitr) * phytopl.All.Atl$Temperature_C_insitu)
summary(lm4)


lm5 <- lm(phytopl.All.Atl$slope.clean ~ 
            log10(phytopl.All.Atl$prod.index4_Chl_nitr) *
            phytopl.All.Atl$Temperature_C_insitu)
summary(lm5)
AIC(lm5)

# Multiple R-squared:  0.3511,	Adjusted R-squared:  0.3458


# Temperature
# Interaction term is significant (p = 0.022) ! 
# But productivity index 3 (Chl and Nitrate) is not...
# 367 degrees of freedom

# With interaction
# Interaction term is significant! But productivity index is not...
# better: without interaction

# linear model: 

lm3 <- lm(phytopl.All.Atl$slope.clean ~ log10(phytopl.All.Atl$prod.index2) + phytopl.All.Atl$Temperature_C_insitu)
summary(lm3)
AIC(lm3)


# linear models with high-quality phytoplankt slopes ---------------
  
# lm24: 


# temp + log10 (1+chlorophyll a)
lm24 <- lm(phytopl.All.Atl.p.rsq.slope_ratio.ok$rob_reg__slopes ~ log10(1+phytopl.All.Atl.p.rsq.slope_ratio.ok$Chlorophyll__mg_m3_insitu) * phytopl.Biovol.All.Atl.ok3$Temperature_C_insitu_OK)
summary(lm24)
AIC(lm24) # -328.545
# best high-df model? Adjusted R-squared: 0.44 , 452 degrees of freedom
BIC(lm24)

# temp + log10 (1+chlorophyll a)
lm25 <- lm(phytopl.All.Atl.p.rsq.slope_ratio.ok$rob_reg__slopes ~ (phytopl.All.Atl.p.rsq.slope_ratio.ok$MLD_Mathilde) * phytopl.Biovol.All.Atl.ok3$Temperature_C_insitu_OK)
summary(lm25)
AIC(lm25) # -277.1254
# best low-df model? Adjusted R-squared: 0.21 , only 345 degrees of freedom



# temp +  (chlorophyll a from model)
lm26 <- lm(phytopl.All.Atl.p.rsq.slope_ratio.ok$rob_reg__slopes ~ 
             (phytopl.All.Atl.p.rsq.slope_ratio.ok$chla_model) * 
             phytopl.Biovol.All.Atl.ok3$Temperature_C_insitu_OK)
summary(lm26)
AIC(lm26) # -277.1254
# BEST high-df model? Adjusted R-squared: 0.44 , 456 degrees of freedom

# sepwise selction (MASS) withhigh-quality slopes -------

# View(phytopl.All.Atl.p.rsq.slope_ratio.ok)
names(phytopl.All.Atl.p.rsq.slope_ratio.ok)


phytopl.All.Atl.for.lmMASS4 <- phytopl.All.Atl.p.rsq.slope_ratio.ok[, c(27, 34:35,98:107,  122 )  ]
  
names(phytopl.All.Atl.for.lmMASS4)



full.model <- lm(rob_reg__slopes ~., data = phytopl.All.Atl.for.lmMASS4)

summary(full.model)



# Stepwise regression model

step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)


### For PAPER ----------

#1a.   THE BEST Best univarate model,log10(1+ Chl a in situ ) ----------
lmlogChla_insitu <- lm(phytopl.All.Atl.p.rsq.slope_ratio.ok$rob_reg__slopes ~ 
                      log10(1+phytopl.All.Atl.p.rsq.slope_ratio.ok$Chlorophyll__mg_m3_insitu) )
summary(lmChla_insitu) # R-squared:  0.36, 456 degrees of freedom, p-value: < 2.2e-16

plot(phytopl.All.Atl.p.rsq.slope_ratio.ok$rob_reg__slopes ~ 
       log10(1+ phytopl.All.Atl.p.rsq.slope_ratio.ok$Chlorophyll__mg_m3_insitu), 
     col = alpha ("darkgreen", 0.3), pch = 16)
abline(lmlogChla_insitu, lwd = 2.5, lty = 2, col = "darkorange")



#1b. second Best univarate model, SST  model ----------

lmSST_model <- lm(phytopl.All.Atl.p.rsq.slope_ratio.ok$rob_reg__slopes ~ 
                    phytopl.All.Atl.p.rsq.slope_ratio.ok$SST_model)
summary(lmSST_model) # R-squared:  0.349, 460 degrees of freedom, p-value: < 2.2e-16

plot(phytopl.All.Atl.p.rsq.slope_ratio.ok$rob_reg__slopes ~ 
       phytopl.All.Atl.p.rsq.slope_ratio.ok$SST_model, 
     col = alpha ("darkgreen", 0.3), pch = 16)
abline(lmSST_model)


#1c.  second best univarate model, SST in situ----------
lmSST_insitu <- lm(phytopl.All.Atl.p.rsq.slope_ratio.ok$rob_reg__slopes ~ 
                     phytopl.All.Atl.p.rsq.slope_ratio.ok$SST_model)
summary(lmSST_insitu) # R-squared:  0.347, 421 degrees of freedom, p-value: < 2.2e-16

plot(phytopl.All.Atl.p.rsq.slope_ratio.ok$rob_reg__slopes ~ 
       phytopl.All.Atl.p.rsq.slope_ratio.ok$SST_C_insitu, 
     col = alpha ("darkgreen", 0.3), pch = 16)
abline(lmSST_insitu)


#1d.  3rd best univarate model, Temp  in situ at depth----------
lmTemp_insitu <- lm(phytopl.All.Atl.p.rsq.slope_ratio.ok$rob_reg__slopes ~ 
                      phytopl.All.Atl.p.rsq.slope_ratio.ok$Temperature_C_insitu)
summary(lmTemp_insitu) # R-squared:  0.26, 421 degrees of freedom, p-value: < 2.2e-16

plot(phytopl.All.Atl.p.rsq.slope_ratio.ok$rob_reg__slopes ~ 
       phytopl.All.Atl.p.rsq.slope_ratio.ok$Temperature_C_insitu, 
     col = alpha ("darkgreen", 0.3), pch = 16)
abline(lmTemp_insitu, lwd = 2.5, lty = 2, col = "darkorange")



#2.   THE 3rd  worst univarate model,salinity) ----------
lm.sal <- lm(phytopl.All.Atl.p.rsq.slope_ratio.ok$rob_reg__slopes ~ 
                (phytopl.All.Atl.p.rsq.slope_ratio.ok$salinity) )
summary(lm.sal) # R-squared:  0.36, 456 degrees of freedom, p-value: < 2.2e-16



#3.   THE second worst univarate model,log10(1+ Chl a in situ ) ----------
lm.nitr <- lm(phytopl.All.Atl.p.rsq.slope_ratio.ok$rob_reg__slopes ~ 
                         (phytopl.All.Atl.p.rsq.slope_ratio.ok$nitrate_model) )
summary(lm.nitr) # R-squared:  0.36, 456 degrees of freedom, p-value: < 2.2e-16



#4.   THE worst univarate model,log10(1+ Chl a in situ ) ----------
lm.mld <- lm(phytopl.All.Atl.p.rsq.slope_ratio.ok$rob_reg__slopes ~ 
                (phytopl.All.Atl.p.rsq.slope_ratio.ok$MLD_Mathilde) )
summary(lm.mld) # R-squared:  0.03, 349 degrees of freedom, p-value: 0.0008

    
    
    ### PLOTs For PAPER ----------

# Chlorophyll__mg_m3_insitu
plot(phytopl.All.Atl.p.rsq.slope_ratio.ok$rob_reg__slopes ~ 
       log10(1+ phytopl.All.Atl.p.rsq.slope_ratio.ok$Chlorophyll__mg_m3_insitu), 
     col = alpha ("darkgreen", 0.3), pch = 16)
abline(lmlogChla_insitu, lwd = 2.5, lty = 2, col = "darkorange")



 # Temperature_C_insitu
    plot(phytopl.All.Atl.p.rsq.slope_ratio.ok$rob_reg__slopes ~ 
           phytopl.All.Atl.p.rsq.slope_ratio.ok$Temperature_C_insitu, 
         col = alpha ("darkgreen", 0.3), pch = 16)
    abline(lmTemp_insitu, lwd = 2.5, lty = 2, col = "darkorange")
    
    
  # three Temperature groups FOR PAPER -----------------------
    # classified by  SST in situ !
   
    
    ### slope vs Chla, in three SST groups --------------------
    
    ###  slope vs Chla, by Temp_class --------------------
    
    length(phytopl.All.Atl.p.rsq.slope_ratio.ok $Date) # 462 phytoplankton
    summary(phytopl.All.Atl.p.rsq.slope_ratio.ok$SST_C_insitu) # 13 NAs
    # range from 5.50  to  29.28 ºC.
    
    # Cold: 3 to 20 degr. Celsius
    # Suptropical: 20 to 27.8 degr. Celsius 
    # Tropical: > 27.8 (27.8 to 31) degr. Celsius
    
    Cold.phytopl.All.Atl <- subset(phytopl.All.Atl.p.rsq.slope_ratio.ok, SST_C_insitu < 20 )
    summary(Cold.phytopl.All.Atl$SST_C_insitu) # 5.5 to 19.8 degr Celsius
    
    SubTrop.phytopl.All.Atl <- subset(phytopl.All.Atl.p.rsq.slope_ratio.ok, SST_C_insitu > 20 )
    SubTrop.phytopl.All.Atl <- subset(SubTrop.phytopl.All.Atl, SST_C_insitu < 27.8 )
    summary(SubTrop.phytopl.All.Atl$SST_C_insitu) # 20.15 to 27.75 degr Celsius
    
    Tropic.phytopl.All.Atl <-subset(phytopl.All.Atl.p.rsq.slope_ratio.ok, SST_C_insitu > 27.8 )
    Tropic.phytopl.All.Atl <-subset(Tropic.phytopl.All.Atl, SST_C_insitu < 31 )
    summary(Tropic.phytopl.All.Atl$SST_C_insitu) # 27.97 to 29.28 degr Celsius
    
    
    
    # Cold waters (temperate & Upwelling): 3 to 20 degr. Celsius -----------------------
    #Cold.phytopl.All.Atl <- subset(phytopl.All.Atl, SST_C_insitu < 20 )
    summary(Cold.phytopl.All.Atl$SST_C_insitu) # 5. to 19.8 degr Celsius
    
    lm1.cold <-  lm(Cold.phytopl.All.Atl$slope ~ log10(Cold.phytopl.All.Atl$Chlorophyll__mg_m3_insitu))
    summary(lm1.cold)
  #  R-squared:  0.3586,  p-value: < 2.2e-16, 193 degrees of freedom
    
    plot(Cold.phytopl.All.Atl$slope ~ log10(Cold.phytopl.All.Atl$Chlorophyll__mg_m3_insitu), 
                xlim = c(-2, 1.5),
         ylim = c(-2.2, 0.1),
         col = alpha ("darkgreen", 0.3), pch = 16)
    abline(lm1.cold, lwd = 2.5, lty = 2, col = "darkorange")
    
    summary(Cold.phytopl.All.Atl$rob_reg__slopes) # 
    #    Min.   1st Qu. Median    Mean 3rd Qu.    Max. 
    # -1.4554 -1.0457 -0.9208 -0.9240 -0.7797 -0.3081  
    # 
    # Subtropical  waters: 20 to  27.8 -----------------------
    summary(SubTrop.phytopl.All.Atl$SST_C_insitu) # 20 to 27.8 degr Celsius
    
    lm1.subtrop <-  lm(SubTrop.phytopl.All.Atl$rob_reg__slopes ~ log10(SubTrop.phytopl.All.Atl$Chlorophyll__mg_m3_insitu))
    summary(lm1.subtrop)
   # R-squared:  0.04376, p = 0.008, 155 degrees of freedom
    
    plot(SubTrop.phytopl.All.Atl$rob_reg__slopes ~ 
           log10(SubTrop.phytopl.All.Atl$Chlorophyll__mg_m3_insitu),
         xlim = c(-2,  1.5),
         ylim = c(-2.2, 0.1),
         col = alpha ("darkgreen", 0.3), pch = 16)
    abline(lm1.subtrop, lwd = 2.5, lty = 2, col = "darkorange")
    
    summary(SubTrop.phytopl.All.Atl$rob_reg__slopes) # 
    #     Min.     1st Qu.  Median   Mean    3rd Qu      Max. 
    #   -1.4092   -1.1593   -1.1079 -1.0992 -1.0376 -0.8184 
    
   
    
    # Tropical  waters: > 27.8 -----------------------
    summary(Tropic.phytopl.All.Atl$SST_C_insitu) # > 27.8 degr Celsius
    summary(Tropic.phytopl.All.Atl$rob_reg__slopes) # 
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    # -1.8343 -1.4394 -1.3019 -1.3125 -1.1853 -0.8082 
    # 
      
    lm1.trop <-  lm(Tropic.phytopl.All.Atl$rob_reg__slopes ~ log10(1+Tropic.phytopl.All.Atl$Chlorophyll__mg_m3_insitu))
    summary(lm1.trop) # ns.
    
    plot(Tropic.phytopl.All.Atl$rob_reg__slopes ~
           log10(Tropic.phytopl.All.Atl$Chlorophyll__mg_m3_insitu),
         xlim = c(-2,  1.5),
         ylim = c(-2.2, 0.1),
         col = alpha ("darkgreen", 0.3), pch = 16)
#   abline(lm1.trop, lwd = 2.5, lty = 2, col = "darkorange")


    
    
    
    
    
    
    


# 3D plots ---------------


lm4 <- lm(phytopl.All.Atl$slope.clean ~ log10(phytopl.All.Atl$chla) * phytopl.All.Atl$Temperature_C_insitu)
summary(lm4)
AIC(lm4)

# Temperature
# Interaction term is significant (p = 0.022) ! 
# But productivity index 3 (Chl and Nitrate) is not...
# 367 degrees of freedom

# With interaction
# Interaction term is significant! But productivity index is not...
# better: without interaction

# linear models: 
# in situ data

attach(phytopl.All.Atl)

lm.index3Temp <- lm(phytopl.All.Atl$slope.clean ~ 
                      log10(phytopl.All.Atl$prod.index3_2Chl_nitr) * 
                      phytopl.All.Atl$Temperature_C_insitu)
summary(lm.index3Temp)
AIC(lm.index3Temp)
# Multiple R-squared:  0.3427,	Adjusted R-squared:  0.3373 


lm.index2Temp <- lm(phytopl.All.Atl$slope.clean ~ 
                      log10(phytopl.All.Atl$prod.index2) * 
                      phytopl.All.Atl$Temperature_C_insitu)
summary(lm.index2Temp)
# Multiple R-squared:  0.3644,	Adjusted R-squared:  0.3592 


lm.ChlaTemp <- lm(phytopl.All.Atl$slope.clean ~ 
                      log10(phytopl.All.Atl$Chlorophyll__mg_m3_insitu) * 
                      phytopl.All.Atl$Temperature_C_insitu)
summary(lm.ChlaTemp)
# multiple R-squared:  0.3631,	Adjusted R-squared:  0.3592 
# R2 = 0.36




# in situ Nitrate & Temp vs slope, Best 2 x vs 1 y model------------------
lm.Nitrate.Temp <- lm(phytopl.All.Atl$slope.clean ~ 
                    log10(1+phytopl.All.Atl$Nitrate__uM_L1_insitu) * 
                    phytopl.All.Atl$Temperature_C_insitu)
summary(lm.Nitrate.Temp)
# R2 = 0.399 -> best 2 indep. variable model is with temp and nitrate! 
# Multiple R-squared:  0.3994,	Adjusted R-squared:  0.3946


cor.test ((log(1+Nitrate__uM_L1_insitu)) , log(1+Chlorophyll__mg_m3_insitu ) )
# p-value = 0.4683, r = 0.03697214 
# nitrate and Chl a (in situ ) are not correlated!!!
par(  opar)
plot(log(1+Chlorophyll__mg_m3_insitu ) ~(log(1+Nitrate__uM_L1_insitu)) ) # in situ data



# 3x vs 1 y model


# in situ Nitrate & Temp & in situ Chla  vs slope, Best 2 x vs 1 y model------------------
lm.Nitrate.Chl.Temp <- lm(phytopl.All.Atl$slope.clean ~ 
                        log10(1+phytopl.All.Atl$Nitrate__uM_L1_insitu) * 
                                  phytopl.All.Atl$Temperature_C_insitu *
                          log10(1+ phytopl.All.Atl$Chlorophyll__mg_m3_insitu)
                        )
summary(lm.Nitrate.Chl.Temp)
# R2 = 0.399 -> best 2 indep. variable model is with temp and nitrate! 
# Multiple R-squared:  0.3994,	Adjusted R-squared:  0.3946


# Nitrate is King! ----------------




# relative importance ------------------

library(relaimpo)
  calc.relimp(lm.ChlaTemp, type="lmg") # Chla is king ! (in situ data)


  library(relaimpo)
  calc.relimp(lm.ChlaTemp, type="lmg") # Chla is king ! (in situ data)
  
    
  library(relaimpo)
  calc.relimp(lm.index3Temp, type="lmg") # 
  
  
  length( slope.clean) # 565 slopes phytopl
  summary(slope.clean) # slopes phytopl (15  NAs)
  +565-15  # 550 slopes phytopl  data -----

length(Chlorophyll__mg_m3_insitu) # 565
summary(Chlorophyll__mg_m3_insitu) # in situ (10 NAs)
+565-10 # 555 in situ chl a data -------------

length(Nitrate__uM_L1_insitu) # 565
summary(Nitrate__uM_L1_insitu) # in situ (169 NAs)
+565-169  # 396 in situ Nitrate data

length(prod.index3_2Chl_nitr) # 565
summary(prod.index3_2Chl_nitr) # in situ (178  NAs)
+565-178  # 387 in situ-based index 3 (Chla and Nitrate data)


# 3D scatter plot

# linear 3d surface -------------

?scatter3d

library(car)
scatter3d(x = phytopl.All.Atl$Temperature_C_insitu, y =  phytopl.All.Atl$slope.clean, 
          z = log10(phytopl.All.Atl$prod.index3_2Chl_nitr), 
          grid = FALSE, fit = "smooth", point.col="darkorange") # bg = "black")


library(car)
scatter3d(x = phytopl.All.Atl$Temperature_C_insitu, y =  phytopl.All.Atl$slope.clean, 
          z = log10(phytopl.All.Atl$prod.index2), 
          grid = FALSE, fit = "smooth", point.col="darkorange") # bg = "black")


library(car)
scatter3d(x = phytopl.All.Atl$Temperature_C_insitu, y =  phytopl.All.Atl$slope.clean, 
          z = log10(phytopl.All.Atl$Chlorophyll__mg_m3_insitu), 
          grid = FALSE, fit = "smooth", point.col="darkorange") # bg = "black")


# Conclusion: temperature, productivity and phytoplankton NBSS slope are strongly  correlated !

# low temperatures (cold): flat slope, regardless of productivity 

# high temperature (warm): steeper slopes - with productivity




# II. CLasses (Temperature and Chla)
#Three classes by temperature ## separate plots for 3 temp. ranges -------------------
#Two classes by Chla (mesotrophic vs oligotrophic) -----------------


# temperatures for classes: 3 to 20, 20 to 27.8, < 27.8 (27.8 to 31)  ----------


### slope vs Chla --------------------

### II.a slope vs Chla, by Temp_class --------------------

length(phytopl.All.Atl$Date) # 555 phytoplankton
summary(phytopl.All.Atl$SST_C_insitu[phytopl.All.Atl$SST_C_insitu < 31]) # 13

# Cold: 3 to 20 degr. Celsius
# Suptropical: 20 to 27.8 degr. Celsius 
# Tropical: > 27.8 (27.8 to 31) degr. Celsius

Cold.phytopl.All.Atl <- subset(phytopl.All.Atl.p.rsq.slope_ratio.ok, SST_C_insitu < 20 )
summary(Cold.phytopl.All.Atl$SST_C_insitu) # 5.3 to 19.8 degr Celsius

SubTrop.phytopl.All.Atl <- subset(phytopl.All.Atl.p.rsq.slope_ratio.ok, SST_C_insitu > 20 )
SubTrop.phytopl.All.Atl <- subset(SubTrop.phytopl.All.Atl, SST_C_insitu < 27.8 )
summary(SubTrop.phytopl.All.Atl$SST_C_insitu) # 20.6 to 27.75 degr Celsius

Tropic.phytopl.All.Atl <-subset(phytopl.All.Atl.p.rsq.slope_ratio.ok, SST_C_insitu > 27.8 )
Tropic.phytopl.All.Atl <-subset(Tropic.phytopl.All.Atl, SST_C_insitu < 31 )
summary(Tropic.phytopl.All.Atl$SST_C_insitu) # 27.97 to 29.28 degr Celsius


# Cold waters (temperate & Upwelling): 3 to 20 degr. Celsius -----------------------
#Cold.phytopl.All.Atl <- subset(phytopl.All.Atl, SST_C_insitu < 20 )
summary(Cold.phytopl.All.Atl$SST_C_insitu) # 5. to 19.8 degr Celsius

lm1.cold <-  lm(Cold.phytopl.All.Atl$slope ~ log10(Cold.phytopl.All.Atl$Chlorophyll__mg_m3_insitu))
summary(lm1.cold)

plot(Cold.phytopl.All.Atl$slope ~ log10(Cold.phytopl.All.Atl$Chlorophyll__mg_m3_insitu))
abline(lm1.cold)    


# Subtropical  waters: 20 to  27.8 -----------------------
summary(SubTrop.phytopl.All.Atl$SST_C_insitu) # 20 to 27.8 degr Celsius

lm1.subtrop <-  lm(SubTrop.phytopl.All.Atl$rob_reg__slopes ~ log10(SubTrop.phytopl.All.Atl$Chlorophyll__mg_m3_insitu))
summary(lm1.subtrop)

plot(SubTrop.phytopl.All.Atl$rob_reg__slopes ~ log10(SubTrop.phytopl.All.Atl$Chlorophyll__mg_m3_insitu))
abline(lm1.subtrop)    


# Tropical  waters: > 27.8 -----------------------
summary(Tropic.phytopl.All.Atl$SST_C_insitu) # > 27.8 degr Celsius

lm1.trop <-  lm(Tropic.phytopl.All.Atl$rob_reg__slopes ~ log10(1+Tropic.phytopl.All.Atl$Chlorophyll__mg_m3_insitu))
summary(lm1.trop) # ns.

plot(Tropic.phytopl.All.Atl$rob_reg__slopes ~ log10(Tropic.phytopl.All.Atl$Chlorophyll__mg_m3_insitu))
# abline(lm1.trop)    


### slope vs nitrate --------------------
### II.b slope vs nitrate, by Temp_class --------------------

# Cold waters (temperate & Upwelling): 3 to 20 degr. Celsius -----------------------
#Cold.phytopl.All.Atl <- subset(phytopl.All.Atl, SST_C_insitu < 20 )
summary(Cold.phytopl.All.Atl$SST_C_insitu) # 5.3 to 19.8 degr Celsius
lm1.coldb <-  lm(Cold.phytopl.All.Atl$slope ~ 
                  log10(1+Cold.phytopl.All.Atl$Nitrate__uM_L1_insitu))
summary(lm1.coldb)

plot(Cold.phytopl.All.Atl$slope ~ 
       log10(1+Cold.phytopl.All.Atl$Nitrate__uM_L1_insitu))
#abline(lm1.coldb)    


# Subtropical  waters: 20 to  27.8 -----------------------
summary(SubTrop.phytopl.All.Atl$SST_C_insitu) # 20 to 27.8 degr Celsius

lm1.subtrop.b <-  lm(SubTrop.phytopl.All.Atl$slope ~ 
                       log10(SubTrop.phytopl.All.Atl$Nitrate__uM_L1_insitu))
summary(lm1.subtrop.b)

plot(SubTrop.phytopl.All.Atl$slope ~ 
       log10(SubTrop.phytopl.All.Atl$Nitrate__uM_L1_insitu))
abline(lm1.subtrop.b)    


# Tropical  waters: > 27.8 -----------------------
summary(Tropic.phytopl.All.Atl$SST_C_insitu) # > 27.8 degr Celsius

lm1.trop.b <-  lm(Tropic.phytopl.All.Atl$slope ~ 
                    log10(Tropic.phytopl.All.Atl$Nitrate__uM_L1_insitu))
summary(lm1.trop.b) # ns.

plot(Tropic.phytopl.All.Atl$slope ~ 
       log10(Tropic.phytopl.All.Atl$Nitrate__uM_L1_insitu))
 abline(lm1.trop.b)    

 
 # obs.: Nitrate may be a bad indicator fo productivity (high Nitrate - low productivity regions!)

 
 ### slope vs MLD --------------------
 ### II.c slope vs MLD --------------------
### II.c slope vs MLD, by Temp_class --------------------

# Cold waters (temperate & Upwelling): 3 to 20 degr. Celsius -----------------------
#Cold.phytopl.All.Atl <- subset(phytopl.All.Atl, SST_C_insitu < 20 )
summary(Cold.phytopl.All.Atl$MLD_Mathilde) # 5.3 to 19.8 degr Celsius
lm1.coldc <-  lm(Cold.phytopl.All.Atl$slope ~ 
                  (Cold.phytopl.All.Atl$MLD_Mathilde))
summary(lm1.coldc)

plot(Cold.phytopl.All.Atl$slope ~ 
       log10(Cold.phytopl.All.Atl$MLD_Mathilde))
abline(lm1.coldc)    


# Subtropical  waters: 20 to  27.8 -----------------------
summary(SubTrop.phytopl.All.Atl$SST_C_insitu) # 20 to 27.8 degr Celsius

lm1.subtrop.c <-  lm(SubTrop.phytopl.All.Atl$slope ~ 
                       log10(SubTrop.phytopl.All.Atl$MLD_Mathilde))
summary(lm1.subtrop.c)

plot(SubTrop.phytopl.All.Atl$slope ~ 
       log10(SubTrop.phytopl.All.Atl$MLD_Mathilde))
#abline(lm1.subtrop.c)    


# Tropical  waters: > 27.8 -----------------------
summary(Tropic.phytopl.All.Atl$SST_C_insitu) # > 27.8 degr Celsius

lm1.trop.c <-  lm(Tropic.phytopl.All.Atl$slope ~ 
                    log10(Tropic.phytopl.All.Atl$MLD_Mathilde))
summary(lm1.trop.c) # ns.

plot(Tropic.phytopl.All.Atl$slope ~ 
       log10(Tropic.phytopl.All.Atl$MLD_Mathilde))
 abline(lm1.trop.c)    

 
 
# 3d plots -------------------------

# smooth 3d surface -----------------

library(car)
scatter3d(x = phytopl.All.Atl$Temperature_C_insitu, y =  phytopl.All.Atl$slope.clean, 
          z = log10(phytopl.All.Atl$prod.index2), 
          grid = FALSE, fit = "linear")



library(car)
scatter3d(x = phytopl.All.Atl$Temperature_C_insitu, y =  phytopl.All.Atl$slope.clean, 
          z = log10(phytopl.All.Atl$prod.index2), 
          grid = FALSE, fit = "smooth")

# collinearity ? ---------------
# no collinearity , Person "|r|" is well below 0.9, even below 0.8 
cor.test(log10(phytopl.All.Atl$prod.index2),phytopl.All.Atl$Temperature_C_insitu) 
#  r = -0.73 
#  collinearity would hamper our ability to detect interactions 
# collinearity is OK


summary(lm( phytopl.All.Atl$slope.clean ~ phytopl.All.Atl$Temperature_C_insitu  +  
              log10(phytopl.All.Atl$prod.index2)))


summary(lm( phytopl.All.Atl$slope.clean ~ phytopl.All.Atl$Temperature_C_insitu  *  
              log10(phytopl.All.Atl$prod.index2)))



# Conclusion: temperature, productivity and phytoplankton NBSS slope are strongly  correlated !


plot(phytopl.All.Atl$slope.clean ~ (phytopl.All.Atl$Temperature_C_insitu))
plot(phytopl.All.Atl$slope.clean ~ (phytopl.All.Atl$Salinity_insitu))
plot(phytopl.All.Atl$slope.clean ~ log10(1+phytopl.All.Atl$Chlorophyll__mg_m3_insitu))
plot(phytopl.All.Atl$slope.clean ~ log10(1+phytopl.All.Atl$Nitrate__uM_L1_insitu))
plot(phytopl.All.Atl$slope.clean ~ log10(1+phytopl.All.Atl$silicate))


# 5.1.b relative importance -------------------
# relaimpo  ------------- 
# detailed anaylis with relaimpo 
# what is more important, temperature or productivity? 

library(relaimpo)

# Best, simple linear model ---------------------------------------
lm3 <- lm(phytopl.All.Atl$slope.clean ~ log10(phytopl.All.Atl$prod.index2) + 
            phytopl.All.Atl$Temperature_C_insitu)
summary(lm3)
# model without interacation
#temperature and productivity are highly significant
# R-squared = 0.35 (model explains  35% of variability)

metrics.lm3 <- calc.relimp(lm3, type = c("lmg", "first"))

metrics.lm3

#                                         lmg     
# log10(phytopl.All.Atl$prod.index2) 0.1473756  
# phytopl.All.Atl$Temperature_C_insitu     0.2073353 

# The productivity index explains 14.7% of the variability in phtyopl. slope
# The Temperature  explains 20.7% of the variability in phtyopl. slope
# in a model without interaction


# Relative importance metrics:  -----------------------
#   
# lmg: aprox. % of the total variability expained within multivariate model
# first: % of the total variability expained by univariate model


# other multiple linear models - more variables

lm4 <- lm(phytopl.All.Atl$slope.clean ~  
            phytopl.All.Atl$Temperature_C_insitu + phytopl.All.Atl$Salinity_insitu)
summary(lm4)

metrics.lm4 <- calc.relimp(lm4, type = c("lmg", "first"))

metrics.lm4





#######
# Task 4 (NBSS by region , phytoplankton)-------------

# Task 4a NBSS plots by 4 regions (phytoplanktn)--------------
# Task 4b Compare 4 regions (boxplots and tests) ------------


# Task 4a slope and intercept by regions, and mean biomass in three  key size ranges by regions

# limits of regions ----------

# Region
# 
# OMZ
# 8°N to 12°N
# -21.6°W to -17°W
# 
# nCCUS
# 16.9°N to 30°N
# -26°W to -18°W
# 
# EQU
# -1°S to 8°N
# -33°W to -20°W
# 
# SBUS
# -29.9°S to -35°S
# 12.7°W to 18.1°W
# 
# NBUS
# -17.5°S to -23.1°S
# 10.2°W to 12.8°W
# 
# BRZ_oc_isl
# -2°S to -6°S
# -35°W to -31°W



# plot regions on a map -----------


#USING MAPS package (simple)
library(maps)
map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-70, 60), xlim=c(-95, 30), mar=c(0,0,0,0))
points( Data.All.Atl$Longitude,Data.All.Atl$Latitude,   col="red", pch=16)

map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-70, 60), xlim=c(-95, 30), mar=c(0,0,0,0))
#points( Data.All.Atl$Longitude,Data.All.Atl$Latitude,   col="red", pch=16)
symbols(Data.All.Atl$Latitude~ Data.All.Atl$Longitude, 
        circles = (sqrt(Data.All.Atl$prod.index4_Chl_nitr)), fg= alpha("red", 0.6), inches=0.3,
        add = TRUE)
map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-70, 60), xlim=c(-95, 30), mar=c(0,0,0,0))
#points( Data.All.Atl$Longitude,Data.All.Atl$Latitude,   col="red", pch=16)
symbols(phytopl.All.Atl.p.rsq.slope_ratio.ok$Latitude ~ phytopl.All.Atl.p.rsq.slope_ratio.ok$Longitude, 
        circles = (10^(-1* phytopl.All.Atl.p.rsq.slope_ratio.ok$rob_reg__slopes)), fg= alpha("darkgreen", 0.6), inches=0.3,
        add = TRUE)

summary(phytopl.All.Atl.p.rsq.slope_ratio.ok$Latitude)


par(opar)

# 4 key regions (where there is an exceptionally dense wealth of data)

# TSWA
rect( ybottom = -12,  ytop = -2,           xleft =  -38, xright =   -25, col = "black")
# -2°S to -12°S
# -38°W to -25°W

# CCUS
rect( ybottom = 16.9 ,  ytop = 30,           xleft = -26 , xright =   -18 )
# 16.9°N to 30°N
# -26°W to -18°W

# EQU
rect( ybottom = -1,  ytop = 8,           xleft = -33 , xright =   -20 )
# -1°S to 8°N
# -33°W to -20°W

# BUS
# (old) rect( ybottom = -17.5,  ytop = -23.1,           xleft =  10.2, xright =   12.8 )
rect( ybottom = -17.5,  ytop = -35,           xleft =  17, xright =   10.2)
# -17.5°S to -28.5°S
# 10.2°W to 17°W


# subsets by regions  (Phytopl)--------------------

phyt.5 <- phytopl.All.Atl.p.rsq.slope_ratio.ok

# TSWA ------------
rect( ybottom = -2,  ytop = -12,           xleft =  -38, xright =   -25)
# -2°S to -12°S
# -38°W to -25°W

phyt.TSWA.5 <- phyt.5
phyt.TSWA.5 <-  phyt.5[ (phyt.TSWA.5$Latitude > -12) & (phyt.TSWA.5$Latitude < -2) &
                          (phyt.TSWA.5$Longitude > -38) & (phyt.TSWA.5$Longitude < -25),  ]

symbols(phyt.TSWA.5$Latitude~ phyt.TSWA.5$Longitude, 
        circles = (10^(-1* phyt.TSWA.5$rob_reg__slopes)), fg= alpha("green", 0.6), inches=0.3,
        add = TRUE)

# CCUS -------------

rect( ybottom = 16.9 ,  ytop = 30,           xleft = -26 , xright =   -18 )
# 16.9°N to 30°N
# -26°W to -18°W

phyt.CCUS.5 <- phyt.5
phyt.CCUS.5 <-  phyt.5[ (phyt.CCUS.5$Latitude > 16.9) & (phyt.CCUS.5$Latitude < 30) &
                          (phyt.CCUS.5$Longitude > -26) & (phyt.CCUS.5$Longitude < -18),  ]

symbols(phyt.CCUS.5$Latitude~ phyt.CCUS.5$Longitude, 
        circles = (10^(-1* phyt.CCUS.5$rob_reg__slopes)), fg= alpha("green", 0.6), inches=0.3,
        add = TRUE)


# EQU -----------
rect( ybottom = -1,  ytop = 8,           xleft = -33 , xright =   -20 )
# -1°S to 8°N
# -33°W to -20°W

phyt.EQU.5 <- phyt.5
phyt.EQU.5 <-  phyt.5[ (phyt.EQU.5$Latitude > -1) & (phyt.EQU.5$Latitude < 8) &
                         (phyt.EQU.5$Longitude > -33) & (phyt.EQU.5$Longitude < -20),  ]

symbols(phyt.EQU.5$Latitude~ phyt.EQU.5$Longitude, 
        circles = (10^(-1* phyt.EQU.5$rob_reg__slopes)), fg= alpha("green", 0.6), inches=0.3,
        add = TRUE)


# BUS ---------------
rect( ybottom = -17.5,  ytop = -35,           xleft =  17, xright =   10.2)
# -17.5°S to -28.5°S
# 10.2°W to 17°W


phyt.BUS.5 <- phyt.5
phyt.BUS.5 <-  phyt.5[ (phyt.BUS.5$Latitude > -35) & (phyt.BUS.5$Latitude < -17.5) &
                         (phyt.BUS.5$Longitude < 17) & (phyt.BUS.5$Longitude > 10.2),  ]

symbols(phyt.BUS.5$Latitude~ phyt.BUS.5$Longitude, 
        circles = (10^(-1* phyt.BUS.5$rob_reg__slopes)), fg= alpha("green", 0.6), inches=0.3,
        add = TRUE)

# Plots and Regressions by regions(Phytopl)--------------------
library(scales)
library(MASS)

# TSWA ----------------

#first NBSS plots, phytopl. --------------

length(phyt.TSWA.5$Longitude) # 109 Data points , 109 Samples (phytopl.)
summary(phyt.TSWA.5)

# X5.75046978753507 X6.05149978325976 X6.35252977892375     slope           Std.Error      minimum   
# Min.   : NA       Min.   : NA       Min.   : NA       Min.   :-1.8344   Min.   : NA   Min.   : NA  
# 1st Qu.: NA       1st Qu.: NA       1st Qu.: NA       1st Qu.:-1.3913   1st Qu.: NA   1st Qu.: NA  
# Median : NA       Median : NA       Median : NA       Median :-1.2683   Median : NA   Median : NA  
# Mean   :NaN       Mean   :NaN       Mean   :NaN       Mean   :-1.2787   Mean   :NaN   Mean   :NaN  
# 3rd Qu.: NA       3rd Qu.: NA       3rd Qu.: NA       3rd Qu.:-1.1558   3rd Qu.: NA   3rd Qu.: NA  
# Max.   : NA       Max.   : NA       Max.   : NA       Max.   :-0.7591   Max.   : NA   Max.   : NA  
# NA's   :109       NA's   :109       NA's   :109                         NA's   :109   NA's   :109  
#     maximum      comment          Conductivity_Mathilde Salinity_Mathilde Oxygen_Mathilde   AOU_Mathilde    
#  Min.   : NA   Length:109         Min.   :5.634         Min.   :35.84     Min.   :0.1916   Min.   :-8.0000  
#  1st Qu.: NA   Class :character   1st Qu.:5.804         1st Qu.:35.89     1st Qu.:0.1936   1st Qu.:-4.9047  
#  Median : NA   Mode  :character   Median :5.838         Median :35.91     Median :0.1973   Median :-2.0766  
#  Mean   :NaN                      Mean   :5.814         Mean   :36.08     Mean   :0.1983   Mean   :-2.4251  
#  3rd Qu.: NA                      3rd Qu.:5.861         3rd Qu.:36.21     3rd Qu.:0.2002   3rd Qu.:-0.3402  
#  Max.   : NA                      Max.   :6.024         Max.   :36.80     Max.   :0.2157   Max.   : 5.2073  
#  NA's   :109                      NA's   :20            NA's   :20        NA's   :20       NA's   :20       
# MLD_Mathilde   Phosphates_Mathilde Silicates_Mathilde Temperature_Mathilde Nitrates_Mathilde
# Min.   :33.34   Min.   :0.003899    Min.   :0.7522     Min.   :25.99        Min.   :0.0000   
# 1st Qu.:39.01   1st Qu.:0.054391    1st Qu.:1.3653     1st Qu.:27.72        1st Qu.:0.1717   
# Median :43.28   Median :0.069015    Median :1.7257     Median :28.13        Median :0.3714   
# Mean   :49.01   Mean   :0.077675    Mean   :1.6768     Mean   :27.78        Mean   :0.5286   
# 3rd Qu.:57.24   3rd Qu.:0.085678    3rd Qu.:1.9265     3rd Qu.:28.36        3rd Qu.:0.6147   
# Max.   :83.79   Max.   :0.218851    Max.   :2.4525     Max.   :28.43        Max.   :2.2916   
# NA's   :20      NA's   :20          NA's   :20         NA's   :20           NA's   :20       
#  Biomass_phytoplankton__ug_m3 target_target_org_code_code      SST             chla            nitrate      
#  Min.   : 0.07                1:  0                       Min.   :25.94   Min.   :0.02300   Min.   :0.0000  
#  1st Qu.: 7.05                2:  0                       1st Qu.:28.23   1st Qu.:0.02700   1st Qu.:0.0000  
#  Median :12.19                3:  0                       Median :28.66   Median :0.02877   Median :0.0000  
#  Mean   :14.49                4:109                       Mean   :28.25   Mean   :0.03044   Mean   :0.1683  
#  3rd Qu.:17.02                5:  0                       3rd Qu.:28.83   3rd Qu.:0.03000   3rd Qu.:0.0000  
#  Max.   :84.77                6:  0                       Max.   :29.28   Max.   :0.10940   Max.   :1.0000  
#                                                           NA's   :11                                        
# silicate                  target.org.colour  slope.clean      prod.index5_2Chl_nitr_MLD_mutipl
# Min.   :1.000   darkorange            :  0     Min.   :-1.8344   Min.   :-0.9542                 
# 1st Qu.:1.000   detritus + zooplankton:  0     1st Qu.:-1.3913   1st Qu.: 0.5201                 
# Median :2.000   dodgerblue            :  0     Median :-1.2683   Median : 1.0617                 
# Mean   :1.717   Phytoplankton         :109     Mean   :-1.2787   Mean   :    Inf                 
# 3rd Qu.:2.000   purple                :  0     3rd Qu.:-1.1558   3rd Qu.: 2.0305                 
# Max.   :2.000                                  Max.   :-0.7591   Max.   :    Inf                 
# NA's   :20                      
#  prod.index4_2Chl_nitr_MLD  prod.index1       prod.index2     prod.index3_2Chl_nitr prod.index4_Chl_nitr
#  Min.   :-6.223            Min.   :0.00000   Min.   :0.4091   Min.   :0.05859       Min.   :0.04999     
#  1st Qu.:-5.537            1st Qu.:0.02465   1st Qu.:0.7116   1st Qu.:0.24430       1st Qu.:0.16442     
#  Median :-5.169            Median :0.05169   Median :0.7852   Median :0.35108       Median :0.23905     
#  Mean   :-5.237            Mean   :0.09575   Mean   :0.8434   Mean   :0.40601       Mean   :0.29781     
#  3rd Qu.:-4.972            3rd Qu.:0.09220   3rd Qu.:0.9288   3rd Qu.:0.47589       3rd Qu.:0.31449     
#  Max.   :-4.378            Max.   :0.95180   Max.   :1.8802   Max.   :1.40312       Max.   :1.40312     
#  NA's   :26                NA's   :17        NA's   :17       NA's   :17            NA's   :17          
# new_slopes      rob_reg__slopes   p_values_olsr_slopes r.square.olsr    ratio_slopes_rr_olsr
# Min.   :-1.8344   Min.   :-1.8343   Min.   :0.000e+00    Min.   :0.5712   Min.   :0.9647      
# 1st Qu.:-1.3913   1st Qu.:-1.3986   1st Qu.:4.791e-06    1st Qu.:0.7573   1st Qu.:0.9991      
# Median :-1.2683   Median :-1.2794   Median :2.580e-05    Median :0.8214   Median :1.0022      
# Mean   :-1.2787   Mean   :-1.2883   Mean   :1.081e-03    Mean   :0.8084   Mean   :1.0077      
# 3rd Qu.:-1.1558   3rd Qu.:-1.1523   3rd Qu.:1.864e-04    3rd Qu.:0.8687   3rd Qu.:1.0146      
# Max.   :-0.7591   Max.   :-0.7727   Max.   :2.566e-02    Max.   :0.9825   Max.   :1.1234  

#par( old.par)

lnumb <- 12
length(( phyt.TSWA.5[lnumb,]))

phyt.TSWA.5NBSSmatrix <- as.matrix(phyt.TSWA.5[,37:92])
dim(phyt.TSWA.5NBSSmatrix)
# 109 rows , 56 columns of Biovolume NBSS values 


lnumb <- 107
plot (log10( phyt.TSWA.5NBSSmatrix[lnumb,]) ~ X_vectormm3log)


# plot all points and lm  ----------

lnumb <- 34
plot (log10( phyt.TSWA.5NBSSmatrix[lnumb,]) ~ X_vectormm3log, main = "TSWA",
      xlab = "log10( Biovolume (mm3 ind. -1)) ",
      ylab = "log10(Normalized Biovolume (mm3 m-3 mm-3) ",
      xlim = c(-11, -1), ylim = c(0, 14) ,
      pch = 16, col = "white")
for (lnumb in 1 : nrow(phyt.TSWA.5NBSSmatrix) ) 
{    points (log10( phyt.TSWA.5NBSSmatrix[lnumb,]) ~ X_vectormm3log ,
             pch = 16, col = alpha("darkgreen", 0.3))
  abline( rlm(log10( phyt.TSWA.5NBSSmatrix[lnumb,]) ~ X_vectormm3log), col =alpha ("navy", 0.3))
}

# TSWA
(median(phyt.TSWA.5$rob_reg__slopes) )
# median RR slope : -1.279371
(min(phyt.TSWA.5$rob_reg__slopes)); (max(phyt.TSWA.5$rob_reg__slopes) )
# min - max  RR slope : -1.279371
# -1.834306 to  -0.7727456


# matrix to to vector (transpose), TSWA -----------------------  
x1 <- rep( X_vectormm3log, nrow(phyt.TSWA.5NBSSmatrix) )
y1 <- c ( t(phyt.TSWA.5NBSSmatrix))

# regression line (Rob. regr.)
#plot(log10(y1) ~ x1, xlim = c(-11, -1), ylim = c(0, 14) )
#points(  log10(y1) ~ x1 )
# abline(rlm (log10(y1) ~ x1, xlim = c(-11, -1), ylim = c(0, 14)))
abline(rlm (log10(y1) ~ x1, xlim = c(-11, -1), ylim = c(0, 14)), 
       lwd = 2.5, lty = 2, col = "darkorange" )

# means and medians without considering NAs  
mean (c(4,NA, 18,34)) # NA
median( c(4,NA, 18, 34)  ) #NA

mean_   <- function(...) mean(..., na.rm=T)
median_ <- function(...) median(..., na.rm=T)

mean_ (c(4,NA, 8, 34)) #15.33, OK
median_ (c(4,NA, 8,34)) #8, OK

# apply median by columns

median_vec_TSWA <- apply(phyt.TSWA.5NBSSmatrix, 2, median_) 
mean_vec_TSWA <- apply(phyt.TSWA.5NBSSmatrix, 2, mean_) 

lines(  log10(median_vec_TSWA) ~ X_vectormm3log,pch = 16, col = "red" , lwd = 2.5)





# CCUS ----------------

#first NBSS plots, phytopl. --------------

length(phyt.CCUS.5$Longitude) # 25 Data points, 25 Samples (phytopl.)
summary(phyt.CCUS.5)
# X5.4494397918711 X5.75046978753507 X6.05149978325976 X6.35252977892375     slope           Std.Error  
# Min.   : NA      Min.   : NA       Min.   : NA       Min.   : NA       Min.   :-1.2158   Min.   : NA  
# 1st Qu.: NA      1st Qu.: NA       1st Qu.: NA       1st Qu.: NA       1st Qu.:-1.1453   1st Qu.: NA  
# Median : NA      Median : NA       Median : NA       Median : NA       Median :-1.0556   Median : NA  
# Mean   :NaN      Mean   :NaN       Mean   :NaN       Mean   :NaN       Mean   :-1.0628   Mean   :NaN  
# 3rd Qu.: NA      3rd Qu.: NA       3rd Qu.: NA       3rd Qu.: NA       3rd Qu.:-1.0094   3rd Qu.: NA  
# Max.   : NA      Max.   : NA       Max.   : NA       Max.   : NA       Max.   :-0.8447   Max.   : NA  
# NA's   :25       NA's   :25        NA's   :25        NA's   :25                          NA's   :25   
#     minimum       maximum      comment          Conductivity_Mathilde Salinity_Mathilde Oxygen_Mathilde 
#  Min.   : NA   Min.   : NA   Length:25          Min.   :5.165         Min.   :36.13     Min.   :0.2100  
#  1st Qu.: NA   1st Qu.: NA   Class :character   1st Qu.:5.247         1st Qu.:36.62     1st Qu.:0.2170  
#  Median : NA   Median : NA   Mode  :character   Median :5.538         Median :36.91     Median :0.2181  
#  Mean   :NaN   Mean   :NaN                      Mean   :5.436         Mean   :36.76     Mean   :0.2197  
#  3rd Qu.: NA   3rd Qu.: NA                      3rd Qu.:5.591         3rd Qu.:36.99     3rd Qu.:0.2228  
#  Max.   : NA   Max.   : NA                      Max.   :5.626         Max.   :37.24     Max.   :0.2290  
#  NA's   :25    NA's   :25                                                                               
#   AOU_Mathilde     MLD_Mathilde   Phosphates_Mathilde Silicates_Mathilde Temperature_Mathilde
#  Min.   :-8.000   Min.   :27.30   Min.   :0.01573     Min.   :0.2879     Min.   :20.55       
#  1st Qu.:-7.783   1st Qu.:32.72   1st Qu.:0.02925     1st Qu.:0.4804     1st Qu.:22.02       
#  Median :-6.885   Median :39.55   Median :0.05202     Median :0.5181     Median :24.02       
#  Mean   :-4.990   Mean   :42.64   Mean   :0.07595     Mean   :0.7108     Mean   :23.24       
#  3rd Qu.:-4.695   3rd Qu.:45.40   3rd Qu.:0.11736     3rd Qu.:0.6950     3rd Qu.:24.32       
#  Max.   : 9.698   Max.   :75.86   Max.   :0.21417     Max.   :2.1752     Max.   :25.24       
#                                                                                              
#  Nitrates_Mathilde Biomass_phytoplankton__ug_m3 target_target_org_code_code      SST             chla        
#  Min.   :0.00000   Min.   :  3.11               1: 0                        Min.   :11.59   Min.   :0.02797  
#  1st Qu.:0.02494   1st Qu.:  7.00               2: 0                        1st Qu.:21.49   1st Qu.:0.02898  
#  Median :0.05519   Median : 15.89               3: 0                        Median :23.43   Median :0.03586  
#  Mean   :0.22657   Mean   : 28.79               4:25                        Mean   :22.23   Mean   :0.04521  
#  3rd Qu.:0.17449   3rd Qu.: 26.70               5: 0                        3rd Qu.:24.77   3rd Qu.:0.05267  
#  Max.   :1.23795   Max.   :119.27               6: 0                        Max.   :25.44   Max.   :0.09134  
#                                                                                                              
#     nitrate         silicate                 target.org.colour  slope.clean     
#  Min.   :0.000   Min.   :1.00   darkorange            : 0      Min.   :-1.2158  
#  1st Qu.:0.000   1st Qu.:1.00   detritus + zooplankton: 0      1st Qu.:-1.1453  
#  Median :1.000   Median :1.00   dodgerblue            : 0      Median :-1.0556  
#  Mean   :1.215   Mean   :1.28   Phytoplankton         :25      Mean   :-1.0628  
#  3rd Qu.:2.000   3rd Qu.:2.00   purple                : 0      3rd Qu.:-1.0094  
#  Max.   :3.000   Max.   :2.00                                  Max.   :-0.8447  
#                                                                                 
#  prod.index5_2Chl_nitr_MLD_mutipl prod.index4_2Chl_nitr_MLD  prod.index1        prod.index2    
#  Min.   :-0.1393                  Min.   :-6.298            Min.   :0.001871   Min.   :0.3891  
#  1st Qu.: 1.4840                  1st Qu.:-5.866            1st Qu.:0.007967   1st Qu.:0.5071  
#  Median : 2.7663                  Median :-5.629            Median :0.096541   Median :0.8978  
#  Mean   :    Inf                  Mean   :-5.318            Mean   :0.534064   Mean   :0.9167  
#  3rd Qu.: 3.8218                  3rd Qu.:-4.793            3rd Qu.:0.207689   3rd Qu.:1.0620  
#  Max.   :    Inf                  Max.   :-3.478            Max.   :6.338432   Max.   :2.1106  
#                                   NA's   :3                 NA's   :9          NA's   :9       
# prod.index3_2Chl_nitr prod.index4_Chl_nitr   new_slopes      rob_reg__slopes   p_values_olsr_slopes
# Min.   :0.08804       Min.   :0.05862      Min.   :-1.2158   Min.   :-1.1912   Min.   :0.000e+00   
# 1st Qu.:0.20610       1st Qu.:0.12223      1st Qu.:-1.1453   1st Qu.:-1.1545   1st Qu.:9.080e-07   
# Median :0.57404       Median :0.38866      Median :-1.0556   Median :-1.0593   Median :1.332e-05   
# Mean   :0.57162       Mean   :0.42066      Mean   :-1.0628   Mean   :-1.0638   Mean   :8.946e-04   
# 3rd Qu.:0.71023       3rd Qu.:0.54866      3rd Qu.:-1.0094   3rd Qu.:-0.9843   3rd Qu.:2.613e-04   
# Max.   :1.63344       Max.   :1.39040      Max.   :-0.8447   Max.   :-0.9106   Max.   :9.107e-03   
# NA's   :9             NA's   :9                                                                    
# r.square.olsr    ratio_slopes_rr_olsr
# Min.   :0.5488   Min.   :0.9291      
# 1st Qu.:0.7188   1st Qu.:0.9777      
# Median :0.8118   Median :0.9936      
# Mean   :0.7931   Mean   :1.0029      
# 3rd Qu.:0.8593   3rd Qu.:1.0307      
# Max.   :0.9772   Max.   :1.1188      



# par( old.par)

lnumb <- 12
length(( phyt.CCUS.5[lnumb,])) # 125 cols

phyt.CCUS.5NBSSmatrix <- as.matrix(phyt.CCUS.5[,37:92])
dim(phyt.CCUS.5NBSSmatrix)
# 25 rows , 56 columns of Biovolume NBSS values 


lnumb <- 12
plot (log10( phyt.CCUS.5NBSSmatrix[lnumb,]) ~ X_vectormm3log)


# plot all points and lm  ----------

lnumb <- 5
plot (log10( phyt.CCUS.5NBSSmatrix[lnumb,]) ~ X_vectormm3log, main = "CCUS",
      xlab = "log10( Biovolume (mm3 ind. -1)) ",
      ylab = "log10(Normalized Biovolume (mm3 m-3 mm-3) ",
      xlim = c(-11, -1), ylim = c(0, 14) ,
      pch = 16, col = "white")
for (lnumb in 1 : nrow(phyt.CCUS.5NBSSmatrix) ) 
{    points (log10( phyt.CCUS.5NBSSmatrix[lnumb,]) ~ X_vectormm3log ,
             pch = 16, col = alpha("darkgreen", 0.3))
  abline( rlm(log10( phyt.CCUS.5NBSSmatrix[lnumb,]) ~ X_vectormm3log), col =alpha ("navy", 0.3))
}

# CCUS
(median(phyt.CCUS.5$rob_reg__slopes) )
# median RR slope :-1.059
(min(phyt.CCUS.5$rob_reg__slopes)); (max(phyt.CCUS.5$rob_reg__slopes) )
# min - max  RR slope : 
# -1.191 to  -0.9106


# matrix to to vector (transpose), CCUS -----------------------  
x1 <- rep( X_vectormm3log, nrow(phyt.CCUS.5NBSSmatrix) )
y1 <- c ( t(phyt.CCUS.5NBSSmatrix))

# regression line (Rob. regr.)
#plot(log10(y1) ~ x1, xlim = c(-11, -1), ylim = c(0, 14) )
#points(  log10(y1) ~ x1 )
# abline(rlm (log10(y1) ~ x1, xlim = c(-11, -1), ylim = c(0, 14)))
abline(rlm (log10(y1) ~ x1, xlim = c(-11, -1), ylim = c(0, 14)), 
       lwd = 2.5, lty = 2, col = "darkorange" )

# means and medians without considering NAs  
mean (c(4,NA, 18,34)) # NA
median( c(4,NA, 18, 34)  ) #NA

mean_   <- function(...) mean(..., na.rm=T)
median_ <- function(...) median(..., na.rm=T)

mean_ (c(4,NA, 8, 34)) #15.33, OK
median_ (c(4,NA, 8,34)) #8, OK

# apply median by columns

median_vec_CCUS <- apply(phyt.CCUS.5NBSSmatrix, 2, median_) 
mean_vec_CCUS <- apply(phyt.CCUS.5NBSSmatrix, 2, mean_) 

#points(log10(median_vec_BUS) ~ X_vectormm3log,pch = 16, col = "red")
#points(log10(mean_vec_BUS) ~ X_vectormm3log,pch = 16, col = "navy")
lines(  log10(median_vec_CCUS) ~ X_vectormm3log,pch = 16, col = "red" , lwd = 2.5)





# EQU -----------


#first NBSS plots, phytopl. --------------

length(phyt.EQU.5$Longitude) # 29 Data points, 29 Samples (phytopl.)
summary(phyt.EQU.5)
# X5.4494397918711 X5.75046978753507 X6.05149978325976 X6.35252977892375     slope          Std.Error  
# Min.   : NA      Min.   : NA       Min.   : NA       Min.   : NA       Min.   :-1.396   Min.   : NA  
# 1st Qu.: NA      1st Qu.: NA       1st Qu.: NA       1st Qu.: NA       1st Qu.:-1.170   1st Qu.: NA  
# Median : NA      Median : NA       Median : NA       Median : NA       Median :-1.098   Median : NA  
# Mean   :NaN      Mean   :NaN       Mean   :NaN       Mean   :NaN       Mean   :-1.137   Mean   :NaN  
# 3rd Qu.: NA      3rd Qu.: NA       3rd Qu.: NA       3rd Qu.: NA       3rd Qu.:-1.081   3rd Qu.: NA  
# Max.   : NA      Max.   : NA       Max.   : NA       Max.   : NA       Max.   :-1.011   Max.   : NA  
# NA's   :29       NA's   :29        NA's   :29        NA's   :29                         NA's   :29   
#     minimum       maximum      comment          Conductivity_Mathilde Salinity_Mathilde Oxygen_Mathilde 
#  Min.   : NA   Min.   : NA   Length:29          Min.   :5.621         Min.   :35.03     Min.   :0.1983  
#  1st Qu.: NA   1st Qu.: NA   Class :character   1st Qu.:5.650         1st Qu.:35.31     1st Qu.:0.2014  
#  Median : NA   Median : NA   Mode  :character   Median :5.681         Median :35.58     Median :0.2035  
#  Mean   :NaN   Mean   :NaN                      Mean   :5.690         Mean   :35.53     Mean   :0.2063  
#  3rd Qu.: NA   3rd Qu.: NA                      3rd Qu.:5.730         3rd Qu.:35.80     3rd Qu.:0.2119  
#  Max.   : NA   Max.   : NA                      Max.   :5.756         Max.   :36.07     Max.   :0.2165  
#  NA's   :29    NA's   :29                                                                               
#   AOU_Mathilde     MLD_Mathilde   Phosphates_Mathilde Silicates_Mathilde Temperature_Mathilde
#  Min.   :-8.000   Min.   :22.86   Min.   :0.01499     Min.   :1.092      Min.   :26.63       
#  1st Qu.:-7.789   1st Qu.:27.28   1st Qu.:0.04373     1st Qu.:1.581      1st Qu.:27.13       
#  Median :-7.000   Median :33.36   Median :0.05236     Median :1.751      Median :27.60       
#  Mean   :-4.770   Mean   :34.55   Mean   :0.06404     Mean   :2.225      Mean   :27.56       
#  3rd Qu.:-2.943   3rd Qu.:38.59   3rd Qu.:0.07413     3rd Qu.:2.785      3rd Qu.:28.08       
#  Max.   : 9.037   Max.   :55.86   Max.   :0.18108     Max.   :4.306      Max.   :28.20       
#                                                                                              
#  Nitrates_Mathilde Biomass_phytoplankton__ug_m3 target_target_org_code_code      SST             chla        
#  Min.   :0.02345   Min.   : 3.89                1: 0                        Min.   :26.16   Min.   :0.03409  
#  1st Qu.:0.06461   1st Qu.: 6.98                2: 0                        1st Qu.:26.88   1st Qu.:0.03604  
#  Median :0.11266   Median :14.19                3: 0                        Median :27.47   Median :0.03685  
#  Mean   :0.18992   Mean   :13.63                4:29                        Mean   :27.52   Mean   :0.03821  
#  3rd Qu.:0.28885   3rd Qu.:18.55                5: 0                        3rd Qu.:28.10   3rd Qu.:0.04036  
#  Max.   :0.60107   Max.   :28.37                6: 0                        Max.   :28.63   Max.   :0.04489  
#                                                                             NA's   :1                        
# nitrate     silicate                  target.org.colour  slope.clean     prod.index5_2Chl_nitr_MLD_mutipl
# Min.   :0   Min.   :1.000   darkorange            : 0      Min.   :-1.396   Min.   :0.4452                  
# 1st Qu.:0   1st Qu.:1.000   detritus + zooplankton: 0      1st Qu.:-1.170   1st Qu.:1.1186                  
# Median :0   Median :2.000   dodgerblue            : 0      Median :-1.098   Median :2.1294                  
# Mean   :0   Mean   :1.552   Phytoplankton         :29      Mean   :-1.137   Mean   :1.9148                  
# 3rd Qu.:0   3rd Qu.:2.000   purple                : 0      3rd Qu.:-1.081   3rd Qu.:2.4340                  
# Max.   :0   Max.   :2.000                                  Max.   :-1.011   Max.   :3.6357                  
# 
# prod.index4_2Chl_nitr_MLD  prod.index1        prod.index2     prod.index3_2Chl_nitr prod.index4_Chl_nitr
# Min.   :-6.104            Min.   :0.006677   Min.   :0.4677   Min.   :0.1202        Min.   :0.07882     
# 1st Qu.:-5.516            1st Qu.:0.015545   1st Qu.:0.5973   1st Qu.:0.2201        1st Qu.:0.15824     
# Median :-5.421            Median :0.152684   Median :0.9795   Median :0.6785        Median :0.45060     
# Mean   :-5.258            Mean   :0.760985   Mean   :1.0620   Mean   :0.6574        Mean   :0.50745     
# 3rd Qu.:-4.884            3rd Qu.:0.961760   3rd Qu.:1.4062   3rd Qu.:0.9291        3rd Qu.:0.73595     
# Max.   :-4.440            Max.   :3.917473   Max.   :1.9138   Max.   :1.4366        Max.   :1.26344     
# NA's   :12         NA's   :12       NA's   :12            NA's   :12          
# new_slopes     rob_reg__slopes   p_values_olsr_slopes r.square.olsr    ratio_slopes_rr_olsr
# Min.   :-1.396   Min.   :-1.3825   Min.   :0.000e+00    Min.   :0.5703   Min.   :0.8691      
# 1st Qu.:-1.170   1st Qu.:-1.1785   1st Qu.:0.000e+00    1st Qu.:0.6826   1st Qu.:0.9749      
# Median :-1.098   Median :-1.1277   Median :7.020e-06    Median :0.8134   Median :0.9947      
# Mean   :-1.137   Mean   :-1.1337   Mean   :2.632e-03    Mean   :0.8181   Mean   :0.9972      
# 3rd Qu.:-1.081   3rd Qu.:-1.0578   3rd Qu.:1.879e-03    3rd Qu.:0.9366   3rd Qu.:1.0243      
# Max.   :-1.011   Max.   :-0.9064   Max.   :2.341e-02    Max.   :0.9831   Max.   :1.1155    


par( old.par)

lnumb <- 12
length(( phyt.EQU.5[lnumb,])) # 125 cols

phyt.EQU.5NBSSmatrix <- as.matrix(phyt.EQU.5[,37:92])
dim(phyt.EQU.5NBSSmatrix)
# 29 rows , 56 columns of Biovolume NBSS values 


lnumb <- 12
plot (log10( phyt.EQU.5NBSSmatrix[lnumb,]) ~ X_vectormm3log)


# plot all points and lm  ----------

lnumb <- 5
plot (log10( phyt.EQU.5NBSSmatrix[lnumb,]) ~ X_vectormm3log, main = "EQU",
      xlab = "log10( Biovolume (mm3 ind. -1)) ",
      ylab = "log10(Normalized Biovolume (mm3 m-3 mm-3) ",
      xlim = c(-11, -1), ylim = c(0, 14) ,
      pch = 16, col = "white")
for (lnumb in 1 : nrow(phyt.EQU.5NBSSmatrix) ) 
{    points (log10( phyt.EQU.5NBSSmatrix[lnumb,]) ~ X_vectormm3log ,
             pch = 16, col = alpha("darkgreen", 0.3))
  abline( rlm(log10( phyt.EQU.5NBSSmatrix[lnumb,]) ~ X_vectormm3log), col =alpha ("navy", 0.3))
}

# EQU
(median(phyt.EQU.5$rob_reg__slopes) )
# median RR slope: -1.128
(min(phyt.EQU.5$rob_reg__slopes)); (max(phyt.EQU.5$rob_reg__slopes) )
# min - max  RR slope : 
#  -1.382 to  -0.906


# matrix to to vector (transpose), EQU -----------------------  
x1 <- rep( X_vectormm3log, nrow(phyt.EQU.5NBSSmatrix) )
y1 <- c ( t(phyt.EQU.5NBSSmatrix))

# regression line (Rob. regr.)
#plot(log10(y1) ~ x1, xlim = c(-11, -1), ylim = c(0, 14) )
#points(  log10(y1) ~ x1 )
# abline(rlm (log10(y1) ~ x1, xlim = c(-11, -1), ylim = c(0, 14)))
abline(rlm (log10(y1) ~ x1, xlim = c(-11, -1), ylim = c(0, 14)), 
       lwd = 2.5, lty = 2, col = "darkorange" )

# means and medians without considering NAs  
mean (c(4,NA, 18,34)) # NA
median( c(4,NA, 18, 34)  ) #NA

mean_   <- function(...) mean(..., na.rm=T)
median_ <- function(...) median(..., na.rm=T)

mean_ (c(4,NA, 8, 34)) #15.33, OK
median_ (c(4,NA, 8,34)) #8, OK

# apply median by columns

median_vec_EQU <- apply(phyt.EQU.5NBSSmatrix, 2, median_) 
mean_vec_EQU <- apply(phyt.EQU.5NBSSmatrix, 2, mean_) 


lines(  log10(median_vec_EQU) ~ X_vectormm3log,pch = 16, col = "red" , lwd = 2.5)







# BUS ---------------


#first NBSS plots, phytopl. --------------

length(phyt.BUS.5 $Longitude) # 33 Data points, 33 Samples (phytopl.)
summary(phyt.BUS.5)

# X5.4494397918711 X5.75046978753507 X6.05149978325976 X6.35252977892375     slope           Std.Error  
# Min.   : NA      Min.   : NA       Min.   : NA       Min.   : NA       Min.   :-1.4087   Min.   : NA  
# 1st Qu.: NA      1st Qu.: NA       1st Qu.: NA       1st Qu.: NA       1st Qu.:-1.1317   1st Qu.: NA  
# Median : NA      Median : NA       Median : NA       Median : NA       Median :-0.9563   Median : NA  
# Mean   :NaN      Mean   :NaN       Mean   :NaN       Mean   :NaN       Mean   :-1.0133   Mean   :NaN  
# 3rd Qu.: NA      3rd Qu.: NA       3rd Qu.: NA       3rd Qu.: NA       3rd Qu.:-0.8789   3rd Qu.: NA  
# Max.   : NA      Max.   : NA       Max.   : NA       Max.   : NA       Max.   :-0.7730   Max.   : NA  
# NA's   :33       NA's   :33        NA's   :33        NA's   :33                          NA's   :33   
#     minimum       maximum      comment          Conductivity_Mathilde Salinity_Mathilde Oxygen_Mathilde 
#  Min.   : NA   Min.   : NA   Length:33          Min.   :4.078         Min.   :34.86     Min.   :0.2242  
#  1st Qu.: NA   1st Qu.: NA   Class :character   1st Qu.:4.284         1st Qu.:35.04     1st Qu.:0.2498  
#  Median : NA   Median : NA   Mode  :character   Median :4.362         Median :35.10     Median :0.2650  
#  Mean   :NaN   Mean   :NaN                      Mean   :4.357         Mean   :35.13     Mean   :0.2590  
#  3rd Qu.: NA   3rd Qu.: NA                      3rd Qu.:4.392         3rd Qu.:35.18     3rd Qu.:0.2681  
#  Max.   : NA   Max.   : NA                      Max.   :5.006         Max.   :35.64     Max.   :0.2810  
#  NA's   :33    NA's   :33                       NA's   :4             NA's   :4         NA's   :4       
# AOU_Mathilde     MLD_Mathilde   Phosphates_Mathilde Silicates_Mathilde Temperature_Mathilde
# Min.   :-8.000   Min.   :35.80   Min.   :0.09099     Min.   :1.137      Min.   :12.25       
# 1st Qu.:-8.000   1st Qu.:41.30   1st Qu.:0.84982     1st Qu.:2.765      1st Qu.:14.71       
# Median :-7.915   Median :47.57   Median :1.06772     Median :3.001      Median :14.94       
# Mean   :-4.462   Mean   :48.17   Mean   :0.97491     Mean   :3.298      Mean   :15.08       
# 3rd Qu.:-6.509   3rd Qu.:53.40   3rd Qu.:1.14187     3rd Qu.:3.494      3rd Qu.:15.46       
# Max.   :18.134   Max.   :58.19   Max.   :1.32462     Max.   :5.608      Max.   :21.06       
# NA's   :4        NA's   :4       NA's   :4           NA's   :4          NA's   :4           
#  Nitrates_Mathilde  Biomass_phytoplankton__ug_m3 target_target_org_code_code      SST       
#  Min.   : 0.05828   Min.   :  5.01               1: 0                        Min.   :14.55  
#  1st Qu.: 8.15965   1st Qu.: 17.25               2: 0                        1st Qu.:14.93  
#  Median : 8.43219   Median : 21.90               3: 0                        Median :15.60  
#  Mean   : 8.01475   Mean   : 34.13               4:33                        Mean   :15.99  
#  3rd Qu.: 9.24684   3rd Qu.: 33.74               5: 0                        3rd Qu.:16.28  
#  Max.   :10.76386   Max.   :154.06               6: 0                        Max.   :21.24  
#  NA's   :4                                                                                  
# chla            nitrate         silicate                  target.org.colour  slope.clean     
# Min.   :0.04542   Min.   :2.000   Min.   :4.000   darkorange            : 0      Min.   :-1.4087  
# 1st Qu.:0.16085   1st Qu.:4.000   1st Qu.:6.000   detritus + zooplankton: 0      1st Qu.:-1.1317  
# Median :0.19161   Median :4.868   Median :6.958   dodgerblue            : 0      Median :-0.9563  
# Mean   :0.20624   Mean   :4.464   Mean   :6.578   Phytoplankton         :33      Mean   :-1.0133  
# 3rd Qu.:0.27078   3rd Qu.:5.000   3rd Qu.:7.868   purple                : 0      3rd Qu.:-0.8789  
# Max.   :0.35838   Max.   :6.000   Max.   :8.957                                  Max.   :-0.7730  
# 
# prod.index5_2Chl_nitr_MLD_mutipl prod.index4_2Chl_nitr_MLD  prod.index1       prod.index2   
# Min.   :-1.4707                  Min.   :-5.573            Min.   :0.03482   Min.   :1.038  
# 1st Qu.:-1.2963                  1st Qu.:-2.454            1st Qu.:0.14222   1st Qu.:1.536  
# Median :-1.1162                  Median :-2.192            Median :0.18412   Median :1.681  
# Mean   :-0.9144                  Mean   :-2.379            Mean   :0.19535   Mean   :1.693  
# 3rd Qu.:-0.8178                  3rd Qu.:-1.886            3rd Qu.:0.22355   3rd Qu.:1.743  
# Max.   : 2.6868                  Max.   :-1.536            Max.   :0.56587   Max.   :2.371  
# NA's   :4                        NA's   :4                 NA's   :2         NA's   :2      
# prod.index3_2Chl_nitr prod.index4_Chl_nitr   new_slopes      rob_reg__slopes   p_values_olsr_slopes
# Min.   :0.2532        Min.   :0.1392       Min.   :-1.4087   Min.   :-1.3869   Min.   :0.0000000   
# 1st Qu.:0.6445        1st Qu.:0.3359       1st Qu.:-1.1317   1st Qu.:-1.1121   1st Qu.:0.0001946   
# Median :0.7681        Median :0.3959       Median :-0.9563   Median :-0.9970   Median :0.0006695   
# Mean   :0.8082        Mean   :0.4197       Mean   :-1.0133   Mean   :-1.0214   Mean   :0.0016269   
# 3rd Qu.:0.8940        3rd Qu.:0.4676       3rd Qu.:-0.8789   3rd Qu.:-0.8946   3rd Qu.:0.0016761   
# Max.   :1.5338        Max.   :0.7934       Max.   :-0.7730   Max.   :-0.7741   Max.   :0.0089221   
# NA's   :2             NA's   :2                                                                    
# r.square.olsr    ratio_slopes_rr_olsr
# Min.   :0.5070   Min.   :0.9407      
# 1st Qu.:0.5932   1st Qu.:0.9827      
# Median :0.6687   Median :0.9956      
# Mean   :0.6820   Mean   :1.0095      
# 3rd Qu.:0.7487   3rd Qu.:1.0443      
# Max.   :0.9612   Max.   :1.1100 
# 


par( old.par)

lnumb <- 12
length(( phyt.BUS.5[lnumb,])) # 125 cols

phyt.BUS.5NBSSmatrix <- as.matrix(phyt.BUS.5[,37:92])
dim(phyt.BUS.5NBSSmatrix)
# 33 rows , 56 columns of Biovolume NBSS values 


lnumb <- 12
plot (log10( phyt.BUS.5NBSSmatrix[lnumb,]) ~ X_vectormm3log)


# plot all points and lm  ----------

lnumb <- 5
plot (log10( phyt.BUS.5NBSSmatrix[lnumb,]) ~ X_vectormm3log, main = "BUS",
      xlab = "log10( Biovolume (mm3 ind. -1)) ",
      ylab = "log10(Normalized Biovolume (mm3 m-3 mm-3) ",
      xlim = c(-11, -1), ylim = c(0, 14) ,
      pch = 16, col = "white")
for (lnumb in 1 : nrow(phyt.BUS.5NBSSmatrix) ) 
{    points (log10( phyt.BUS.5NBSSmatrix[lnumb,]) ~ X_vectormm3log ,
             pch = 16, col = alpha("darkgreen", 0.3))
  abline( rlm(log10( phyt.BUS.5NBSSmatrix[lnumb,]) ~ X_vectormm3log), col =alpha ("navy", 0.3))
}

# BUS
(median(phyt.BUS.5$rob_reg__slopes) )
# median RR slope: -0.997
(min(phyt.BUS.5$rob_reg__slopes)); (max(phyt.BUS.5$rob_reg__slopes) )
# min - max  RR slope : 
# -1.3868 to -0.774

# matrix to to vector (transpose), BUS -----------------------  
x1 <- rep( X_vectormm3log, nrow(phyt.BUS.5NBSSmatrix) )
y1 <- c ( t(phyt.BUS.5NBSSmatrix))

# regression line (Rob. regr.)
#plot(log10(y1) ~ x1, xlim = c(-11, -1), ylim = c(0, 14) )
#points(  log10(y1) ~ x1 )
# abline(rlm (log10(y1) ~ x1, xlim = c(-11, -1), ylim = c(0, 14)))
abline(rlm (log10(y1) ~ x1, xlim = c(-11, -1), ylim = c(0, 14)), 
       lwd = 2.5, lty = 2, col = "darkorange" )

# means and medians without considering NAs  
mean (c(4,NA, 18,34)) # NA
median( c(4,NA, 18, 34)  ) #NA

mean_   <- function(...) mean(..., na.rm=T)
median_ <- function(...) median(..., na.rm=T)

mean_ (c(4,NA, 8, 34)) #15.33, OK
median_ (c(4,NA, 8,34)) #8, OK

# apply median by columns

median_vec_BUS <- apply(phyt.BUS.5NBSSmatrix, 2, median_) 
mean_vec_BUS <- apply(phyt.BUS.5NBSSmatrix, 2, mean_) 

#points(log10(median_vec_BUS) ~ X_vectormm3log,pch = 16, col = "red")
#points(log10(mean_vec_BUS) ~ X_vectormm3log,pch = 16, col = "navy")
lines(  log10(median_vec_BUS) ~ X_vectormm3log,pch = 16, col = "red" , lwd = 2.5)


# COMPARISONS of 4 regions -----------------------------
# Boxplots and tests (comparisons between 4 regions) ------------

phyt.TSWA.rr.slop <- phyt.TSWA.5$rob_reg__slopes
phyt.CCUS.rr.slop <- phyt.CCUS.5$rob_reg__slopes
phyt.EQU.rr.slop <- phyt.EQU.5$rob_reg__slopes
phyt.BUS.rr.slop <- phyt.BUS.5$rob_reg__slopes

phyt.ALL.Atl.rr.slop <- phytopl.All.Atl.ok5$rob_reg__slopes




# boxplot RR slopes------

boxplot (phyt.TSWA.rr.slop, phyt.EQU.rr.slop, phyt.CCUS.rr.slop, phyt.BUS.rr.slop)

boxplot (phyt.TSWA.rr.slop, phyt.EQU.rr.slop, phyt.CCUS.rr.slop, phyt.BUS.rr.slop,
         phyt.ALL.Atl.rr.slop)


# test for differences in RR slopes------

# some pairwise comparisons...
t.test(phyt.TSWA.rr.slop, phyt.BUS.rr.slop)
# p-value = 2.21e-10

wilcox.test(phyt.TSWA.rr.slop, phyt.BUS.rr.slop)
# p-value = 1.194e-09

wilcox.test(phyt.EQU.rr.slop, phyt.BUS.rr.slop)
# p-value = 0.00031

wilcox.test(phyt.CCUS.rr.slop, phyt.BUS.rr.slop)
# p-value = n.s.

# Subsample for equal "N" 

phyt.TSWA.25rr.slop <- sample (phyt.TSWA.rr.slop, 25)

phyt.CCUS.25rr.slop <- sample (phyt.CCUS.rr.slop, 25)

phyt.EQU.25rr.slop <- sample (phyt.EQU.rr.slop, 25)

phyt.BUS.25rr.slop <- sample (phyt.BUS.rr.slop, 25)


table4areas.rrslopes.n25 <- data.frame(TSWA = phyt.TSWA.25rr.slop,
                                       CCUS = phyt.CCUS.25rr.slop,
                                       EQU = phyt.EQU.25rr.slop,
                                       BUS = phyt.BUS.25rr.slop)


boxplot(table4areas.rrslopes.n25, ylim = c(-1.8, -0.2))


tab_stacked <- stack (table4areas.rrslopes.n25)

# ANOVA
# summary( aov( tab_stacked$values ~ tab_stacked$ind  ))  # p = 3.57e-06 ***

# Permutation test
library(coin)

independence_test(tab_stacked$values ~ tab_stacked$ind)

# p-value = 5.119e-08

# post-hoc tests ------------------------

# PMCMR, kruskal-nemenyi test --------
library(PMCMRplus)

PMCMRplus::kwAllPairsNemenyiTest( tab_stacked$values ~ tab_stacked$ind)

# several samplings (25 random samples from all data) 
#        TSWA    CCUS    EQU    
# CCUS   0.00084 -       -      
#   EQU  0.07924 0.49378 -      
#   BUS  4.2e-05 0.90012 0.15273

#        TSWA    CCUS    EQU    
# CCUS   0.00027 -       -      
#   EQU  0.03998 0.48462 -      
#   BUS  1.8e-06 0.72400 0.06524

#        TSWA    CCUS   EQU   
#  CCUS  4.4e-05 -      -     
#   EQU  0.0064  0.6056 -     
#   BUS  1.6e-07 0.7005 0.0945

# Conclusion:

# TSWA is significantly different from the other regions
# CCUS and EQU are  not signif. different from each other
# BUS is not significantly different from CCUS and  EQU   


boxplot(table4areas.rrslopes.n25, ylim = c(-1.8, -0.2))
text ("a",  x = 1, y = -0.8)
text ("b",  x = 2, y = -0.6)
text ("b",  x = 3, y = -0.6)
text ("b",  x = 4, y = -0.6)








#### 
###
# ZOOPLANKTON  -----------------------



# 5.2 Zooplankton NBSS slopes-------------------------------------------



# Calculate new slopes from maximum to first non-empty bin -----------------


#  Define a function that selects from maximum to last non_empty bin -----------
# and fits a linear model
# Inputs: two vectors of the same length, "bin.means", "raw.NBSS"
# Output:  results  with "NBSS.slope", "NBSS.intercept", 
# "NBSS.p.value.of.Intercept",  "NBSS.p.value.of.Slope", "r-squared", 
# "first.bin.used". "last.bin.used" , "N.of.bins.used" , etc.

# define function NBSS.select.w.lm.MS.A ----------
NBSS.select.w.lm.MS.A   <- function (bin.means = bin.means, raw.NBSS = raw.NBSS) {
  
  # 
    # bin.means = c(2^1, 2^2, 2^3, 2^4, 2^5 , 2^6, 2^7, 2^8, 2^9)
    # raw.NBSS <- c (NA, 10,   28,  3,  0.4,  0.04 ,  NA,  3, NA)  

    
  #  mean individual biomass (g.C ind.-1) in each size class (X-Axis)
  mean.ind.biomass  <- bin.means
  # Comment: a geometric series whose terms are the successive powers of two 
  
  
  #  Normalized Biomass (NBSS)  in each size class (y-Axis) unit: "gC * m-3 * gC-1 * ind."   or simply "ind. m-3"
  NBSS  <- raw.NBSS
  # Comment: each value represents the normalized biomass in each size class
  
  d1_all <- data.frame(mean.ind.biomass, NBSS)

 #plot (log10(mean.ind.biomass), log10(NBSS))  
  # 3. Automatic selection (from the maximum to the last non-empty bin before empty bins occur) ------------------------------
  
  # 3.0 preparations -------------------------------------------------------------
  
  # 3.0.a create log10 data, for convenience --------------------------------------
  z <- log10(NBSS)
  z[z== -Inf] <- NA   # replace "-Inf" with NA
  
  # 3.0.b create an index vector (1,2,3,..) with the positions of the data,  for convenience ----------
  
  d1_index <- 1:(length(z))
  d1_index
  #  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14
  
  # 3.0.b build a data.frame (a table)
  
  d1_all <- data.frame(d1_index, mean.ind.biomass, NBSS, z )
  
  # 3.1 find the maximum 
  
  d_1MAXindex <- which.max(z) # position of the maximum
  
  d_1MAXindex # 3 (the maximum is located at the position 3)
  
  # 3.2 select only the data from the maximum onwards
  
  d1_all.2 <- d1_all[ d1_all$d1_index[d1_all$d1_index > (d_1MAXindex-1) ] ,]
  
  # 3.3 find the last non-empty bin before empty bins occur
  
  NAindex <- which(is.na(d1_all.2$z)) # locate ALL NA values in vector z
  firstNA <- min(NAindex)# locate the first NA value in vector z 
  firstNA # position of first NA value in vector z
  LastNonNA <-firstNA - 1 # position of last non_NA value in vector z
  LastNonNA  # position last non-empty bin before empty bins occur, index as for d1_all.2
  
  # 3.4. select the data (based on automatically selected indices)
  
  d1_all.3 <- d1_all.2[ 1:LastNonNA ,]
  
  # position last non-empty bin before empty bins occur, index as for d1_all.2
  #ALL.LastNonNA <- max(d1_all.3$d1_index) # maximum index 
  # position last non-empty bin before empty bins occur, index as for d1_all.2
  
  
  lm.sel <- lm(log10(d1_all.3$NBSS) ~ log10(d1_all.3$mean.ind.biomass))
  summary(lm.sel)
  coef(lm.sel)
  
# abline(lm.sel)
  
    NBSS.Intercept<- as.numeric(coef(lm.sel)[1]) # NBSS Intercept
  NBSS.slope <- as.numeric(coef(lm.sel)[2] )# NBSS Slope
  
  sum.table <- as.list (summary(lm.sel))
  
  p.values <- summary(lm.sel)$coefficients[,4]  # p-values 
  r.squared <- summary(lm.sel)$r.squared # R-squared
  
  #N.bins.used <- length(d_1MAXindex:ALL.LastNonNA)
  
  
  
  # res <- c(NBSS.slope, NBSS.Intercept, p.values, r.squared, d_1MAXindex, 
  #          ALL.LastNonNA, N.bins.used)
  
  
  
  
  results <- list(slope = NBSS.slope, intercept = NBSS.Intercept, p.values = p.values, 
                  r.squared = r.squared, index.of.maximum = d_1MAXindex, 
                  x.value.of.maximum = d1_all$mean.ind.biomass[d_1MAXindex],
                  y.value.of.maximum =  d1_all$NBSS[d_1MAXindex],
                  index.of.start.bin <- d_1MAXindex, 
                  x.value.of.start.bin = d1_all$mean.ind.biomass[d_1MAXindex]
                  #endpoint = ALL.LastNonNA, 
                 # x.value.of.end.bin = d1_all$mean.ind.biomass[ALL.LastNonNA],
                  #N.bins.used = N.bins.used)
                   )
  
  names(results)
  
  res <- list(results = results, sum.table = sum.table, selected.data = d1_all.3 , all.data = d1_all)
  
  
  
  return(res)
  
  
}


# create matrix to fit slopes ---------

# zoopl.NBSSmatrix ---------

#View(zoopl.All.Atl[35:97])

names(zoopl.All.Atl[37:92])
length(names(zoopl.All.Atl[37:92])) # 56 NBSS data columns



zoopl.NBSSmatrix <- as.matrix(zoopl.All.Atl[,37:92])


zoopl.NBSSmatrix_no_na <-  zoopl.NBSSmatrix[rowSums(is.na(zoopl.NBSSmatrix)) != ncol(zoopl.NBSSmatrix), ]


bin.means <-  exp( X_vectormm3)

X_vectormm3

# first plots

plot(zoopl.NBSSmatrix_no_na [22, ]  ~   X_vectormm3log$V1)

lnumb = sample( 1: nrow(zoopl.NBSSmatrix_no_na) , 1)

plot(log10(zoopl.NBSSmatrix_no_na [lnumb, ])  ~   X_vectormm3log$V1,
     main = paste (lnumb))

abline (lm (  log10(zoopl.NBSSmatrix_no_na [lnumb, ])  ~   X_vectormm3log$V1 ) )

# apply function NBSS.select.w.lm.MS.A-------

results <- NBSS.select.w.lm.MS.A ( 10^X_vectormm3log$V1 , 
                                   zoopl.NBSSmatrix_no_na[lnumb,] )

plot(log10(zoopl.NBSSmatrix_no_na [lnumb , ])  ~    (X_vectormm3log$V1),
     main = paste (lnumb)    )

b =  results$results$slope
#[1]  -0.6459868

a = results$results$intercept
#[1] -0.7477856

abline( a= a , b= b, col = "red")




  raw.NBSSi <-  zoopl.NBSSmatrix[4,]
  raw.NBSSi[is.na(raw.NBSSi)] <- 0
  
  NBSS.select.w.lm.MS.A(as.vector(bin.means), as.vector(raw.NBSSi)) 
  
  
  # define function NBSS.select.w.lm.MS.B ------------------
  # with robust regression, output includes Rsquared, "p" vlues, and rr/olsr ratio  ...
  
  
  ####
#  4 Tasks (Zooplankton NBSS) ----------------------------------------
#  Task 1 --------------------
  # recalculate  all zoopl slopes  with point selection and Robust Regression 
  # apply a function with robust regression, output includes Rsquared, "p" values, and rr/olsr ratio  ...
  # create an output table, by applying the function NBSS.select.w.lm.MS.B 
  # filter for high-quality models only
  
  #  Task 2 --------------------
  # plots (all Atlantic), carbon and  biovolume
  # compare carbon vs   bivolume slopes
  
  #  Task 3 --------------------
  # analyse driving factors for Zoopl. NBSS
  
  #  Task 4 --------------------
  # Zoopl. NBSS by 4 regions
  
  
  
  

# Zoopl. T-S diagram with circle size = NBSS slope -------------------

symbols(zoopl.All.Atl$SST_C_insitu ~ zoopl.All.Atl$Salinity_insitu, main = "circles = NBSS slope, zoopl.", 
        circles = (zoopl.All.Atl$slope.clean * -1), fg= alpha("blue", 0.6), inches=0.09,
        xlim = c(31,38),
        ylim = c(0, 32))


# limit at 20 degress Celsius
rect(36, 27.8, 38, 32) # TW, oligotrophic tropical water (high salinity high temp, low nutr, low chl a)
rect(34.5, 27.8, 36, 32) # Warm Coastal waters, warm, nutrient-rich waters (e.g., productive coastal waters in summer") 
rect(34.5, 20, 38, 27.8)  # mixed warm waters 
rect(31.5, 3, 34, 20) # upwelling, extremely productive, cold waters, temp < 20 deg. celsius, salinity < 34, upwelling
rect(34, 3, 36, 20) # productive, cold waters, temp < 20 deg. celsius, salinity 34 to 36, upwelling
rect(36, 3, 37, 20) # less productive cold waters, salinity > 36


# Pytoopl. T-S diagram with circle size = NBSS slope -------------------

symbols(phytopl.All.Atl$SST_C_insitu ~ phytopl.All.Atl$Salinity_insitu, main = "circles = NBSS slope, phytopl.", 
        circles = (phytopl.All.Atl$slope.clean * -1), fg= alpha("darkgreen", 0.6), inches=0.09,
        xlim = c(31,38),
        ylim = c(0, 32))

# limit at 20 degress Celsius
rect(36, 27.8, 38, 32) # TW, oligotrophic tropical water (high salinity high temp, low nutr, low chl a)
rect(34.5, 27.8, 36, 32) # Warm Coastal waters, warm, nutrient-rich waters (e.g., productive coastal waters in summer") 
rect(34.5, 20, 38, 27.8)  # mixed warm waters 
rect(31.5, 3, 34, 20) # upwelling, extremely productive, cold waters, temp < 20 deg. celsius, salinity < 34, upwelling
rect(34, 3, 36, 20) # productive, cold waters, temp < 20 deg. celsius, salinity 34 to 36, upwelling
rect(36, 3, 37, 20) # less productive cold waters, salinity > 36

# NBSS slopes vs productivity index -------------------------- 

par(opar)

#plot(zoopl.All.Atl$slope.clean ~ log10(1+zoopl.All.Atl$prod.index1), col = alpha("darkgreen", alpha = 0.6))
#plot(zoopl.All.Atl$slope.clean ~ log10(1+zoopl.All.Atl$prod.index2), col = alpha("darkgreen", alpha = 0.6))

#lm1 <- lm(zoopl.All.Atl$slope.clean ~ log10(phytopl.All.Atl$prod.index2))
#summary(lm1)

plot(phytopl.All.Atl$slope.clean ~ (phytopl.All.Atl$Temperature_C_insitu))
lm2 <- lm(phytopl.All.Atl$slope.clean ~ (phytopl.All.Atl$Temperature_C_insitu))
summary(lm2)


lm3 <- lm(phytopl.All.Atl$slope.clean ~ log10(phytopl.All.Atl$prod.index2) * phytopl.All.Atl$Temperature_C_insitu)
summary(lm3)
# With interaction
# Iteration term is significant! But productivity index is not... this not a useful model
# better: without inteaction

lm3 <- lm(phytopl.All.Atl$slope.clean ~ log10(phytopl.All.Atl$prod.index2) +
            phytopl.All.Atl$Temperature_C_insitu)
summary(lm3)


lm4 <- lm(phytopl.All.Atl$slope.clean ~ log10(phytopl.All.Atl$prod.index2) *
            phytopl.All.Atl$Temperature_C_insitu)
summary(lm4)


# 3D plots ---------------

# 3D scatter plot

# linear 3d surface -------------
library(car)
scatter3d(x = phytopl.All.Atl$Temperature_C_insitu, y =  phytopl.All.Atl$slope.clean, 
          z = log10(phytopl.All.Atl$prod.index2), 
          grid = FALSE, fit = "smooth")

# Conclusion: temperature, productivity and phytoplankton NBSS slope are strongly  correlated !

# smooth 3d surface -----------------

library(car)
scatter3d(x = phytopl.All.Atl$Temperature_C_insitu, y =  phytopl.All.Atl$slope.clean, 
          z = log10(phytopl.All.Atl$prod.index2), bg = "black",
          grid = TRUE, fit = "smooth")

# Conclusion: temperature, productivity and phytoplankton NBSS slope are strongly  correlated !


plot(phytopl.All.Atl$slope.clean ~ (phytopl.All.Atl$Temperature_C_insitu))
plot(phytopl.All.Atl$slope.clean ~ (phytopl.All.Atl$Salinity_insitu))
plot(phytopl.All.Atl$slope.clean ~ log10(1+phytopl.All.Atl$Chlorophyll__mg_m3_insitu))
plot(phytopl.All.Atl$slope.clean ~ log10(1+phytopl.All.Atl$Nitrate__uM_L1_insitu))
plot(phytopl.All.Atl$slope.clean ~ log10(1+phytopl.All.Atl$silicate))









# 6. relative importance -------------------
# relaimpo  ------------- 
# detailed anaylis with relaimpo 
# what is more important, temperature or productivity? 

library(relaimpo)

# Best, simple linear model ---------------------------------------
lm3 <- lm(phytopl.All.Atl$slope.clean ~ log10(phytopl.All.Atl$prod.index2) + 
            phytopl.All.Atl$Temperature_C_insitu)
summary(lm3)
# model without interacation
#temperature and productivity are highly significant
# R-squared = 0.35 (model explains  35% of variability)

metrics.lm3 <- calc.relimp(lm3, type = c("lmg", "first"))

metrics.lm3

#                                         lmg     
# log10(phytopl.All.Atl$prod.index2) 0.1473756  
# phytopl.All.Atl$Temperature_C_insitu     0.2073353 

# The productivity index explains 14.7% of the variability in phtyopl. slope
# The Temperature  explains 20.7% of the variability in phtyopl. slope
# in a model without interaction


# Relative importance metrics:  -----------------------
#   
# lmg: aprox. % of the total variability expained within multivariate model
# first: % of the total variability expained by univariate model


# other multiple linear models

lm4 <- lm(phytopl.All.Atl$slope.clean ~  phytopl.All.Atl$Temperature_C_insitu + phytopl.All.Atl$Salinity_insitu)
summary(lm4)


metrics.lm4 <- calc.relimp(lm4, type = c("lmg", "first"))

metrics.lm4















# 6. Quick first plots and linear models  --------------------------------------
 
 
 
 # 6.1  Slope vs SST  , All data -----------------------------------------------
 
 # SST from in situ data
 
 plot(Data.All.Atl$slope ~ Data.All.Atl$SST_C_insitu, 
      col = alpha( "purple", 0.4) , pch = 16)  
 # legend("topleft", c("zoopl." , "pytopl."), 
 #        col = alpha(c("dodgerblue","forestgreen"), 0.8),
 #        text.col = "darkgrey",  pch = c( 16))
 
 
 
 # the plot shows only phyo- and zoopl.
 
 lm1 <-lm( Data.All.Atl$slope ~ Data.All.Atl$SST_C_insitu) # highly sigif. (p = 0.0008), R-squared:  0.009
 summary(lm1) 
abline(lm1, col = "red", lwd = 1.5)
# n = 1173 data pairs
# highly signif. or in situ Data (SST_C_insitu) 


# SST from remote sensing 2002 - 2009
plot(Data.All.Atl$slope ~ Data.All.Atl$SST,
     col = Data.All.Atl$target.org..code, main = "NBSS slope vs remote sensing  SST ")
# includes fish and nekton

lm1b <-lm( Data.All.Atl$slope ~ Data.All.Atl$SST) # not  signif., R-squared:  0.009
summary(lm1b) 
abline(lm1, col = "red")
# n = 903 data pairs


# 6.2  Slope vs Chl a  , All data ----------------------------------------------


# Slope vs Chla  , All data
plot(Data.All.Atl$slope ~ Data.All.Atl$Chlorophyll__mg_m3_insitu,
     col = Data.All.Atl$target.org..code, main = "NBSS slope vs In situ Chl a ")

lm2 <-lm( Data.All.Atl$slope ~ Data.All.Atl$Chlorophyll__mg_m3_insitu) # not  sigif.
summary(lm2) 
abline(lm2, col = "red")


# chla from  remote sensing 2002 - 2009
plot((Data.All.Atl$slope) ~ log10(Data.All.Atl$chla))
lm2log <-lm( Data.All.Atl$slope ~ log10(Data.All.Atl$chla)) 
#  sigif. (p =0.038) , 586 df, R-squared:  0.005, very weak relationship
summary(lm2log) 
abline(lm2log, col = "red")
plot((Data.All.Atl$slope) ~ log10(Data.All.Atl$chla), 
     col = Data.All.Atl$target.org..code, main = "NBSS slope vs In situ Chl a ")
abline(lm2log, col = "red")
# n = 889, includes nekton and fish data


# chla from  in situ data
plot((Data.All.Atl$slope) ~ log10(Data.All.Atl$Chlorophyll__mg_m3_insitu))
lm2log.b <-lm( Data.All.Atl$slope ~ log10(Data.All.Atl$Chlorophyll__mg_m3_insitu)) 
# highly sigif. (p < 10-16) , 586 df, R-squared:  0.108
summary(lm2log.b) 
abline(lm2log.b, col = "red")
# very very strong log-linear  relationship!!!
plot((Data.All.Atl$slope) ~ log10(Data.All.Atl$Chlorophyll__mg_m3_insitu), 
     col = Data.All.Atl$target.org..code, main = "NBSS slope vs In situ Chl a ")
abline(lm2log.b, col = "red")
# n = 586, only phyto- and zooplankton
# highly sigif. (p < 10-16) , 586 df, R-squared:  0.108


# 6.3  Multiple linear models (Slope vs SST and Chla ) -------------------------

# include logChla variables in data.frame
Data.All.Atl$logChla.rem.sens <- log10(Data.All.Atl$chla)
Data.All.Atl$logChla.in.situ <- log10(Data.All.Atl$Chlorophyll__mg_m3_insitu)

# manual selection
# Chla and SST from in situ data 

# 6.3a no interaction -----------------------------------------------------------------
lm.sst.logchla.no.int <- lm(Data.All.Atl$slope ~ Data.All.Atl$logChla.in.situ + Data.All.Atl$SST_C_insitu)
summary(lm.sst.logchla.no.int)
#  model is great  (p-value: < 2.2e-16),  Chl a is highly  sign!!!

# 6.3b with interaction -------------------------------------------------------------
lm.sst.logchla.w.int <- lm(Data.All.Atl$slope ~ Data.All.Atl$logChla.in.situ *
                             Data.All.Atl$SST_C_insitu)
summary(lm.sst.logchla.w.int)
#  Interaction is not  signif. at all! (p = 0.54)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                      -0.557205   0.053766 -10.363  < 2e-16 ***
#   Data.All.Atl$logChla.in.situ                      0.202991   0.102025   1.990   0.0471 *  
#   Data.All.Atl$SST_C_insitu                              -0.026292   0.003109  -8.456 2.31e-16 ***
#   Data.All.Atl$logChla.in.situ:Data.All.Atl$SST_C_insitu -0.003262   0.005318  -0.613   0.5398    
# --


# 6.3c with target organisms as covariate, with interaction  -------------------------
# Chla and SST from in situ data 

# lm.sst.logchla.w.int.covar <- lm(Data.All.Atl$slope ~ Data.All.Atl$logChla.in.situ * 
#                              Data.All.Atl$SST_C_insitu + Data.All.Atl$target_organisms )
# summary(lm.sst.logchla.w.int.covar)
# # covariate is signif! #  Interaction  chla vs sst is not  signif. at all! (p = 0.54)

# 6.3dMost compex model, with target organism as covariate, also  interaction with covariate  -------------------------
# Chla and SST from in situ data 
# 
# lm.sst.logchla.w.int.covar.b <- lm(Data.All.Atl$slope ~ Data.All.Atl$logChla.in.situ * 
#                                    Data.All.Atl$SST_C_insitu * Data.All.Atl$target_organisms )
# summary(lm.sst.logchla.w.int.covar.b)
# # covariate is signif! #  Interaction  cha vs sst is not  signif. at all! (p = 0.54)

# Data.All.Atl$logChla.in.situ , p =  0.0116
# Data.All.Atl$SST_C_insitu       , p =  8.95e-16
# only Chl a and SST are signif. ! no covarites and no interaction terms, too complex! 

# 6.4 automatic selection of variables ----------------------------------------------
# (backward selection, package MASS, relaimpo) --------------------------------------


#Data.lin.mod <- Data.All.Atl[,c(8,22:35,92)]
# Data.lin.mod <- Data.All.Atl[,c(22,35,92)]
# Data.lin.mod <- Data.All.Atl[,c(8,22,24,25,26,27,28,29,30,31,32,33,35,35,92)]
# Data.lin.mod.insitu <- Data.All.Atl[,c(8,22,24,27,28,29, 31, 32, 34, 92)]
Data.lin.mod.insitu <- Data.All.Atl[,c(8, 31, 32, 34,  92, 93,94)]
Data.lin.mod.remote.sens <- Data.All.Atl[,c(8, 22,24,25,26,27,28,29,  92, 93)]


head(Data.lin.mod.remote.sens)

# target_organisms      SST salinity nitrate silicate slope   logChla
# 1 mesopelagic fish 20.00000      36       0        1          0 -1.504109
# 2 mesopelagic fish 21.00000      36       0        1         NA -1.483266
# 3 mesopelagic fish 22.00000      36       2        1         NA -1.388659

Data.lin.mod.na.omit <- na.omit(Data.lin.mod.remote.sens) # remove lines with NAs

head(Data.lin.mod.na.omit)
summary(Data.lin.mod.na.omit)
length(Data.lin.mod.na.omit$slope) #  n = 853 useful rows
# View(Data.lin.mod.na.omit)

# Find the best model for NBSS
# let's find the most useful model (lowest R adjust, best AIC)
# Stepwise Regression
# library(MASS)
# fit <- lm(slope ~ . , data = Data.lin.mod.na.omit)
# step <- stepAIC(fit, direction="backward")
# step$anova # display results 

#plot(Data.lin.mod.na.omit$slope ~Data.lin.mod.na.omit$salinity)


# Finding the best linear model
# "Selecting a subset of predictor variables from a larger set 
# (e.g., stepwise selection) is a controversial topic. You can perform stepwise selection (forward, backward, both) using the stepAIC( ) function from the MASS package. stepAIC( ) performs stepwise model selection by exact AIC. 
# "forward" method does not work here (retains all variables)
# same results are obtained for "backward" and "both" methods", OK. 

# Salinty is an indicator of upwelling and productivity, low salinity ( 33 to 34) in upwelling regions hiogh salit in wester tropcl atlanmtic!)

# best model (overall):
# slope ~ target_organisms + SST + salinity + Temperature_C_insitu

# best model (in situ data, 317  data rows only):
# Initial Model:
#   slope ~ target_organisms + Temperature_C_insitu + Salinity_insitu + Chlorophyll__mg_m3_insitu
# 
# Final Model:
#   slope ~ target_organisms + Temperature_C_insitu + Salinity_insitu

# Salinity_insitu is an indicator of upwelling and productivity, low salinity ( 33 to 34) in upwelling regions hiogh salit in wester tropcl atlanmtic!)
# Chl a does not make it into the best model (in situ data)!


# best model (remote sensing data, n = 853 stations = 853 useful lines of data):
# Initial Model:
#   slope ~ target_organisms + SST + salinity + chla + nitrate + 
#   silicate
# 
# Final Model:
#   slope ~ target_organisms + SST + salinity + nitrate

attach(Data.lin.mod.na.omit)

# lmfull.remote.sens <- lm(Data.lin.mod.na.omit$slope ~
#                            Data.lin.mod.na.omit$target_organisms + 
#                            Data.lin.mod.na.omit$SST_model + Data.lin.mod.na.omit$salinity + Data.lin.mod.na.omit$nitrate_model)
# 
# 
# summary(lmfull.remote.sens)
# 
# # relaimpo  ------------- 
# # detailed anaylis with relaimpo 
# library(relaimpo)
# 
# metricsd15N <- calc.relimp(m_BEST_d15n2, type = c("lmg", "first"))
# 
# metricsd15N
# 
# # Relative importance metrics: 
# #   
# # lmg: aprox. % of the total variability expained within multivariate model
# # first: % of the total variability expained by univariate model
# 


# III. Zooplankton ----------------------

# recalculate slopes --------

dim(zoopl.All.Atl)

zoopl.All.Atl$Gear <- as.factor(zoopl.All.Atl$Gear)

attach(zoopl.All.Atl)

summary(zoopl.All.Atl$Gear)
length(zoopl.All.Atl$Gear)# 831 zooplankton samples


# III.1 Calculate slopes

# first NBSS plots ---------

# ?replace

head (zoopl.All.Atl[,37:92])

zoopl.nbssmatrix <- (zoopl.All.Atl[,37:92])
#zoopl.nbssmatrix <- as.numeric(zoopl.nbssmatrix)

 #View(zoopl.nbssmatrix)
zoopl.LOGnbssmatrix <- zoopl.nbssmatrix 
 
zoopl.LOGnbssmatrix[is.na(zoopl.LOGnbssmatrix)] <- 0

dim(zoopl.LOGnbssmatrix) # 831 rows, 56 cols

zoopl.LOGnbssmatrix <- sapply( zoopl.LOGnbssmatrix, as.numeric )

zoopl.LOGnbssmatrix <- log10(zoopl.LOGnbssmatrix)


zoopl.LOGnbssmatrix[zoopl.LOGnbssmatrix == "-Inf"] <- NA


X_vector

plot (zoopl.LOGnbssmatrix[22,1:56] ~ X_vector$V1)



for(w in 1:56 )

{ 
  plot ((zoopl.LOGnbssmatrix[w,1:56]) ~ X_vector$V1, xlim =c(-3,3) )
}


## IV. NBSS slope and intecepts ------------------- 

# IV.b  manual selection from x values--------------------------------------------------------
# manual selection of points above and below pre-defined x-values

# Data for analysis

mean.ind.biomass <- X_vector
NBSS <- zoopl.LOGnbssmatrix[14,1:56]
d1_all <- data.frame(mean.ind.biomass = mean.ind.biomass, NBSS = NBSS)


# define lower limit ------------------------------------------------------------

lower.lim <- -0.2 

# define upper limit ----------------------------------------------------------

upper.lim <-  1.2

# Select points between lower and upper limit ----------------------------------

d1_selected <- subset (d1_all, mean.ind.biomass < upper.lim)
d1_selected <- subset (d1_selected,  mean.ind.biomass > lower.lim)

attach(d1_selected)


# I.2.c plot NBSS showing  manual selection and linear model --------------------------------------------------------------------

plot((NBSS) ~ (mean.ind.biomass$V1), col = "lightblue",pch = 16, 
     main = "log-log NBSS plot, manual selection",) # log-log NBSS plot

points((NBSS) ~ (mean.ind.biomass$V1), col = "darkorange", pch = 16)
lm1 <- lm(NBSS ~ (mean.ind.biomass$V1))
abline (lm1, lwd = 0.8, col = "red")
summary(lm1)

#rlm
rlm1 <- rlm(NBSS ~ (mean.ind.biomass$V1))
abline (rlm1, lwd = 0.8, col = "green")
summary(rlm1)

# rlm is much better!

# loop for plots ---------------
dim (zoopl.LOGnbssmatrix)
# 
# for (w  in 1:831){
#   mean.ind.biomass <- X_vector
#   NBSS <- zoopl.LOGnbssmatrix[w,1:56]
#   d1_all <- data.frame(mean.ind.biomass = mean.ind.biomass, NBSS = NBSS)
#   
#   
#   # define lower limit ------------------------------------------------------------
#   
#   lower.lim <- -0.2 
#   
#   # define upper limit ----------------------------------------------------------
#   
#   upper.lim <-  1.2
#   
#   # Select points between lower and upper limit ----------------------------------
#   
#   d1_selected <- subset (d1_all, mean.ind.biomass < upper.lim)
#   d1_selected <- subset (d1_selected,  mean.ind.biomass > lower.lim)
#   
#   attach(d1_selected)
#   
#   
#   # I.2.c plot NBSS showing  manual selection and linear model --------------------------------------------------------------------
#   
#   plot((NBSS) ~ (mean.ind.biomass$V1), col = "lightblue",pch = 16, 
#        main = "log-log NBSS plot, manual selection",) # log-log NBSS plot
#   
#   points((NBSS) ~ (mean.ind.biomass$V1), col = "darkorange", pch = 16)
#   lm1 <- lm(NBSS ~ (mean.ind.biomass$V1))
#   abline (lm1, lwd = 0.8, col = "red")
#   summary(lm1)
#   
#   #rlm
#   rlm1 <- rlm(NBSS ~ (mean.ind.biomass$V1))
#   abline (rlm1, lwd = 0.8, col = "green")
#   summary(rlm1)
#   
#   # rlm is much better!
#   
#   
# }
# 

# IV.c  automatic selection from max to below first zero --------------------------------------------------------
# automatic selection selection of points above max and below first zero

# Data for analysis

mean.ind.biomass <- X_vector
NBSS <- zoopl.LOGnbssmatrix[w,1:56]
d1_all <- data.frame(mean.ind.biomass = mean.ind.biomass, NBSS = NBSS)


# define  limits ------------------------------------------------------------


z <- (NBSS)
z[z== -Inf] <- NA   # replace "-Inf" with NA

z[z== 0] <- NA   # replace "-0" with NA

z

# I.3.0.b create an index vector (1,2,3,..) with the positions of the data,  for convenience ----------

d1_index <- 1:(length(z))
d1_index
#  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14

# I.3.0.b build a data.frame (a table)

d1_all <- data.frame(d1_index, mean.ind.biomass, NBSS, z )

head(d1_all)
# 
# d1_index mean.ind.biomass NBSS           z
# 1         1                   2 0.0e+00          NA
# 2         2                   4 0.0e+00          NA
# 3         3                   8 1.2e+02  2.07918125
# 4         4                  16 7.8e+02  2.89209460
# 5         5                  32 1.0e+03  3.00000000
# 6         6                  64 8.0e+01  1.90308999
# 7         7                 128 1.2e+01  1.07918125
# 8         8                 256 8.0e-01 -0.09691001
# 9         9                 512 1.3e-01 -0.88605665
# 10       10                1024 0.0e+00          NA
# 11       11                2048 9.0e-03 -2.04575749
# 12       12                4096 0.0e+00          NA
# 13       13                8192 7.0e-03 -2.15490196
# 14       14               16384 0.0e+00          NA
# 

# I.3.1 find the maximum 

d_1MAXindex <- which.max(z) # position of the maximum

d_1MAXindex # 34 (the maximum is located at the position 5)


# I.3.2 select only the data from the maximum onwards

d1_all.2 <- d1_all[ d1_all$d1_index[d1_all$d1_index > (d_1MAXindex - 1) ] ,]

d1_all.2

# d1_index mean.ind.biomass NBSS           z
# 5         5                  32 1.0e+03  3.00000000
# 6         6                  64 8.0e+01  1.90308999
# 7         7                 128 1.2e+01  1.07918125
# 8         8                 256 8.0e-01 -0.09691001
# 9         9                 512 1.3e-01 -0.88605665
# 10       10                1024 0.0e+00          NA
# 11       11                2048 9.0e-03 -2.04575749
# 12       12                4096 0.0e+00          NA
# 13       13                8192 7.0e-03 -2.15490196
# 14       14               16384 0.0e+00          NA



# I.3.3 find the last non-empty bin before empty bins occur

NAindex <- which(is.na(d1_all.2$z)) # locate ALL NA values in vector z
firstNA <- min(NAindex)# locate the first NA value in vector z 
firstNA # position of first NA value in vector z
LastNonNA <-firstNA - 1 # position of last non_NA value in vector z
LastNonNA  # position last non-empty bin before empty bins occur, index as for d1_all.2


# I.3.4. select the data (based on automatically selected indices)

d1_all.3 <- d1_all.2[ 1:LastNonNA ,]

d1_all.3

#View(d1_all.3)

# 
# d1_index          V1       NBSS          z
# X.0.270130125744547       34 -0.27013013  1.5106716  1.5106716
# X0.0308998699194346       35  0.03089987  0.7196874  0.7196874
# X0.33192986558341         36  0.33192987  0.5902346  0.5902346
# X0.632959861247397        37  0.63295986  0.9479307  0.9479307
# X0.933989856911378        38  0.93398986  0.5744530  0.5744530
# X1.23501985257536         39  1.23501985  0.2290604  0.2290604
# X1.53604984823934         40  1.53604985  0.2145562  0.2145562
# X1.83707984390332         41  1.83707984 -0.2151431 -0.2151431
# X2.1381098395673          42  2.13810984 -0.4123652 -0.4123652

# attach(d1_all.3)




# plot the automatically selected data ------------------------------------------


plot((d1_all.3$NBSS) ~ (d1_all.3$V1), col = "darkorange",pch = 16, 
     main = "log-log NBSS plot, automatic selection",) # log-log NBSS plot

lm1 <- lm((d1_all.3$NBSS) ~ (d1_all.3$V1))
# abline (lm1, lwd = 0.8, col = "red")
# summary(lm1)

#rlm
rlm1 <- rlm(d1_all.3$NBSS ~ d1_all.3$V1)
# abline (rlm1, lwd = 0.8, col = "green")
# summary(rlm1)

# rlm is  better!

## LOOP ## ------------------
## loop for plots ---------------

dim (zoopl.LOGnbssmatrix) # 831  rows , 56 cols
# 
#prepare output table

outp.table <- data.frame(   zoopl.All.Atl,
  SLOPE.LM.aut.sel = rep(NA, 831),
INTERC.LM.aut.sel= rep(NA, 831),
R_SQ.LM.aut.sel = rep(NA, 831), 
p_VALUE.LM.aut.sel = rep(NA, 831),   
SLOPE.RLM.aut.sel = rep(NA, 831),
INTERC.RLM.aut.sel = rep(NA, 831)  )

dim(outp.table)

# View(outp.table)

# w = 1
# 
#  for (w  in 1:831){
#    
#    # Data for analysis
#    
#    mean.ind.biomass <- X_vector
#    NBSS <- zoopl.LOGnbssmatrix[w,1:56]
#    d1_all <- data.frame(mean.ind.biomass = mean.ind.biomass, NBSS = NBSS)
#    
#    
#    # define  limits ------------------------------------------------------------
#    
#    
#    z <- (NBSS)
#    z[z== -Inf] <- NA   # replace "-Inf" with NA
#    
#    z[z== 0] <- NA   # replace "-0" with NA
#    
#    z
#    
#    # I.3.0.b create an index vector (1,2,3,..) with the positions of the data,  for convenience ----------
#    
#    d1_index <- 1:(length(z))
#    d1_index
#    #  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14
#    
#    # I.3.0.b build a data.frame (a table)
#    
#    d1_all <- data.frame(d1_index, mean.ind.biomass, NBSS, z )
#    
#    head(d1_all)
#    # 
#    # d1_index mean.ind.biomass NBSS           z
#    # 1         1                   2 0.0e+00          NA
#    # 2         2                   4 0.0e+00          NA
#    # 3         3                   8 1.2e+02  2.07918125
#    # 4         4                  16 7.8e+02  2.89209460
#    # 5         5                  32 1.0e+03  3.00000000
#    # 6         6                  64 8.0e+01  1.90308999
#    # 7         7                 128 1.2e+01  1.07918125
#    # 8         8                 256 8.0e-01 -0.09691001
#    # 9         9                 512 1.3e-01 -0.88605665
#    # 10       10                1024 0.0e+00          NA
#    # 11       11                2048 9.0e-03 -2.04575749
#    # 12       12                4096 0.0e+00          NA
#    # 13       13                8192 7.0e-03 -2.15490196
#    # 14       14               16384 0.0e+00          NA
#    # 
#    
#    # I.3.1 find the maximum 
#    
#    d_1MAXindex <- which.max(z) # position of the maximum
#    
#    d_1MAXindex # 34 (the maximum is located at the position 5)
#    
#    
#    # I.3.2 select only the data from the maximum onwards
#    
#    d1_all.2 <- d1_all[ d1_all$d1_index[d1_all$d1_index > (d_1MAXindex - 1) ] ,]
#    
#    d1_all.2
#    
#    # d1_index mean.ind.biomass NBSS           z
#    # 5         5                  32 1.0e+03  3.00000000
#    # 6         6                  64 8.0e+01  1.90308999
#    # 7         7                 128 1.2e+01  1.07918125
#    # 8         8                 256 8.0e-01 -0.09691001
#    # 9         9                 512 1.3e-01 -0.88605665
#    # 10       10                1024 0.0e+00          NA
#    # 11       11                2048 9.0e-03 -2.04575749
#    # 12       12                4096 0.0e+00          NA
#    # 13       13                8192 7.0e-03 -2.15490196
#    # 14       14               16384 0.0e+00          NA
#    
#    
#    
#    # I.3.3 find the last non-empty bin before empty bins occur
#    
#    NAindex <- which(is.na(d1_all.2$z)) # locate ALL NA values in vector z
#    firstNA <- min(NAindex)# locate the first NA value in vector z 
#    firstNA # position of first NA value in vector z
#    LastNonNA <-firstNA - 1 # position of last non_NA value in vector z
#    LastNonNA  # position last non-empty bin before empty bins occur, index as for d1_all.2
#    
#    
#    # I.3.4. select the data (based on automatically selected indices)
#    
#    d1_all.3 <- d1_all.2[ 1:LastNonNA ,]
#    
#    d1_all.3
#    
#    #View(d1_all.3)
#    
#    # 
#    # d1_index          V1       NBSS          z
#    # X.0.270130125744547       34 -0.27013013  1.5106716  1.5106716
#    # X0.0308998699194346       35  0.03089987  0.7196874  0.7196874
#    # X0.33192986558341         36  0.33192987  0.5902346  0.5902346
#    # X0.632959861247397        37  0.63295986  0.9479307  0.9479307
#    # X0.933989856911378        38  0.93398986  0.5744530  0.5744530
#    # X1.23501985257536         39  1.23501985  0.2290604  0.2290604
#    # X1.53604984823934         40  1.53604985  0.2145562  0.2145562
#    # X1.83707984390332         41  1.83707984 -0.2151431 -0.2151431
#    # X2.1381098395673          42  2.13810984 -0.4123652 -0.4123652
#    
#    # attach(d1_all.3)
#    
#    
#    
#    
#    # plot the automatically selected data ------------------------------------------
#    
#    
#    plot((d1_all.3$NBSS) ~ (d1_all.3$V1), col = "darkorange",pch = 16, 
#         main = "log-log NBSS plot, automatic selection",) # log-log NBSS plot
#    
#     lm1 <- lm((d1_all.3$NBSS) ~ (d1_all.3$V1))
#    # abline (lm1, lwd = 0.8, col = "red")
#    # summary(lm1)
#    # 
#    outp.table$SLOPE.LM.aut.sel[w] <-  SLOPE.LM <-  as.numeric (lm1$coefficients[2])
#    
#     outp.table$INTERC.LM.aut.sel[w] <-  INTERC.LM <-  as.numeric (lm1$coefficients[1])
#    
#    outp.table$R_SQ.LM.aut.sel[w] <-   R_SQ.LM <- summary(lm1)$r.squared
#    
#    outp.table$p_VALUE.LM.aut.sel[w] <-   p_VALUE.LM <- as.numeric( summary(lm1)$coefficients[2,4] )
#    
#    
#   
#    
#    #rlm
#    rlm1 <- rlm(d1_all.3$NBSS ~ d1_all.3$V1)
#    # abline (rlm1, lwd = 0.8, col = "green")
#    # summary(rlm1)
#    # 
#    outp.table$SLOPE.RLM.aut.sel[w] <- SLOPE.RLM <-  as.numeric (rlm1$coefficients[2])
#    
#    outp.table$INTERC.RLM.aut.sel[w] <-  INTERC.RLM <-  as.numeric (rlm1$coefficients[1])
#   
#     
#   
#               
#    
#    # rlm is  better!
#    
#    
# }
# 
# head(outp.table)
# 
# write.table(outp.table , file = "slopes_Zoopl_autom.sel.csv")

#write.table(outp.table , file = "slopes_Zoopl_autom.sel_11_OK.csv")

## Zooplankton NBSS slopes, automatic selction ------------------------
# Analyse results ----------------------------

#NBSS_Zoopl <- read.csv("~/Papers/000 -Paper_Fish_Zoopl_Size_Spectra_Brazil_Africa/slopes_Zoopl_autom.sel8.csv", sep="")

NBSS_Zoopl  <- read.csv("~/Papers/000 -Paper_Fish_Zoopl_Size_Spectra_Brazil_Africa/slopes_Zoopl_autom.sel_11_OK.csv", sep="")



summary(NBSS_Zoopl)

#write.table(NBSS_Zoopl , file = "slopes_Zoopl_autom.sel_OK.csv")



# clean results (bad "p" values, slopes below -3 )-------------------------------
# delete slopes and intercepts with bad "p" values 

hist(NBSS_Zoopl$SLOPE.LM.aut.sel)
hist(NBSS_Zoopl$SLOPE.RLM.aut.sel)
plot(NBSS_Zoopl$SLOPE.LM.aut.sel, NBSS_Zoopl$SLOPE.RLM.aut.sel)

# delete slopes and intercepts with bad "p" values 
NBSS_Zoopl$SLOPE.LM.aut.sel[NBSS_Zoopl$p_VALUE.LM.aut.sel > 0.05] <- NA
NBSS_Zoopl$SLOPE.RLM.aut.sel[NBSS_Zoopl$p_VALUE.LM.aut.sel > 0.05] <- NA
NBSS_Zoopl$INTERC.LM.aut.sel[NBSS_Zoopl$p_VALUE.LM.aut.sel > 0.05] <- NA
NBSS_Zoopl$INTERC.RLM.aut.sel[NBSS_Zoopl$p_VALUE.LM.aut.sel > 0.05] <- NA

hist(NBSS_Zoopl$SLOPE.LM.aut.sel)
hist(NBSS_Zoopl$SLOPE.RLM.aut.sel)
plot(NBSS_Zoopl$SLOPE.LM.aut.sel, NBSS_Zoopl$SLOPE.RLM.aut.sel)

# delete slopes and intercepts with slope < -3
NBSS_Zoopl$SLOPE.LM.aut.sel[NBSS_Zoopl$SLOPE.LM.aut.sel < -3] <- NA
NBSS_Zoopl$SLOPE.RLM.aut.sel[NBSS_Zoopl$SLOPE.RLM.aut.sel < -3] <- NA
NBSS_Zoopl$INTERC.LM.aut.sel[NBSS_Zoopl$SLOPE.LM.aut.sel < -3] <- NA
NBSS_Zoopl$INTERC.RLM.aut.sel[NBSS_Zoopl$SLOPE.RLM.aut.sel < -3] <- NA

hist(NBSS_Zoopl$SLOPE.LM.aut.sel)
hist(NBSS_Zoopl$SLOPE.RLM.aut.sel)
plot(NBSS_Zoopl$SLOPE.LM.aut.sel, NBSS_Zoopl$SLOPE.RLM.aut.sel)

# check with previous slope estimate (all sizes)

plot(NBSS_Zoopl$SLOPE.LM.aut.sel, NBSS_Zoopl$slope.clean)


plot(NBSS_Zoopl$SLOPE.RLM.aut.sel, NBSS_Zoopl$slope.clean)


# III. ZOOPL slopes - Analyse  slope vs Temp, Chl. -------------

attach(NBSS_Zoopl)

prod.index4_2Chl_nitr_MLD <- (2*log10(NBSS_Zoopl$chla_model) +
                              log10(NBSS_Zoopl$Nitrates_Mathilde)+
                                log10(1/NBSS_Zoopl$MLD) )
prod.index4_2Chl_nitr_MLD[prod.index4_2Chl_nitr_MLD== "-Inf"] <- NA


hist(2* log10(NBSS_Zoopl$chla_model))
hist( log10(NBSS_Zoopl$Nitrates_Mathilde) )
hist(log10(1/NBSS_Zoopl$MLD))
hist(prod.index4_2Chl_nitr_MLD)

plot(prod.index4_2Chl_nitr_MLD ~ log10(NBSS_Zoopl$primprod))
cor.test(prod.index4_2Chl_nitr_MLD , log10(NBSS_Zoopl$primprod))
# r = 0.7291 PP vs Productivity index

plot(log10(NBSS_Zoopl$chla) ~ log10(NBSS_Zoopl$primprod))
cor.test( log10(NBSS_Zoopl$chla) , log10(NBSS_Zoopl$primprod) )
# r= 0.935645

plot(log10(NBSS_Zoopl$primprod) ~  log10(NBSS_Zoopl$MLD))
cor.test( log10(NBSS_Zoopl$MLD) , log10(NBSS_Zoopl$primprod) )
# r= 0.935645

# NBSS_Zoopl$SLOPE.RLM.aut.sel[SLOPE.RLM.aut.sel == 0] <- NA

#plot(log10(NBSS_Zoopl$SLOPE.RLM.aut.sel) ~  log10(NBSS_Zoopl$chla_model))
cor.test( log10(NBSS_Zoopl$MLD) , log10(NBSS_Zoopl$primprod) )
# r= 0.935645




NBSS_Zoopl$prod.index4_2Chl_nitr_MLD <- prod.index4_2Chl_nitr_MLD
# ZOOPL T-S diagram with circle size = NBSS slope -------------------

symbols(NBSS_Zoopl$SST_C_insitu ~ NBSS_Zoopl$Salinity_insitu, main = "circles = NBSS slope, zoopl.", 
        circles = (NBSS_Zoopl$SLOPE.RLM.aut.sel * -1), fg= alpha("blue", 0.6), inches=0.09,
        xlim = c(31,38),
        ylim = c(0, 32))



# limit at 20 degress Celsius
rect(36, 27.8, 38, 32) # TW, oligotrophic tropical water (high salinity high temp, low nutr, low chl a)
rect(34.5, 27.8, 36, 32) # Warm Coastal waters, warm, nutrient-rich waters (e.g., productive coastal waters in summer") 
rect(34.5, 20, 38, 27.8)  # mixed warm waters 
rect(31.5, 3, 34, 20) # upwelling, extremely productive, cold waters, temp < 20 deg. celsius, salinity < 34, upwelling
rect(34, 3, 36, 20) # productive, cold waters, temp < 20 deg. celsius, salinity 34 to 36, upwelling
rect(36, 3, 37, 20) # less productive cold waters, salinity > 36


# Zoopl. T-S diagram with circle size = NBSS slope -------------------

symbols(NBSS_Zoopl$SST_C_insitu ~ NBSS_Zoopl$Salinity_insitu, main = "circles = NBSS slope, Zooplpl.", 
        circles = (NBSS_Zoopl$SLOPE.RLM.aut.sel * -1), fg= alpha("darkgreen", 0.6), inches=0.09,
        xlim = c(31,38),
        ylim = c(0, 32))

# limit at 20 degress Celsius
rect(36, 27.8, 38, 32) # TW, oligotrophic tropical water (high salinity high temp, low nutr, low chl a)
rect(34.5, 27.8, 36, 32) # Warm Coastal waters, warm, nutrient-rich waters (e.g., productive coastal waters in summer") 
rect(34.5, 20, 38, 27.8)  # mixed warm waters 
rect(31.5, 3, 34, 20) # upwelling, extremely productive, cold waters, temp < 20 deg. celsius, salinity < 34, upwelling
rect(34, 3, 36, 20) # productive, cold waters, temp < 20 deg. celsius, salinity 34 to 36, upwelling
rect(36, 3, 37, 20) # less productive cold waters, salinity > 36

# # NBSS slopes vs productivity index -------------------------- 
# 
# par(opar)
# 
 plot(NBSS_Zoopl$SLOPE.RLM.aut.sel ~ (NBSS_Zoopl$prod.index4_2Chl_nitr_MLD), col = alpha("darkgreen", alpha = 0.6))
# 
 lm1 <- lm(NBSS_Zoopl$SLOPE.RLM.aut.sel ~
             (NBSS_Zoopl$prod.index4_2Chl_nitr_MLD))
abline(lm1, col = "red")
 summary(lm1)
 
 rlm1 <- rlm(NBSS_Zoopl$SLOPE.RLM.aut.sel ~
             (NBSS_Zoopl$prod.index4_2Chl_nitr_MLD))
 abline(rlm1, col = "blue")
 summary(rlm1)
 
 
 
# 
# plot(NBSS_Zoopl$SLOPE.RLM.aut.sel ~ (NBSS_Zoopl$Temperature_C_insitu))
# lm2 <- lm(NBSS_Zoopl$SLOPE.RLM.aut.sel ~ (NBSS_Zoopl$Temperature_C_insitu))
# summary(lm2)
# 
# 
# lm4 <- lm(NBSS_Zoopl$SLOPE.RLM.aut.sel ~ log10(NBSS_Zoopl$prod.index3_2Chl_nitr) * NBSS_Zoopl$Temperature_C_insitu)
# summary(lm4)
# 
# 
# lm5 <- lm(NBSS_Zoopl$SLOPE.RLM.aut.sel ~ 
#             log10(NBSS_Zoopl$prod.index4_Chl_nitr) *
#             NBSS_Zoopl$Temperature_C_insitu)
# summary(lm5)
# 
# # Multiple R-squared:  0.3511,	Adjusted R-squared:  0.3458
# 
# 
# # Temperature
# # Interaction term is significant (p = 0.022) ! 
# # But productivity index 3 (Chl and Nitrate) is not...
# # 367 degrees of freedom
# 
# # With interaction
# # Interaction term is significant! But productivity index is not...
# # better: without interaction
# 
# # linear model: 
# 
# lm3 <- lm(NBSS_Zoopl$SLOPE.RLM.aut.sel ~ log10(NBSS_Zoopl$prod.index2) + NBSS_Zoopl$Temperature_C_insitu)
# summary(lm3)
# 
# 3D plots ---------------


lm4 <- lm(NBSS_Zoopl$SLOPE.RLM.aut.sel ~ log10(NBSS_Zoopl$chla) * NBSS_Zoopl$Temperature_C_insitu)
summary(lm4)

# Conclusions:
# chla is highly signifficant p = 0.00186 
# Temperature is  irrelevant!
# Interaction term is not significant (p = 0.022) ! 
# But productivity index 3 (Chl and Nitrate) is not...
# 367 degrees of freedom

# better: without interaction

# without interaction!
lm4b <- lm(NBSS_Zoopl$SLOPE.RLM.aut.sel ~ log10(NBSS_Zoopl$chla) + NBSS_Zoopl$Temperature_C_insitu)
summary(lm4b)

# Conclusions:
# chla is highly signifficant, p < 0.0001 
# Temperature is   highly signifficant, p < 0.0001




library(car)
scatter3d(x = NBSS_Zoopl$Temperature_C_insitu, y =  NBSS_Zoopl$SLOPE.RLM.aut.sel, 
          z = log10(NBSS_Zoopl$chla), 
          grid = FALSE, fit = "smooth", point.col="darkorange") # bg = "black")


library(car)
scatter3d(x = NBSS_Zoopl$Temperature_C_insitu, y =  NBSS_Zoopl$SLOPE.RLM.aut.sel, 
          z = log10(NBSS_Zoopl$chla_model), 
          grid = FALSE, fit = "smooth", point.col="darkorange") # bg = "black")



library(car)
scatter3d(x = NBSS_Zoopl$Temperature_C_insitu, y =  NBSS_Zoopl$SLOPE.RLM.aut.sel, 
          z = log10(NBSS_Zoopl$chla_model), 
          grid = FALSE, point.col="darkorange") # bg = "black")



# 
# 
# 
# # linear models: 
# # in situ data
# 
# attach(NBSS_Zoopl)
# 
# # lm.index3Temp <- lm(NBSS_Zoopl$SLOPE.RLM.aut.sel ~ 
# #                       log10(NBSS_Zoopl$prod.index3_2Chl_nitr) * 
# #                       NBSS_Zoopl$Temperature_C_insitu)
# # summary(lm.index3Temp)
# # Multiple R-squared:  0.3427,	Adjusted R-squared:  0.3373 
# # 
# # lm.index2Temp <- lm(NBSS_Zoopl$SLOPE.RLM.aut.sel ~ 
# #                       log10(NBSS_Zoopl$prod.index2) * 
# #                       NBSS_Zoopl$Temperature_C_insitu)
# # summary(lm.index2Temp)
# # Multiple R-squared:  0.3644,	Adjusted R-squared:  0.3592 
# 
# lm.ChlaTemp <- lm(NBSS_Zoopl$SLOPE.RLM.aut.sel ~ 
#                     log10(NBSS_Zoopl$Chlorophyll__mg_m3_insitu) * 
#                     NBSS_Zoopl$Temperature_C_insitu)
# summary(lm.ChlaTemp)
# # multiple R-squared:  0.3631,	Adjusted R-squared:  0.3592 
# # R2 = 0.36
# 
# 
# 
# 
# # in situ Nitrate & Temp vs slope, Best 2 x vs 1 y model------------------
# lm.Nitrate.Temp <- lm(NBSS_Zoopl$SLOPE.RLM.aut.sel ~ 
#                         log10(1+NBSS_Zoopl$Nitrate__uM_L1_insitu) * 
#                         NBSS_Zoopl$Temperature_C_insitu)
# summary(lm.Nitrate.Temp)
# # R2 = 0.399 -> best 2 indep. variable model is with temp and nitrate! 
# # Multiple R-squared:  0.3994,	Adjusted R-squared:  0.3946
# 
# 
# cor.test ((log(1+Nitrate__uM_L1_insitu)) , log(1+Chlorophyll__mg_m3_insitu ) )
# # p-value = 0.4683, r = 0.03697214 
# # nitrate and Chl a (in situ ) are not correlated!!!
# par(  opar)
# plot(log(1+Chlorophyll__mg_m3_insitu ) ~(log(1+Nitrate__uM_L1_insitu)) ) # in situ data



## IV. Pooled data ---------------------

## IV.b HTL data (Pooled HIGHER TROPHIC LEVELS)----------------------------

# import HTL data (Pooled)---------

pooled.HTL <- read.csv("~/Papers/000 -Paper_Fish_Zoopl_Size_Spectra_Brazil_Africa/Data/pooled HTL_data NBSS_ Oct 11_2023_V3.csv")
 #View(pooled.HTL)


 summary(pooled.HTL) # NBSS slope: 
 summary(pooled.HTL$slope) 
 # NBSS slope: 
 # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 # -1.822  -1.426  -1.190  -1.148  -0.930  -0.310 
 
 attach(pooled.HTL)
 
 # first plots -----------------

## Colors:
# mesopelagic fish =	1 (Black)
# micronekton	 = 2 (red)
 
 
 plot(slope ~ SST, col = pooled.HTL$target_org._code,
      main = "Higher Trophic levels", ylab = "NBSS Slope")
 summary(lm(slope ~ SST)) # n.s.
 
 
 plot(slope ~ log10(chla), col = pooled.HTL$target_org._code,
      main = "Higher Trophic levels", ylab = "NBSS Slope")
 summary(lm(slope ~ log10(chla))) # n.s.
 
 
 plot(slope ~ (primprod), col = pooled.HTL$target_org._code,
      main = "Higher Trophic levels", ylab = "NBSS Slope")
 summary(lm(slope ~ (primprod))) # n.s.
 
 
 plot(slope ~ log10(primprod), col = pooled.HTL$target_org._code,
      main = "Higher Trophic levels", ylab = "NBSS Slope")
 summary(lm(slope ~ log10(primprod))) # n.s.
 
 
 plot(slope ~ Nitrates_Mathilde, col = pooled.HTL$target_org._code,
      main = "Higher Trophic levels", ylab = "NBSS Slope")
 summary(lm(slope ~ Nitrates_Mathilde)) # n.s.
 
 
 # GAM non-linear (gam) models ---------------------
 
 # library(gam)
 library(mgcv)
 
 
 ## Colors:
 # mesopelagic fish =	1 (Black)
 # micronekton	 = 2 (red)
 
 # SST -> non.signif. !
 
 plot(slope ~ SST, col = pooled.HTL$target_org._code,
      main = "Higher Trophic levels", ylab = "NBSS Slope")
  summary(lm(slope ~ SST)) # n.s.
 
   summary(gam(slope ~ SST)) # n.s.
  
summary(  gam(slope ~ s(SST)))# n.s.
  
# Chl a -> non.signif. !

 plot(slope ~ log10(chla), col = pooled.HTL$target_org._code,
      main = "Higher Trophic levels", ylab = "NBSS Slope")
 summary(lm(slope ~ log10(chla))) # n.s.
 
 summary(gam(slope ~ log10(chla))) # n.s.
 
 summary(  gam(slope ~ s(log10(chla))))# n.s.
 
 ## Prim. prod ---------
 
 plot(slope ~ (primprod), col = pooled.HTL$target_org._code,
      main = "Higher Trophic levels", ylab = "NBSS Slope")
 summary(lm(slope ~ (primprod))) # n.s.
 summary(gam(slope ~ (chla))) # n.s.
 
 summary(  gam(slope ~ s((chla))))# n.s.
 
 ## log10(Prim. prod.) ---------
 
 plot(slope ~ log10(primprod), col = pooled.HTL$target_org._code,
      main = "Higher Trophic levels", ylab = "NBSS Slope")
 summary(lm(slope ~ log10(primprod))) # n.s.
 summary(gam(slope ~ log10(primprod))) # n.s.
 
 summary(  gam(slope ~ s(log10(primprod))))# n.s.
 
 #?gam
 
########## Check biomass per group ############
 
 
 
 