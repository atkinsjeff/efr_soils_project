# base_sat_nh4oac_ph_7

# 

# dependencies
library(tidyverse)
library(forestmangr)
library(ggplot2)
library(viridis)
library(zoo)
# import data
df <- read.csv("./data/southeast_soils.csv")


###### ERROR CHECKING
which(is.na(df$hzn_bot))

# fix row 3101
df[3101, ]
df[3101, 10] <- 100

df[4662, ]        # this one is just empty
df <- df[-4662, ]



# fix formatting include "NULL" values and return character columsn as numer
df[df == "NULL"] <- NA
#df[ , 6:16] <- lapply(df[, c(6:16)],as.numeric)
####
# head@@he
Bulk Density Oven dry methods 

4A1h and Db – reported to the nearest 0.01g cc-1 of <2-mm soil fabric 

# PARTICLE SIZE is percentage (%) 
# Phosphorus reported as % to the nearest hundredth 
# phosphorus reported as % to the nearest whole number 
# Walkley-Black Method % at 2 decimal places, on an oven-dry basis 
# Total nitrogen All methods - report total N as a dimensionless value to the nearest 0.001 unit on an ovendry basis. 
# Total Carbon % on oven dry basis 
# Ca_nH4_ph_7_method, K_nh4_ph_7_method and mg_nh4_ph_7_method 4B1a – moisture content as a percentage of <2-mm. report procedure code 4B1a and the equilibrium tension 
# 6N2, 6O2, and 6Q2 – report the extractable Ca, K and Mg in units of meq 100g-1 of oven dry soil to the nearest 0.1 meq 100g-1 


# check for structure
str(df)

df$hzn_bot <- as.numeric(df$hzn_bot)
df$hzn_top <- as.numeric(df$hzn_top)
# make midpoint
df$horizon_midpoint <- (df$hzn_bot + df$hzn_top)/2

# make horizons
for (i in 1:nrow(df)){
    if (df$horizon_midpoint[i] <= 10) {
        df$horizon[i] <- "0_10"
        
    } else if (df$horizon_midpoint[i] > 10 & df$horizon_midpoint[i] <= 20) {
        df$horizon[i] <- "10_20"
    } else if (df$horizon_midpoint[i] > 20 & df$horizon_midpoint[i] <= 40) {
        df$horizon[i] <- "20_40"
    } else if (df$horizon_midpoint[i] > 40) {
        df$horizon[i] <- "40_100"
    } else {
        df$horizon[i] <- NA
    }
}
table(df$horizon)


# fix the soil name issue
df$soil_name <- tolower(df$soil_name)
# og
og <- df            ######## we lose ~99 observations likely because there is a soil series we don't have the bulk density for????

# ph CACL
####
df %>%
    dplyr::select(contains("soil_name") | contains("horizon") | contains("base")) %>%
    dplyr::group_by(soil_name, horizon) %>%
    dplyr::summarise(base_sat = mean(base_sat_nh4oac_ph_7, na.rm = TRUE)) %>%
    data.frame() -> df.base

##### numerical interpolation of 
# first we make all of the correct values
df.grid <- expand.grid(soil_name = unique(df$soil_name), horizon = c("0_10", "10_20", "20_40", "40_100"))

# merge together
bob <- merge(df.grid, df.ph,  by = c("soil_name", "horizon"), all.x = TRUE)

# linear interpolation between values
bob %>%
    select(soil_name, horizon, ph) %>%
    #filter(!horizon == "0_10" && is.na(ca_per) == TRUE) %>%
    group_by(soil_name) %>%
    mutate(horizon = horizon,
           ph.cacl = na.approx(ph.cacl, na.rm = FALSE, rule = 2),
           ph.h2o = na.approx(ph.h2o, na.rm = FALSE, rule = 2)) %>%
    data.frame() -> andy

head(andy)



#####
# 
# andy %>%
# write to file
write.csv(andy, "./data/southeast_soils_ph_output.csv", row.names = FALSE)

