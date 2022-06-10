# carbon model part two

#### CARBON MODEL

# carbon is calculated in units of grams per cubic centimeter to the nearest 0.01 g/cm^3


# dependencies
library(tidyverse)
library(forestmangr)
library(ggplot2)
library(viridis)
library(zoo)
# import data
# santee <- read.csv("./data/santee_test_data.csv")
df <- read.csv("./data/southeast_soils.csv")


###### ERROR CHECKING
which(is.na(df$hzn_bot))

# fix row 3101
df[3101, 10] <- 100

df[4662, ]        # this one is just empty
df <- df[-4662, ]



# fix formatting include "NULL" values and return character columsn as numer
df[df == "NULL"] <- NA
#df[ , 6:16] <- lapply(df[, c(6:16)],as.numeric)

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

# ######
# df %>%
#     filter(hzn_bot > 200) %>%
#     data.frame() -> deep.bois

# fix the soil name issue
df$soil_name <- tolower(df$soil_name)
# og
og <- df            ######## we lose ~99 observations likely because there is a soil series we don't have the bulk density for????




####################### NEW MODEL FOR REAL

# fix the soil name issue
df$soil_name <- tolower(df$soil_name)
# og
og <- df            ######## we lose ~99 observations likely because there is a soil series we don't have the bulk density for????



# panel plot for carbon
x11(width = 6, height = 3)
ggplot(og, aes(x = oc_walkley_black_method, y = log10(organic_carbon_walkley_black)))+
    geom_boxplot()+
    theme_bw()+
    ylab(expression(paste("Organic Carbon (C) log "[10]*"(%)")))+
    xlab("Extraction Method")

# Filter down and average

####
df %>%
    dplyr::select(contains("soil_name") | contains("horizon") | contains("walkley")) %>%
    #dplyr::group_by(soil_name, horizon) %>%
    # dplyr::mutate(ca_per = case_when(ca_nh4_ph_7_method == "6N" ~ ca_nh4_ph_7 * 0.02,
    #                                  ca_nh4_ph_7_method == "4B" ~ ca_nh4_ph_7,
    #                                  ca_nh4_ph_7_method == "NK" ~ 0,
    #                                  ca_nh4_ph_7_method == "" ~ 0)) %>%
    dplyr::group_by(soil_name, horizon) %>%
    dplyr::summarise(c_per = mean(organic_carbon_walkley_black, na.rm = TRUE)) %>%
    data.frame() -> df.c

##### numerical interpolation of 
# first we make all of the correct values
df.grid <- expand.grid(soil_name = unique(df$soil_name), horizon = c("0_10", "10_20", "20_40", "40_100"))

# merge together
bob <- merge(df.grid, df.c,  by = c("soil_name", "horizon"), all.x = TRUE)

# linear interpolation between values
bob %>%
    select(soil_name, horizon, c_per) %>%
    #filter(!horizon == "0_10" && is.na(ca_per) == TRUE) %>%
    group_by(soil_name) %>%
    mutate(horizon = horizon,
           c_per = na.approx(c_per, na.rm = FALSE, rule = 2)) %>%
    data.frame() -> andy


# bring in bulk density
df.bulk <- read.csv("./data/soil_series_bulk_density_horizon_method_southeast.csv")
#### merge to only those with bulk density
df <- merge(andy, df.bulk)

# bring in soil physical properities
# bring in physical    
phys <- read.csv("./data/soil_series_physical_southeast.csv")
df <- merge(df, phys)

# makes grams of c per square meter per horizon
df$c_g_cm3 <- (df$c_per * 0.01) * df$bulk_density * ((100 - df$frag)/100)

# plot for test
x11()
hist(df$c_g_cm3)

##### calculate p BRAY 
# #### MODEL ONE Form


# make c for horizons
for (i in 1:nrow(df)){
    
    if (df$horizon[i] == "0_10") {
        
        (df$c_Mg_horizon[i] = df$c_g_cm3[i] * 0.1)
        
    } else if (df$horizon[i] == "10_20") {
        
        (df$c_Mg_horizon[i] = df$c_g_cm3[i] * 0.1)
        
    } else if (df$horizon[i] == "20_40") {
        
        (df$c_Mg_horizon[i] = df$c_g_cm3[i] * 0.2)
        
    } else if (df$horizon[i] == "40_100") {
        
        (df$c_Mg_horizon[i] = df$c_g_cm3[i] * 0.6)
        
    } else {
        
        df$c_Mg_horizon[i] <- NA
        
    }
    
}


# convert to Mg per hectare for EACH horizon
df$c_Mg_ha <- df$c_Mg_horizon * 10000

# writing the horizons to disk
write.csv(df, "./data/FinalData/oranic_carbon_by_horizon.csv")

# 1 gram per cubic meter equals 1 megagram per cubic meter#
df %>%
    group_by(soil_name) %>%
    summarize(c_Mg_ha = sum(c_Mg_ha, na.rm = TRUE)) %>%
    data.frame() -> soil.c


# histogram of finals
x11(width = 6, height = 3)
hist(soil.c$c_Mg_ha, breaks = 100,
     ylab = ("No. of Samples"),
     xlab = (expression(paste("Organic Carbon (C) (Mg ha"^-1, ")"))),
     main = NULL)##### clculate p BRAY

# writing final data to disk
write.csv(soil.n, "./data/FinalData/organic_carbon_to_1m_by_soil_series.csv")
