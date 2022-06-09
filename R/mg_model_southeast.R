# MG modeling
# Below this converts values of chemical properties to ppm (part per million)
# Report the saturation extraction cations of Ca2+, Mg2+, Na+ ,  and K+
# in units of meq L-1 to the
# nearest 0.1 meq L-1
# Calcium conversion is 1 meq L-1 = 120 ppm
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

# panel plot for calcium
x11(width = 6, height = 3)
ggplot(og, aes(x = mg_nh4_ph_7_method, y = log10(mg_nh4_ph_7)))+
    geom_boxplot()+
    theme_bw()+
    ylab(expression(paste("Magnesium (Mg) log "[10]*"("~mu*eq*L^-1*")")))+
    xlab("Extraction Method")




# Filter down and average
####
# make the methods more easy 
df$mg_nh4_ph_7_method <- substr(df$mg_nh4_ph_7_method, 0, 2)
####
df %>%
    dplyr::select(contains("soil_name") | contains("horizon") | contains("mg_")) %>%
    dplyr::group_by(soil_name, horizon) %>%
    # dplyr::mutate(mg_per = case_when(mg_nh4_ph_7_method == "6O" ~ mg_nh4_ph_7 * 0.012,
    #                                  mg_nh4_ph_7_method == "4B" ~ mg_nh4_ph_7,
    #                                  mg_nh4_ph_7_method == "NK" ~ 0,
    #                                  mg_nh4_ph_7_method == "" ~ 0)) %>%
    dplyr::group_by(soil_name, horizon) %>%
    dplyr::summarise(mg_mqevL = mean(mg_nh4_ph_7, na.rm = TRUE)) %>%
    data.frame() -> df.mg

##### numerical interpolation of 
# first we make all of the correct values
df.grid <- expand.grid(soil_name = unique(df$soil_name), horizon = c("0_10", "10_20", "20_40", "40_100"))

# merge together
bob <- merge(df.grid, df.mg,  by = c("soil_name", "horizon"), all.x = TRUE)

# linear interpolation between values
bob %>%
    select(soil_name, horizon, mg_mqevL) %>%
    #filter(!horizon == "0_10" && is.na(ca_per) == TRUE) %>%
    group_by(soil_name) %>%
    mutate(horizon = horizon,
           mg_mqevL = na.approx(mg_mqevL, na.rm = FALSE, rule = 2)) %>%
    data.frame() -> andy

# convert to ppm
andy$mg_ppm <- andy$mg_mqevL * 120


# bring in bulk density
df.bulk <- read.csv("./data/soil_series_bulk_density_horizon_method_southeast.csv")
#### merge to only those with bulk density
df <- merge(andy, df.bulk)

# bring in soil physical properities
# bring in physical    
phys <- read.csv("./data/soil_series_physical_southeast.csv")
df <- merge(df, phys)

# makes grams of P per square meter to one meter depth
# P is in mg per kg, so convert it! by multiplying by 0.0001
df$mg_g_cm3 <- (df$mg_ppm * 0.0001 * 0.01) * df$bulk_density * ((100 - df$frag)/100)

# plot for test
x11()
hist(df$mg_g_cm3)

# make mg for horizons
for (i in 1:nrow(df)){
    
    if (df$horizon[i] == "0_10") {
        
        (df$mg_Mg_horizon[i] = df$mg_g_cm3[i] * 0.1)
        
    } else if (df$horizon[i] == "10_20") {
        
        (df$mg_Mg_horizon[i] = df$mg_g_cm3[i] * 0.1)
        
    } else if (df$horizon[i] == "20_40") {
        
        (df$mg_Mg_horizon[i] = df$mg_g_cm3[i] * 0.2)
        
    } else if (df$horizon[i] == "40_100") {
        
        (df$mg_Mg_horizon[i] = df$mg_g_cm3[i] * 0.6)
        
    } else {
        
        df$mg_Mg_horizon[i] <- NA
        
    }
    
}


# convert to Mg per hectare for EACH horizon
df$mg_Mg_ha <- df$mg_Mg_horizon * 10000

# writing the horizons to disk
write.csv(df, "./data/FinalData/magnesium_by_horizon.csv")

# 1 gram per cubic meter equals 1 megagram per cubic meter#
df %>%
    group_by(soil_name) %>%
    summarize(mg_Mg_ha = sum(mg_Mg_ha, na.rm = TRUE)) %>%
    data.frame() -> soil.mg


# histogram of finals
x11(width = 6, height = 3)
hist(soil.mg$mg_Mg_ha, breaks = 100,
     ylab = ("No. of Samples"),
     xlab = (expression(paste("Extractable Mg (Mg ha"^-1, ")"))),
     main = NULL)##### calculate p BRAY

# writing final data to disk
write.csv(soil.n, "./data/FinalData/magnesium_to_1m_by_soil_series.csv")
