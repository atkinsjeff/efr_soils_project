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

# Filter down and average

####
df %>%
    dplyr::select(contains("soil_name") | contains("horizon") | contains("total_carbon")) %>%
    #dplyr::group_by(soil_name, horizon) %>%
    # dplyr::mutate(ca_per = case_when(ca_nh4_ph_7_method == "6N" ~ ca_nh4_ph_7 * 0.02,
    #                                  ca_nh4_ph_7_method == "4B" ~ ca_nh4_ph_7,
    #                                  ca_nh4_ph_7_method == "NK" ~ 0,
    #                                  ca_nh4_ph_7_method == "" ~ 0)) %>%
    dplyr::group_by(soil_name, horizon) %>%
    dplyr::summarise(c_g_cm3 = mean(total_carbon_ncs, na.rm = TRUE)) %>%
    data.frame() -> df.c

##### numerical interpolation of 
# first we make all of the correct values
df.grid <- expand.grid(soil_name = unique(df$soil_name), horizon = c("0_10", "10_20", "20_40", "40_100"))

# merge together
bob <- merge(df.grid, df.c,  by = c("soil_name", "horizon"), all.x = TRUE)

# linear interpolation between values
bob %>%
    select(soil_name, horizon, c_g_cm3) %>%
    #filter(!horizon == "0_10" && is.na(ca_per) == TRUE) %>%
    group_by(soil_name) %>%
    mutate(horizon = horizon,
           c_g_cm3 = na.approx(c_g_cm3, na.rm = FALSE, rule = 2)) %>%
    data.frame() -> andy

# 
# andy %>%
#     filter(soil_name == "evergreen")
# 
# bob %>%
#     filter(soil_name == "evergreen")
# 
# 


# bring in bulk density
df.bulk <- read.csv("./data/soil_series_bulk_density_horizon_method_southeast.csv")
#### merge to only those with bulk density
df <- merge(andy, df.bulk)

# bring in soil physical properities
# bring in physical    
phys <- read.csv("./data/soil_series_physical_southeast.csv")
df <- merge(df, phys)

# makes grams of c per square meter per horizon
df$c_g_m2 <- (df$c_g_cm3) * df$bulk_density * ((100 - df$frag)/100)

# plot for test
x11()
hist(df$c_g_m2)

##### calculate p BRAY 
# #### MODEL ONE Form



# 
df %>%
    mutate(total_c_horizon = case_when(horizon == "0_10" ~ c_g_m2 * 10, 
                                        horizon == "10_20" ~ c_g_m2 * 10,
                                        horizon == "20_40" ~ c_g_m2 * 20,
                                        horizon == "40_100" ~ c_g_m2 * 60)) %>%
    data.frame() -> df.final


# #### make the leon problem
# make tractable
length(which(df.final$total_c_horizon == 0))
df.final$total_c_horizon[df.final$total_c_horizon == 0] <- NA

df.final$total_c_horizon <- round(df.final$total_c_horizon, 5)
# df.final$total_ca_horizon_kg <- df.final$total_ca_horizon * 0.001  # makes it in kg

# # now remove the frag part
# df.final %>%
#     filter(total_ca_horizon_kg < 2) %>%
#     select(soil_name, horizon, total_ca_horizon_kg) %>%
#     group_by(soil_name, horizon) %>%
#     summarise(total_ca = mean(total_ca_horizon_kg, na.rm = TRUE)) %>%
#     data.frame() -> andy
# 
# # bring in physical    
# phys <- read.csv("./data/soil_series_physical_southeast.csv")
# 
# # 
# carl <- merge(andy, phys)
# 
# carl$total_ca_soil <- carl$total_ca * ((100 - carl$frag)/100)
require(ggplot2)

# remove this wild boy
# carl %>%
#     filter(soil_name != "leon") %>%
#     data.frame() -> dave



x11()
ggplot(df.final, aes(fill = horizon, y = total_c_horizon, x = soil_name)) + 
    geom_bar(position="stack", stat="identity", color = "black")+
    scale_fill_viridis(discrete = T, option = "G") +
    ggtitle("SE EFT Carbon Model Test") +
    theme_classic() +
    ylab("Total C per soil horizon [g m-2]")+
    xlab("")
    
#write.csv(df.final, "./data/southeast_soils_total_c_new_output.csv", row.names = FALSE)
