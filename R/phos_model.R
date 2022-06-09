#### PHOSPHORUS
# Reported in % 

# 4D - Phosphorus reported as % to the nearest hundredth 
# 
# 6S3 – phosphorus reported as % to the nearest whole number 


#### Phosphorus by Bray reported as ppm
# dependencies
library(tidyverse)
library(forestmangr)
library(ggplot2)
library(viridis)
library(ggridges)
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

# ######
# df %>%
#     filter(hzn_bot > 200) %>%
#     data.frame() -> deep.bois

# fix the soil name issue
df$soil_name <- tolower(df$soil_name)
# og
og <- df            ######## we lose ~99 observations likely because there is a soil series we don't have the bulk density for????



####
df %>%
    select(contains("soil_name") | contains("horizon") | contains("phos")) %>%
    data.frame() -> df.p

# remove the errors
df.p %>%
    select(!contains("metho") ) %>%
    data.frame() -> df.p

df.p %>%
    select(!contains("bray2") ) %>%
    data.frame() -> df.p

# make tidy for boxplot
df.p <- gather(df.p, key = "p_method", value = "p_ppm", phosphorus_bray1:phosphorus_mehlich_3)


# panel plot for phosphorus
x11(width = 6, height = 3)
ggplot(df.p, aes(x = p_method, y = log10(p_ppm)))+
    geom_boxplot()+
    theme_bw()+
    ylab(expression(paste("Total Phosphorus (P) log "[10]*"(ppm)")))+
    xlab("Extraction Method")

# panel plot for phosphorus
x11(width = 6, height = 3)
ggplot(df.p, aes(x = p_method, y = log10(p_ppm)))+
    geom_boxplot()+
    theme_bw()+
    ylab(expression(paste("Total Phosphorus (P) log "[10]*"(ppm)")))+
    xlab("Extraction Method")



# Filter down and average
####
df.p %>%
    dplyr::group_by(soil_name, horizon) %>%
    dplyr::summarise(p_ppm = mean(p_ppm, na.rm = TRUE)) %>%
    data.frame() -> df.p





##### numerical interpolation of 
# first we make all of the correct values
df.grid <- expand.grid(soil_name = unique(df$soil_name), horizon = c("0_10", "10_20", "20_40", "40_100"))

# merge together
bob <- merge(df.grid, df.p,  by = c("soil_name", "horizon"), all.x = TRUE)

# linear interpolation between values
bob %>%
    select(soil_name, horizon, p_ppm) %>%
    #filter(!horizon == "0_10" && is.na(ca_per) == TRUE) %>%
    group_by(soil_name) %>%
    mutate(horizon = horizon,
           p_ppm = zoo::na.approx(p_ppm, na.rm = FALSE, rule = 2)) %>%
    data.frame() -> andy

# conver the ppm to percent
andy$p_per = andy$p_ppm * 0.0001
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

# bring in soil physical properties
# bring in physical    
phys <- read.csv("./data/soil_series_physical_southeast.csv")
df <- merge(df, phys)

# combine with bulk
df <- merge(df, df.bulk)
df <- merge(df, phys)


# makes grams of P per square meter to one meter depth
# P is in mg per kg, so convert it! by multiplying by 0.0001
df$p_g_cm3 <- (df$p_per * 0.01) * df$bulk_density * ((100 - df$frag)/100)

# plot for test
x11()
hist(df$p_g_cm3)

##### 
# make P for horizons
for (i in 1:nrow(df)){
    
    if (df$horizon[i] == "0_10") {
        
        (df$p_Mg_horizon[i] = df$p_g_cm3[i] * 0.1)
        
    } else if (df$horizon[i] == "10_20") {
        
        (df$p_Mg_horizon[i] = df$p_g_cm3[i] * 0.1)
        
    } else if (df$horizon[i] == "20_40") {
        
        (df$p_Mg_horizon[i] = df$p_g_cm3[i] * 0.2)
        
    } else if (df$horizon[i] == "40_100") {
        
        (df$p_Mg_horizon[i] = df$p_g_cm3[i] * 0.6)
        
    } else {
        
        df$p_Mg_horizon[i] <- NA
        
    }
    
}


# convert to Mg per hectare for EACH horizon
df$p_Mg_ha <- df$p_Mg_horizon * 10000

x11()
hist(df$p_Mg_horizon)

# writing the horizons to disk
write.csv(df, "./data/FinalData/phosphorous_by_horizon.csv")

# 1 gram per cubic meter equals 1 megagram per cubic meter#
df %>%
    group_by(soil_name) %>%
    summarize(p_Mg_ha = sum(p_Mg_horizon, na.rm = TRUE)) %>%
    data.frame() -> soil.p


# histogram of finals
x11(width = 6, height = 3)
hist(soil.p$p_Mg_ha, breaks = 100,
     ylab = ("No. of Samples"),
     xlab = (expression(paste("Extractable P (Mg ha"^-1, ")"))),
     main = NULL)##### calculate p BRAY


# writing final data to disk
write.csv(soil.p, "./data/FinalData/phosphorous_to_1m_by_soil_series.csv")


# 
# 
# # #### make the leon problem
# # make tractable
# length(which(df.final$total_p_horizon == 0))
# df.final$total_p_horizon[df.final$total_p_horizon == 0] <- NA
# 
# df.final$total_p_horizon <- round(df.final$total_p_horizon, 5)
# # df.final$total_ca_horizon_kg <- df.final$total_ca_horizon * 0.001  # makes it in kg
# 
# # # now remove the frag part
# # df.final %>%
# #     filter(total_ca_horizon_kg < 2) %>%
# #     select(soil_name, horizon, total_ca_horizon_kg) %>%
# #     group_by(soil_name, horizon) %>%
# #     summarise(total_ca = mean(total_ca_horizon_kg, na.rm = TRUE)) %>%
# #     data.frame() -> andy
# # 
# # # bring in physical    
# # phys <- read.csv("./data/soil_series_physical_southeast.csv")
# # 
# # # 
# # carl <- merge(andy, phys)
# # 
# # carl$total_ca_soil <- carl$total_ca * ((100 - carl$frag)/100)
# require(ggplot2)
# 
# # remove this wild boy
# # carl %>%
# #     filter(soil_name != "leon") %>%
# #     data.frame() -> dave
# 
# 
# 
# x11()
# ggplot(df.final, aes(fill = horizon, y = total_p_horizon, x = soil_name)) + 
#     geom_bar(position="stack", stat="identity", color = "black")+
#     scale_fill_viridis(discrete = T, option = "G") +
#     ggtitle("SE EFT ca Model Test") +
#     theme_classic() +
#     ylab("Total Ca per soil horizon [kg m-2]")+
#     xlab("")
# 
# # write to file
# write.csv(df.final, "./data/southeast_soils_p_output.csv", row.names = FALSE)
# 
# 
# ######
