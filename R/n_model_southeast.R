#### NITROGEN



# dependencies

library(tidyverse)
library(forestmangr)
library(ggplot2)
library(viridis)

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
# check for structure

str(df)

# fix class issue
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

# 
x11(width = 6, height = 3)
ggplot(og, aes(x = total_nitrogen_ncs_method, y = log10(total_nitrogen_ncs)))+
    geom_boxplot()+
    theme_bw()+
    ylab(expression(paste("Total Nitrogen (N) log "[10]*"(%)")))+
    xlab("Extraction Method")



####
df %>%
    select(contains("soil_name") | contains("horizon") | contains("nitrogen")) %>%
    data.frame() -> df.n

# remove the errors
df.n %>%
    select(!contains("metho") ) %>%
    data.frame() -> df.n

df.n %>%
    select(!contains("ratio") ) %>%
    data.frame() -> df.n



##### BRAY!



x11()
hist(df.n$total_nitrogen_ncs)



# Filter down and average

####

df.n %>%
    dplyr::group_by(soil_name, horizon) %>%
    dplyr::summarise(n_per = mean(total_nitrogen_ncs, na.rm = TRUE)) %>%
    data.frame() -> df.n



##### numerical interpolation of
# first we make all of the correct values
df.grid <- expand.grid(soil_name = unique(df$soil_name), horizon = c("0_10", "10_20", "20_40", "40_100"))

# merge together

bob <- merge(df.grid, df.n,  by = c("soil_name", "horizon"), all.x = TRUE)

# linear interpolation between values

bob %>%
    select(soil_name, horizon, n_per) %>%
    #filter(!horizon == "0_10" && is.na(ca_per) == TRUE) %>%
    group_by(soil_name) %>%
    mutate(horizon = horizon,
           n_per = zoo::na.approx(n_per, na.rm = FALSE, rule = 2)) %>%
    data.frame() -> andy


# bring in bulk density
df.bulk <- read.csv("./data/soil_series_bulk_density_horizon_method_southeast.csv")

#### merge to only those with bulk density
df <- merge(andy, df.bulk)



# bring in soil physical properities
phys <- read.csv("./data/soil_series_physical_southeast.csv")

df <- merge(df, phys)

# combine with bulk
df <- merge(df, df.bulk)
df <- merge(df, phys)

# N is percent
df$n_g_cm3 <- ((df$n_per * 0.01) * df$bulk_density) * ((100 - df$frag)/100)

# make N for horizons
for (i in 1:nrow(df)){
    
    if (df$horizon[i] == "0_10") {
        
        (df$n_Mg_horizon[i] = df$n_g_cm3[i] * 0.1)
        
    } else if (df$horizon[i] == "10_20") {
        
        (df$n_Mg_horizon[i] = df$n_g_cm3[i] * 0.1)
        
    } else if (df$horizon[i] == "20_40") {
        
        (df$n_Mg_horizon[i] = df$n_g_cm3[i] * 0.2)
        
    } else if (df$horizon[i] == "40_100") {
        
        (df$n_Mg_horizon[i] = df$n_g_cm3[i] * 0.6)
        
    } else {
        
        df$n_Mg_horizon[i] <- NA
        
    }
    
}


# convert to Mg per hectare for EACH horizon
df$n_Mg_ha <- df$n_Mg_horizon * 10000

# writing the horizons to disk
write.csv(df, "./data/FinalData/nitrogen_by_horizon.csv")

# 1 gram per cubic meter equals 1 megagram per cubic meter#
df %>%
    group_by(soil_name) %>%
    summarize(n_Mg_ha = sum(n_Mg_ha, na.rm = TRUE)) %>%
    data.frame() -> soil.n



x11(width = 6, height = 3)
hist(soil.n$n_Mg_ha, breaks = 100,
     ylab = ("No. of Samples"),
     xlab = (expression(paste("Extractable N (Mg ha"^-1, ")"))),
     main = NULL)##### calculate p BRAY
# writing final data to disk
write.csv(soil.n, "./data/FinalData/nitrogen_to_1m_by_soil_series.csv")


# #### make the leon problem
# 
# # make tractable
# 
# length(which(df.final$total_n_horizon == 0))
# 
# df.final$total_n_horizon[df.final$total_n_horizon == 0] <- NA
# 
# df.final$total_n_horizon <- round(df.final$total_n_horizon, 5)
# 
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

ggplot(df.final, aes(fill = horizon, y = total_n_horizon, x = soil_name)) +
    
    geom_bar(position="stack", stat="identity", color = "black")+
    
    scale_fill_viridis(discrete = T, option = "G") +
    
    ggtitle("SE EFT ca Model Test") +
    
    theme_classic() +
    
    ylab("Total Ca per soil horizon [kg m-2]")+
    
    xlab("")



# write to file

write.csv(df.final, "./data/southeast_soils_n_output.csv", row.names = FALSE)





######