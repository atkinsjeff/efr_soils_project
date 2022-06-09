# PH CACL


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

# ph CACL
# panel plot for calcium
x11(width = 6, height = 3)
ggplot(og, aes(x = ph_cacl2_method, y = (ph_cacl2)))+
    geom_boxplot()+
    theme_bw()+
    ylab("Soil pH - CaCl extraction")+
    xlab("Extraction Method")

# panel plot for calcium
x11(width = 6, height = 3)
ggplot(og, aes(x = ph_h2o_method, y = (ph_h2o)))+
    geom_boxplot()+
    theme_bw()+
    ylab(expression(paste("Soil pH - H "[2]*"O extraction")))+
    xlab("Extraction Method")
####
df %>%
    dplyr::select(contains("soil_name") | contains("horizon") | contains("ph_")) %>%
    dplyr::group_by(soil_name, horizon) %>%
    dplyr::summarise(ph.cacl = mean(ph_cacl2, na.rm = TRUE),
                     ph.h2o = mean(ph_h2o, na.rm = TRUE)) %>%
    data.frame() -> df.ph

##### numerical interpolation of 
# first we make all of the correct values
df.grid <- expand.grid(soil_name = unique(df$soil_name), horizon = c("0_10", "10_20", "20_40", "40_100"))

# merge together
bob <- merge(df.grid, df.ph,  by = c("soil_name", "horizon"), all.x = TRUE)

# linear interpolation between values
bob %>%
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
write.csv(andy, "./data/FinalData/southeast_soils_ph_output.csv", row.names = FALSE)


# 1 gram per cubic meter equals 1 megagram per cubic meter#
andy %>%
    group_by(soil_name) %>%
    summarize(ph.h2o = mean(ph.h2o, na.rm = TRUE),
              ph.cacl = mean(ph.cacl, na.rm = TRUE)) %>%
    data.frame() -> soil.ph


# histogram of finals
x11(width = 6, height = 3)
hist(soil.ph$ph.h2o, breaks = 100,
     ylab = ("No. of Samples"),
     xlab = (expression(paste("Soil pH - H "[2]*"O extraction"))),
     main = NULL)

# histogram of finals
x11(width = 6, height = 3)
hist(soil.ph$ph.cacl, breaks = 100,
     ylab = ("No. of Samples"),
     xlab = ("Soil pH - CaCl extraction"),
     main = NULL)##### calculate p BRAY##### calculate p BRAY