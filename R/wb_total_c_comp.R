#### Walkley Black comp CARBON MODEL

# dependencies
library(tidyverse)
library(forestmangr)
library(ggplot2)
library(viridis)

# import data
# santee <- read.csv("./data/santee_test_data.csv")
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

# bring in bulk density
df.bulk <- read.csv("./data/soil_series_bulk_density_horizon_method_southeast.csv")
#### merge to only those with bulk density
df <- merge(df, df.bulk)


##### calculate carbon 
df %>%
    select(pedlabsampnum, soil_name, horizon, hzn_top, hzn_bot, total_carbon_ncs, horizon_midpoint, bulk_density) %>%
    drop_na(total_carbon_ncs) -> df.model

#### outlier removal
df.model %>% 
    summarise(thresh.mean = mean(total_carbon_ncs, na.rm = TRUE), thresh.sd = sd(total_carbon_ncs, na.rm = TRUE)) %>%
    data.frame() -> thresh

outlier.limit = thresh$thresh.mean + (thresh$thresh.sd)

df.model %>%
    filter(total_carbon_ncs < outlier.limit) %>%
    data.frame() -> df.model

# make the carbon content
# i think by multiplying it by 100 you get the per 0 - 1m layer value
df.model$carbon_g_cm3 <- df.model$total_carbon_ncs * df.model$bulk_density * 100

# before removing frag
x11()
hist(df.model$carbon_g_cm3, main = "C * Bulk Density")
# REMOVE THE FRAG

# now remove the frag part
# bring in physical    
phys <- read.csv("./data/soil_series_physical_southeast.csv")

# 
df.model <- merge(df.model, phys)

df.model$carbon_g_cm3 <- df.model$carbon_g_cm3 * ((100 - df.model$frag)/100)

# before removing frag
x11()
hist(df.model$carbon_g_cm3, main = "C * Bulk Density / FRAG")
# REMOVE THE FRAG

# plot for test
x11()
ggplot(df.model, aes(x = carbon_g_cm3 , y = horizon_midpoint, color = soil_name))+
    geom_point()+
    scale_y_reverse()+
    xlim(c(0, 500))+
    xlab("Total C [g m-2]")+
    ylab("Soil Depth [cm]")+
    theme_bw()+
    theme(legend.position = "none")



# ----- 
# MODEl

#### MODEL ONE Form
c.model.table <- nls_table(df.model, carbon_g_cm3 ~ b0 * exp( -b1 * horizon_midpoint), 
                        mod_start = c( b0 = 1, b1 = 1),
                        .groups = "soil_name",
                        keep_model = TRUE)


# merge filter to only the model ready data
df.model %>%
    filter(soil_name %in% c.model.table$soil_name) %>%
    select(pedlabsampnum, soil_name, hzn_top, hzn_bot, horizon, horizon_midpoint, carbon_g_cm3) -> df.carbon

# merge with model coeff
df.carbon <- merge(df.carbon, c.model.table[, c("soil_name", "b0", "b1")])

# modeled carbon
df.carbon$carbon_stock <- df.carbon$b0 * exp( -df.carbon$b1 * df.carbon$horizon_midpoint)

# model test
summary(lm(carbon_g_cm3 ~ carbon_stock, data = df.carbon))

#
df.carbon %>%
    filter(carbon_stock < 20) %>%
    data.frame() -> df.carbon.small

summary(lm(carbon_g_cm3 ~ carbon_stock, data = df.carbon))

x11()
ggplot(df.carbon, aes(x = (carbon_g_cm3 * 0.001), y = (carbon_stock * 0.001)))+
    geom_point(size = 2, shape = 21)+
    xlab("Measured Carbon [kg m-2]")+
    ylab("Modelled Carbon [kg m-2]")+
    geom_smooth(method = "lm")

########################


#### calculate total carbon per horizon
pedons <- unique(df.carbon$pedlabsampnum)
horizons <- c("0_10", "10_20", "20_40", "40_100")

# bring together
df.final <- merge(pedons, horizons)

# rename
colnames(df.final) <- c("pedlabsampnum", "horizon")

# bring in coeff
df.final <- distinct(merge(df.final, df.carbon[, c("pedlabsampnum", "soil_name", "b0", "b1")]))

df.final <- na.omit(df.final)

####for (i in 1:nrow(df)){
for (i in 1:nrow(df.final)){
    if (df.final$horizon[i] == "0_10") {
        
        # SEQUENCE
        z <- 1:10
        y <- df.final$b0[i] * exp(-df.final$b1[i] * z)
        df.final$total_carbon_horizon[i] = sum(y)
        
    } else if (df.final$horizon[i] == "10_20") {
        
        # SEQUENCE
        z <- 11:20
        y <- df.final$b0[i] * exp(-df.final$b1[i] * z)
        df.final$total_carbon_horizon[i] = sum(y)
        
    } else if (df.final$horizon[i] == "20_40") {
        # SEQUENCE
        z <- 21:40
        y <- df.final$b0[i] * exp(-df.final$b1[i] * z)
        df.final$total_carbon_horizon[i] = sum(y)
        
    } else if (df.final$horizon[i] == "40_100") {
        # SEQUENCE
        z <- 41:100
        y <- df.final$b0[i] * exp(-df.final$b1[i] * z)
        df.final$total_carbon_horizon[i] = sum(y)
        
    } else {
        df.final$total_carbon_horizon[i] <- NA
    }
}

# #### make the leon problem
# df.final %>%
#     filter(soil_name == "leon") %>%
#     data.frame() -> leon
# 
# nums <- 1:100
# leon2 <- data.frame(1020276, 1.5815, nums)
# colnames(leon2) <- c("b0", "b1", "z")
# 
# 
# leon2$carbon <-  leon2$b0 * exp((-1 * leon2$b1) * as.numeric(leon2$z))
# 
# x11()
# ggplot(leon2, aes(x = (carbon * 0.001), y = z))+
#     geom_point()+
#     scale_y_reverse()+
#     theme_bw()+
#     xlab("Modelled C [kg]")

# make tractable
df.final$total_carbon_horizon <- round(df.final$total_carbon_horizon, 2)
df.final$total_carbon_horizon_kg <- df.final$total_carbon_horizon * 0.001  # makes it in kg

# now remove the frag part
df.final %>%
    select(soil_name, horizon, total_carbon_horizon_kg) %>%
    group_by(soil_name, horizon) %>%
    summarise(total_c = mean(total_carbon_horizon_kg, na.rm = TRUE)) %>%
    data.frame() -> andy


x11()
ggplot(andy, aes(fill = horizon, y = total_c, x = soil_name)) + 
    geom_bar(position="stack", stat="identity", color = "black")+
    scale_fill_viridis(discrete = T, option = "G") +
    ggtitle("Santee Soil Carbon Model Test") +
    theme_classic() +
    ylab("Total Carbon per soil horizon [kg m-2]")+
    xlab("")+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))



###### now we compare!!!!!


df %>%
    select(soil_name, total_carbon_ncs, organic_carbon_walkley_black) %>%
    group_by(soil_name) %>%
    summarise(no.total = n_distinct(total_carbon_ncs, na.rm = TRUE), no.walkley = n_distinct(organic_carbon_walkley_black, na.rm = TRUE)) %>%
    data.frame() -> carbon.types

#####
# find where we have both total C and walkley black
carbon.types[carbon.types == 0] <- NA

c.types <- carbon.types[complete.cases(carbon.types), ]




####### NOW WE DO WALKLEY BLACK

##### calculate carbon 
df %>%
    select(pedlabsampnum, soil_name, horizon, hzn_top, hzn_bot, organic_carbon_walkley_black, horizon_midpoint, bulk_density) %>%
    drop_na() -> df.model2

#### outlier removal
df.model2 %>% 
    summarise(thresh.mean = mean(organic_carbon_walkley_black, na.rm = TRUE), thresh.sd = sd(organic_carbon_walkley_black, na.rm = TRUE)) %>%
    data.frame() -> thresh2

outlier.limit2 = thresh2$thresh.mean + (thresh2$thresh.sd)

df.model2 %>%
    filter(organic_carbon_walkley_black < outlier.limit2) %>%
    data.frame() -> df.model2

# make the carbon content
# i think by multiplying it by 100 you get the per layer value
df.model2$carbon_g_cm3_WALK <- df.model2$organic_carbon_walkley_black * df.model2$bulk_density * 100
# now remove the frag part

# 
df.model2 <- merge(df.model2, phys)

df.model2$carbon_g_cm3_WALK <- df.model2$carbon_g_cm3_WALK * ((100 - df.model2$frag)/100)

# before removing frag
x11()
hist(df.model2$carbon_g_cm3_WALK, main = "C * Bulk Density / FRAG")
# REMOVE THE FRAG


# plot for test
x11()
ggplot(df.model2, aes(x = carbon_g_cm3_WALK * 0.001, y = horizon_midpoint, color = soil_name))+
    geom_point()+
    scale_y_reverse()+
    xlim(c(0, 2))+
    xlab("Total Organic C [kg m-2]")+
    ylab("Soil Depth [cm]")+
    theme_bw()+
    theme(legend.position = "none")



# ----- 
# MODEl

#### MODEL ONE Form
c.model.table <- nls_table(df.model2, carbon_g_cm3_WALK ~ b0 * exp( -b1 * horizon_midpoint), 
                           mod_start = c( b0 = 1, b1 = 1),
                           .groups = "soil_name",
                           keep_model = TRUE)


# merge filter to only the model ready data
df.model2 %>%
    filter(soil_name %in% c.model.table$soil_name) %>%
    select(pedlabsampnum, soil_name, hzn_top, hzn_bot, horizon, horizon_midpoint, carbon_g_cm3_WALK) -> df.carbon.walk

# merge with model coeff
df.carbon.walk <- merge(df.carbon.walk, c.model.table[, c("soil_name", "b0", "b1")])

# modeled carbon
df.carbon.walk$carbon_stock <- df.carbon.walk$b0 * exp( -df.carbon.walk$b1 * df.carbon.walk$horizon_midpoint)

# model test
summary(lm(carbon_g_cm3_WALK ~ carbon_stock, data = df.carbon.walk))

#
df.carbon %>%
    filter(carbon_stock < 20) %>%
    data.frame() -> df.carbon.small

summary(lm(carbon_g_cm3 ~ carbon_stock, data = df.carbon))

x11()
ggplot(df.carbon.walk, aes(x = (carbon_g_cm3_WALK * 0.001), y = (carbon_stock * 0.001)))+
    geom_point(size = 2, shape = 21)+
    xlab("Measured Carbon [kg m-2]")+
    ylab("Modelled Carbon [kg m-2]")+
    geom_smooth(method = "lm")
######
# now we need to compare

# sort the first
df.carbon %>%
    select(soil_name, horizon, carbon_stock, carbon_g_cm3) %>%
    group_by(soil_name, horizon) %>%
    summarise(total.c = round(mean(carbon_g_cm3, na.rm = TRUE), 2), total.model.c = round(mean(carbon_stock, na.rm = TRUE), 2)) %>%
    data.frame() -> df.carbon.avg

df.carbon.walk %>%
    select(soil_name, horizon, carbon_stock, carbon_g_cm3_WALK) %>%
    group_by(soil_name, horizon) %>%
    summarise(total.org.c = round(mean(carbon_g_cm3_WALK, na.rm = TRUE), 2), total.model.org.c = round(mean(carbon_stock, na.rm = TRUE), 2)) %>%
    data.frame() -> df.org.carbon.avg


# comp
model.carbon.table <- merge(df.carbon.avg, df.org.carbon.avg)

#conver tot KG per m 
model.carbon.table$total.c <- model.carbon.table$total.c * 0.001
model.carbon.table$total.model.c <- model.carbon.table$total.model.c * 0.001
model.carbon.table$total.org.c <- model.carbon.table$total.org.c * 0.001
model.carbon.table$total.model.org.c <- model.carbon.table$total.model.org.c  * 0.001


x11()
ggplot(model.carbon.table, aes(x = (total.org.c ), y = (total.c)))+
    geom_point(size = 2, shape = 21)+
    xlab("Measured Organic Carbon (WB method) [kg m-2]")+
    ylab("Measured Total Carbon [kg m-2]")+
    geom_smooth(method = "lm")

summary(lm( (total.c ) ~ (total.org.c ), data = model.carbon.table ))

x11()
ggplot(model.carbon.table, aes(x = (total.model.org.c ), y = (total.model.c)))+
    geom_point(size = 2, shape = 21)+
    xlab("Modelled Organic Carbon (WB method) [kg m-2]")+
    ylab("Modelled Total Carbon [kg m-2]")+
    geom_smooth(method = "lm")

summary(lm( (total.model.org.c ) ~ (total.model.c ), data = model.carbon.table ))
summary(lm( (total.org.c ) ~ (total.c ), data = model.carbon.table ))


# write to disk
write.csv(model.carbon.table, "southeast_soils_organic_c_output.csv")
