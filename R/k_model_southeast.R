# k modeling
# 

Ca_nH4_ph_7_method, K_nh4_ph_7_method and mg_nh4_ph_7_method 

4B1a – moisture content as a percentage of <2-mm. report procedure code 4B1a and the equilibrium tension 

6N2, 6O2, and 6Q2 – report the extractable Ca, K and Mg in units of meq 100g-1 of oven dry soil to the nearest 0.1 meq 100g-1 

#### for k in Meq per 100 g the converision is 1 Meq per 100 g = 390 ppm
# 1 ppm = 0.0001 % 

# the unique methods for K are
[1] "6Q2b"   "4B1a1a" "NK"     "6Q2"    "4B1a1"  "6Q2a"   ""       "6Q2f"  
[9] "6Q2c"  
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

# Filter down and average

df %>%
    group_by(k_nh4_ph_7_method) %>%
    summarize(k = range(k_nh4_ph_7, na.rm = TRUE))
    

library(ggplot2)
x11()
ggplot(df, aes(x = k_nh4_ph_7))+
    geom_histogram()+
    theme_bw()+
    xlim(c(0, 5))+
    facet_wrap(. ~ k_nh4_ph_7_method, nrow  = 3)

# make the methods more easy 
df$k_nh4_ph_7_method <- substr(df$k_nh4_ph_7_method, 0, 2)
####
df %>%
    dplyr::select(contains("soil_name") | contains("horizon") | contains("k_")) %>%
    #dplyr::group_by(soil_name, horizon) %>%
    dplyr::mutate(k_per = case_when(k_nh4_ph_7_method == "6Q" ~ k_nh4_ph_7 * 0.039,
                                    k_nh4_ph_7_method == "4B" ~ k_nh4_ph_7,
                                    k_nh4_ph_7_method == "NK" ~ 0,
                                    k_nh4_ph_7_method == "" ~ 0)) %>%
    dplyr::group_by(soil_name, horizon) %>%
    dplyr::summarise(k_per = mean(k_per, na.rm = TRUE)) %>%
    data.frame() -> df.k

##### numerical interpolation of 
# first we make all of the correct values
df.grid <- expand.grid(soil_name = unique(df$soil_name), horizon = c("0_10", "10_20", "20_40", "40_100"))

# merge together
bob <- merge(df.grid, df.k,  by = c("soil_name", "horizon"), all.x = TRUE)

# linear interpolation between values
bob %>%
    select(soil_name, horizon, k_per) %>%
    #filter(!horizon == "0_10" && is.na(ca_per) == TRUE) %>%
    group_by(soil_name) %>%
    mutate(horizon = horizon,
           k_per = na.approx(k_per, na.rm = FALSE, rule = 2)) %>%
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

# makes grams of P per square meter to one meter depth
# P is in mg per kg, so convert it! by multiplying by 0.0001
df$k_g_m2 <- (df$k_per) * df$bulk_density * ((100 - df$frag)/100)

# plot for test
x11()
hist(df$k_g_m2)

##### calculate p BRAY 
# #### MODEL ONE Form
# model.table <- nls_table(df.ca, ca_nh4_ph_7 ~ b0 * exp( -b1 * horizon_midpoint), 
#                            mod_start = c( b0 = 1, b1 = 1),
#                            .groups = "soil_name",
#                            keep_model = TRUE)
# 
# # make empty
# model.table$aic <- NA
# 
# 
# # create test stat AIC
# for(i in 1:nrow(model.table)){
#     if (is.data.frame(model.table$Reg[[i]]) == FALSE ){
#         
#         model.table$aic[i] = AIC(model.table$Reg[[i]]) 
#     } else {
#         model.table$aic[i] = NA
#     }
# }    
# 
# 
# 
# # merge filter to only the model ready data
# df.ca%>%
#     filter(soil_name %in% model.table$soil_name) -> df.ca.model
# 
# # merge with model coeff
# df.ca.model <- merge(df.ca.model, model.table[, c("soil_name", "b0", "b1")])
# 
# # modeled carbon
# df.ca.model$ca_stock <- round(df.ca.model$b0 * exp( -df.ca.model$b1 * df.ca.model$horizon_midpoint), 5)
# 
# x11()
# ggplot(df.ca.model, aes(x = ca_nh4_ph_7 , y = ca_stock))+
#     geom_point(size = 2, shape = 21)+
#     xlab("Measured Ca [g m-2]")+
#     ylab("Modelled Ca [g m-2]")+
#     xlim(c(0, 50))+
#     ylim(c(0, 50))+
#     geom_smooth(method = "lm")
# 
# # model test
# summary(lm(ca_nh4_ph_7 ~ ca_stock, data = df.ca.model))


# 
df %>%
    mutate(total_k_horizon = case_when(horizon == "0_10" ~ k_g_m2 * 10, 
                                        horizon == "10_20" ~ k_g_m2 * 10,
                                        horizon == "20_40" ~ k_g_m2 * 20,
                                        horizon == "40_100" ~ k_g_m2 * 60)) %>%
    data.frame() -> df.final


# #### make the leon problem
# make tractable
# make tractable
length(which(df.final$total_p_horizon == 0))
df.final$total_k_horizon[df.final$total_k_horizon == 0] <- NA
df.final$total_k_horizon <- round(df.final$total_k_horizon, 5)
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
ggplot(df.final, aes(fill = horizon, y = total_k_horizon, x = soil_name)) + 
    geom_bar(position="stack", stat="identity", color = "black")+
    scale_fill_viridis(discrete = T, option = "G") +
    ggtitle("SE EFT ca Model Test") +
    theme_classic() +
    ylab("Total K per soil horizon [g m-2]")+
    xlab("")


# write to file
write.csv(df.final, "./data/southeast_soils_k_output.csv", row.names = FALSE)
