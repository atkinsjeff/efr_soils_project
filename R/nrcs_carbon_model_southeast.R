#### CARBON MODEL

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
df[3101, 10] <- 100

df[4662, ]        # this one is just empty
df <- hulk[-4662, ]



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
# i think by multiplying it by 100 you get the per layer value
df.model$carbon_g_cm3 <- df.model$total_carbon_ncs * df.model$bulk_density * 100



# plot for test
x11()
ggplot(df.model, aes(x = total_carbon_ncs, y = horizon_midpoint, color = soil_name))+
    geom_point()+
    scale_y_reverse()+
    xlim(c(0, 40))+
    xlab("Total C [g m-2]")+
    ylab("Soil Depth [cm]")+
    theme_bw()

#################### A CLOSER LOOK

df.model %>%
    filter(soil_name == "bibb" | soil_name == "bonneau" | soil_name == "cecil" | soil_name == "dothan" | soil_name == "chewacla" |
               soil_name == "fuquay" | soil_name == "reddies" | soil_name == "sharkey" | soil_name == "smithdale" | soil_name == "tate") %>%
    data.frame() -> x

x11()
ggplot(x, aes(x = total_carbon_ncs, y = horizon_midpoint, color = soil_name))+
    geom_point()+
    scale_y_reverse()+
    #xlim(c(0, 300))+
    xlab("Total C [g m-2]")+
    ylab("Soil Depth [cm]")+
    theme_bw()+
    facet_wrap(. ~ soil_name)

# look at bibb
x %>%
    filter(soil_name == "bibb") -> bibb

nls.model <- nls(carbon_g_cm3 ~ b0 * exp( -b1 * horizon_midpoint), data = bibb, start = list(b0 = 10000, b1 = 1))

x11()
plot(bibb$carbon_g_cm3, bibb$horizon_midpoint)
lines(bibb$horizon_midpoint, fitted(nls.model), col='orange')








#########################################################

###### MODEL TEST BED
#### MODEL ONE 
test.table <- nls_table(df.model, carbon_g_cm3 ~ b0 * exp( -b1 * horizon_midpoint), 
                        mod_start = c( b0 = 1, b1 = 1),
                        .groups = "soil_name",
                        keep_model = TRUE)

test.table$aic <- NA

    # create test stat AIC
    for(i in 1:nrow(test.table)){
        if (is.data.frame(test.table$Reg[[i]]) == FALSE ){
            
            test.table$aic[i] = AIC(test.table$Reg[[i]]) 
        } else {
            test.table$aic[i] = NA
        }
    }


    # reformat to model one table
    model.one.table <- test.table
    model.one.table$test_model <- "model_one"
    

#### MODEL TWo
test.table <- nls_table(df.model, carbon_g_cm3 ~ b0 * exp(1 - b1)^horizon_midpoint, 
                        mod_start = c( b0 = 1, b1 = 1),
                        .groups = "soil_name",
                        keep_model = TRUE)

# make empty
test.table$aic <- NA

# create test stat AIC
for(i in 1:nrow(test.table)){
    if (is.data.frame(test.table$Reg[[i]]) == FALSE ){
    
    test.table$aic[i] = AIC(test.table$Reg[[i]]) 
    } else {
        test.table$aic[i] = NA
    }
}
    
    # reformat to model one table
    model.two.table <- test.table
    model.two.table$test_model <- "model_two"


# # MODEL THREE
# test.table <- nls_table(df.model, carbon_g_cm3 ~ b0 * exp(b1 * horizon_midpoint),
#                         mod_start = c( b0 = 1, b1 = 1),
#                         .groups = "pedlabsampnum",
#                         keep_model = TRUE)
# 
#     # create test stat AIC
#     for(i in 1:nrow(test.table)){
#         #if (is.data.frame(test.table$Reg[[i]]) == FALSE ){
#         
#         test.table$aic[i] = AIC(test.table$Reg[[i]]) 
#         # } else {
#         #     test.table$aic[i] = NA
#         # }
#     }
# 
#     # reformat to model one table
#     model.three.table <- test.table
#     model.three.table$test_model <- "model_three"

# MODEL three
test.table <- nls_table(df.model, carbon_g_cm3 ~ b0 / (1 - b1 * exp(-b2 * horizon_midpoint)), 
                        mod_start = c( b0 = 1, b1 = 1, b2 = 1),
                        .groups =  "soil_name",
                        keep_model = TRUE)

# make empty
test.table$aic <- NA


    # create test stat AIC
    for(i in 1:nrow(test.table)){
        if (is.data.frame(test.table$Reg[[i]]) == FALSE ){
        
        test.table$aic[i] = AIC(test.table$Reg[[i]]) 
        } else {
            test.table$aic[i] = NA
        }
    }    

    # reformat to model one table
    model.three.table <- test.table
    model.three.table$test_model <- "model_three"

# MODEL four
test.table <- nls_table(df.model, carbon_g_cm3 ~ b0  - b1 * log(horizon_midpoint), 
                        mod_start = c( b0 = 1, b1 = 1),
                        .groups = "soil_name",
                        keep_model = TRUE)

# make empty
test.table$aic <- NA

    # create test stat AIC
    for(i in 1:nrow(test.table)){
        if (is.data.frame(test.table$Reg[[i]]) == FALSE ){
        
        test.table$aic[i] = AIC(test.table$Reg[[i]]) 
            } else {
                test.table$aic[i] = NA
            }
    }    
    
    # reformat to model one table
    model.four.table <- test.table
    model.four.table$test_model <- "model_four"
    
    
    
#### concatenate
model.table <- rbind(model.one.table[, c("soil_name", "test_model", "aic")], 
                     model.two.table[, c( "soil_name", "test_model", "aic")],
                     model.three.table[, c("soil_name", "test_model", "aic")], 
                     model.four.table[, c( "soil_name", "test_model", "aic")])

# gather to make AIC table
aic.table <- data.frame(spread(model.table, key = test_model,  value = aic))

# replace the INf values
aic.table <- do.call(data.frame,                      # Replace Inf in data by NA
                   lapply(aic.table,
                          function(x) replace(x, is.infinite(x), NA)))

# remove rows with all NAs
aic.table <- aic.table[rowSums(is.na(aic.table)) != ncol(aic.table) - 1, ]



# need to remove model_three because of convergence issues
aic.table$model_three <- NULL

# find the best model
aic.table %>% 
    mutate(best_model = apply(.[ ,2:4], 1, function(x) names(x)[which.min(x)])) -> aic.table


# unlist that weird column
aic.table$best_model <- unlist(aic.table$best_model)

table(aic.table$best_model)


#
#
#
#



####
model.one.table %>%
    filter(soil_name == "bibb") -> model.bibb

summary(model.bibb$Reg[[1]])



##### 
##### 
##### 
##### 
##### 

# first clean the model one table
# remove rows with all NAs
model.one <- model.one.table[!is.na(model.one.table$aic),]
#model.one <- model.one[rowSums(is.na(model.one)) != ncol(model.one) - 4, ]

# merge filter to only the model ready data
df.model %>%
    filter(soil_name %in% model.one$soil_name) %>%
    select(pedlabsampnum, soil_name, hzn_top, hzn_bot, horizon, horizon_midpoint, carbon_g_cm3) -> df.carbon

# merge with model coeff
df.carbon <- merge(df.carbon, model.one[, c("soil_name", "b0", "b1")])

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




##### looking closer
df.carbon %>%
    filter(carbon_g_cm3 > 1000)




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
    
# bring in physical    
phys <- read.csv("./data/soil_series_physical_southeast.csv")

# 
carl <- merge(andy, phys)

carl$total_c_soil <- carl$total_c * ((100 - carl$frag)/100)
require(ggplot2)

# remove this wild boy
# carl %>%
#     filter(soil_name != "leon") %>%
#     data.frame() -> dave



x11()
ggplot(carl, aes(fill = horizon, y = total_c_soil, x = soil_name)) + 
    geom_bar(position="stack", stat="identity", color = "black")+
    scale_fill_viridis(discrete = T, option = "G") +
    ggtitle("Santee Soil Carbon Model Test") +
    theme_classic() +
    ylab("Total Carbon per soil horizon [kg m-2]")+
    xlab("")


# write total carbon
write.csv(carl, "southeast_soils_total_c_output.csv")

# df %>%
#     select(soil_name, total_carbon_ncs, organic_carbon_walkley_black) %>%
#     group_by(soil_name) %>%
#     summarise(no.total = n_distinct(total_carbon_ncs, na.rm = TRUE), no.walkley = n_distinct(organic_carbon_walkley_black, na.rm = TRUE)) 
# 

