# creates the bulk density for all the other models
# 
# 
# # calc bulk density by series, by layer. Where missing, fill in using global average of each layer
# 
# # needs to include soil phys per horizon:  sand, clay, silt, coarse fraction. Adjust in model for coarse by removal. 

require(tidyverse)

# import the soils data
#hulk <- read.csv("santee_soils.csv")
hulk <- read.csv("./data/southeast_soils.csv")

# # sort to what we need
# hulk %>%
#     select(contains("pedlabsampnum") | contains("hzn") | contains("bulk")) %>%
#     data.frame() -> bulk

###### ERROR CHECKING
which(is.na(hulk$hzn_bot))

# fix row 3101
hulk[3101, ]
hulk[3101, 10] <- 100

hulk[4662, ]        # this one is just empty
hulk <- hulk[-4662, ]

# sort to what we need
hulk %>%
    select(contains("soil_name") | contains("hzn") | contains("bulk")) %>%
    data.frame() -> bulk

# reduce the case
bulk$soil_name <- tolower(bulk$soil_name)

length(unique(bulk$soil_name))
# remove the errors
bulk %>%
    select(!contains("metho")) %>%
    data.frame() -> bulk

# remove the NULL
bulk[bulk == "NULL"] <- NA

# change to numeric
#bulk[ , 5:11] <- lapply(bulk[, c(5:11)],as.numeric)
bulk[ , c(2:3, 5)] <- lapply(bulk[, c(2:3, 5)],as.numeric)


# remove rows with all NAs
# bulk <- bulk[rowSums(is.na(bulk)) != ncol(bulk) - 4, ]
# bulk <- bulk[colSums(is.na(bulk)) != nrow(bulk)]
# 
# ###### ERROR CHECKING
# which(is.na(bulk$hzn_bot))
# 
# # fix row 3101
# bulk[3101, ]
# bulk[3101, 3] <- 100
# 
# bulk[4662, ]        # this one is just empty
# bulk <- bulk[-4662, ]
# 
# make midpoint
bulk$hzn_bot <- as.numeric(bulk$hzn_bot)
bulk$hzn_top <- as.numeric(bulk$hzn_top)
# make midpoint
bulk$horizon_midpoint <- (bulk$hzn_bot + bulk$hzn_top)/2



######

# make horizons
for (i in 1:nrow(bulk)){
    if (bulk$horizon_midpoint[i] <= 10) {
        bulk$horizon[i] <- "0_10"
    } else if (bulk$horizon_midpoint[i] > 10 & bulk$horizon_midpoint[i] <= 20) {
        bulk$horizon[i] <- "10_20"
    } else if (bulk$horizon_midpoint[i] > 20 & bulk$horizon_midpoint[i] <= 40) {
        bulk$horizon[i] <- "20_40"
    } else if (bulk$horizon_midpoint[i] > 40) {
        bulk$horizon[i] <- "40_100"
    } else {
        bulk$horizon[i] <- NA
    }
}

which(is.na(bulk$horizon_midpoint))

#
table(bulk$horizon)

# # make the means
# bulk$bulk_density <- rowMeans(bulk[, 5:8], na.rm = TRUE)
# 
# # filter down and take the means
# bulk %>%
#     select(soil_name, horizon_midpoint, horizon, bulk_density) %>%
#     group_by(soil_name, horizon) %>%
#     summarize(bulk_density = mean(bulk_density, na.rm = TRUE)) %>%
#     data.frame() -> df.bulk

# sorting to get values for counting
hulk %>%
    filter(is.na(bulk_density_oven_dry) == FALSE) %>%
    data.frame() -> hulk2
length(unique(hulk2$pedon_key))

#### ADAPTED FOR SOUTHEAST data which only have on soil bulk density value
# filter down and take the means
bulk %>%
    select(soil_name, horizon_midpoint, horizon, bulk_density_oven_dry) %>%
    group_by(soil_name, horizon) %>%
    summarize(bulk_density = mean(bulk_density_oven_dry, na.rm = TRUE), 
              bulk_density_sd = sd(bulk_density_oven_dry, na.rm = TRUE),
              n = sum(!is.na(bulk_density_oven_dry))) %>%
    data.frame() -> df.bulk



# make all horizons
#### calculate total carbon per horizon
pedons <- unique(df.bulk$soil_name)
horizons <- c("0_10", "10_20", "20_40", "40_100")

# bring together
df.all.bulk <- as.data.frame(merge(pedons, horizons))

# rename
colnames(df.all.bulk) <- c("soil_name", "horizon")

# bring in coeff
df.bulk.final <- merge(df.all.bulk, df.bulk, all.x = TRUE)
#### make wide for table
df.bulk.final %>% 
    pivot_wider(names_from = horizon, values_from = c("bulk_density", "bulk_density_sd", "n")) %>%
    data.frame() -> bulk.wide
# 
# # this makes a hack PEDON level bulk density
# df.bulk.final %>%
#     group_by(pedlabsampnum) %>%
#     summarise(bulk_density = mean(bulk_density, na.rm = TRUE)) %>%
#     data.frame() -> bulk.bulk
# 
# 
# write.csv(bulk.bulk, "pedon_bulk_density.csv")
# 


# # need to replace NAs 
# sanity check, first we keep the og dataframe
zzz <- df.bulk.final
# 
# # first method uses linear interpolation between layers
# for (i in 1:nrow(df.bulk.final)){
#  
#     if (is.na(df.bulk.final$bulk_density[i]) == FALSE) {
#         next()
#     } else if (is.na(df.bulk.final$bulk_density[i]) == TRUE) {
#         df.bulk.final$bulk_density[i] = (df.bulk.final$bulk_density[i - 1] + df.bulk.final$bulk_density[i + 1]) / 2   
#     }
# }
# 
# 
# # write to file
# #write.csv(df.bulk.final, "soil_series_bulk_density_interpolated.csv")

# now we want to make replace NAs using the broad scale, horizon wide values. 
zzz %>%
    group_by(horizon) %>%
    summarise(bulk_density = mean(bulk_density, na.rm = TRUE)) %>%
    data.frame() -> bulk.horizon


zzz %>%
    group_by(horizon) %>%
    summarise(bulk_density_sd = sd(bulk_density, na.rm = TRUE)) %>%
    data.frame() -> bulk.horizon.sd

bulk.horizon <- merge(bulk.horizon, bulk.horizon.sd)

# fills in with horizon mean of all data
zzz$bulk_density[is.na(zzz$bulk_density)] <- bulk.horizon$bulk_density[match(zzz$horizon, bulk.horizon$horizon)][which(is.na(zzz$bulk_density))]
zzz$bulk_density_sd[is.na(zzz$bulk_density_sd)] <- bulk.horizon$bulk_density_sd[match(zzz$horizon, bulk.horizon$horizon)][which(is.na(zzz$bulk_density_sd))]


zzz[is.na(zzz)] = 0
#### make wide for table
zzz %>% 
    pivot_wider(names_from = horizon, values_from = c("bulk_density", "bulk_density_sd", "n")) %>%
    data.frame() -> zzz.wide
# 


# # write to file
# write.csv(zzz, "soil_series_bulk_density_horizon_method_southeast.csv", row.names = FALSE)
# write.csv(zzz.wide, "soil_series_bulk_density_horizon_method_southeast_wide_format.csv", row.names = FALSE)





# soil physical properties 

# keep the 2 mm fraction
# sort to what we need
hulk %>%
    select(contains("soil_name") | contains("hzn") | contains("total_frag"), sand_total, silt_total, clay_total) %>%
    data.frame() -> phys



# reduce the case
phys$soil_name <- tolower(phys$soil_name)


# remove the NULL
phys[phys == "NULL"] <- NA

# # change to numeric
# phys[ , 5:8] <- lapply(phys[, c(5:8)],as.numeric)

# make midpoint
phys$hzn_bot <- as.numeric(phys$hzn_bot)
phys$hzn_top <- as.numeric(phys$hzn_top)
# make midpoint
phys$horizon_midpoint <- (phys$hzn_bot + phys$hzn_top)/2

# make horizons
for (i in 1:nrow(phys)){
    if (phys$horizon_midpoint[i] <= 10) {
        phys$horizon[i] <- "0_10"
    } else if (phys$horizon_midpoint[i] > 10 & phys$horizon_midpoint[i] <= 20) {
        phys$horizon[i] <- "10_20"
    } else if (phys$horizon_midpoint[i] > 20 & phys$horizon_midpoint[i] <= 40) {
        phys$horizon[i] <- "20_40"
    } else if (phys$horizon_midpoint[i] > 40) {
        phys$horizon[i] <- "40_100"
    } else {
        phys$horizon[i] <- NA
    }
}


table(phys$horizon)
which(is.na(phys$hzn_bot))

#### replace the missing sand values and the missing greater than 2mm
# may have to run it a couple of times
phys$sand_total[is.na(phys$sand_total)] <- 100 - (phys$silt_total + phys$clay_total)
phys$silt_total[is.na(phys$silt_total)] <- 100 - (phys$sand_total + phys$clay_total)
phys$clay_total[is.na(phys$clay_total)] <- 100 - (phys$silt_total + phys$sand_total)


# error checking
which(is.na(phys$silt_total))

phys$total_frag_wt_pct_gt_2_mm_ws[is.na(phys$total_frag_wt_pct_gt_2_mm_ws)] <- 0
# filter down and take the means
phys %>%
    group_by(soil_name, horizon) %>%
    summarize(sand = mean(sand_total, na.rm = TRUE), 
              silt = mean(silt_total, na.rm = TRUE),
              clay = mean(clay_total, na.rm = TRUE),
              frag = mean(total_frag_wt_pct_gt_2_mm_ws, na.rm = TRUE)) %>%
    data.frame() -> df.phys

# make all horizons
#### calculate total carbon per horizon
soil_names<- unique(df.phys$soil_name)
horizons <- c("0_10", "10_20", "20_40", "40_100")

# bring together
df.all.phys <- as.data.frame(merge(soil_names, horizons))

# rename
colnames(df.all.phys) <- c("soil_name", "horizon")

# bring in coeff
df.phys.final <- merge(df.all.phys, df.phys, all.x = TRUE)


#### note the missing ones
# now we want to make replace NAs using the broad scale, horizon wide values. 
df.phys.final %>%
    group_by(horizon) %>%
    summarize(sand = mean(sand, na.rm = TRUE), 
              silt = mean(silt, na.rm = TRUE),
              clay = mean(clay, na.rm = TRUE),
              frag = mean(frag, na.rm = TRUE)) %>%
    data.frame() -> phys.horizon


# fills in with horizon mean of all data
df.phys.final$sand[is.na(df.phys.final$sand)] <- phys.horizon$sand[match(df.phys.final$horizon, phys.horizon$horizon)][which(is.na(df.phys.final$sand))]
df.phys.final$silt[is.na(df.phys.final$silt)] <- phys.horizon$silt[match(df.phys.final$horizon, phys.horizon$horizon)][which(is.na(df.phys.final$silt))]
df.phys.final$clay[is.na(df.phys.final$clay)] <- phys.horizon$clay[match(df.phys.final$horizon, phys.horizon$horizon)][which(is.na(df.phys.final$clay))]
df.phys.final$frag[is.na(df.phys.final$frag)] <- phys.horizon$frag[match(df.phys.final$horizon, phys.horizon$horizon)][which(is.na(df.phys.final$frag))]

# # write to file
#write.csv(df.phys.final, "soil_series_physical_southeast.csv", row.names = FALSE)


























