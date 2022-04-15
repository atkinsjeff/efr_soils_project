# compile data

#### the data for carbon as of april 6, is in kg per sq m. everything else is in g per sq m

# list the output files
output.files <- list.files(path = "./data/", pattern = "output")

# read in and work with files
ca <- read.csv("./data/southeast_soils_ca_output.csv")
k <- read.csv("./data/southeast_soils_k_output.csv")
mg <- read.csv("./data/southeast_soils_mg_output.csv")
p <- read.csv("./data/southeast_soils_p_output.csv")
total.n <- read.csv("./data/southeast_soils_n_output.csv")
org.c <- read.csv("./data/southeast_soils_organic_c_output.csv")
total.c <- read.csv("./data/southeast_soils_total_c_output.csv")
total.c2 <- read.csv("./data/southeast_soils_total_c_new_output.csv")
base.sat <- read.csv("./data/southeast_soils_base_sat_output.csv")
ph <- read.csv("./data/southeast_soils_ph_output.csv")

# total sites info

# import data
df <- read.csv("./data/southeast_soils.csv")
df$soil_name <- tolower(df$soil_name)
df.grid <- expand.grid(soil_name = unique(df$soil_name), horizon = c("0_10", "10_20", "20_40", "40_100"))


# lets work through here
jim <- merge(df.grid, ca[ , c(1:2, 12)], by = c("soil_name", "horizon"), all.x = TRUE)
jim <- merge(jim, k[ , c(1:2, 12)], by = c("soil_name", "horizon"), all.x = TRUE)
jim <- merge(jim, mg[ , c(1:2, 12)], by = c("soil_name", "horizon"), all.x = TRUE)
jim <- merge(jim, p[ , c(1:2, 12)], by = c("soil_name", "horizon"), all.x = TRUE)
jim <- merge(jim, total.n[ , c(1:2, 12)], by = c("soil_name", "horizon"), all.x = TRUE)
jim <- merge(jim, org.c[ , c(2:3, 6)], by = c("soil_name", "horizon"), all.x = TRUE)
jim <- merge(jim, total.c[ , c(2:3, 9)], by = c("soil_name", "horizon"), all.x = TRUE)
jim <- merge(jim, base.sat, by = c("soil_name", "horizon"), all.x = TRUE)
jim <- merge(jim, ph, by = c("soil_name", "horizon"), all.x = TRUE)

# add the bulk and physical properities

# bring in bulk density
df.bulk <- read.csv("./data/soil_series_bulk_density_horizon_method_southeast.csv")
#### merge to only those with bulk density
jim <- merge(jim, df.bulk[ , 1:4], by = c("soil_name", "horizon"), all.x = TRUE)


# bring in soil physical properities
# bring in physical    
phys <- read.csv("./data/soil_series_physical_southeast.csv")
jim <- merge(jim, phys, by = c("soil_name", "horizon"), all.x = TRUE)

# make tractable
length(which(jim$total_ca_horizon == 0))
length(which(jim$total_k_horizon == 0))
length(which(jim$total_mg_horizon == 0))
length(which(jim$total_n_horizon == 0))
length(which(jim$total.org.c == 0))
length(which(jim$total_c_soil == 0))
length(which(jim$bulk_density == 0))
length(which(jim$sand == 0))
length(which(jim$silt == 0))
length(which(jim$clay == 0))
length(which(jim$frag == 0))

##### now to adjust to Mg per HA
jim$total_ca_horizon <- jim$total_ca_horizon * 0.01
jim$total_k_horizon <- jim$total_k_horizon * 0.01
jim$total_mg_horizon <- jim$total_mg_horizon * 0.01
jim$total_p_horizon <- jim$total_p_horizon * 0.01
jim$total_n_horizon <- jim$total_n_horizon * 0.01
jim$total.org.c <- jim$total.org.c * 10
jim$total_c_soil <- jim$total_c_soil * 10


write.csv(jim, "southeast_soils_data_compiled_20220415.csv", row.names = FALSE)

jim %>%
    group_by(soil_name) %>%
    summarize()