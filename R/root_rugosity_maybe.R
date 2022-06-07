depth = c(10, 20, 30, 40, 50, 60)
root.mass = c(0.64, 0.64, NA, NA, 0.84)
bulk = c(1.68, 1.80, NA, 1.92,NA, 1.99)

sd(andy$frf) / mean(andy$frf)


df <- data.frame(depth =depth, 
                 root.density = root.mass, 
                 bulk = bulk)

df$frf = df$root.density / df$bulk


df %>%
    # select(soil_name, horizon, base_sat) %>%
    # #filter(!horizon == "0_10" && is.na(ca_per) == TRUE) %>%
    # group_by(soil_name) %>%
    mutate(depth = depth,
           root.density = na.approx(root.density, na.rm = FALSE, rule = 2),
           bulk = na.approx(bulk, na.rm = FALSE, rule = 2),
           frf = na.approx(frf, na.rm = FALSE, rule = 2)) %>%
    data.frame() -> andy

head(andy)

#andy$depth.z <- andy$frf * andy$depth
depth.bin = (sum(andy$frf * andy$depth) / sum(andy$frf)) 

std.std = ((sd(c(50.3, 45.5, 20.5)))^2) /  100
mean.std = mean(andy$depth.z) / 100
