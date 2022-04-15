# carbon comp
total.c <- read.csv("./data/southeast_soils_total_c_output.csv")
total.c2 <- read.csv("./data/southeast_soils_total_c_new_output.csv")


x <- merge(total.c[, c("soil_name", "total_c_soil")], total.c2[, c("soil_name", "c_g_m2")])


# make comp
x$soil_name <- as.factor(x$soil_name)

# rename
colnames(x) <- c("soil_name","nls","linear")
# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)
y <- gather(x, model, carbon, nls:linear, factor_key=TRUE)
# data_long
# x11()
# ggplot(x, aes(x = total_c_soil, y = c_g_m2))+
#     geom_point()+
#     xlab("Fancy model")+
#     ylab("Simple model")+
#     xlim(c(0, 20))+
#     ylim(c(0, 20))+
#     geom_abline(slope = 1)

ggplot(sample, aes(value, colour=shortname, group=shortname)) + geom_density()

x11()
ggplot(y, aes(x = carbon, color = model, group = model))+
    geom_density(size = 1.5)+
    ylab("Proportion of Total Measurements")+
    xlab("Carbon in grams per square meter")+
    theme_classic()+
    xlim(c(0, 10))

y %>%
    group_by(model) %>%
    summarise(median(carbon, na.rm = TRUE),
              sd(carbon, na.rm = TRUE))

#### do stack bar plot comp
z <- merge(total.c[, c("soil_name", "horizon", "total_c_soil")], total.c2[, c("soil_name", "horizon", "c_g_m2")])
# rename
colnames(z) <- c("soil_name","horizon", "nls","linear")
# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)
a <- gather(z, model, carbon, nls:linear, factor_key=TRUE)
# data_long

x11()
ggplot(a, aes(fill = horizon, y = carbon, x = soil_name)) + 
    geom_bar(position="stack", stat="identity", color = "black")+
    scale_fill_viridis(discrete = T, option = "G") +
    ggtitle("SE EFT Carbon Model Test") +
    theme_classic() +
    ylab("Total C per soil horizon [g m-2]")+
    xlab("")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    facet_wrap(. ~ model, ncol = 1)

a$carbon <- a$carbon * 10
x11()
ggplot(a, aes(fill = horizon, y = carbon, x = soil_name)) + 
    geom_bar(position="stack", stat="identity", color = "black")+
    scale_fill_viridis(discrete = T, option = "G") +
    ggtitle("SE EFT Carbon Model Test") +
    theme_classic() +
    ylab("Total C per soil horizon [Mg ha^-1]")+
    xlab("")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    facet_wrap(. ~ model, ncol = 1)

