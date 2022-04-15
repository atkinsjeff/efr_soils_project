# install.packages('soilDB')
# install.packages('aqp')
# install.packages('mapview')
# install.packages('sf')
# install.packages('soiltexture')
# install.packages('scales')
# install.packages('Hmisc')


# check to see if packages are already installed, otherwise install and load
if(!require(soilDB)){install.packages('soilDB')}
library(soilDB)

if(!require(aqp)){install.packages('aqp')}
library(aqp)

if(!require(mapview)){install.packages('mapview')}
library(mapview)

if(!require(sf)){install.packages('sf')}
library(sf)

if(!require(soiltexture)){install.packages('soiltexture')}
library(soiltexture)

if(!require(scales)){install.packages('scales')}
library(scales)

if(!require(Hmisc)){install.packages('Hmisc')}
library(Hmisc)





# remove missing values

phys <- read.csv("./data/soil_series_physical_southeast.csv")
df <- merge(df, phys)

ssc <- na.omit(phys[, c('sand', 'silt', 'clay')])

# re-name columns, required by textureTriangleSummary()
names(ssc) <- c('SAND', 'SILT', 'CLAY')

x11()
textureTriangleSummary(ssc, cex = 0.5, main = "Soil Texture by Horizon  for Soil Series Represented in Southeastern US EFRs (n = 149 Soils; n = 596 Total Horizons")




# source function for plot to change
# 
# function (ssc, p = c(0.05, 0.5, 0.95), delta = 1, rv.col = "red", 
#           range.border = "black", range.col = "RoyalBlue", 
#           range.alpha = 80, range.lty = 1, range.lwd = 2, main = "Soil Textures", 
#           legend.cex = 0.75, legend = TRUE, ...) 
# {
#     if (!requireNamespace("soiltexture", quietly = TRUE) | 
#         !requireNamespace("Hmisc", quietly = TRUE)) {
#         stop("packages `Hmisc` and `soiltexture` are required", 
#              call. = FALSE)
#     }
#     name.check <- sapply(c("SAND", "SILT", "CLAY"), 
#                          function(i) {
#                              any(names(ssc) %in% i)
#                          })
#     if (!all(name.check)) {
#         stop("`ssc` must contain columns: `SAND`, `SILT`, `CLAY`.")
#     }
#     ssc <- ssc[, c("SAND", "SILT", "CLAY")]
#     range.col <- rgb(t(col2rgb(range.col)), maxColorValue = 255, 
#                      alpha = range.alpha)
#     rv.text <- paste0("Sample RV (", paste(p[c(2)], collapse = "-"), 
#                       ")")
#     low.high.range.text <- paste0("Low-High Range (", paste(p[c(1, 
#                                                                 3)], collapse = "-"), ")")
#     legend.text <- c(rv.text, low.high.range.text)
#     legend.cols <- c("black", "black")
#     legend.bg <- c(rv.col, range.col)
#     legend.pch <- c(22, 22)
#     TT <- soiltexture::TT.plot(class.sys = "USDA-NCSS.TT", 
#                                main = main, tri.sum.tst = FALSE, cex.lab = 0.75, cex.axis = 0.75, 
#                                frame.bg.col = "white", class.lab.col = "black", 
#                                lwd.axis = 1.5, lwd.lab = 2, arrows.show = TRUE)
#     res <- .get.ssc.low.rv.high(ssc, p = p, delta = delta, TT.obj = TT)
#     polygon(res$range$x, res$range$y, col = range.col, border = range.border, 
#             lty = range.lty, lwd = range.lwd)
#     soiltexture::TT.points(tri.data = ssc, geo = TT, tri.sum.tst = FALSE, 
#                            lwd = 1, ...)
#     soiltexture::TT.points(tri.data = data.frame(t(res$stats[2, 
#     ])), geo = TT, bg = rv.col, pch = 22, cex = 1.25, lwd = 1, 
#     tri.sum.tst = FALSE)
#     if (legend) {
#         legend("topleft", legend = legend.text, pt.bg = legend.bg, 
#                pch = legend.pch, col = legend.cols, bty = "n", 
#                cex = legend.cex, pt.cex = 1.25, horiz = TRUE)
#     }
#     invisible(res$stats)
# }
# <bytecode: 0x000000dd77225460>
#     <environment: namespace:aqp>
#     > 