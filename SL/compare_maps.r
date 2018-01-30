#################################################################
# R Script for visulaising District data on the map
# JW
# 30-01-2018
#################################################################

library(sp)
library(rgdal)
library(readr)
library(ggplot2)
library(cowplot)
library(tmap)
library(tmaptools)

wd <- setwd("D:/Scripts/R_Git/hexagon_tiles/SL")

# Read csv
pop_df <- read.csv("sl_pop_2012.csv", header = TRUE)

# Read shapefiles
real_dis <- readOGR(dsn = '.', layer = "sl_adm_2_real")
plot(real_dis)

real_dis <- merge(real_dis, pop_df, by.x = "d_code", by.y = "district_code")

hexa_dis <- readOGR(dsn = '.', layer = "sl_adm_2_hexgon_tiles")
plot(hexa_dis)

hexa_dis <- merge(hexa_dis, pop_df, by.x = "d_code", by.y = "district_code")

# Plotting

real_map <- tm_shape(real_dis) +
              tm_polygons("total_pop_2012", title = "2012 Population in thousands",
              border.col = "gray50", border.alpha = .5, lwd = 2) +
              tm_credits("Population data @ Department of Census and Statistics Sri Lanka\n Author @ Jmatics",
              position = c("left", "bottom"), size = 0.7) +
              tm_text("d_code", just = c("center", "center")) +
              tm_layout(title = "", 
                title.position = c("center", "top"), 
                legend.position = c("right", "top"), 
                frame = TRUE, 
                inner.margins = c(0.1, 0.1, 0.05, 0.05))
real_map

#

hexa_map <- tm_shape(hexa_dis) +
  tm_polygons("total_pop_2012", title = "2012 Population in thousands",
              border.col = "gray50", border.alpha = .5, lwd = 2) +
  tm_credits("Population data @ Department of Census and Statistics Sri Lanka\n Author @ Jmatics",
             position = c("left", "bottom"), size = 0.7) +
  tm_text("d_code", just = c("center", "center")) +
  tm_layout(title = "", 
            title.position = c("center", "top"), 
            legend.position = c("right", "top"), 
            frame = TRUE, 
            inner.margins = c(0.1, 0.1, 0.05, 0.05))
hexa_map
