#################################################################
# R Script for visulaising District data on the map
# JW
# 15-06-2018
#################################################################

options(digits = 3, scipen=999)

library(sp)
library(rgdal)
library(readr)
library(ggplot2)
library(cowplot)
library(tmap)
library(tmaptools)
library(broom)
library(tibble)
library(ggpolypath)
library(viridis)
library(grid)
library(Cairo)

wd <- setwd("D:/Scripts/R_Git/hexagon_tiles/SL")

# Read csv
pop_df <- read.csv("sl_local_poll_2018.csv", header = TRUE)

# Read shapefiles
real_dis <- readOGR(dsn = '.', layer = "sl_adm_2_real")
plot(real_dis)

real_dis <- merge(real_dis, pop_df, by.x = "d_code", by.y = "district_code")
real_dis@data$id <- rownames(real_dis@data)

hexa_dis <- readOGR(dsn = '.', layer = "sl_adm_2_hexgon_tiles")
plot(hexa_dis)

hexa_dis <- merge(hexa_dis, pop_df, by.x = "d_code", by.y = "district_code")
hexa_dis@data$id <- rownames(hexa_dis@data)

#######

######################################################################################
# plotting with tmap
real_dis@data$p_code <- factor(real_dis@data$p_code,
                               levels = c("WP", "SP", "EP", "NP", "CP", "NW", "NC", "SG", "UP"))
hexa_dis@data$p_code <- factor(hexa_dis@data$p_code,
                                  levels = c("WP", "SP", "EP", "NP", "CP", "NW", "NC", "SG", "UP"))

r <- tm_shape(real_dis) +
  tm_polygons("p_code", 
              style="pretty",
              palette="Paired",
              border.col = "white", border.alpha = .5, lwd = 1,
              auto.palette.mapping=FALSE) +
  #title=c("Happy Planet Index", "GDP per capita")) +
  tm_style_grey() + tm_legend(show=FALSE) +
  tm_text("d_3_code", size = 0.8, just = c("center", "center")) +
  tm_credits("\uA9 Jmatics", col = "blueviolet", 
             position = c(.85, .01), size = 0.7)

h <- tm_shape(hexa_dis) +
  tm_polygons("p_code", 
              style="pretty",
              palette="Paired",
              border.col = "white", border.alpha = .5, lwd = 4,
              auto.palette.mapping=FALSE) +
              #title=c("Happy Planet Index", "GDP per capita")) +
  tm_style_grey() + tm_legend(show=FALSE) +
  tm_text("d_3_code", size = 0.8, just = c("center", "center")) +
  tm_credits("\uA9 Jmatics", col = "blueviolet", 
             position = c(.85, .01), size = 0.7)

png(filename="real_vs_hexa.png", type="cairo",
    width = 3600, height = 2100, res = 300)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
print(r, vp=viewport(layout.pos.col = 1))
print(h, vp=viewport(layout.pos.col = 2))
dev.off()

######################


real_map <- tm_shape(real_dis) +
  tm_polygons("won",
              title = "Party",
              style = "pretty",
              border.col = "white", border.alpha = .5, lwd = 1,
              palette = c("indianred4", "goldenrod1", "forestgreen")) +
  tm_text("d_3_code", size = 0.7, just = c("center", "center")) +
  tm_layout(title = "District Results - LG Polls 2018",
            title.position = c(.10, .98),
            legend.position = c(.01, .01),
            frame = TRUE, 
            inner.margins = c(0.1, 0.1, 0.05, 0.05),
            bg.color="lightblue",
            space.color="lightblue") +
  tm_credits("Data by https://election.news.lk/", col = "black", 
             position = c(.52, .03), size = 0.75) +
  tm_credits("\uA9 Jmatics", col = "blueviolet", 
             position = c(.85, .01), size = 0.7) +
  tm_compass(position = c(.85, .80), color.light = "grey90")

real_map

hexa_map <- tm_shape(hexa_dis) +
  tm_polygons("won",
              title = "Party",
              style = "pretty",
              border.col = "white", border.alpha = .5, lwd = 2,
              palette = c("indianred4", "goldenrod1", "forestgreen")) +
  tm_text("d_3_code", size = 0.7, just = c("center", "center")) +
  tm_layout(title = "District Results - LG Polls 2018",
            title.position = c(.10, .98),
            legend.position = c(.01, .01),
            frame = TRUE, 
            inner.margins = c(0.1, 0.1, 0.05, 0.05),
            bg.color="lightblue",
            space.color="lightblue") +
  tm_credits("Data by https://election.news.lk/", col = "black", 
             position = c(.52, .03), size = 0.75) +
  tm_credits("\uA9 Jmatics", col = "blueviolet", 
             position = c(.85, .01), size = 0.7) +
  tm_compass(position = c(.85, .80), color.light = "grey90")

hexa_map

grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
print(real_map, vp=viewport(layout.pos.col = 1))
print(hexa_map, vp=viewport(layout.pos.col = 2))

png(filename="lg_poll_2018.png", type="cairo",
    width = 3600, height = 2100, res = 300)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
print(real_map, vp=viewport(layout.pos.col = 1))
print(hexa_map, vp=viewport(layout.pos.col = 2))
dev.off()
