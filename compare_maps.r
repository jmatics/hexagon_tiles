#################################################################
# R Script for visulaising District data on the map
# JW
# 30-01-2018
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
pop_df <- read.csv("sl_pop_2012.csv", header = TRUE)

# Read shapefiles
real_dis <- readOGR(dsn = '.', layer = "sl_adm_2_real")
plot(real_dis)

real_dis <- merge(real_dis, pop_df, by.x = "d_code", by.y = "district_code")
real_dis@data$id <- rownames(real_dis@data)

hexa_dis <- readOGR(dsn = '.', layer = "sl_adm_2_hexgon_tiles")
plot(hexa_dis)

hexa_dis <- merge(hexa_dis, pop_df, by.x = "d_code", by.y = "district_code")
hexa_dis@data$id <- rownames(hexa_dis@data)

######################################################################################
# Plotting

# plotting with ggplot2

# Transform to data for ggplot

spdf_to_df <- function(spdf){
  tidy(spdf) %>% 
    merge(spdf, by = 'id') %>% 
    as.tibble
}

#real_df <- spdf_to_df(real_dis)
real_points <- fortify(real_dis, region = "id")
real_df <- merge(real_points, real_dis@data, by = "id")

real_center <- as.data.frame(coordinates(real_dis))
real_center$d_code <- as.vector(real_dis@data$d_code)
names(real_center) <- c("long", "lat", "d_code")

windowsFonts(Helvetica=windowsFont("Helvetica-Regular"))

real_gmap <- ggplot(data = real_df, aes(long, lat, group = group, fill = total_pop_2012)) +
              geom_polygon() + geom_path(colour="gray", lwd=0.05) + 
              coord_equal() + theme_map() +
              geom_text(data = real_center, aes(label = d_code, x = long, y = lat, 
                        group = NULL, fill = NULL), family="Helvetica", colour="black",
                        hjust = 0.5, vjust = 0.5, size=3) +
              #scale_fill_viridis(option = "plasma", direction = -1) +
              scale_fill_gradient(low = "yellow", high = "red", 
                                  name = "2012 Population \nin thousands") +
              labs(x = NULL, y = NULL, 
                caption = "Distrcit level population with actual district boundaries") +
              theme(plot.caption = element_text(size=8, family="Helvetica", face="plain"),
                    legend.title = element_text(size=10, family="Helvetica", face="plain"),
                    legend.text = element_text(size=8, family="Helvetica", face="plain")) +
              annotate(geom="text", x=550000, y=630000, 
                        label="Pop Data: DCS, SL; GIS data: GDAM", 
                        colour="chocolate4", size=2, family="Helvetica", fontface="plain") +
              annotate(geom="text", x=565000, y=620000, 
                        label="Product of Jamtics", 
                        colour="blueviolet", size=2, family="Helvetica", fontface="bold")


real_gmap

#############

hexa_points <- fortify(hexa_dis, region = "id")
hexa_df <- merge(hexa_points, hexa_dis@data, by = "id")

hexa_center <- as.data.frame(coordinates(hexa_dis))
hexa_center$d_code <- as.vector(hexa_dis@data$d_code)
names(hexa_center) <- c("long", "lat", "d_code")

hexa_gmap <- ggplot(data = hexa_df, aes(long, lat, group = group, fill = total_pop_2012)) +
  geom_polygon() + geom_path(colour="gray", lwd=0.05) + 
  coord_equal() + theme_map() +
  geom_text(data = hexa_center, aes(label = d_code, x = long, y = lat, 
                                    group = NULL, fill = NULL), family="Helvetica", colour="black",
            hjust = 0.5, vjust = 0.5, size=3) +
  scale_fill_gradient(low = "yellow", high = "red", 
                      name = "2012 Population \nin thousands") +
  labs(x = NULL, y = NULL, 
       caption = "Distrcit level population with actual district boundaries") +
  theme(plot.caption = element_text(size=8, family="Helvetica", face="plain"),
        legend.title = element_text(size=10, family="Helvetica", face="plain"),
        legend.text = element_text(size=8, family="Helvetica", face="plain")) +
  annotate(geom="text", x=550000, y=630000, 
           label="Pop Data: DCS, SL; GIS data: GDAM", 
           colour="chocolate4", size=2, family="Helvetica", fontface="plain") +
  annotate(geom="text", x=565000, y=620000, 
           label="Product of Jamtics", 
           colour="blueviolet", size=2, family="Helvetica", fontface="bold")


hexa_gmap


######################################################################################
# plotting with tmap
real_map <- tm_shape(real_dis) +
              tm_polygons("total_pop_2012",
                          title = "Population\n(x 1000)",
                          style = "pretty",
                          palette= "RdPu") +
              tm_text("d_code", size = 0.7, just = c("center", "center")) +
              tm_layout(title = "Distrcit level population in 2012", 
                        title.position = c(.20, .98),
                        legend.position = c(.00, .00),
                        frame = TRUE, 
                        inner.margins = c(0.1, 0.1, 0.05, 0.05),
                        bg.color="lightblue",
                        space.color="lightblue") +
              tm_credits("Data by DCS, SL", col = "black", 
                         position = c(.75, .03), size = 0.75) +
              tm_credits("\uA9 Jmatics", col = "blueviolet", 
                         position = c(.85, .01), size = 0.7) +
              tm_compass(position = c(.85, .80), color.light = "grey90")

real_map

hexa_map <- tm_shape(hexa_dis) +
  tm_polygons("total_pop_2012",
              title = "Population\n(x 1000)",
              style = "pretty",
              palette= "RdPu") +
  tm_text("d_code", size = 0.7, just = c("center", "center")) +
  tm_layout(title = "Distrcit level population in 2012", 
            title.position = c(.20, .98),
            legend.position = c(.00, .00),
            frame = TRUE, 
            inner.margins = c(0.1, 0.1, 0.05, 0.05),
            bg.color="lightblue",
            space.color="lightblue") +
  tm_credits("Data by DCS, SL", col = "black", 
             position = c(.75, .03), size = 0.75) +
  tm_credits("\uA9 Jmatics", col = "blueviolet", 
             position = c(.85, .01), size = 0.7) +
  tm_compass(position = c(.85, .80), color.light = "grey90")

hexa_map

grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
print(real_map, vp=viewport(layout.pos.col = 1))
print(hexa_map, vp=viewport(layout.pos.col = 2))

png(filename="real_vs_hexa.png", type="cairo",
    width = 3600, height = 2100, res = 300)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
print(real_map, vp=viewport(layout.pos.col = 1))
print(hexa_map, vp=viewport(layout.pos.col = 2))
dev.off()
