
#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          January 01, 2020
# Purpose:       Note
#
#
# Copyright (c): Logan Stundal, 2020
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------#
#
# Notes:
#
# ----------------------------------- #
#
# One of the limitations of Hammond and Weidmann is they evaluate the spatial dynamics of machine
# coded data sources using spatial covariates which lack the precision to identify patterns which 
# would let us make conclucions about machine reporting acuracy vs. human coders.
#
# Earlier work could not have done better as computing power and data access limitations prevented
# the analysis of these more precise covariates in models to evaluate event data spatial accuracy. 
# These limitations no longer persist. With widely avaialable spatial covariates which exhibit 
# significant variation at resolutions as fine as 30 meters, we now have the ability to evaluate 
# how models which employ computer-coded violence events compare against human-coded equivalents 
# to capture true on-the-ground patterns relating features of geography / topography to the location 
# of violence perpetuated by armed actors.
#
# FURTHER NOTE -- THIS ONE SMALL 1-DEGREE SLICE CONTAINS 112 MUNICIPALITIES, OR OVER 10% OF OUR SAMPLE.
#     - If we used PRIO we would have to essentially assign 10 percent of our units with the same value 
#       for our spatial covariates which, I believe, would significantly and negatively impact our 
#       capacity to evaluate patterns that could indicate the accuracy of machine coded data when 
#       compared to human coded.
#
# ----------------------------------- #
#
# I need to create a DEM extract script for Google Earth Engine to produce relief maps. --
# depending on the output of the SPDE model (probs. estimated at mesh verticies to raster?), may be able to
# produce a relief map for the probability of insurgent attack reporting which would be super cool.
#
# https://stackoverflow.com/questions/52889540/speed-up-rendering-of-large-heatmap-from-ggplot-in-r
#
# ... Also This seems like a PERFECT workflow to reduce the resolution of a raster in R....
#
# 1. Load original raster.
# 2. convert to a dataframe.
# 3. Downsample using tidyverse setup above.
# 4. Convert back into a raster using rasterfromxyz()
# 5. Profit???
#
# Better method for producing inset maps with library(cowplot)
#   https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-3.html
#
#-----------------------------------------------------------------------------#



# ADMINISTRATIVE --------------------------------------------------------------

#---------------------------#
# Clear working directory
#---------------------------#
rm(list = ls())
dev.off()
# .rs.restartR()

#---------------------------#
# Load required libraries
#---------------------------#
library(raster)
library(sf)
library(tidyverse)
library(gridExtra)
library(grid)
library(cowplot)

#---------------------------#
# Set working directory
#---------------------------#
setwd('c:/users/logan/googledrive/umn/research/ra_john/event_data_project/')

#---------------------------#
# Load data
#---------------------------#

# COLOMBIA VECTOR AND ATTRIBUTE DATA
load('data/colombia.rdata')

# RASTERS
forest <- raster('data/covariate_data/googleearthengine/colombia_forestcover_2005.tif')
# night  <- raster('data/covariate_data/googleearthengine/colombia_NightLights_2002_2009_mean.tif')
# pop    <- raster('data/covariate_data/googleearthengine/Colombia_Population_2002_2009_mean.tif')
# tri    <- raster('data/covariate_data/googleearthengine/Colombia_TRI_mean.tif')

# PRIO RASTER DATA
prio   <- read_csv('c:/users/logan/googledrive/umn/research/data/priogrid/PRIO-GRID Static Variables - 2018-08-12.csv') %>%
          rename(ID_Mun = 1)


# TIDY DATA -------------------------------------------------------------------
# Convert prio to a raster
prio <- rasterFromXYZ(xyz = prio[,c('xcoord','ycoord','forest_gc')],
                      res = c(0.5, 0.5),
                      crs = crs("+proj=longlat +datum=WGS84 +no_defs"))


# Since I am not performing any spatial analysis on these data, they do NOT need
# to be projected, so convert all to longlat here for easier mapping. 
unproj        <- st_crs("+proj=longlat +datum=WGS84 +no_defs")
colombia_lvl0 <- st_transform(colombia_lvl0, unproj)
colombia      <- st_transform(colombia, unproj)
forest        <- projectRaster(from = forest, crs = "+proj=longlat +datum=WGS84 +no_defs")
# night         <- projectRaster(night,  "+proj=longlat +datum=WGS84 +no_defs")
# pop           <- projectRaster(pop,    "+proj=longlat +datum=WGS84 +no_defs")
# tri           <- projectRaster(tri,    "+proj=longlat +datum=WGS84 +no_defs")

# Clip islands out of large map
colombia_lvl0 <- st_crop(colombia_lvl0, extent(-80,-66, -5, 15))


# Create object for zoomed-in extent
e <- extent(-74, -73, 5, 6)

# Create objects for zoomed map:
colombia <- st_crop(colombia, e)
prio     <- crop(prio, e)
forest   <- crop(forest, e)



# Downscale Raster
# downsample <- 100
# df2 <- df %>%
#   group_by(x = downsample * round(x / downsample),
#            y = downsample * round(y / downsample)) %>%
#   summarise(z = mean(z))

# Convert rasters to DFs for plotting with geom_raster()
forest <- as.data.frame(forest, xy = TRUE) %>%
  rename(z = colombia_forestcover_2005)

prio <- as.data.frame(prio, xy = TRUE) %>%
  rename(z = forest_gc) %>%
  mutate(z1 = z,
         z = fct_inorder(as_factor(z)))

# Create a "water" background for any gaps:
water <- as(e, 'SpatialPolygons') %>% st_as_sf()
st_crs(water) <- "+proj=longlat + datum=WGS84"

# Create a boundary object for mini-map to highlight zoomed region
bound <- data.frame(xmin = -74,
                    xmax = -73,
                    ymin = 5,
                    ymax = 6)

# Clean-up
rm(e, unproj)

# CREATE SUB-MAPS -------------------------------------------------------------

# Main map [NASA]:
m1.nasa <- ggplot() +
  geom_sf(data  = water,
          fill  = 'lightblue',
          color = 'transparent') +
  geom_raster(data = forest,
              aes(x    = x,
                  y    = y,
                  fill = z),
              alpha = 0.85) +
  labs(title = 'NASA GLS, ~30m res.') +
  scale_fill_gradientn(name     = '',
                       colours  = rev(terrain.colors(20)),
                       na.value = 'transparent',
                       # guide    = FALSE,
                       limits   = c(0,100)) +
  geom_sf(data = colombia, fill = 'transparent') +
  theme_minimal() +
  theme(axis.text  = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank());m1.nasa

# Main map [PRIO]
m1.prio <- ggplot() +
  geom_raster(data = prio,
              aes(x    = x,
                  y    = y,
                  fill = z1), 
              alpha = 0.85) +
  labs(title = 'PRIO Grid, ~55km res.') +
  scale_fill_gradientn(name     = '',
                       colours  = rev(terrain.colors(20)),
                       na.value = 'transparent',
                       limits   = c(0,100),
                       # guide    = FALSE,
                       # labels   = function(x) round(x*100,0)
                       ) +
  # scale_fill_manual(values = alpha(rev(terrain.colors(4)), 0.2),
  #                   guide  = FALSE) +
  geom_sf(data = colombia, fill = 'transparent') +
  theme_minimal() +
  theme(axis.text  = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank());m1.prio

# Inset map:
m2 <- ggplot() +
  geom_sf(data = colombia_lvl0, fill = '#fff7bc') +
  geom_rect(data = bound, aes(xmin = xmin,
                              xmax = xmax,
                              ymin = ymin,
                              ymax = ymax), 
            alpha = 0, colour = 'red', linetype = 1, size = 1) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text  = element_blank(),
        panel.grid = element_blank());m2


# BUILD MAP -------------------------------------------------------------------
m3 <- ggdraw(xlim = c(0, 70), ylim = c(0, 100)) +
  draw_plot(plot   = m1.nasa,
            x      = 0,
            y      = 10, 
            width  = 50,
            height = 50) + 
  draw_plot(plot   = m1.prio, 
            x      = 0,
            y      = 50,
            width  = 50, 
            height = 50) +
  draw_plot(plot   = m2,
            x      = 40,
            y      = 35,
            width  = 30,
            height = 30) + 
  draw_label(label = 'Covariate data comparison: Forest Cover (%)',
             x = 35, y = 95) + 
  draw_label(label = paste0(sprintf('Plot date: %s.\n',format(Sys.Date(),"%B %d, %Y")),
                            sprintf('Area contains %d municipalities covering approximately %d square km.',
                                    nrow(colombia), 110*110)),
             x = 60, 
             y = 7,
             size = 10,
             hjust = 1)

# SAVE ------------------------------------------------------------------------
ggsave(filename = 'plots/Data_compare.png',
       plot = m3,
       width = 6,
       height = 10,
       units = 'in',
       dpi = 350)





# --------------------------------------------------------------------------- #


