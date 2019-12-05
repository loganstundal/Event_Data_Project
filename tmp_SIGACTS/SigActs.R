
#-----------------------------------------------------------------------------#
#                                                                             
# Author:        Logan Stundal                                                    
# Date:          January 02, 2020                                                 
# Purpose:       SigActs - data tidying and plotting                      
#                                                                             
#
# Copyright (c): Logan Stundal, 2020                      
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------# 
#
# Notes:                                                                    
# Good tutorial on heatmaps with sf - https://www.robert-hickman.eu/post/getis-ord-heatmaps-tutorial/                                 
#                      
# 
# To convert cardinal lat/lon to decimal lat/long, see:
# https://stackoverflow.com/questions/4536996/positive-negative-latitude-and-longitude-values-vs-cardinal-directions
#   North is positive, south is negative.
#   East is positive, West is negative.
# 
# To align maps using cowplot, see:
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-3.html
# 
#-----------------------------------------------------------------------------#



# ADMINISTRATIVE --------------------------------------------------------------

#---------------------------#
# Clear working directory   #
#---------------------------#
rm(list = ls())


#---------------------------#
# Load required libraries   #
#---------------------------#
library(tidyverse)
library(lubridate)
library(readxl)
library(sp)
library(sf)
library(cowplot)


#---------------------------#
# Set working directory     #
#---------------------------#
setwd('C:/Users/logan/GoogleDrive/UMN/research/ra_john/event_data_project/')


#---------------------------#
# Load data                 #
#---------------------------#
sig <- read_xlsx('c:/users/logan/googledrive/umn/research/data/AfgSigacts.xlsx')
# print(object.size(sigact), unit = 'Mb')


#---------------------------#
# Download map data         #
#---------------------------#
afgh.nat <- read_rds(url('https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_AFG_0_sf.rds')) %>%
  st_transform(CRS(st_crs("+proj=aeqd +lat_0=34.560413 +lon_0=69.206650 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ")$proj4string))

afgh.sub <- read_rds(url('https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_AFG_2_sf.rds')) %>%
  st_transform(CRS(st_crs("+proj=aeqd +lat_0=34.560413 +lon_0=69.206650 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ")$proj4string))

# iraq <- read_rds(url('https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_IRQ_2_sf.rds'))
# iran <- read_rds(url('https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_IRN_2_sf.rds'))


# TIDY DATA -------------------------------------------------------------------

# OlsonNames() - Base function on built-in timezone names
# grep("Asia/",OlsonNames(),value=TRUE)

#--------------------------------------------#
# FULL DATA - Direct fire category data tidy #
#--------------------------------------------#
# Creat a tidy dataframe of SigActs data
sig <- sig %>%
  filter(PrimaryEventCategroy == 'Direct Fire' &
           PrimaryEventType   == 'Enemy Action') %>%
  mutate(Date  = ymd_hms(DateOccurred, tz = 'Asia/Baghdad')) %>%
  mutate(Year  = year(Date),
         Month = month(Date),
         Latitude = ifelse(str_detect(DDLat, "N"), 
                           as.numeric(str_extract(DDLat, "\\d+\\.*\\d*")),
                           -1 * as.numeric(str_extract(DDLat, "\\d+\\.*\\d*"))),
         Longitude = ifelse(str_detect(DDLon, "E"), 
                            as.numeric(str_extract(DDLon, "\\d+\\.*\\d*")),
                            -1 * as.numeric(str_extract(DDLon, "\\d+\\.*\\d*"))),
         ID = row_number()) %>%
  select(ID, Title, Date, Year, Month, Latitude, Longitude)
  # select(-DDLat, -DDLon, -Field1, -MGRS, -PrimaryEventType, -PrimaryEventCategroy, -DateOccurred, -export)


# Create a subset for January 2008 - as SF points object
sig.2008.01.pts <- sig %>%
  filter(Month == 1, Year == 2008) %>%
  mutate(Event = 1) %>%
  st_as_sf(x = ., coords = c("Longitude", "Latitude")) %>%
  st_set_crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
  st_transform(CRS(st_crs("+proj=aeqd +lat_0=34.560413 +lon_0=69.206650 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ")$proj4string)) %>% 
  .[unlist(st_intersects(afgh.nat, .)),]  # Exclude points outside of Afghanistan


# Create an aggregated version of these points to smallest administrative level
sig.2008.01.shp <- sig.2008.01.pts %>%
  .spAg(shp = afgh.sub, pts = ., vr = 'Event', fn = sum, na.rm = TRUE)


# Time series data frame of sigacts events
sig.ts <- table(sig$Month, sig$Year) %>%
  as.data.frame() %>%
  rename(Year   = Var2, 
         Events = Freq) %>%
  mutate(Month  = month.name[Var1]) %>%
  mutate(Date = ymd(paste0(Year,'-',Month,'-','01')),
         ID   = row_number()) %>%
  select(ID,Date, Month, Year, Events)



# PLOT DATA -------------------------------------------------------------------


#---------------------------#
# MAPS                      
#---------------------------#

plot.sp <- ggplot() + 
  geom_sf(data = sig.2008.01.shp, aes(fill = Event)) +
  theme_minimal() + 
  # labs(title    = 'Afghanistan - SigActs, January 2008',
  #      subtitle = 'Subnational Aggregation - District Level',
  #      caption  = paste('Plot date:', format(lubridate::today(), "%a, %m %Y"),
  #                       '\nEvents - Direct fire events initiated by enemy forces recorded by US military personnel.\n', 
  #                       sprintf('N = %d events in January 2008', 
  #                               sig.ts %>% filter(Month == 'January' & Year == 2008) %>% .$Events))) +
  scale_fill_gradient(name = 'Events') + 
  theme(legend.position = 'bottom') + 
  theme(plot.margin = unit(c(0,0.5,0,0.5), "cm")); plot.sp

# Extract colors from polygon map to use in points map:
g <- ggplot_build(plot.sp)
sp.colors <- unique(g$data[[1]]["fill"])

plot.pt <- ggplot() + 
  geom_sf(data = afgh.nat, fill = sp.colors$fill[1]) +
  geom_sf(data = sig.2008.01.pts, color = sp.colors$fill[7], size = 1) + 
  theme_minimal() + 
  theme(plot.margin = unit(c(0,0.5,0,0.5), "cm"));plot.pt




# grp <- grid.arrange(plot.pt, plot.sp, nrow = 1, ncol = 2)

# Extract legend from polygon map to plot with alignment respected.
legend  <- get_legend(plot.sp) 
plot.sp <- plot.sp + theme(legend.position = 'none',
                           plot.margin = unit(c(0,0.5,0,0.5), "cm")) # Remove original legend now

plot.sf.sp <- plot_grid(plot_grid(plot.sp, plot.pt, 
                                  nrow = 1, ncol = 2, align = "hv"),
                        plot_grid(legend, NULL, 
                                  nrow = 1, ncol = 2),
                        # rel_widths = c(1, 0.2),
                        rel_heights = c(-1, -.12),
                        nrow = 2, ncol = 1,
                        labels = list('Afghanistan, SigActs Events',''),
                        hjust = -0.1)
ggdraw(plot.sf.sp)

caption = paste('\nEvents - Direct fire events initiated by enemy forces recorded by US military personnel.\n',
                sprintf('N = %d events in January 2008\n',
                        sig.ts %>% filter(Month == 'January' & Year == 2008) %>% .$Events),
                'Plot date:', format(lubridate::today(), "%a, %B %d, %Y"))

plot.sf.sp <- ggdraw(plot.sf.sp) + 
  draw_label(caption, 
             x = 0.98, y = -0.05, hjust = 1, vjust = -1,
             size = 10,
             fontface = 'italic') + 
  draw_label('January 2008', 
             x = 0.04, y = 0.95, size = 12, fontface = 'italic')


save_plot(filename = 'tmp_SIGACTS/SigActs_Jan08.png',
          plot     = plot.sf.sp,
          base_height = 10.0,
          base_width  = 20.0)









#---------------------------#
# Time series of all events #
#---------------------------#
plot.ts <- ggplot() + 
  # geom_rect(data = data.frame(xmin = ymd(paste0(2008:2014,"-03-20")),
  #                             xmax = ymd(paste0(2008:2014,"-10-22")),
  #                             ymin = -Inf,
  #                             ymax = Inf),
  #           aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
  #           fill = "grey", alpha = 0.5) + 
  geom_line(data = sig.ts, aes(Date, Events), colour = "#00AFBB", size = 1) + 
  # geom_line(data = tmp, aes(x = Date, y = Events),color = "#00AFBB", size = 1) + 
  scale_x_date(date_breaks = '1 year',
               date_labels = "%Y",
               limits = c(ymd("2008-01-01"), ymd("2014-06-01"))) + 
  theme_minimal() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.caption       = element_text(face = 'bold.italic')) + 
  labs(title    = 'Afghanistan SigAct Events',
       subtitle = 'January 2008 - June 2014',
       caption  = paste('Events are significant activity reports identifying \'direct fire\'\n from enemy forces reported by US military personnel.\nPlot date: ', format(lubridate::today(), "%a %b %d, %Y"))) 

ggsave(filename = 'tmp_SIGACTS/Sigacts_TS.png',
       plot = plot.ts,
       width = 8,
       height = 6,
       dpi = 320)







# PDF - TIME SERIES PLOTS -----------------------------------------------------

# Full Month, Year loop
plot_list = list(); i =0 
for(year in min(sig$Year):max(sig$Year)){
  for (month in 1:12) {
    print(sprintf('Working on %s %d',month.name[month], year))
    i = i + 1
    
    if(!paste(year,month,sep = '.') %in% c(paste('2014', 7:12, sep = '.'))){
      tmp = sig %>%
        filter(Month == month & Year == year)
      
      coordinates(tmp) <- c("Longitude","Latitude")
      proj4string(tmp) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") 
      
      tmp = tmp %>%
        spTransform(., CRS(st_crs(afgh)$proj4string)) %>% 
        st_as_sf()
      
      plt = ggplot() + 
        labs(title    = 'SigActs',
             subtitle = paste0(month.name[month], ', ', year)) +
        # geom_sf(data = iraq, fill = alpha("red",0.1)) +
        geom_sf(data = afgh, fill = alpha("green",0.1)) +
        # geom_sf(data = iran, fill = alpha("blue",0.1)) +
        geom_sf(data = tmp, color = 'black', size = 1) + 
        theme_minimal() + 
        theme(plot.subtitle = element_text(face = 'italic')) +
        xlim(60,75) + ylim(29, 39)
      
      plot_list[[i]] = plt
    }
  }
}


pdf(file = paste0('tmp_SIGACTS/',format(today(),'%Y%m%d'),'_SIGACTS.pdf'),
    paper = 'a4r', onefile = T, compress = T, width = 11, height = 8.5)
for(i in 1:length(plot_list)){
  print(plot_list[[i]])
}
dev.off()










#______________________________________---- 
# OTHER CODE - OLD ------------------------------------------------------------

sig2008.01 <- sig %>%
  filter(Month == 1, Year == 2008) %>% 
  mutate(ID = row_number(),
         Event = 1) %>%
  select(ID, Longitude, Latitude, Event)


coordinates(sig2008.01) <- c("Longitude", "Latitude")
proj4string(sig2008.01) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


# Reproject - both afgh and sigacts, https://epsg.io/4255

# sig2008.01 <- sig2008.01 %>% 
#   st_transform(CRS(st_crs("+init=epsg:4255")$proj4string))

# afgh <- afgh %>%
#   st_transform(CRS(st_crs("+init=epsg:4255")$proj4string))
  

# +proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs 
# Kabul: 34.560413, 69.206650

# REPROJECT World Azimuthal Equidistant, centered on Kabul, https://epsg.io/54032

sig2008.01 <- sig2008.01 %>%
  st_as_sf %>%
  st_transform(CRS(st_crs("+proj=aeqd +lat_0=34.560413 +lon_0=69.206650 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ")$proj4string))

# afgh <- afgh %>%
#   st_as_sf %>%
#   st_transform(CRS(st_crs("+proj=aeqd +lat_0=34.560413 +lon_0=69.206650 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ")$proj4string))


ggplot() + geom_sf(data = afgh) + 
  geom_point(aes(y = 34.560413, x = 69.206650), color = 'red', size = 2)


x <- .spAg(shp = afgh, 
           pts = sig2008.01, 
           vr  = 'Event',
           fn  = sum)

ggplot(data = x) + 
  geom_sf(aes(fill = Event))

ggplot() + geom_sf(data = afgh) + 
  geom_sf(data = sig2008.01, color = 'red', size = 1)


#-----------------------------------------------------------------------------#


coordinates(sig) <- c("Longitude","Latitude")
proj4string(sig) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

sig <- sig %>%
  spTransform(., CRS(st_crs(iraq)$proj4string)) %>%
  st_as_sf()


ggplot() + 
  geom_sf(data = iraq, fill = alpha("red",0.1)) +
  geom_sf(data = afgh, fill = alpha("green",0.1)) +
  geom_sf(data = iran, fill = alpha("blue",0.1)) +
  geom_sf(data = sig2008.01, color = 'black', size = 1) + 
  theme_minimal()













