#-----------------------------------------------------------------------------#
#                                                                             
# Author:        Logan Stundal                                                    
# Date:          January 20, 2020                                                 
# Purpose:       Data tidy for Colombia project, version 2.                      
#                                                                             
#
# Copyright (c): Logan Stundal, 2020                      
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------# 
#
# Notes:                                                                    
#         -- This version employs much higher resolution covariate information 
#            rather than prio grid data which does not apply well for small 
#            municipalities in Colombia. 
#
#         -- To simplify shapefile object, refer to:
#            https://cran.r-project.org/web/packages/rmapshaper/vignettes/rmapshaper.html
#
#-----------------------------------------------------------------------------#



# ADMINISTRATIVE --------------------------------------------------------------

#---------------------------#
# Clear working directory   
#---------------------------#
rm(list = ls(all.names = TRUE))
# .rs.restartR()

#---------------------------#
# Load required libraries   
#---------------------------#
library(tidyverse)
library(sf)
library(sp)
library(raster)

#---------------------------#
# Set working directory     
#---------------------------#
setwd("C:/Users/logan/GoogleDrive/UMN/RESEARCH/RA_John/Event_Data_Project")

#---------------------------#
# Load data                 
#---------------------------#

# COLOMBIA ADMIN --------------------------------------------------------------
# Colombia Administrative units - gen. in 'Colombia_Admin_Units.R' script 
# Projected to South America Albers Equal Area Conic
load('data/colombia_admin.rdata')

# ICEWS / GED -----------------------------------------------------------------
# event_original <- read_csv('data/event_data/gedicews_20190627.csv',
#                            col_types = cols(.default = "d"))
event_farc     <- read_csv('data/event_data/gedicews_FARC_20190908.csv',
                           col_types = cols(.default = "d"))
# event_noUNK    <- read_csv('data/event_data/gedicews_NoUKN_20190908.csv',
#                            col_types = cols(.default = "d"))


# CINEP -----------------------------------------------------------------------
# Courtsey JEN HOLMES, UT Dallas
cinep1 <- haven::read_dta('data/event_data/CINEP_AB.dta')
cinep2 <- haven::read_dta('data/event_data/CINEP_HRV.dta')

# Municipality covariates -------------
cinep_cov  <- haven::read_dta('data/covariate_data/CINEP_Mun_Covariates.dta')


# PRIO ------------------------------------------------------------------------
prio.yr       <- read_csv(paste0('c:/users/logan/googledrive/umn/RESEARCH/DATA/priogrid/',
                                 'PRIO-GRID Yearly Variables for 1995-2014 - 2018-08-12.csv'))
prio.static   <- read_csv(paste0('c:/users/logan/googledrive/umn/RESEARCH/DATA/priogrid/',
                                 'PRIO-GRID Static Variables - 2018-08-12.csv'))

# Prio shapefile at: 'c:/users/logan/googledrive/umn/RESEARCH/DATA/priogrid/priogrid_cell.shp'
prio.crs      <- st_crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


# GOOGLE EARTH ENGINE EXTRACTS ------------------------------------------------

forest <- read_csv('data/covariate_data/googleearthengine/Colombia_ForestCover_municipality_2005.csv') %>%
  select(2:4) %>% 
  rename(ID_Mun = 1,
         Municiaplity = 2,
         forest = 3) %>%
  mutate(ID_Mun = as.numeric(str_remove_all(ID_Mun, 'CO')))

nightlight <- read_csv('data/covariate_data/googleearthengine/Colombia_NightLights_municipality_2002_2009_mean.csv') %>%
  select(2:4) %>%
  rename(ID_Mun = 1, 
         Municipality = 2,
         nl = 3) %>%
  mutate(ID_Mun = as.numeric(str_remove_all(ID_Mun, 'CO')))

worldpop <- read_csv('data/covariate_data/googleearthengine/Colombia_Population_municipality_2002_2009_mean.csv') %>%
  select(2:4) %>%
  rename(ID_Mun = 1,
         Municipality = 2,
         population = 3) %>%
  mutate(ID_Mun = as.numeric(str_remove_all(ID_Mun, 'CO')))

tri <- read_csv('data/covariate_data/googleearthengine/Colombia_tri_municipality_mean.csv') %>%
  select(2:4) %>%
  rename(ID_Mun = 1,
         Municipality = 2,
         population = 3) %>%
  mutate(ID_Mun = as.numeric(str_remove_all(ID_Mun, 'CO')))


# GeoNames Populated Places ---------------------------------------------------
# gnames <- read_tsv(file      = 'data/covariate_data/geonames_co/co.txt',
#                    col_names = c('geoname_id',
#                                  'name',
#                                  'ascii_name',
#                                  'alternate_name',
#                                  'latitude',
#                                  'longitude',
#                                  'feature_class',
#                                  'feature_code',
#                                  'c_code',
#                                  'cc2',
#                                  'admin1_code',
#                                  'admin2_code',
#                                  'admin3_code',
#                                  'admin4_code',
#                                  'population',
#                                  'elevation',
#                                  'dem',
#                                  'time_zone',
#                                  'mod_date'),
#                    col_types = paste(c('d','c','c','c','d','d','c','c','c','c',
#                                        'c','c','c','c','d','d','d','c','D'), collapse = ''))

# ADDED TO "COLOMBIA_ADMIN_UNITS.R" SCRIPT ON 1/25.

# ELECTIONS: 2002, 2006 -------------------------------------------------------
load('data/covariate_data/elections/clea_lc_20190617.rdata')
elec <- clea_lc_20190617;rm(clea_lc_20190617)

#---------------------------#
# Load functions            
#---------------------------#
{
  .spAg <- function(shp,pts,vr,fn,na.rm=T){
    if(class(shp)[1] == 'sf'){
      shp$ID = rownames(shp) <- 1:nrow(shp)
      shp2   = aggregate(pts[vr], by = shp, FUN = function(x) c(fn(x, na.rm = T)))
      
      shp2[is.na(shp2)] = 0
      shp2$ID = rownames(shp2) = 1:nrow(shp2)
      
      shp2$geometry = NULL
      
      return(shp = left_join(shp,shp2,by='ID'))
      
    } else {
      shp$ID = rownames(shp@data) <- 1:nrow(shp@data)
      shp2   = aggregate(pts[vr], by = shp, FUN = function(x) c(fn(x,na.rm=T)))
      
      # Replace NAs with 0 for spatial units with no events.
      shp2@data[is.na(shp2@data)] <- 0
      
      shp2$ID    <- rownames(shp2@data) <- 1:nrow(shp2@data)
      return(shp <- merge(shp,shp2,by='ID'))
    }
  }
  
  .scaler <- function(x) log(x+1) / max(log(x+1))
}

# TIDY DATA -------------------------------------------------------------------

years <- seq(2002,2009) 
# For John 6/26 meeting, was only 2005 data
# Re: Ben email 6/27 -- switched from test of 00-06 to 02-09

#---------------------------------#
# Municipality geography
#---------------------------------#
# colombia <- colombia %>%
#   mutate(km2    = as.numeric(st_area(.) / 1e6)) %>%
#   mutate(km2_ln = log(km2))
  # JAN 25 - DUMPED THIS INTO COLOMBIA_ADMIN_UNITS.R PREP SCRIPT -- CAN DELETE

# ICEWS / GED -----------------------------------------------------------------

#---------------------------------#
# ORIGINAL - POLMETH 2019 version #
#---------------------------------#
# event_original <- event_original %>%
#   dplyr::select(year,latitude,longitude, sort(names(.))) %>%
#   filter(year %in% years) %>%
#   rowwise() %>%
#   mutate(icews_original       = rowSums(cbind(rebcivicews, rebgovicews),na.rm=T),
#          icews_stand_original = rowSums(cbind(rebcivicewsstand, rebgovicewsstand),na.rm=T),
#          ged_original         = rowSums(cbind(rebcivged, rebgovged),na.rm=T),
#          ged_stand_original   = rowSums(cbind(rebcivgedstand, rebgovgedstand),na.rm=T)) %>%
#   dplyr::select(latitude, longitude, icews_original, icews_stand_original, 
#                 ged_original, ged_stand_original)
# # Note -- dropping year because we are dropping the temporal dimension.  

# coordinates(event_original) <- c("longitude","latitude")
# proj4string(event_original) <- CRS("+proj=longlat +datum=WGS84")
# event_original  <- event_original %>%
#   st_as_sf() %>%
#   st_transform(., st_crs(colombia))
  

#---------------------------------#
# ICEWS - No unidentified actors  #
#---------------------------------#
# event_noUNK <- event_noUNK %>%
#   dplyr::select(year,latitude,longitude, sort(names(.))) %>%
#   filter(year %in% years) %>%
#   rowwise() %>%
#   mutate(icews_noUNK       = rowSums(cbind(rebcivicews, rebgovicews),na.rm=T),
#          icews_stand_noUNK = rowSums(cbind(rebcivicewsstand, rebgovicewsstand),na.rm=T),
#          ged_noUNK         = rowSums(cbind(rebcivged, rebgovged),na.rm=T),
#          ged_stand_noUNK   = rowSums(cbind(rebcivgedstand, rebgovgedstand),na.rm=T)) %>%
#   dplyr::select(latitude, longitude, icews_noUNK, icews_stand_noUNK, 
#                 ged_noUNK, ged_stand_noUNK)
# # Note -- dropping year because we are dropping the temporal dimension.  
# 
# coordinates(event_noUNK) <- c("longitude","latitude")
# proj4string(event_noUNK) <- CRS("+proj=longlat +datum=WGS84")
# event_noUNK  <- event_noUNK %>%
#   st_as_sf() %>%
#   st_transform(., st_crs(colombia))


#---------------------------------#
# ICEWS AND GED - ONLY FARC       #
#---------------------------------#
event_farc <- event_farc %>%
  dplyr::select(year,latitude,longitude, sort(names(.))) %>%
  filter(year %in% years) %>%
  rowwise() %>%
  mutate(icews_farc       = rowSums(cbind(rebcivicews, rebgovicews),na.rm=T),
         icews_stand_farc = rowSums(cbind(rebcivicewsstand, rebgovicewsstand),na.rm=T),
         ged_farc         = rowSums(cbind(rebcivged, rebgovged),na.rm=T),
         ged_stand_farc   = rowSums(cbind(rebcivgedstand, rebgovgedstand),na.rm=T)) %>%
  dplyr::select(latitude, longitude, icews_farc, icews_stand_farc, 
                ged_farc, ged_stand_farc)
# Note -- dropping year because we are dropping the temporal dimension.  

coordinates(event_farc) <- c("longitude","latitude")
proj4string(event_farc) <- CRS("+proj=longlat +datum=WGS84")
event_farc  <- event_farc %>%
  st_as_sf() %>%
  st_transform(., st_crs(colombia))


# PRIO ------------------------------------------------------------------------
prio <- prio.yr %>%
  left_join(.,prio.static,by='gid') %>%
  filter(gwno %in% c(100,101,130,135,95,140)) %>%
  # mutate(petroleum_s = ifelse(is.na(petroleum_s)==T,0,petroleum_s),
  #        petroleum_y = ifelse(is.na(petroleum_y)==T,0,petroleum_y),
  #        drug_y      = ifelse(is.na(drug_y)==T,0,drug_y))%>%
  dplyr::select(gid,year,gwno,everything(),-c(row,col)) %>%
  group_by(gid) %>%
  summarise_each( ~ mean(., na.action = NULL, na.rm = TRUE))

# PRIO - Spatial extract
prio_vars <- c('mountains_mean','capdist','pop_gpw_sum','ttime_mean','forest_gc')

prio <- rasterFromXYZ(prio[,c('xcoord','ycoord',prio_vars)],
                      res = c(0.5, 0.5),
                      crs = prio.crs$proj4string)

prio <- projectRaster(prio, crs = st_crs(colombia)$proj4string) 

rm(prio.yr, prio.static, prio.crs)


# CINEP -----------------------------------------------------------------------
cinep1_grid <- cinep1 %>%
  dplyr::select(Department, ID_Dept, Municipality, ID_Mun) %>%
  distinct()

cinep2_grid <- cinep2 %>%
  dplyr::select(Department, ID_Dept, Municipality, ID_Mun) %>%
  distinct()

cinep1 <- cinep1 %>%
  filter(Year %in% years & 
           (Dead        != 0 |
              Hurt        != 0 |
              Kidnapped   != 0 |
              Disappeared != 0 |
              Tortures    != 0 |
              Attempts    != 0 |
              Arbitrary_detention != 0)) %>%
  group_by(ID_Mun) %>%
  count() %>%
  left_join(x  = ., 
            y  = cinep1_grid, 
            by = 'ID_Mun') %>%
  mutate(Department   = as.character(Department),
         Municipality = as.character(Municipality))

cinep2 <- cinep2 %>%
  filter(Year %in% years & 
           (Dead        != 0 |
              Hurt        != 0 |
              Kidnapped   != 0 |
              Disappeared != 0 |
              Tortures    != 0 |
              Attempts    != 0 |
              Arbitrary_detention != 0)) %>%
  group_by(ID_Mun) %>%
  count() %>%
  left_join(x  = .,
            y  = cinep2_grid,
            by = 'ID_Mun') %>%
  mutate(Department   = as.character(Department),
         Municipality = as.character(Municipality))

CINEP <- cinep2 %>%
  full_join(.,cinep1,by = 'ID_Mun') %>%
  mutate(CINEP = sum(n.x,n.y, na.rm=T),
         Deptartment.x  = ifelse(is.na(Department.x),Department.y,Department.x),
         Municipality.x = ifelse(is.na(Municipality.x),Municipality.y,Municipality.x)) %>%
  dplyr::select(ID_Mun, Department.x, Municipality.x, CINEP) %>%
  rename(Department   = Department.x,
         Municipality = Municipality.x)

rm(cinep1,cinep2,cinep1_grid,cinep2_grid)

# Municipality covariates -------------
cinep_cov <- cinep_cov %>%
  filter(year %in% years) %>%
  group_by(ID_MUN) %>%
  summarise(Forest_pctg = mean(Forest_pctg, na.rm = TRUE),
            slope       = mean(slope, na.rm = TRUE)) %>%
  rename(ID_Mun = ID_MUN) %>%
  mutate(ID_Mun = as.numeric(ID_Mun))

rm(years)


# GOOGLE EARTH RASTERS --------------------------------------------------------



# GEONAMES --------------------------------------------------------------------
# Extract the location and populations of the Department capitals:

# table(gnames$feature_code =='PPLA') # Great! There are 31 departments in the data since the islands are dropped.
# 
# gnames <- gnames %>%
#   filter(feature_class == 'P',                      # city or village
#          feature_code  == 'PPLA') %>%               # seat of a first-order administrative division
#   dplyr::select(name, alternate_name, latitude, longitude, population)
# 
# coordinates(gnames) <- c('longitude','latitude')
# 
# gnames <- gnames %>% st_as_sf()
# st_crs(gnames) <- "+proj=longlat +datum=WGS84"

# ###

# ^ ADDED TO COLOMBIA_ADMIN_UNITS.r SCRIPT ON 1/25
# Temp - temp temp

col <- colombia
col <- st_transform(col, crs = "+proj=longlat +datum=WGS84")

ggplot() +
  geom_sf(data = col, fill = 'transparent') +
  geom_sf(data = gnames, aes(size = population),
          color = 'red', size = 1.25)

###


# ELECTION RESULTS 2002, 2006 -------------------------------------------------

elec <- elec %>%
  filter(ctr_n == 'Colombia',
         yr %in% c(2002,2006)) %>%
  select(ctr_n, yr, cst_n, cst, pty_n, pty, vv1, pv1, pvs1, seat)


# _____________________ ----
# MERGE DATA-------------------------------------------------------------------

# ICEWS / GED -------------------------
# Col.DF <-> EVENT DATA               #
#-------------------------------------#
event_original <- .spAg(shp = colombia,
                        pts = event_original,
                        fn  = sum,
                        vr  = names(event_original)) %>% 
  as.data.frame() %>%
  dplyr::select(-Department, -Municipality, - km2, -km2_ln, -ID) %>%
  mutate(geometry = NULL)

event_noUNK <- .spAg(shp = colombia,
                      pts = event_noUNK,
                      fn  = sum,
                      vr  = names(event_noUNK)) %>%
  as.data.frame() %>%
  dplyr::select(-Department, -Municipality, - km2, -km2_ln, -ID) %>%
  mutate(geometry = NULL)

event_farc <- .spAg(shp = colombia,
                    pts = event_farc,
                    fn  = sum,
                    vr  = names(event_farc)) %>%
  as.data.frame() %>%
  dplyr::select(-Department, -Municipality, - km2, -km2_ln, -ID) %>%
  mutate(geometry = NULL)

colombia <- colombia %>%
  left_join(., event_original, by = 'ID_Mun') %>%
  left_join(., event_noUNK,    by = 'ID_Mun') %>%
  left_join(., event_farc,     by = 'ID_Mun')

rm(event_original, event_noUNK, event_farc)

# PRIO --------------------------------
# Col.DF <-> PRIO                     #
#-------------------------------------#
for(x in 1:length(prio_vars)) {
  var = prio_vars[x]
  
  if(var == 'pop_gpw_sum'){
    tmp = raster::extract(prio[[var]],colombia.shp,fun=sum,na.rm=T)
  } else{
    tmp = raster::extract(prio[[var]],colombia.shp,fun=mean,na.rm=T)
  }
  
  colombia[paste('PRIO', var, sep = '_')] = as.vector(tmp)
  
  if(var %in% c('capdist','pop_gpw_sum','ttime_mean')){
    colombia[paste('PRIO',var,'ln', sep = '_')] = log(colombia[paste('PRIO', var, sep = '_')])
  }
}
rm(prio_vars, prio, var, x, tmp)


# CINEP -------------------------------
# Col.DF <-> CINEP                    #
#-------------------------------------#
colombia <- colombia %>%
  left_join(.,CINEP, by = 'ID_Mun')
rm(CINEP)


# Municipality covariates -------------
# Col.DF <-> Municipality covariates  #
#-------------------------------------#
colombia <- colombia %>%
  left_join(., m_cov, by = 'ID_Mun')
rm(m_cov)


#-------------------------------------#
# SF <-> Final data tidy              #
#-------------------------------------#
colombia <- colombia.shp %>%
  st_as_sf() %>%
  left_join(., colombia, by = 'ID_Mun') %>%
  
  mutate(icews_original_scale       = .scaler(icews_original),
         icews_stand_original_scale = .scaler(icews_stand_original),
         ged_original_scale         = .scaler(ged_original),
         ged_stand_original_scale   = .scaler(ged_stand_original),
         CINEP                      = ifelse(is.na(CINEP), 0, CINEP)) %>%
  
  mutate(icews_noUNK_scale       = .scaler(icews_noUNK),
         icews_stand_noUNK_scale = .scaler(icews_stand_noUNK),
         ged_noUNK_scale         = .scaler(ged_noUNK),
         ged_stand_noUNK_scale   = .scaler(ged_stand_noUNK)) %>%
  
  mutate(icews_farc_scale       = .scaler(icews_farc),
         icews_stand_farc_scale = .scaler(icews_stand_farc),
         ged_farc_scale         = .scaler(ged_farc),
         ged_stand_farc_scale   = .scaler(ged_stand_farc),
         CINEP_scale            = .scaler(CINEP)) %>%
  
  
  mutate(Department.y   = str_to_title(ifelse(is.na(Department.y), Department.x, Department.y)),
         Municipality.y = str_to_title(ifelse(is.na(Municipality.y), Municipality.x, Municipality.y))) %>%
  rename(Department   = Department.y,
         Municipality = Municipality.y) %>%
  mutate(bogata = ifelse(Department == 'BOGOTA, D.C.', 1, 0)) %>%
  dplyr::select(ID_Mun, Department, Municipality,
                
                icews_original, icews_original_scale,
                icews_stand_original, icews_stand_original_scale,
                ged_original, ged_original_scale,
                ged_stand_original, ged_stand_original_scale, 
                
                icews_noUNK, icews_noUNK_scale,
                icews_stand_noUNK, icews_stand_noUNK_scale,
                ged_noUNK, ged_noUNK_scale,
                ged_stand_noUNK, ged_stand_noUNK_scale,
                
                icews_farc, icews_farc_scale,
                icews_stand_farc, icews_stand_farc_scale,
                ged_farc, ged_farc_scale,
                ged_stand_farc, ged_stand_farc_scale,
                
                CINEP, CINEP_scale,
                
                PRIO_mountains_mean, 
                PRIO_capdist, PRIO_capdist_ln,
                PRIO_pop_gpw_sum, PRIO_pop_gpw_sum_ln,
                PRIO_ttime_mean, PRIO_ttime_mean_ln,
                PRIO_forest_gc, 
                
                Forest_pctg, slope,
                
                km2, km2_ln, Shape_Area)

























