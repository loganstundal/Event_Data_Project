#-----------------------------------------------------------------------------#
#                                                                             
# Author:        Logan Stundal                                                    
# Date:          December 31, 2019                                                 
# Purpose:       Merge Event, PRIO, CINEP data                      
#                                                                             
#
# Copyright (c): Logan Stundal, 2020                      
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------# 
#
# Notes:                                                                    
#       January 16, 2020 -- updated header information, addressed missing CINEP
#                           scale variable omission, added outer administrative 
#                           Colombia boundary to the data file at the end of this
#                           script.
#                                                                             
#-----------------------------------------------------------------------------#



# ADMINSTRATIVE ---------------------------------------------------------------

#---------------------------#
# Clear working directory   
#---------------------------#
rm(list=ls())

#---------------------------#
# Load required libraries   
#---------------------------#
library(foreign)
library(tidyverse)
library(sp)
library(sf)
library(raster)
library(spdep)

#---------------------------#
# Set working directory     
#---------------------------#
setwd(paste('C:/Users/logan/GoogleDrive/UMN/RESEARCH/RA_JOHN/Event_Data_Project/Scripts'))

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

# .simpleCap <- function(x) {
#   s <- strsplit(x, " ")[[1]]
#   paste(toupper(substring(s, 1, 1)), substring(s, 2),
#         sep = "", collapse = " ")
# }
# 
# .simpleCap2 <- function(x) {
#   s <- strsplit(x, ' ')[[1]]
#   
#   firsts <- substring(s,1,1)
#   
#   firsts[1] <- toupper(firsts[1])
#   firsts[length(firsts)] <- toupper(firsts[length(firsts)])
#   
#   paste(firsts, substring(s, 2),
#         sep = "", collapse = " ")  
# }
}



# _____________________ ----
# LOAD DATA -------------------------------------------------------------------


# Colombia map ----------------------------------------------------------------

# NOTE - GADM is wrong: 
# https://gis.stackexchange.com/questions/167247/seeking-colombia-municipality-geometries

# ESOC still use this data source - they direct to it on their colombia page:
# https://esoc.princeton.edu/country/colombia

# INSTEAD - USE THIS BETTER DATA SOURCE FROM OCHA AT HUMDATA:
# https://data.humdata.org/dataset/colombia-administrative-boundaries-levels-0-3


colombia.shp <- raster::shapefile('../data/col_admbnda_adm2_unodc_ocha/col_admbnda_adm2_unodc_ocha.shp')

# Save data frame from shapefile as a separate object for easier merging
colombia <- colombia.shp@data %>%
  mutate(ID = row_number(),
         admin1Name   = iconv(admin1Name, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         admin2RefN   = iconv(admin2RefN, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         ID_Mun       = as.numeric(str_remove_all(admin2Pcod, 'CO'))) %>%
  rename(Department   = admin1Name,
         Municipality = admin2RefN) %>%
  dplyr::select(ID_Mun, Department, Municipality)

# Subset shapefile for eventual merging.
colombia.shp@data <- colombia.shp@data %>%
  mutate(ID_Mun = as.numeric(str_remove_all(admin2Pcod, 'CO'))) %>%
  dplyr::select(ID_Mun, Shape_Area)

# Project shapefile and estimate municipality areas.
colombia.shp <- spTransform(colombia.shp, CRS("+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60
+x_0=0 +y_0=0 +datum=WGS84 +ellps=aust_SA +units=m +no_defs"))

colombia.shp$km2    <- rgeos::gArea(colombia.shp, byid = T)/1e6
colombia.shp$km2_ln <- log(colombia.shp$km2)

# ICEWS / GED -----------------------------------------------------------------
event_original <- read.csv('../data/gedicews_20190627.csv',       stringsAsFactors = FALSE)
event_farc     <- read.csv('../data/gedicews_FARC_20190908.csv',  stringsAsFactors = FALSE)
event_noUNK    <- read.csv('../data/gedicews_NoUKN_20190908.csv', stringsAsFactors = FALSE)
# Ben's new Version here: AS of DD-MM-YYYY
# event2 <- read.csv('X.csv',stringsAsFactors=F)


# PRIO ------------------------------------------------------------------------
prio.yr       <- read.csv('c:/users/logan/googledrive/umn/RESEARCH/DATA/priogrid/PRIO-GRID Yearly Variables for 1995-2014 - 2018-08-12.csv',
                          stringsAsFactors = F);colnames(prio.yr)[1]<-'gid'
prio.static   <- read.csv('c:/users/logan/googledrive/umn/RESEARCH/DATA/priogrid/PRIO-GRID Static Variables - 2018-08-12.csv',
                          stringsAsFactors = F);colnames(prio.static)[1]<-'gid'

# Prio shapefile at: 'c:/users/logan/googledrive/umn/RESEARCH/DATA/priogrid/priogrid_cell.shp'
prio.crs      <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


# CINEP -----------------------------------------------------------------------
cinep1 <- read.dta('../data/CINEP_AB.dta')
cinep2 <- read.dta('../data/CINEP_HRV.dta')
# cinep3 <- read.dta('colombia data/CINEP_paramilitary_HRV.dta')


# Municipality covariates -----------------------------------------------------
# Courtsey JEN HOLMES, UT Dallas
m_cov <- readstata13::read.dta13('../data/Mun_Covariates.dta')



# _____________________ ----
# TIDY DATA -------------------------------------------------------------------
years <- seq(2002,2009) 
# For John 6/26 meeting, was only 2005 data
# Re: Ben email 6/27 -- switched from test of 00-06 to 02-09


# ICEWS / GED -----------------------------------------------------------------

#---------------------------------#
# ORIGINAL - POLMETH 2019 version #
#---------------------------------#
event_original <- event_original %>%
  dplyr::select(year,latitude,longitude, sort(names(.))) %>%
  filter(year %in% years) %>%
  rowwise() %>%
  mutate(icews_original       = rowSums(cbind(rebcivicews, rebgovicews),na.rm=T),
         icews_stand_original = rowSums(cbind(rebcivicewsstand, rebgovicewsstand),na.rm=T),
         ged_original         = rowSums(cbind(rebcivged, rebgovged),na.rm=T),
         ged_stand_original   = rowSums(cbind(rebcivgedstand, rebgovgedstand),na.rm=T)) %>%
  dplyr::select(latitude, longitude, icews_original, icews_stand_original, 
                                     ged_original, ged_stand_original)
# Note -- dropping year because we are dropping the temporal dimension.  

coordinates(event_original) <- c("longitude","latitude")
proj4string(event_original) <- CRS("+proj=longlat +datum=WGS84")
event_original  <- spTransform(event_original, CRS(proj4string(colombia.shp)))


#---------------------------------#
# ICEWS - No unidentified actors  #
#---------------------------------#
event_noUNK <- event_noUNK %>%
  dplyr::select(year,latitude,longitude, sort(names(.))) %>%
  filter(year %in% years) %>%
  rowwise() %>%
  mutate(icews_noUNK       = rowSums(cbind(rebcivicews, rebgovicews),na.rm=T),
         icews_stand_noUNK = rowSums(cbind(rebcivicewsstand, rebgovicewsstand),na.rm=T),
         ged_noUNK         = rowSums(cbind(rebcivged, rebgovged),na.rm=T),
         ged_stand_noUNK   = rowSums(cbind(rebcivgedstand, rebgovgedstand),na.rm=T)) %>%
  dplyr::select(latitude, longitude, icews_noUNK, icews_stand_noUNK, 
                                     ged_noUNK, ged_stand_noUNK)
# Note -- dropping year because we are dropping the temporal dimension.  

coordinates(event_noUNK) <- c("longitude","latitude")
proj4string(event_noUNK) <- CRS("+proj=longlat +datum=WGS84")
event_noUNK  <- spTransform(event_noUNK, CRS(proj4string(colombia.shp)))


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
event_farc  <- spTransform(event_farc, CRS(proj4string(colombia.shp)))


# PRIO ------------------------------------------------------------------------
prio <- prio.yr %>%
  # filter(gwno %in% c(100,101,130,135,95,140)) %>%
  left_join(.,prio.static,by='gid') %>%
  filter(gwno %in% c(100,101,130,135,95,140)) %>%
  mutate(petroleum_s = ifelse(is.na(petroleum_s)==T,0,petroleum_s),
         petroleum_y = ifelse(is.na(petroleum_y)==T,0,petroleum_y),
         drug_y      = ifelse(is.na(drug_y)==T,0,drug_y))%>%
  dplyr::select(gid,year,gwno,everything(),-c(row,col)) %>%
  group_by(gid) %>%
  summarise_each( ~ mean(., na.action = NULL, na.rm = TRUE))

# PRIO - Spatial extract
prio_vars <- c('mountains_mean','capdist','pop_gpw_sum','ttime_mean','forest_gc')

prio <- rasterFromXYZ(prio[,c('xcoord','ycoord',prio_vars)],
                      res = c(0.5, 0.5),
                      crs = prio.crs)

prio <- projectRaster(prio, crs = CRS(proj4string(colombia.shp))) 

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


# Municipality covariates -----------------------------------------------------------------------
m_cov <- m_cov %>%
  filter(year %in% years) %>%
  group_by(ID_MUN) %>%
  summarise(Forest_pctg = mean(Forest_pctg, na.rm = TRUE),
            slope       = mean(slope, na.rm = TRUE)) %>%
  rename(ID_Mun = ID_MUN) %>%
  mutate(ID_Mun = as.numeric(ID_Mun))

rm(years)

# _____________________ ----
# MERGE DATA-------------------------------------------------------------------

# ICEWS / GED -------------------------
# Col.DF <-> EVENT DATA               #
#-------------------------------------#
colombia.shp_event_orig <- .spAg(shp = colombia.shp,
                                 pts = event_original,
                                 fn  = sum,
                                 vr  = names(event_original))

colombia.shp <- merge(x = colombia.shp,
                      y = colombia.shp_event_orig[names(colombia.shp_event_orig)[!names(colombia.shp_event_orig) %in% 
                                                                                   c('ID','Shape_Area', 'km2', 'km2_ln')]],
                      by = 'ID_Mun')

rm(event_original, colombia.shp_event_orig)


colombia.shp_event_noUNK <- .spAg(shp = colombia.shp,
                                  pts = event_noUNK,
                                  fn  = sum,
                                  vr  = names(event_noUNK))

colombia.shp <- merge(x = colombia.shp,
                      y = colombia.shp_event_noUNK[names(colombia.shp_event_noUNK)[!names(colombia.shp_event_noUNK) %in% 
                                                       c('ID','Shape_Area', 'km2', 'km2_ln',
                                                         'icwews_stand_original','icews_original',
                                                         'ged_stand_original','ged_original')]],
                      by = 'ID_Mun')

rm(event_noUNK, colombia.shp_event_noUNK)


colombia.shp_event_farc <- .spAg(shp = colombia.shp,
                                 pts = event_farc,
                                 fn  = sum,
                                 vr  = names(event_farc))

colombia.shp <- merge(x = colombia.shp,
                      y = colombia.shp_event_farc[names(colombia.shp_event_farc)
                                                 [!names(colombia.shp_event_farc) %in% 
                                                      c('ID','Shape_Area', 'km2', 'km2_ln',
                                                        'icwews_stand_original','icews_original',
                                                        'ged_stand_original','ged_original',
                                                        'icews_stand_noUNK','icews_noUNK',
                                                        'ged_stand_noUNK','ged_noUNK')]],
                      by = 'ID_Mun')

rm(event_farc, colombia.shp_event_farc)



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



# _____________________ ----
# COLOMBIA - NATIONAL BOUNDARY ------------------------------------------------

col0 <- rworldmap::getMap(resolution = 'high') %>%
  st_as_sf(col0) %>%
  filter(SOVEREIGNT == 'Colombia') %>%
  rename(Country = SOVEREIGNT) %>%
  dplyr::select(Country)

col0 <- st_crop(col0, c(xmin = -80,
                        ymin = -5,
                        xmax = -66,
                        ymax = 14))
col0 <- st_transform(col0, crs = st_crs(colombia))



# _____________________ ----
# SPATIAL WEIGHTS -------------------------------------------------------------

# Drop islands here: [this code may prove important for sensitivity test later]
# colombia$ID_Mun[which(colombia$Municipality == 'San Andres Y Providencia')]

colombia.shp <- subset(colombia.shp,
                       subset = c(!ID_Mun %in% c(88001)))

colombia <- colombia %>%
  filter(Municipality != 'San Andres Y Providencia')

#-----------------------------------------------------------------------------#
# Queens contiguity --                                              
#-----------------------------------------------------------------------------#
#                                                                   
nb.r        <- poly2nb(pl        = colombia.shp,                    
                       row.names = colombia.shp$ID,                 
                       queen     = TRUE)                            
nb.lst      <- nb2listw(nb.r)                                       
W           <- nb2mat(nb.r, style="B")                            
colnames(W) <- rownames(W)                                        
#                                                                   
#-----------------------------------------------------------------------------#

#-----------------------------------------------------------------------------#
# K nearest neighbors -- (k=1)                                      
#-----------------------------------------------------------------------------#
#                                                                   
# "this adapts across the study area, taking account of differences 
#  in the densities of areal entities."                             
#  (Source: Roger Bivand, nb.pdf in wd, p. 5)                       
#                                                                   
# coords <- coordinates(col_dat)                                    
# colnames(coords) <- c('x','y')                                    
# coords <- as.data.frame(coords)                                   
# coordinates(coords) <- ~x+y                                       
## plot(coords,add=T,col='red')                                     
#                                                                   
# IDs <- row.names(as(col_dat, "data.frame"))                       
# nn_nb <- knn2nb(knearneigh(coords, k = 1), row.names = IDs)       
#                                                                   
# dsts <- unlist(nbdists(nn_nb, coords))                            
# summary(dsts)                                                     
#                                                                   
# summary(dsts) yields: "The greatest value will be the minimum     
# distance needed to make sure that all the areas are linked to at  
# least one neighbour. (Source: Roger Bivand, nb.pdf in wd, p. 5)   
#                                                                   
# nb2 <- dnearneigh(coords, d1 = 0, d2 = 1*max(dsts),               
#                   row.names = IDs)                                
## nb2                                                              
## plot(nb2,coords=coordinates(col_dat))                            
# nb2_list <- nb2listw(nb2)                                         
#                                                                   
#-----------------------------------------------------------------------------#

#-----------------------------------------------------------------------------#
# GeoDa GWT Distance-based weights                                               
#-----------------------------------------------------------------------------#
#                                                                   
# geoda  <- read.gwt2nb('tempdir_shp/col_dat2.gwt')                 
# geoda2 <- nb2mat(geoda,style = 'W')                               
# geoda3 <- nb2listw(geoda)                                         
#                                                                   
#-----------------------------------------------------------------------------#
#                                                                   
# png('neighbors.png',width=10,height=7,units='in',res=300)         
#   par(mfrow=c(1,3))                                               
#                                                                   
#   plot(col_dat)                                                   
#   mtext('Colombia Municipalities',side=3,line=-2.5,font=2)        
#                                                                   
#   plot(col_dat)                                                   
#   mtext('Neighbor contiguity',side=3,line=-2.5,font=2)            
#   plot(nb.r,coords=coordinates(col_dat),add=T)                    
#                                                                   
#   plot(col_dat)                                                   
# mtext('Distance based \n(minimum for all                          
#       to have 1 neighbor at least)',side=3,line=-2.5,font=2)      
#   plot(nb2,coords=coordinates(col_dat),add=T)                     
# dev.off()                                                         
#                                                                   
#------------------------------------------------------------------------------#


rm(colombia.shp)


#-----------------------------------------------------------------------------#
#                                                                             
# SAVE DATA OBJECTS                                                           
#                                                                             
#-----------------------------------------------------------------------------#

# save(col_dat,file='2019 Summer - Data Work/Models and Plots/colombia_unified_2005.RData')
# save(col_dat,file='2019 Summer - Data Work/Models and Plots/colombia_unified_2000_06.RData')
# save(col_dat,file='2019 Summer - Data Work/Models and Plots/colombia_unified_2002_09.RData')

save.image(file='../data/colombia.RData')

# dir.create('tempdir_shp')
# rgdal::writeOGR(obj=col_dat2, dsn="tempdir_shp", layer="col_dat2", driver="ESRI Shapefile") 


rm(list=ls())

#------------------------------------------------------------------------------#


