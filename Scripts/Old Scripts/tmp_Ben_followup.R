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
# Notes: To do for Ben email follow-up                                                                    
#        - missing data
#        - individual plots
#        - aggreement / disagreement between municipalities with events in icews/ged    
#                                                                             
#-----------------------------------------------------------------------------#



# ADMINISTRATIVE --------------------------------------------------------------

#---------------------------#
# Clear working directory   
#---------------------------#
rm(list = ls())


#---------------------------#
# Load required libraries   
#---------------------------#
library(tidyverse)
library(sf)

#---------------------------#
# Set working directory     
#---------------------------#
setwd('c:/users/logan/googledrive/umn/research/ra_john/event_data_project')

#---------------------------#
# Load data                 
#---------------------------#
load('data/colombia.rdata')

#---------------------------#
# Load functions            
#---------------------------#
# Plotting theme 

.my_theme <-  theme_void() +
              theme(plot.title           = element_text(size = 14, face = 'bold'),
                    plot.caption         = element_text(size = 10),
                    legend.position      = c(0.95, 1.00),
                    legend.justification = c( "right", "top"),
                    legend.box.just      = "right",
                    legend.margin        = margin(6, 6, 6, 6),
                    legend.key.width     = unit(0.55, "cm"),
                    legend.key.height    = unit(0.65, 'cm'))

# Individual plot saver
.ind_saver <- function(plt, name){
  ggsave(plt, 
         width  = .ind.width,
         height = .ind.height, 
         units  = 'in',
         dpi    = 300,
         filename = paste0('Plots/', name, '.png'))
}

# PLOTTING PARAMETERS - INCHES
# Individual (temporary) plots)
.ind.width  = 6.0
.ind.height = 6.0


# DATA TIDY -------------------------------------------------------------------

# Create a variable indicating agreement between ICEWS and GED for FARC

colombia <- colombia %>%
  mutate(icews_ged_farc  = case_when(icews_farc == ged_farc ~ 1,
                                     TRUE ~ 0),
         icews_ged_farc2 = case_when((icews_farc > 0) == (ged_farc > 0) ~ 1,
                                     TRUE ~ 0))


# # Simpled DF with no geometry
# colombia2 <- colombia
# colombia2$geometry <- NULL


# PLOTS -----------------------------------------------------------------------

# DATA MISSINGNESS --------------------
sapply(names(colombia2), function(x){
  table(is.na(colombia2[x]))
})

# SLOPE AND FOREST PERCENTAGE ARE MISSING FOR 142 MUNICIPALITIES

colombia <- colombia %>%
  mutate(missing_forest = case_when(is.na(Forest_pctg) == T ~ 1,
                                    TRUE ~ 0),
         missing_slope  = case_when(is.na(slope) == T ~ 1, 
                                    TRUE ~ 0))


missing <- ggplot(data = colombia) + 
  geom_sf(aes(fill = as_factor(missing_slope)), color = 'transparent') +
  geom_sf(data = col0, color = 'black', fill = NA, size = .5) +
  scale_fill_manual(name   = '',
                    values = c(NA, 'gray60'),
                    breaks = c(NA, 1),
                    labels = c(NA, 'Missing')) +
  labs(title = 'Missing Values (slope, forest cover)',
       caption = paste0(sprintf('Municipalities with missing values, n = %d',
                                as.numeric(table(is.na(colombia$slope))[2])),
                        '\nPlot date: ', format(lubridate::today(), "%B %d, %Y"))) + 
  .my_theme
.ind_saver(missing, 'COLOMBIA_MISSING_COVARIATES')

# ICEWS / GED AGREEMENT ---------------

# RAW COUNTS
agree1 <- ggplot(data = colombia) + 
  geom_sf(aes(fill = as_factor(icews_ged_farc)), color = 'transparent') +
  geom_sf(data = col0, color = 'black', fill = NA) +
  scale_fill_manual(name   = '',
                    values = c(NA, 'gray60'),
                    breaks = c(NA, 1),
                    labels = c(NA, 'Agree')) +
  labs(title = 'Coding Agreement: ICEWS and GED',
       subtitle = 'FARC only events, raw counts',
       caption = paste0(sprintf('Municipalities with identical event counts, n = %d',
                                as.numeric(table(colombia$icews_ged_farc)[2])),
                        '\nPlot date: ', format(lubridate::today(), "%B %d, %Y"))) + 
  .my_theme
.ind_saver(agree1, 'ICEWS_GED_Agreement_Counts')

# BINARY - BOTH REPORT EVENT
agree2 <- ggplot(data = colombia) + 
  geom_sf(aes(fill = as_factor(icews_ged_farc2)), color = 'transparent') +
  geom_sf(data = col0, color = 'black', fill = NA) +
  scale_fill_manual(name   = '',
                    values = c(NA, 'gray60'),
                    breaks = c(NA, 1),
                    labels = c(NA, 'Agree')) +
  labs(title = 'Coding Agreement: ICEWS and GED',
       subtitle = 'FARC only events, municipalities where both \nICEWS and GED report at least 1 event',
       caption = paste0(sprintf('Municipalities with ICEWS/GED agreement that a FARC event occurred, n = %d',
                                as.numeric(table(colombia$icews_ged_farc2)[2])),
                        '\nPlot date: ', format(lubridate::today(), "%B %d, %Y"))) + 
  .my_theme
.ind_saver(agree2, 'ICEWS_GED_Agreement_Binary')




# SAVE ------------------------------------------------------------------------
# save()
rm(list = ls())





