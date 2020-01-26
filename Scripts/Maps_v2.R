#-----------------------------------------------------------------------------#
#                                                                             
# Author:        Logan Stundal                                                    
# Date:          January 20, 2020                                                 
# Purpose:       Maps for Colombia project, version 2.                      
#                                                                             
#
# Copyright (c): Logan Stundal, 2020                      
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------# 
#
# Notes:                                                                    
#         -- This version employs a more robust, custom choropleth mapping 
#            function that I feel better exposes importatn spatail variation in
#            our outcome variables.
#
#         -- GREAT resource on wonderful R maps here:
#              https://timogrossenbacher.ch/2016/12/beautiful-thematic-
#              maps-with-ggplot2-only/
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
library(viridis)

#---------------------------#
# Set working directory     
#---------------------------#
setwd("C:/Users/logan/GoogleDrive/UMN/RESEARCH/RA_John/Event_Data_Project")

#---------------------------#
# Load data                 
#---------------------------#

# Colombia Administrative units - set in 'Colombia_Admin_Units.R' script 
# Projected to South America Albers Equal Area Conic
load('data/colombia.rdata')

#---------------------------#
# Load functions            
#---------------------------#
source('functions/map_theme.R')
source('functions/map_cat_map.R')

# TIDY DATA -------------------------------------------------------------------


# PLOT PARAMETERS -------------------------------------------------------------
set_brks <- 6

# ICEWS IS TOO FUCKED FOR THIS - DON'T EVEN BOTHER TRYING - USE THIS FOR COVARIATES.
# BUT COULD TRY THE FOLLOWING FOR ICEWS
table(colombia$icews_farc)

colombia <- colombia %>%
  mutate(icews_farc_quant = as_factor(case_when(
  icews_farc == 0 ~ '0 attacks',
  icews_farc == 1 ~ '1 attack',
  icews_farc %in% c(2:5) ~ '2 - 5 attacks',
  icews_farc %in% c(6:10) ~ '6 - 10 attacks',
  icews_farc > 10 ~ 'More than 10 attacks'
))) %>%
  mutate(icews_farc_quant = fct_relevel(icews_farc_quant, sort))

# TRYING DICHOTOMIZED -- LEADS TO QUESTION -- CAN THE COVARIATES WE HAVE EXTRACT THE SAME PREDICTIVE PATTERN
# AS CINEP FOR PREDICTING *WHERE* FARC IS ACTIVE?
colombia <- colombia %>%
  mutate(icews_farc_quant = as_factor(case_when(
    icews_farc == 0 ~ 'No FARC presence',
    icews_farc >  0 ~ 'FARC presence'
  ))) %>%
  mutate(icews_farc_quant = fct_relevel(icews_farc_quant, sort))

table(colombia$icews_farc_quant)




# table(cat_helper(no_classes = set_brks,
#                  my_data    = colombia,
#                  my_var     = 'km2',
#                  prec       = 0))

# Trying with 'close, clean' cuts - looks good.
# table(cat_helper(custom_brks = c(100,200,300,500,1200),
#                  my_data    = colombia, 
#                  my_var     = 'km2'))

colombia$km2_quant <- cat_helper(custom_brks = c(100,200,300,500,1200),
                                 my_data    = colombia, 
                                 my_var     = 'km2',
                                 prec       = 0,
                                 small_n    = 0)

# colombia$km2_ln_quant <- cat_helper(no_classes = set_brks,
#                                     my_data    = colombia, 
#                                     my_var     = 'km2_ln',
#                                     prec       = 0)

# map_var <- 'km2_quant'
map_var <- 'icews_farc_quant'

scale_brks   <- levels(colombia[,map_var,drop = T])
# scale_brks   <- rev(levels(colombia$km2_quant))
# scale_labels <- rev(scale_brks)
scale_labels <- scale_brks

# scale_brks
# scale_labels

# scale_colors <- rev(ggthemes::economist_pal()(set_brks))
scale_colors <- rev(viridis(length(scale_brks), alpha = 0.8))
scale_colors <- viridis(length(scale_brks), alpha = 0.8)[c(4,6)]
scale_colors <- viridis(6, alpha = 0.8)[c(4,1)]

# plot(x = c(1,2), y = c(1,2), pch = 16, cex = 4 , col = scale_colors)

# ----------------------------------- #
# Note: As of 1/20/20, I feel like these are perfect dims.
#       Recommending to future self to NOT change.

gg_height = 6
gg_width  = 0.75 * gg_height
# ----------------------------------- #

# PRODUCE MAP -----------------------------------------------------------------
# p<-cat_map(my_data = colombia,
#            my_var  = 'km2_ln_quant',
#            border_color = 'transparent',
#            map_title = 'Colombia: Municipality Area',
#            map_subtitle = 'n = 1116',
#            map_caption  = paste0('Plot date: ',format(lubridate::today(), "%A, %B %d, %Y.\n"),
#                                  'Area reported in square kilometers.')) + 
#   cat_scale(scale_color  = scale_colors,
#             scale_name   = '',
#             scale_brks   = scale_brks,
#             scale_labels = scale_labels,
#             gg_width = gg_width)

# SAVE MAP --------------------------------------------------------------------

# ----------------------------------- #
# Future note - may need to adjust 
# legend label size. The parameter for this 
# is set in the cat_scale function
# ----------------------------------- #

# ggsave(filename = 'Plots/tst3.png',
#        p, height = gg_height, width = gg_width, units = 'in')







# May not be helpful now since code below is written, but just in case:
# https://stackoverflow.com/questions/52979035/ggplot2-top-legend-key-symbol-size-changes-with-legend-key-label

# SEE DIVERGENT GGPLOT PALETTES HERE:
# http://rstudio-pubs-static.s3.amazonaws.com/5312_98fc1aba2d5740dd849a5ab797cc2c8d.html

p<-cat_map(my_data = colombia,
        my_var  = map_var,
        border_color = 'transparent',
        map_title = 'Colombia: Municipality Area',
        map_subtitle = 'n = 1116',
        map_caption  = paste0('Plot date: ',format(lubridate::today(), "%A, %B %d, %Y.\n"),
                              'Area reported in square kilometers.')) +
  
  scale_fill_manual(values = scale_colors,
                    breaks = scale_brks,
                    drop   = FALSE,
                    labels = scale_labels) +
  
  theme(legend.position="bottom",
        legend.key.width=unit((gg_width - 2) / set_brks, "in"),
        legend.key.height = unit(3,'mm'),
        legend.direction = 'horizontal',
        legend.title = element_blank())+
  guides(fill = guide_legend(name = '',
                             label.position = "bottom", 
                             reverse = F, 
                             nrow = 1,
                             label.theme = element_text(size = 8)))

ggsave(filename = 'Plots/icews2.png',
       p, height = gg_height, width = gg_width, units = 'in')

# UGH... so much time fucking around and this is the format I want. MUCH simpler!
# update above functions to reflect this!

