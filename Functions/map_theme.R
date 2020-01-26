#-----------------------------------------------------------------------------#
#                                                                             
# Author:        Logan Stundal                                                    
# Date:          January 19, 2020                                                 
# Purpose:       Function for mapping theme for Colombia Project, version 1                      
#                                                                             
#
# Copyright (c): Logan Stundal, 2020                      
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------# 
#
# Notes:                                                                    
#         -- This script provides a default plotting theme to apply for all maps
#            created for this project.
#                                                                             
#-----------------------------------------------------------------------------#



# ADMINISTRATIVE --------------------------------------------------------------

#---------------------------#
# Attach required libraries   
#---------------------------#
require(sf)
require(ggplot2)


# FUNCTION --------------------------------------------------------------------

# Alternative good background color: "transparent"

theme_map <- function(background = '#f5f5f2',
                      grid_size  = 0.2,
                      # gg_width   = 6,
                      ...) {
  theme_minimal() +
    theme(
      axis.line         = element_blank(),
      axis.text         = element_blank(),
      axis.ticks        = element_blank(),
      axis.title        = element_blank(),
      
      panel.border      = element_blank(),
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_line(color = "#ebebe5", 
                                       size  = grid_size),
      
      plot.background   = element_rect(fill  = background, 
                                       color = NA), 
      panel.background  = element_rect(fill  = background, 
                                       color = NA),
      
      legend.background = element_rect(fill  = background, 
                                       color = NA),
      # legend.key.size   = unit(.2, 'lines'),
      # legend.text       = element_text(size = 10),
      # legend.key.width  = unit(.25 * gg_width, 'in'),
      legend.position   = "bottom",
      ...
    )
}




# -----------------------------------------------------------------------------

