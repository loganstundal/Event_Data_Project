#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          January 19, 2020
# Purpose:       Function for rapidly producing consistent categorical maps for
#                continuous or count variables analyzed in this project.
#
#
# Copyright (c): Logan Stundal, 2020
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------#
#
# Notes:
#       -- This script contains three functions:
#
#          cat_helper() - generates breaks for continuous or count variables to 
#                         produce factor variable for categorical mapping.
#                       - Intended to be used separately from mapping functions 
#                         to create new variable in sf object data frame.
#
#          cat_scale()  - Generates a custom color scales for choropleth maps.
#                         This can be used independently or in conjunction with
#                         the cat_map function, such as: cat_map() + cat_scale()
#
#          cat_map()    - Simple map producer that can be used for any varible type.
#                         Takes fill parameter as a string value.
#
#-----------------------------------------------------------------------------#



# ADMINISTRATIVE --------------------------------------------------------------

#---------------------------#
# Attach required libraries
#---------------------------#
require(sf)
require(ggplot2)
require(viridis)

# FUNCTIONS -------------------------------------------------------------------

cat_helper <- function(no_classes = 6,
                       my_data,
                       my_var,
                       prec = 2,
                       custom_brks = NULL,
                       small_n = 2){
  
  # Extract original variable
  orig_var = my_data[, my_var, drop = T]
  # Create an empty vector to store rounded labels
  lbls  = c()
  
  
  # Custom breaks
  if(!is.null(custom_brks)){
    # Find extremes
    minVal <- min(orig_var, na.rm = T)
    maxVal <- max(orig_var, na.rm = T)
    
    # Create labels
    brks <- c(minVal, custom_brks, maxVal)
    
    # Round label extemes according to precision
    # labels <- lapply(1:length(brks), function(x){
    #   lbls = c(lbls,
    #            round(brks[x +1], 0))
    # })
    
    labels <- lapply(2:length(brks), function(x){
      
      lbls = c(lbls, paste0(format(round(brks[x], prec)+1, nsmall = small_n),
                            ' - ',
                            format(round(brks[x+1], prec), nsmall = small_n)))
    })
    
    labels <- c(paste0(format(round(minVal, prec)), ' - ', format(round(brks[2], prec))), labels)
    
    
  } else {
    
    # Estimated breaks - Equal intervals
    brks = quantile(x     = orig_var,
                    probs = seq(0, 1, length.out = no_classes + 1))
    
    # Create custom labels
    labels <- lapply(1:length(brks),
                     function(x){
                       lbls = c(lbls, paste0(format(round(brks[x], prec),
                                                    nsmall = 2),
                                             ' - ',
                                             format(round(brks[x+1], prec),
                                                    nsmall = 2)))
                     })
  }
  
  
  # Drop the last label which will be NA by default
  labels <- labels[1:length(labels)-1]
  
  # Return quantile breaks factor
  return(cut(x      = orig_var,
             breaks = brks,
             labels = labels,
             include.lowest = TRUE))
}


# --------------------------------------------------------------------------- #

cat_scale <- function(scale_color, 
                      scale_name,
                      scale_brks,
                      scale_labels,
                      gg_width){
  scale_fill_manual(
    values = scale_color,
    breaks = scale_brks,
    name   = scale_name,
    drop   = FALSE,
    labels = scale_labels,
    guide  = guide_legend(
      direction = "horizontal",
      keyheight = unit(2, units = "mm"),
      keywidth  = unit(70 / length(scale_labels), units = 'mm'),
      # keywidth  = unit(.25 * gg_width, units = "in"),
      title.position = "top",
      label.position = "bottom",
      title.hjust    = 0.5,
      label.hjust    = 1,
      nrow    = 1,
      byrow   = T,
      reverse = F))
}

# --------------------------------------------------------------------------- #

cat_map <- function(my_data,
                    my_var,
                    map_title    = NULL,
                    map_subtitle = NULL,
                    map_caption  = NULL,
                    border_color = 'transparent',
                    background_color = 'transparent',
                    background_grid  = TRUE){
  # Build base map
  ggplot(data = my_data) +
    geom_sf(aes_string(fill = my_var), color = border_color, size = 0.1) +
    coord_sf() +
    
    # Apply custom theme
    theme_map(background = background_color) +
    
    labs(title    = map_title,
         subtitle = map_subtitle,
         caption  = map_caption)
}

# -----------------------------------------------------------------------------

