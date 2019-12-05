
#-----------------------------------------------------------------------------#
#                                                                             #
# Author:   Logan Stundal                                                     #
# Date:     December 04, 2019                                                 #
# Purpose:  Colombia - Event Data Project - Data Tidying                      #
#                                                                             #
#-----------------------------------------------------------------------------#



# ADMINISTRATIVE --------------------------------------------------------------

rm(list=ls())

library(tidyverse)
library(scales)
library(sf)
library(grid)
library(gridExtra)
library(leaflet)      # https://rstudio.github.io/leaflet/

setwd("C:/Users/logan/GoogleDrive/UMN/research/ra_john/event_data_project/")

# LOAD DATA
load('data/colombia.RData')

# Create a simple data frame for quick reviewing, geometry is very large.
colombia2          <- colombia
colombia2$geometry <- NULL

rm(nb.lst, nb.r) # Remove regression weight matrices


#-------------------------------------#
# CREATE CUSTOM COLOR PALETTES        #
#-------------------------------------#

cuts   <- 8

# Palettes
gray.pal  <- colorNumeric(c('gray70','gray30'), 1:(cuts+1))
blue.pal  <- colorNumeric(c("slategray1", "royalblue1"), 1:(cuts+1))
green.pal <- colorNumeric(c('palegreen2','forestgreen'), 1:(cuts+1))
red.pal   <- colorNumeric(c('lightcoral','red4'),1:(cuts+1))

# ALL COLORS
cols    <- list( '_Multi_Color' = list(
  'Blues'  = blue.pal(1:(cuts+1)),
  'Reds'   = red.pal(1:(cuts+1)),
  'Greens' = green.pal(1:(cuts+1)),
  'Grays'  = gray.pal(1:(cuts+1))
));rm(blue.pal, gray.pal, green.pal, red.pal, cuts)


# CUSTOM PLOTTING FUNCTIONS ---------------------------------------------------
#-----------------------------------------------------------------------------#
{
# Plotting function
  .my_plot <- function(var, 
                       title        = NULL,
                       subtitle     = NULL,
                       caption      = NULL,
                       scale_name   = NULL,
                       border_color = NA){
  
    pal_col = case_when(str_starts(str_to_lower(var), 'icews') ~ cols$`_Multi_Color`$Blues,
                        str_starts(str_to_lower(var), 'ged')   ~ cols$`_Multi_Color`$Greens,
                        str_starts(str_to_lower(var), 'cinep') ~ cols$`_Multi_Color`$Reds)
    
    
    ggplot(data = colombia) +
      geom_sf(aes_string(fill = var), color = border_color) +
      scale_fill_gradientn(name = scale_name,
                           colors = pal_col,
                           labels = function(x) formatC(x, digits = 2, 
                                                        width = 5, format = "f", flag = "0")) + 
      labs(title    = title,
           subtitle = subtitle,
           caption  = caption) + 
      theme_void() +
      theme(plot.title           = element_text(size = .title.size, face = 'bold'),
            plot.caption         = element_text(size = .caption.size),
            legend.position      = c(0.95, 1.00),
            legend.justification = c( "right", "top"),
            legend.box.just      = "right",
            legend.margin        = margin(6, 6, 6, 6),
            legend.key.width     = unit(0.55, "cm"),
            legend.key.height    = unit(0.65, 'cm'),
            # legend.title.align   = 1
            # legend.text          = element_text(size = 12),
            # legend.title         = element_text(size = 14)
            ) 
  }

  # Individual plot saver
  .ind_saver <- function(plt, name){
    ggsave(plt, 
           width  = .ind.width,
           height = .ind.height, 
           units  = 'in',
           dpi    = 300,
           filename = paste0('Plots/temp/', name, '.png'))

  # Grid plot saver function
  .plot_saver <- function(plot_c, main_title, grp.width, grp.height, cols, rows,
                          file_name){
    
    lapply(1:length(plot_c), function(x){
      .ind_saver(get(plot_c[x]),
                 paste0('temp',x))
    })
    
    tmp = lapply(paste('Plots/temp', list.files('Plots/temp'), sep = '/'), png::readPNG)
    file.remove(paste('Plots/temp', list.files('Plots/temp'), sep = '/'))
    
    tmp = lapply(tmp, grid::rasterGrob)
  
    tmp = gridExtra::arrangeGrob(grobs = tmp,
                                 ncol  = cols,
                                 nrow  = rows,
                                 top   = main_title)
    
    ggsave(plot     = tmp, 
           filename = paste0('Plots/', file_name, '.png'),
           width    = grp.width,
           height   = grp.height,
           dpi      = 320)
    
  }
}

}



# PLOTTING PARAMETERS ---------------------------------------------------------
#-----------------------------------------------------------------------------#


# FONT SIZES, standard units
.title.size   = 12
.caption.size = 10

# PLOTTING PARAMETERS - INCHES
# Individual (temporary) plots)
.ind.width  = 4.0
.ind.height = 7.0

# Grouped plot
.grp.width  = 6.5
.grp.height = 5.0


# PLOTS -----------------------------------------------------------------------
#-----------------------------------------------------------------------------#

# UNCHANGING PLOTS -------------------#
#-------------------------------------#
cinep       <- .my_plot(var   = 'CINEP',
                        title = 'CINEP')

cinep.sc    <- .my_plot(var   = 'CINEP_scale',
                         title = 'CINEP - Scaled')

ged.orig    <- .my_plot(var   = 'ged_original',
                        title = 'GED, Original')

ged.orig.sc <- .my_plot(var   = 'ged_original_scale',
                        title = 'GED, Original - Scaled')


# PLOT 1 - ICEWS ORIGINAL ------------#
#-------------------------------------#
icews.orig    <- .my_plot(var   = 'icews_original',
                          title = 'ICEWS, Original')

icews.orig.sc <- .my_plot(var   = 'icews_original_scale',
                          title = 'ICEWS, Original - Scaled')

.plot_saver(plot_c     = c('icews.orig', 'ged.orig', 'cinep',
                           'icews.orig.sc','ged.orig.sc','cinep.sc'),
            main_title = 'Original variables',
            grp.width  = .grp.width,
            grp.height = .grp.height,
            cols       = 3,
            rows       = 2,
            file_name  = 'GROUPED_ORIGINAL')


# PLOT 2 - NO UNKNOWN ACTORS ---------#
#-------------------------------------#
icews.nounk    <- .my_plot(var   = 'icews_noUNK',
                           title = 'ICEWS, No Unknowns')
icews.nounk.sc <- .my_plot(var   = 'icews_noUNK_scale',
                           title = 'ICEWS, No Unknowns - Scaled')

.plot_saver(plot_c     = c('icews.nounk', 'ged.orig', 'cinep',
                           'icews.nounk.sc','ged.orig.sc','cinep.sc'),
            main_title = 'ICEWS - Excludes Unknown Actors',
            grp.width  = .grp.width,
            grp.height = .grp.height,
            cols       = 3,
            rows       = 2,
            file_name  = 'GROUPED_NOUNK')


# PLOT 3 - FARC ONLY -----------------#
#-------------------------------------#
icews.farc    <- .my_plot(var   = 'icews_farc',
                          title = 'ICEWS, FARC Only')
icews.farc.sc <- .my_plot(var   = 'icews_farc_scale',
                          title = 'ICEWS, FARC Only - Scaled')

ged.farc    <- .my_plot(var   = 'ged_farc',
                        title = 'GED, FARC Only')
ged.farc.sc <- .my_plot(var   = 'ged_farc_scale',
                        title = 'GED FARC Only - Scaled')

.plot_saver(plot_c     = c('icews.farc', 'ged.farc', 'cinep',
                           'icews.farc.sc','ged.farc.sc','cinep.sc'),
            main_title = 'FARC Events',
            grp.width  = .grp.width,
            grp.height = .grp.height,
            cols       = 3,
            rows       = 2,
            file_name  = 'GROUPED_FARC')




# PLOT 2 - ICEWS, NO UNKNOWN ---------#


# PLOT 3 - ICWES/GED - FARC ONLY -----#



# ICEWS, GED - RAW VALUES ------------#
#-------------------------------------#
# dev.new(width=5, height=4, unit="in")

# ICEWS - Variable construction used at Polmeth 2019.
icews1 <- my_plot(var        = 'icews_original_scale',
                  title      = 'ICEWS - Original, Counts')

# ICEWS - New variable construction which excludes all unknown actor types.
icews2 <- my_plot(var        = 'icews_noUNK_scale',
                  title      = 'ICEWS - No Unknown Actors, Counts')

# ICEWS - New variable construction which includes only FARC perpetuated events.
icews3 <- my_plot(var        = 'icews_farc_scale',
                  title      = 'ICEWS - FARC only, Counts')

ged1 <- my_plot(var        = 'ged_original_scale',
                title      = 'GED - Original, Counts')

ged2 <- my_plot(var        = 'ged_noUNK_scale',
                title      = 'GED - No Unknown Actors, Counts')

ged3 <- my_plot(var        = 'ged_farc_scale',
                  title      = 'GED - FARC only, Counts')


ggsave('plots/icews_original_scale.png', icews1)
ggsave('plots/icews_noUNK_scale.png', icews2)
ggsave('plots/icews_farc_scale.png', icews3)
ggsave('plots/ged_original_scale.png', ged1)
ggsave('plots/ged_noUNK_scale.png', ged2)
ggsave('plots/ged_farc_scale.png', ged3)


#-------------------------------------#
#                                     #
#-------------------------------------#

rl = lapply(paste('plots',list.files('plots'),sep = '/'), png::readPNG)
gl = lapply(rl, grid::rasterGrob)
gridExtra::grid.arrange(grobs=gl,
                        ncol = 3, 
                        nrow = 2)

#-------------------------------------#
#                                     #
#-------------------------------------#


#-------------------------------------#
# Summary of Event data values 
#-------------------------------------#

summary(colombia2[names(colombia2)[4:28]])


vars <- c('icews_original','icews_noUNK','icews_farc',
          'ged_original','ged_farc',
          'CINEP')

knitr::kable(sapply(vars, function(x) table(colombia2[x]>0)))

# Cases lost - ged (original to farc)
round(100 * ((193-177) / 193),2)

# Cases lost - ices (original to farc)
round(100 * ((277-175) / 277),2)



