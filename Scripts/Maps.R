
#-----------------------------------------------------------------------------#
#                                                                             #
# Author:   Logan Stundal                                                     #
# Date:     December 04, 2019                                                 #
# Purpose:  Colombia - Event Data Project - Data Tidying                      #
#                                                                             #
#-----------------------------------------------------------------------------#

# Note -- Default encoding: ISO8859-1.
#         Changed to UTF-8 on Nov. 16 to save shekel symbol in income factor

# Administrative --------------------------------------------------------------

rm(list=ls())

# To tidy data
library(tidyverse)
library(scales)
# library(gridExtra)
library(margins)

setwd("C:/Users/logan/GoogleDrive/UMN/research/ra_john/event_data_project/scripts")

# LOAD DATA
# d <- haven::as_factor(haven::read_dta('Data/iPanel Full Study.dta'))
load('../data/colombia_polmeth19.rdata')

# Data formatting -------------------------------------------------------------
#-----------------------------------------------------------------------------#
