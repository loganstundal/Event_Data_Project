#-----------------------------------------------------------------------------#
#                                                                             
# Author:        Logan Stundal                                                    
# Date:          February 23, 2020                                                 
# Purpose:       Note                      
#                                                                             
#
# Copyright (c): Logan Stundal, 2020                      
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------# 
#
# Notes:  Initial spatial probit models with colombia data                                                                  
#                                                                             
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
library(ProbitSpatial)
library(Matrix)
library(spdep)
library(ggplot2)
library(sp)
library(gridExtra)

#---------------------------#
# Set working directory     
#---------------------------#
setwd('c:/users/logan/googledrive/umn/research/ra_john/event_data_project')

#---------------------------#
# Load data                 
#---------------------------#
load('data/Colombia.Rdata')

#---------------------------#
# Load functions            
#---------------------------#



# TIDY DATA -------------------------------------------------------------------

# Create DV - Municipalities reporting FARC violence == 1, not reporting == 0
colombia$icews_farc_bin        <- as.integer(colombia$icews_farc > 0)

# Modified controls
colombia$distance_50k_co_km_ln <- log(colombia$distance_50k_co_km)
colombia$google_ee_pop_sum_ln  <- log(colombia$google_ee_pop_sum)

# Specify regression formula
model_formula <- . ~ border_international + distance_50k_co_km_ln + area_km2_ln + 
  google_ee_nl_index + google_ee_pop_sum_ln + google_terrain_ri_mean_m + google_ee_forest_per


# VANILLA PROBIT AND SPECIFICATION TESTING ------------------------------------

# Estimate vanilla probit
f0 <- glm(f1, data = colombia, family = binomial('probit'))
summary(f0)
data.frame('MODEL' = c(coef(fit0),NA), 'TRUE' = c(b, rho))

f0_res <- residuals.glm(f0, type = 'deviance')
f0_fit <- fitted(f0)

colombia$f0_fit <- f0_fit
colombia$f0_res <- f0_res

# Specification testing for spatial lag or spatial error process
# Spatial Lag - Moran's I using Latent Variable
moran_lag <- moran.mc(f0_fit, listw = nb.lst, nsim = 3000)
moran_lag

# Spatial Error - Moran's I using Naive Residuals
moran_res <- moran.mc(f0_res, listw = nb.lst, nsim = 3000)
moran_res

# LM Tests
lm_latent <- lm(formula = update(model_formula, f0_fit ~ .), data = colombia)
summary(lm_latent)

lm_all <- lm.LMtests(lm_latent, listw = nb.lst, test = "all")
lm_all

m_err <- spatialreg::errorsarlm(formula = update(model_formula, f0_fit ~ .), 
                                data    = colombia,
                                listw   = nb.lst)
summary(m_err)

m_lag <- spatialreg::lagsarlm(formula = update(model_formula, f0_fit ~ .), 
                              data    = colombia,
                              listw   = nb.lst)
summary(m_lag)



# QUICK LATENT VARIABLE / RESIDUALS MAP FROM LMs ------------------------------

mp_fit <- ggplot(data = colombia) + 
  geom_sf(aes(fill = f0_fit)) + 
  theme_minimal()

mp_res <- ggplot(data = colombia) + 
  geom_sf(aes(fill = f0_res)) + 
  theme_minimal()

grid.arrange(mp_fit, mp_res, ncol = 2)

# --------------------------------------------------------------------------- #


# SPATIAL PROBIT MODEL --------------------------------------------------------

# Row-standardization
# http://wlm.userweb.mwn.de/R/wlmRspma.htm
# wl <- mat2listw(as.matrix(W), style = 'W')
W <- spdep::listw2mat(nb.lst, style = 'W') # W = row-standardized
W <- as(W, 'dgCMatrix')

# Spatial Probit - Spatial Error
f1 <- SpatialProbitFit(formula = model_formula,
                       data    = colombia,
                       W       = W,
                       DGP     = 'SEM',
                       method  = 'conditional',
                       varcov  = 'varcov')
summary(f1, covar = T)                          # This takes a WHILE!

# Spatial Probit - Spatial Lag (just for checking)
f2 <- SpatialProbitFit(formula = model_formula,
                       data    = colombia,
                       W       = W,
                       DGP     = 'SAR',
                       method  = 'conditional',
                       varcov  = 'varcov')
summary(f2, covar = T)                          # This takes a WHILE!



# SAVE ------------------------------------------------------------------------
save()
rm(list = ls())









