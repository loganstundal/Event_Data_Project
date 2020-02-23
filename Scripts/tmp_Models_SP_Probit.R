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
# Notes:                                                                    
#           - wordspace, matrix row normalization
#             https://www.rdocumentation.org/packages/wordspace/versions/0.2-6/topics/rowNorms
#             
#           - spatial probit docs
#             https://cran.r-project.org/web/packages/ProbitSpatial/ProbitSpatial.pdf
#
#           - bivand's spatial regression repo
#             https://cran.r-project.org/web/views/Spatial.html
#
#           - an autologistic function, but may be limited to dv lag
#             https://www.rdocumentation.org/packages/ngspatial/versions/1.2-1/topics/autologistic
#
#           - This may be of more help for general models [e.g., poisson, negative binomial]
#             https://rpubs.com/corey_sparks/111362
#
#           - Bayesian spatial error model - numerical integration is likely to brute force this thing out.
#             https://cran.r-project.org/web/packages/spatialprobit/spatialprobit.pdf
#             https://journal.r-project.org/archive/2013/RJ-2013-013/RJ-2013-013.pdf   # No option to extract res
#
#           - https://github.com/AllanClark/Rcppocc
#             This ^ may work. Article citing it here:
#             https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6362608/
#
#           - So, Rob Franze has some really ugly slide on qualitative DVs here
#             http://www-personal.umich.edu/~franzese/ModelsTSCS.9.S-EcomexModsInterdep.TAMU2011.pdf
# 
#             Jude mentioned as working on this - he may have mcmc code?
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
load('c:/users/logan/googledrive/umn/research/ra_john/event_data_project/data/colombia.rdata')

#---------------------------#
# Set working directory     
#---------------------------#
# library(ProbitSpatial)           # this is producing errors with no internet search results
# library(spatialprobit)           # okay, this seems great, but no option to extract random effects
# library(spatialreg)
# library(Rcppocc)                 # not looking great...
library(spdep)
library(sp)
library(sf)



#---------------------------#
# Load data                 
#---------------------------#


#---------------------------#
# Load functions            
#---------------------------#


#---------------------------#
# Example
#---------------------------#

# replicate LeSage et al. (2011), Table 3, p.1017
# require(spdep)
# 
# data(Katrina)
# attach(Katrina)
# # (a) 0-3 months time horizon
# # LeSage et al. (2011) use k=11 nearest neighbors in this case
# nb <- knn2nb(knearneigh(cbind(Katrina$lat, Katrina$long), k=11))
# listw <- nb2listw(nb, style="W")
# W1 <- as(as_dgRMatrix_listw(listw), "CsparseMatrix")
# fit1_cond <- SpatialProbitFit(y1 ~ flood_depth + log_medinc + small_size +
#                                 large_size +low_status_customers + high_status_customers +
#                                 owntype_sole_proprietor + owntype_national_chain,
#                               W=W1, data=Katrina, DGP='SAR', method="conditional", varcov="varcov")
# summary(fit1_cond)
# 
# residuals.SpatialProbit(fit1_cond)

# SPATIAL WEIGHTS -------------------------------------------------------------
# colombia.shp <- colombia[,c('ID_Mun','geometry')]
# colombia.shp$centroid_mun_proj <- NULL
# colombia.shp <- st_set_geometry(colombia.shp, colombia.shp$geometry)
# colombia.shp <- as_Spatial(from = colombia.shp, 
#                            IDs  = colombia.shp$ID_Mun)
# 
# 
# # Estimate spatial neighbors - queen's contiguity matrix and lists:
# nb.r        <- poly2nb(pl        = colombia.shp,                    
#                        row.names = colombia.shp$ID_Mun,                 
#                        queen     = TRUE)                            
# listw       <- nb2listw(nb.r, style = "W")
# 
# W           <- as(as_dgRMatrix_listw(listw), 'CsparseMatrix')
# 
# rm(colombia.shp)

W <- as(W_matrix, "sparseMatrix")
diag(W) < -0

# TIDY DATA -------------------------------------------------------------------


colombia$icews_farc_bin <- ifelse(colombia$icews_farc >0,1,0)
table(colombia$icews_farc_bin)

# W <- wordspace::normalize.rows(M = W_matrix, method = 'manhattan')

# summary(Matrix::rowSums(W))
# any(abs(Matrix::rowSums(W)-1)>1e-12)


f1 <- formula(icews_farc_bin ~ google_ee_forest_per + area_km2_ln + border_international + distance_50k_sa_km)

X <- cbind(intercept   = 1, 
           forest      = colombia$google_ee_forest_per, 
           area        = colombia$area_km2_ln,
           border      = colombia$border_international,
           distance50k = colombia$distance_50k_sa_km,
           bogota      = colombia$bogota_dummy)
y <- colombia$icews_farc_bin

sp_probit1 <- semprobit(formula      = y ~ X -1,
                        W            = W, 
                        ndraw        = 5e3, 
                        burn.in      = 1e3, 
                        thinning     = 10, 
                        prior        = NULL,
                        showProgress = TRUE)

summary(sp_probit1)

# sp_probit1 <- SpatialProbitFit(formula = f1, 
#                                data    = colombia,
#                                W       = W,
#                                DGP     = 'SEM',
#                                method  = 'conditional')
# 
# sp_probit1_summary <- summary(sp_probit1)




# ANALYSIS --------------------------------------------------------------------

# Extract residuals from spatial probit:
# sp_probit1_res <- residuals.SpatialProbit(sp_probit1)


# SAVE ------------------------------------------------------------------------
save()
rm(list = ls())


